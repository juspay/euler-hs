{- |
Module      :  EulerHS.Extra.Monitoring.Flow
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module EulerHS.Extra.Monitoring.Flow where

import           GHC.Float (int2Double)
import           EulerHS.Prelude
import qualified Data.Aeson as A
import qualified EulerHS.Framework.Language as L
import qualified EulerHS.Logger.Language as L
import qualified EulerHS.Framework.Runtime as R
import qualified EulerHS.Logger.Runtime as R
import qualified EulerHS.Extra.Monitoring.Types as EEMT
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Time.Clock (nominalDiffTimeToSeconds)
import           Data.Fixed (Fixed (MkFixed))
import qualified Juspay.Extra.Config as Conf
import qualified Data.Map as Map
import           EulerHS.Api (ApiTag(..))
import           EulerHS.KVConnector.Types (MerchantID(..))
import           Unsafe.Coerce (unsafeCoerce)
import           EulerHS.Options (OptionEntity, mkOptionKey)
import           Euler.Events.MetricApi.MetricApi
import           EulerHS.Logger.Types (LogLevel(Info), Message(..))
import           EulerHS.Logger.Interpreter (runLogger)

isLatencyMetricEnabled :: Bool
isLatencyMetricEnabled = fromMaybe False $ readMaybe =<< Conf.lookupEnvT "LATENCY_METRIC_ENABLED"

isLatencyPromMetricEnabled :: Bool
isLatencyPromMetricEnabled = fromMaybe False $ readMaybe =<< Conf.lookupEnvT "LATENCY_PROM_METRIC_ENABLED"

withMonitoringIO :: EEMT.LatencyHandle -> R.FlowRuntime -> IO a -> IO a
withMonitoringIO lantencyHandle flowRt func =
    if isLatencyMetricEnabled
        then do
            tick <- getCurrentDateInMillisIO
            val <- func
            tock <- getCurrentDateInMillisIO
            case lantencyHandle of
                EEMT.REDIS -> incrementRedisLatencyMetric flowRt (tock-tick)
                EEMT.DB    -> incrementDBLatencyMetric flowRt (tock-tick)
                EEMT.API   -> incrementAPILatencyMetric flowRt (tock-tick)
            pure val
        else func

getCurrentDateInMillisIO :: IO Double
getCurrentDateInMillisIO = do
  t <- getPOSIXTime
  let (MkFixed i) = nominalDiffTimeToSeconds t
  pure $ fromInteger i * 1e-9

defaultLatencyMetric :: EEMT.LatencyInfo
defaultLatencyMetric = EEMT.LatencyInfo 0 0

getOptionLocalIO :: forall k v. (OptionEntity k v) => R.FlowRuntime -> k -> IO (Maybe v)
getOptionLocalIO R.FlowRuntime{..} k = do
    m <- readMVar _optionsLocal
    let valAny = Map.lookup (mkOptionKey @k @v k) m
    pure $ unsafeCoerce valAny

setOptionLocalIO :: forall k v. (OptionEntity k v) => R.FlowRuntime -> k -> v ->  IO ()
setOptionLocalIO R.FlowRuntime{..} k v = do
    m <- takeMVar _optionsLocal
    let newMap = Map.insert (mkOptionKey @k @v k) (unsafeCoerce @_ @Any v) m
    putMVar _optionsLocal newMap

incrementDBLatencyMetric :: R.FlowRuntime -> Double -> IO ()
incrementDBLatencyMetric flowRt latency = when isLatencyMetricEnabled $ do
    (EEMT.LatencyInfo oldLatency count) <- maybe defaultLatencyMetric (\(EEMT.DBMetricInfo x) -> x) <$> getOptionLocalIO flowRt EEMT.DBMetricInfoKey
    setOptionLocalIO flowRt EEMT.DBMetricInfoKey $ EEMT.DBMetricInfo (EEMT.LatencyInfo (oldLatency + latency) (count + 1))

incrementRedisLatencyMetric :: R.FlowRuntime -> Double -> IO ()
incrementRedisLatencyMetric flowRt latency = when isLatencyMetricEnabled $  do
    when (latency > 500) $
          runLogger (Nothing) (R._loggerRuntime . R._coreRuntime $ flowRt)
            . L.masterLogger Info ("REDIS_METRIC" :: String) "DOMAIN" (Just "REDIS_METRIC") (Nothing) Nothing (Just $ round latency) (Nothing) $ Message Nothing (Just $ A.toJSON latency)
    (EEMT.LatencyInfo oldLatency count) <- maybe defaultLatencyMetric (\(EEMT.RedisMetricInfo x) -> x) <$> getOptionLocalIO flowRt EEMT.RedisMetricInfoKey
    setOptionLocalIO flowRt EEMT.RedisMetricInfoKey $ EEMT.RedisMetricInfo (EEMT.LatencyInfo (oldLatency + latency) (count + 1))

incrementAPILatencyMetric :: R.FlowRuntime -> Double -> IO ()
incrementAPILatencyMetric flowRt latency = when isLatencyMetricEnabled $ do
    (EEMT.LatencyInfo oldLatency count) <- maybe defaultLatencyMetric (\(EEMT.APIMetricInfo x) -> x) <$> getOptionLocalIO flowRt EEMT.APIMetricInfoKey
    setOptionLocalIO flowRt EEMT.APIMetricInfoKey $ EEMT.APIMetricInfo (EEMT.LatencyInfo (oldLatency + latency) (count + 1))

logLatencyMetricLog :: (HasCallStack, L.MonadFlow m) => m ()
logLatencyMetricLog = when isLatencyMetricEnabled $ do
    dbMetric    <- L.getOptionLocal EEMT.DBMetricInfoKey <&> ((\(EEMT.DBMetricInfo x) -> x) <$>) >>= extractAndIncrementLatencyMetric EEMT.DB <&> fromMaybe A.Null 
    redisMetric <- L.getOptionLocal EEMT.RedisMetricInfoKey <&> ((\(EEMT.RedisMetricInfo x) -> x) <$>) >>= extractAndIncrementLatencyMetric EEMT.REDIS <&> fromMaybe A.Null
    apiMetric    <- L.getOptionLocal EEMT.APIMetricInfoKey <&> ((\(EEMT.APIMetricInfo x) -> x) <$>) >>= extractAndIncrementLatencyMetric EEMT.API <&> fromMaybe A.Null
    L.logInfoV ("LATENCY_METRIC" :: Text) (A.object $ [("dbMetric",dbMetric),("redisMetric",redisMetric),("apiMetric",apiMetric)])
    where
        extractAndIncrementLatencyMetric latencyHandle = \case 
            (Just (latencyInfo)) -> incrementKVMetric latencyHandle latencyInfo *> pure (Just $ A.toJSON latencyInfo)
            Nothing              -> pure Nothing


incrementKVMetric :: (HasCallStack, L.MonadFlow m) => EEMT.LatencyHandle -> EEMT.LatencyInfo -> m ()
incrementKVMetric latencyHandle latencyInfo  = do
  mHandle <- L.getOption EEMT.LatencyMetricCfg
  maybe (pure ()) (\handle -> do
      mid    <- fromMaybe "UNKNOWN" <$> L.getOptionLocal MerchantID 
      tag    <- fromMaybe "UNKNOWN" <$> L.getOptionLocal ApiTag
      L.runIO $ ((EEMT.latencyCounter handle) (latencyHandle, mid, tag, latencyInfo))) mHandle

mkIOLatencyMetricHandler :: IO EEMT.LatencyMetricHandler
mkIOLatencyMetricHandler = do
  metrics <- register collectionLock
  pure $ EEMT.LatencyMetricHandler $ \case
    (latencyHandle, tag, mid, EEMT.LatencyInfo{..}) -> do
      observe (metrics </> #io_count_observe) (int2Double _requests) latencyHandle tag mid
      observe (metrics </> #io_latency_observe) _latency latencyHandle tag mid

io_count_observe = histogram #io_count_observe
      .& lbl @"io_handle" @EEMT.LatencyHandle
      .& lbl @"tag" @Text
      .& lbl @"mid" @Text
      .& build

io_latency_observe = histogram #io_latency_observe
      .& lbl @"io_handle" @EEMT.LatencyHandle
      .& lbl @"tag" @Text
      .& lbl @"mid" @Text
      .& build

collectionLock =
     io_count_observe
  .> io_latency_observe
  .> MNil