{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module EulerHS.Extra.Monitoring.Flow where

import           Control.Concurrent.MVar (modifyMVar)
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
import qualified Data.Map as Map
import           Unsafe.Coerce (unsafeCoerce)
import           EulerHS.Common (FlowGUID)
import           EulerHS.Options (OptionEntity, OptionKey, mkOptionKey)
import           EulerHS.Logger.Types (LogLevel(Info), Message(..))
import           EulerHS.Logger.Interpreter (runLogger)
import qualified Data.Text as T
import qualified Juspay.Extra.Env as Env
import GHC.Exts (threadCPUTime#)
import GHC.Int(Int64(..), Int32(..))
import GHC.IO(IO(..))

isLatencyMetricEnabled :: Bool
isLatencyMetricEnabled =
    let envType =  Env.JuspayEnv
                    { key = "LATENCY_METRIC_ENABLED"
                    , actionLeft = Env.mkDefaultEnvAction (False :: Bool)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

isForkFlowLatencyMetricEnabled :: Bool
isForkFlowLatencyMetricEnabled =
    let envType =  Env.JuspayEnv
                    { key = "FORK_FLOW_LATENCY_METRIC_ENABLED"
                    , actionLeft = Env.mkDefaultEnvAction (False :: Bool)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

logLatencyMetricLog :: (HasCallStack, L.MonadFlow m) => m ()
logLatencyMetricLog = do
    latencyMetric <- getLatencyMetric Nothing
    L.logInfoV ("LATENCY_METRIC" :: Text) latencyMetric

getLatencyMetric :: (HasCallStack, L.MonadFlow m) => Maybe Integer -> m (Maybe EEMT.IOLatencyMetric)
getLatencyMetric mbOverallLat =  
    if isLatencyMetricEnabled 
        then do
            mbDbMetric    <- L.getOptionLocal EEMT.DBMetricInfoKey <&> ((\(EEMT.DBMetricInfo x) -> convertToMs x) <$>)  
            mbRedisMetric <- L.getOptionLocal EEMT.RedisMetricInfoKey <&> ((\(EEMT.RedisMetricInfo x) -> convertToMs x) <$>)
            mbInternalApiMetric  <- L.getOptionLocal EEMT.InternalAPIMetricInfoKey
            mbExternalApiMetric  <- L.getOptionLocal EEMT.ExternalMetricInfoKey <&> ((\(EEMT.ExternalMetricInfo x) -> x) <$>)
            mKmsMetric <- L.getOptionLocal EEMT.KMSMetricInfoKey <&> ((\(EEMT.KMSMetricInfo x) -> convertToMs x) <$>)
            (pure . Just) $ EEMT.IOLatencyMetric mbDbMetric mbRedisMetric mbInternalApiMetric mbExternalApiMetric mKmsMetric (getEulerOverHead mbOverallLat mbInternalApiMetric mbExternalApiMetric)
        else pure Nothing
    where
        convertToMs (EEMT.LatencyInfo _latency _requests) = EEMT.LatencyInfo (_latency*1e-9) _requests
        getEulerOverHead (Just overallLat) (Just internalApiMetric) (Just externalApiMetric) = Just $ (fromIntegral overallLat) - internalApiMetric.outgoingLatency - externalApiMetric.latency
        getEulerOverHead (Just overallLat) Nothing (Just externalApiMetric) = Just $ (fromIntegral overallLat) - externalApiMetric.latency
        getEulerOverHead (Just overallLat) (Just internalApiMetric) Nothing = Just $ (fromIntegral overallLat) - internalApiMetric.outgoingLatency
        getEulerOverHead _ _ _ =  fromIntegral <$> mbOverallLat

internalApiLatencyHeader :: Text
internalApiLatencyHeader = "x-outgoingapi-overhead"

withMonitoringIO :: Maybe FlowGUID -> EEMT.LatencyHandle -> R.FlowRuntime -> IO a -> IO a
withMonitoringIO mbFlowGuid lantencyHandle flowRt func =
    if isLatencyMetricEnabled
        then do
            tick <- getCurrentDateInPico
            res <- func
            tock <- getCurrentDateInPico
            case lantencyHandle of
                EEMT.REDIS       -> incrementRedisLatencyMetric mbFlowGuid flowRt (tock-tick)
                EEMT.DB          -> incrementDBLatencyMetric flowRt (tock-tick)
            pure res
        else func

updateApiLatencyMonitoring :: MVar (Map OptionKey Any) -> Maybe Text -> Integer -> IO ()
updateApiLatencyMonitoring optionsLocal mbDownstreamLat overallLat = do 
    if isLatencyMetricEnabled
        then case mbDownstreamLat of 
                Just downstreamLat -> do
                   let downStreamLatParsed = fromMaybe 0 $ readMaybe $ T.unpack downstreamLat
                   (EEMT.InternalAPIMetricInfo oldOutgoingLat count oldLat) <- (fromMaybe defaultInternalAPIMetric) <$> getOptionLocalIO optionsLocal EEMT.InternalAPIMetricInfoKey
                   setOptionLocalIO optionsLocal EEMT.InternalAPIMetricInfoKey (EEMT.InternalAPIMetricInfo (oldOutgoingLat + downStreamLatParsed) (count + 1) (oldLat + (fromInteger overallLat)))
                Nothing -> do 
                    (EEMT.LatencyInfo oldLatency count) <- maybe defaultLatencyMetric (\(EEMT.ExternalMetricInfo x) -> x) <$> getOptionLocalIO optionsLocal EEMT.ExternalMetricInfoKey
                    setOptionLocalIO optionsLocal EEMT.ExternalMetricInfoKey $ EEMT.ExternalMetricInfo (EEMT.LatencyInfo (oldLatency + (fromInteger overallLat)) (count + 1))    
        else pure ()

refreshIOMetricLocalOptions :: R.FlowRuntime -> IO R.FlowRuntime
refreshIOMetricLocalOptions rt = do
    optionsLocalHM <- readMVar (R._optionsLocal rt)
    newOptionsLocal <- newMVar $ foldr'  Map.delete optionsLocalHM optionsArray
    return (rt {R._optionsLocal = newOptionsLocal})
    where
        optionsArray = [
            mkOptionKey EEMT.DBMetricInfoKey, 
            mkOptionKey EEMT.InternalAPIMetricInfoKey, 
            mkOptionKey EEMT.ExternalMetricInfoKey, 
            mkOptionKey EEMT.RedisMetricInfoKey,
            mkOptionKey EEMT.KMSMetricInfoKey
            ]

defaultInternalAPIMetric :: EEMT.InternalAPIMetricInfo
defaultInternalAPIMetric = EEMT.InternalAPIMetricInfo 0 0 0

getCurrentDateInPico :: IO Double
getCurrentDateInPico = do
  t <- getPOSIXTime
  let (MkFixed i) = nominalDiffTimeToSeconds t
  pure $ fromInteger i

defaultLatencyMetric :: EEMT.LatencyInfo
defaultLatencyMetric = EEMT.LatencyInfo 0 0

getOptionLocalIO :: forall k v. (OptionEntity k v) => MVar (Map OptionKey Any) -> k -> IO (Maybe v)
getOptionLocalIO _optionsLocal k = do
    m <- readMVar _optionsLocal
    let valAny = Map.lookup (mkOptionKey @k @v k) m
    pure $ unsafeCoerce valAny

setOptionLocalIO :: forall k v. (OptionEntity k v) => MVar (Map OptionKey Any) -> k -> v ->  IO ()
setOptionLocalIO _optionsLocal k v = do
    m <- takeMVar _optionsLocal
    let newMap = Map.insert (mkOptionKey @k @v k) (unsafeCoerce @_ @Any v) m
    putMVar _optionsLocal newMap

modifyOptionIO :: forall k v. (OptionEntity k v) => MVar (Map OptionKey Any) -> k -> (v -> v) -> IO (Maybe v, Maybe v)
modifyOptionIO _options k fn = do
  let k' = mkOptionKey @k @v k
  modifyMVar _options (modifyAndCallFn k')
    where
      modifyAndCallFn k' curOptions = do
        let valAny = Map.lookup k' curOptions
        case valAny of
          Nothing -> pure (curOptions,(Nothing,Nothing))
          Just val -> do
            let oldVal = unsafeCoerce val
                modifiedVal = fn oldVal
            pure (Map.insert k' (unsafeCoerce @_ @Any modifiedVal) curOptions,
                  (Just oldVal, Just modifiedVal)
                )

incrementDBLatencyMetric :: R.FlowRuntime -> Double -> IO ()
incrementDBLatencyMetric flowRt latency = when isLatencyMetricEnabled $ do
    (EEMT.LatencyInfo oldLatency count) <- maybe defaultLatencyMetric (\(EEMT.DBMetricInfo x) -> x) <$> getOptionLocalIO (R._optionsLocal flowRt) EEMT.DBMetricInfoKey
    setOptionLocalIO (R._optionsLocal flowRt) EEMT.DBMetricInfoKey $ EEMT.DBMetricInfo (EEMT.LatencyInfo (oldLatency + latency) (count + 1))

incrementRedisLatencyMetric :: Maybe FlowGUID -> R.FlowRuntime -> Double -> IO ()
incrementRedisLatencyMetric mbFlowGuid flowRt latency = when isLatencyMetricEnabled $  do
    when (latency > 500000000000) $ -- 500ms
          runLogger mbFlowGuid (R._loggerRuntime . R._coreRuntime $ flowRt)
            $ L.masterLogger Info ("REDIS_METRIC" :: String) "DOMAIN" (Just "REDIS_METRIC") (Nothing) Nothing Nothing Nothing (Just $ round latency) Nothing Nothing Nothing Nothing Nothing (Message Nothing (Just $ A.toJSON latency)) Nothing
    (EEMT.LatencyInfo oldLatency count) <- maybe defaultLatencyMetric (\(EEMT.RedisMetricInfo x) -> x) <$> getOptionLocalIO (R._optionsLocal flowRt) EEMT.RedisMetricInfoKey
    setOptionLocalIO (R._optionsLocal flowRt) EEMT.RedisMetricInfoKey $ EEMT.RedisMetricInfo (EEMT.LatencyInfo (oldLatency + latency) (count + 1))

statLowLevel :: IO (Int64, Int64, Int32, Int32)
statLowLevel = IO $ \s ->
   case threadCPUTime# s of
    (# s', sec, nsec, allocated, count_sched #) ->
        (# s', (I64# sec, I64# nsec, I32# allocated, I32# count_sched) #)

threadStat :: IO (Int64, Int32, Int32)
threadStat = do
    (a1, a2, a3, a4) <- statLowLevel
    let tenPow9 = 1000000000
        threadCPUTime = a1 * tenPow9 + a2 -- nanoseconds
        threadAllocated = a3 * 8 -- bytes
        threadSchedOut = a4     -- count (int)
    pure (threadCPUTime, threadAllocated, threadSchedOut)
