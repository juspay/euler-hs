{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module EulerHS.ART.ReplayFunctions (
    callBrahmaReplayR,
    callBrahmaReplayA,
    callBrahmaReplayDB,
    callBrahmaReplayWithRespCodeCheck,
    -- callARTMockAndReplay,
    CallServantAPIReplayException (..),
) where

import Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import EulerHS.ART.Types (RecordingEntry (..))
import qualified EulerHS.Options ()
import EulerHS.Prelude hiding (Proxy)
import EulerHS.Types ()
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP

-- import qualified Network.HTTP.Client as NC

import Control.Exception
import qualified Data.HashMap.Strict as HM
import Data.Time.Clock (diffTimeToPicoseconds)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (AbsoluteTime, diffAbsoluteTime)
import EulerHS.ART.EnvVars as Env
import qualified EulerHS.Extra.Monitoring.Flow as EEMF
import qualified EulerHS.Framework.Language as L
import qualified EulerHS.Logger.Language as L
import qualified EulerHS.Logger.Types as LT
import qualified EulerHS.Runtime as R
import qualified GHC.Stack as GHC
import GHC.Stats (RTSStats (..), getRTSStats)
import Network.HTTP.Types.Status (Status (..))

getApiHIT :: (L.MonadFlow m) => R.FlowRuntime -> HTTP.Request -> m (HTTP.Response LBS.ByteString)
getApiHIT flowRt httpReq = do
    let mang = case HM.lookup "artManager" (R._httpClientManagers flowRt) of
            Just mngr -> mngr
            Nothing -> R._defaultHttpClientManager flowRt
    eresp <- L.runIO $ EulerHS.Prelude.try $! HTTP.httpLbs httpReq mang
    case eresp of
        Left (err :: SomeException) -> do
            let exception = fromMaybe (A.toJSON $ displayException err) $ (toJSON <$> (fromException err :: Maybe HTTP.HttpException))
            L.logErrorV @Text "MOCK_SERVER_API_CALL_FAILED" exception
            L.runIO $ throw (MockSeverApiCallFail "Mock server API call failed")
        Right val -> pure val

data ReplayDecodeException = ReplayDecodeException String deriving (Show, Exception)

data LOG = LOG
    { req :: A.Value
    , resp :: A.Value
    , urlpath :: Text
    , latency :: Integer
    , cpuTime :: Maybe Double
    , memAlloc :: Maybe Int32
    , gcTime :: Maybe Double
    , gcCounts :: Integer
    , majorGcCounts :: Integer
    , memBytesRts :: Integer
    }
    deriving (Generic, Show)
    deriving anyclass (ToJSON)

data MissingSessionID = MissingSessionID Text
    deriving (Generic, Show)
    deriving anyclass (ToJSON, Exception)

data MockSeverApiCallFail = MockSeverApiCallFail Text
    deriving (Generic, Show)
    deriving anyclass (ToJSON, Exception)

data CallServantAPIReplayException = CallServantAPIReplayException Text
    deriving (Generic, Show)
    deriving anyclass (ToJSON, Exception)

data VitalsTrackingStatus = VitalsTrackingStatus
    { sysTime :: AbsoluteTime
    , rtsStats :: RTSStats
    , cpuStats :: Int64
    , memStats :: Int32
    }
    deriving (Show)

getEntityFromRecordingEntry :: RecordingEntry -> Text
getEntityFromRecordingEntry recEntry = case recEntry of
    CallAPIEntryT _ -> "CallAPIEntryT"
    RunDBEntryT _ -> "RunDBEntryT"
    RunKVDBEntryT _ -> "RunKVDBEntryT"
    RunInMemEntryT _ -> "RunInMemEntryT"
    ForkFlowEntryT _ -> "ForkFlowEntryT"
    TimeStampEntryT _ -> "TimeStampEntryT"
    RandomRIOEntryT _ -> "RandomRIOEntryT"
    RandomBytesEntryT _ -> "RandomBytesEntryT"
    RunIOWithArtEntryT _ -> "RunIOWithArtEntryT"
    GlobalOptionsEntryT _ -> "GlobalOptionsEntryT"
    ARTConfigEntryT _ -> "ARTConfigEntryT"
    UuidEntryT _ -> "UuidEntryT"

nanosecondsToMilliseconds :: Int64 -> Double
nanosecondsToMilliseconds nanoseconds = fromIntegral nanoseconds / 1e6

logMockServerRequest :: (L.MonadFlow m, L.Loggable val) => Text -> LT.Action -> LT.Entity -> LT.Latency -> LT.RespCode -> val -> m ()
logMockServerRequest tag http_method api_tag lat resp_code message =
    L.evalLogger' $ L.masterLogger LT.Debug tag "OUTGOING_API_MOCK_SERVER" (Just $ Text.toUpper http_method) Nothing Nothing (Just api_tag) Nothing (Just lat) Nothing Nothing Nothing Nothing (Just resp_code) (LT.Message Nothing (Just $ L.toLogValue message)) Nothing

getVitalsTrackingStatus :: (L.MonadFlow m) => m VitalsTrackingStatus
getVitalsTrackingStatus = do
    rts <- L.runIO getRTSStats
    (cpu, mem, _) <- L.runIO $ EEMF.threadStat
    ts <- L.runIO $ systemToTAITime <$> getSystemTime
    pure
        $ VitalsTrackingStatus
            { sysTime = ts
            , rtsStats = rts
            , cpuStats = cpu
            , memStats = mem
            }

callBrahmaReplay :: (HasCallStack, L.MonadFlow m) => Text -> RecordingEntry -> Text -> m (HTTP.Response LBS.ByteString)
callBrahmaReplay mockUrl recEntry sessionId = do
    start <- getVitalsTrackingStatus
    flowRt <- L.getFlowRuntime
    let url = (Text.replace "__VERSION__" replayVersion getMockServerURL) <> mockUrl <> "?guuid=" <> sessionId
    httpRequest <- HTTP.parseRequest $ Text.unpack url
    let !finalRequest = httpRequest { HTTP.requestBody = HTTP.RequestBodyBS (LBS.toStrict $ A.encode recEntry), HTTP.method = "POST"}
    eResponse <- getApiHIT flowRt finalRequest
    end <- getVitalsTrackingStatus
    let lat = div (diffTimeToPicoseconds $ diffAbsoluteTime end.sysTime start.sysTime) picoMilliDiff
        cpuTimeV = nanosecondsToMilliseconds (end.cpuStats - start.cpuStats)
        memBytes = end.memStats - start.memStats
        gcTimeV = nanosecondsToMilliseconds $ (gc_elapsed_ns end.rtsStats) - (gc_elapsed_ns start.rtsStats)
        gcCountsV = toInteger $ (gcs end.rtsStats) - (gcs start.rtsStats)
        majorGcCountsV = toInteger $ (major_gcs end.rtsStats) - (major_gcs start.rtsStats)
        memBytesRtsV = toInteger $ (allocated_bytes end.rtsStats) - (allocated_bytes start.rtsStats)
    L.runIO $ EEMF.updateApiLatencyMonitoring flowRt._optionsLocal Nothing lat
    logMockServerRequest "OUTGOING_API_MOCK_SERVER" "POST" (getEntityFromRecordingEntry recEntry) lat (HTTP.statusCode $ HTTP.responseStatus eResponse)
        $ LOG
            { req = toJSON recEntry
            , resp = fromMaybe (toJSON $ HTTP.responseBody eResponse) $ A.decode @A.Value $ HTTP.responseBody eResponse
            , urlpath = url
            , latency = lat
            , cpuTime = Just cpuTimeV
            , memAlloc = Just memBytes
            , gcTime = Just gcTimeV
            , gcCounts = gcCountsV
            , majorGcCounts = majorGcCountsV
            , memBytesRts = memBytesRtsV
            }
    pure eResponse

callBrahmaReplayR :: (HasCallStack, L.MonadFlow m) => RecordingEntry -> Maybe Text -> m LBS.ByteString
callBrahmaReplayR recEntry sessionId = do
    when shouldLogCallStackART $ L.logDebugV @Text "CALLSTACK_ART" $ GHC.prettyCallStack $ GHC.callStack
    sessId <- maybe (throw (MissingSessionID "Missing session-id")) pure sessionId
    eResponse <- callBrahmaReplay "/mockRedis" recEntry sessId
    pure $ HTTP.responseBody eResponse

callBrahmaReplayDB :: (HasCallStack, L.MonadFlow m) => RecordingEntry -> Maybe Text -> m LBS.ByteString
callBrahmaReplayDB recEntry sessionId = do
    when shouldLogCallStackART $ L.logDebugV @Text "CALLSTACK_ART" $ GHC.prettyCallStack $ GHC.callStack
    sessId <- maybe (throw (MissingSessionID "Missing session-id")) pure sessionId
    eResponse <- callBrahmaReplay "/mockDB" recEntry sessId
    pure $ HTTP.responseBody eResponse

callBrahmaReplayA :: (HasCallStack, L.MonadFlow m) => RecordingEntry -> Maybe Text -> m (LBS.ByteString, ByteString)
callBrahmaReplayA recEntry sessionId = do
    when shouldLogCallStackART $ L.logDebugV @Text "CALLSTACK_ART" $ GHC.prettyCallStack $ GHC.callStack
    sessId <- maybe (throw (MissingSessionID "Missing session-id")) pure sessionId
    eResponse <- callBrahmaReplay "/mockRedis" recEntry sessId
    pure $ (HTTP.responseBody eResponse, statusMessage $ HTTP.responseStatus eResponse)

callBrahmaReplayWithRespCodeCheck :: (HasCallStack, L.MonadFlow m) => RecordingEntry -> Maybe Text -> m (Either String LBS.ByteString)
callBrahmaReplayWithRespCodeCheck recEntry sessionId = do
    when shouldLogCallStackART $ L.logDebugV @Text "CALLSTACK_ART" $ GHC.prettyCallStack $ GHC.callStack
    sessId <- maybe (throw (MissingSessionID "Missing session-id")) pure sessionId
    eResponse <- callBrahmaReplay "/mockRedis" recEntry sessId
    let respStatusCode = HTTP.statusCode $ HTTP.responseStatus eResponse
    if respStatusCode == 200
        then pure . Right $ HTTP.responseBody eResponse
        else pure . Left $ "Mock server returned non 2XX response: " <> show respStatusCode

getMockServerURL :: Text
getMockServerURL =
    if isArtReplayEnabled
        then mockServerURL
        else mockServerURLV2

picoMilliDiff :: Integer
picoMilliDiff = 1000000000
