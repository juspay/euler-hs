{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE BangPatterns     #-}

module EulerHS.Framework.Interpreter
  ( -- * Flow Interpreter
    runFlow
  , runFlow'
  , modify302RedirectionResponse
  , getConnTagFromDbName
  ) where

import           Control.Concurrent.MVar (modifyMVar)
import           Control.Exception (throwIO, throw)
import qualified Control.Exception as Exception
import qualified Control.Concurrent.Map as CMap
import qualified Data.Aeson as A
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.CaseInsensitive as CI
import qualified Data.DList as DL
import           EulerHS.Types(IsLoadTesting(..))
import           Data.Either.Extra (mapLeft)
import qualified Data.HashMap.Strict as HM
import           Data.IORef (readIORef, writeIORef)
import qualified Data.LruCache as LRU
import qualified Data.Cache.LRU as SimpleLRU
import qualified Data.Map as Map
import qualified Data.Pool as DP
import           Data.Profunctor (dimap)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import           Data.Time.Clock (diffTimeToPicoseconds)
import           Data.Time.Clock.System (getSystemTime, systemToTAITime)
import           Data.Time.Clock.TAI (diffAbsoluteTime)
import qualified Data.UUID.V4 as UUID (nextRandom)
import qualified Data.UUID as UUID
import           EulerHS.Api ( EulerClient(..), OutApiTag(..))
import           EulerHS.ApiHelpers ( runEulerClient)
import           EulerHS.BinaryString (LBinaryString (LBinaryString),
                                       getLBinaryString)
import           EulerHS.Common (Awaitable (Awaitable), FlowGUID,
                                 ManagerSelector (ManagerSelector),
                                 Microseconds (Microseconds))
import           EulerHS.Extra.Snowflakes.Flow (generateSnowflake')
import           EulerHS.Extra.Snowflakes.Types (SnowflakeError(Fatal), Snowflake)
import qualified EulerHS.Framework.Language as L
import           EulerHS.KVConnector.InMemConfig.Types
import           EulerHS.KVConnector.Utils ()
import qualified EulerHS.Framework.Runtime as R
import           EulerHS.HttpAPI (HTTPMethod (Connect, Delete, Get, Head, Options, Patch, Post, Put, Trace),
                                  HTTPRequest(..), HTTPResponse(..),
                                  HTTPResponse (..), buildSettings, AwaitingError(..), RequestType(..),
                                  defaultTimeout, getRequestBody,
                                  getRequestHeaders, getRequestMethod,
                                  getRequestRedirects, getRequestTimeout,
                                  getRequestURL, getResponseBody,
                                  getResponseCode, getResponseHeaders,
                                  getResponseStatus, maskHTTPRequest,maskHTTPResponse,
                                  mkHttpApiCallLogEntry, mkHttpApiCallLogEntryWithoutMasking, shouldBypassProxy, withOptionalHeader, hasHeader, withOptionalHeaders)
import           EulerHS.KVDB.Interpreter (runKVDB)
import           EulerHS.KVDB.Types (KVDBAnswer,
                                     KVDBConfig (KVDBClusterConfig, KVDBConfig),
                                     KVDBConn (Redis),
                                     KVDBError (KVDBConnectionAlreadyExists, KVDBConnectionDoesNotExist, KVDBConnectionFailed),
                                     KVDBReplyF (KVDBError), kvdbToNative,
                                     mkRedisConn, nativeToKVDB)
import           EulerHS.Logger.Interpreter (runLogger)
import qualified EulerHS.Logger.Language as L
import qualified EulerHS.Logger.Runtime as R
import           EulerHS.Logger.Types (LogLevel (Debug, Error, Info), LogEncryptionKey(..), LogEncryptionIV(..),
                                       Message (Message), Action , Category, Entity, ErrorL(..), Latency, RespCode, ErrorInfo(..), getTrackingIdsFromLogContext)
import           EulerHS.Prelude hiding (readIORef, writeIORef, toList)
import           EulerHS.PubSub.Interpreter (runPubSub)
import           EulerHS.SqlDB.Interpreter (runSqlDB)
import           EulerHS.SqlDB.Types (ConnTag,
                                      ModelDBConfig(..),
                                      DBConfig (MySQLPoolConf, PostgresPoolConf, SQLitePoolConf, MockingConf),
                                      DBError (DBError),
                                      DBErrorType (ConnectionAlreadyExists, ConnectionDoesNotExist, ConnectionFailed, UnrecognizedError),
                                      DBResult,
                                      NativeSqlConn (NativeMySQLConn, NativePGConn, NativeSQLiteConn),
                                      SqlConn (MySQLPool, PostgresPool, SQLitePool, MockingPool),
                                      bemToNative, mkSqlConn,
                                      mysqlErrorToDbError, nativeToBem,
                                      postgresErrorToDbError,
                                      sqliteErrorToDbError,
                                      MockingError(..))
import           GHC.Conc (labelThread)
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Client.Internal
import qualified Network.HTTP.Types as HTTP
import qualified Servant.Client as S
import           System.Process (readCreateProcess, shell)
import           Unsafe.Coerce (unsafeCoerce)
import qualified EulerHS.Extra.Monitoring.Flow as EEMF
import qualified EulerHS.Extra.Monitoring.Types as EEMT
import qualified Data.Bool as Bool
import EulerHS.ART.Types
import EulerHS.ART.Utils (shouldARTRecord, toErrorPayload,toServantResponse,fromServantRequest)
import EulerHS.ART.EnvVars (isArtReplayEnabled,shouldLogCallStackART,dbRetryAttemps, dbRetryTime, shouldRecordTimestamp, shouldRecordSetOption)
import EulerHS.ART.FlowUtils (readRecordingsAndWriteToFileForkFLow, shouldRecordForkFLow)
import qualified EulerHS.ART.ReplayFunctions as ER
import qualified GHC.Stack as GHC
import qualified Servant.Client.Free as SCF
import qualified Servant.Client as SC
import qualified Servant.Client.Internal.HttpClient as SCIHC
import qualified Servant.Client.Core as SCC
-- import           System.IO.Unsafe (unsafePerformIO)
import           EulerHS.Extra.Monitoring.Flow (logLatencyMetricLog, isForkFlowLatencyMetricEnabled)
import EulerHS.Compression (compressionDecider, compress, mkCompressionConfig, compressionHeader, logCompressionHelper)
import Streamly.Data.Array as Arr
import EulerHS.KVConnector.Types
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as NC
import qualified Data.Time as DT
import qualified Servant as S
import qualified Data.Time.Clock.POSIX as DT
import qualified EulerHS.EnvVars as Env
import qualified System.Random as SR
import qualified "cryptonite" Crypto.Random as CryptoRandom (MonadRandom(getRandomBytes))
import qualified Data.ByteString.Builder as BB
import qualified EulerHS.Extra.KafkaClient.Utils as KUtils
import qualified EulerHS.ART.V2.Utils as ARTUtils
import           EulerHS.ART.V2.Option
import           EulerHS.ART.V2.Types (ArtRecordable(..))
import qualified EulerHS.Extra.Redis as R
import qualified EulerHS.ART.V2.FlowUtils as ARTFlowUtils
import EulerHS.EnvVars (shouldLogEncryptedLogs, getHeadersToFilterFromOutgoingRequest)
import EulerHS.Encryption (aesEncryptText)
import qualified Data.Sequence as Seq

type W8Arr = Arr.Array Word8

connect :: DBConfig be -> IO (DBResult (SqlConn be))
connect cfg = do
  eConn <- try $ mkSqlConn cfg
  case eConn of
    Left (e :: SomeException) -> pure $ Left $ DBError ConnectionFailed $ show e
    Right conn                -> pure $ Right conn

connectRedis :: KVDBConfig -> IO (KVDBAnswer KVDBConn)
connectRedis cfg = do
  eConn <- try $ mkRedisConn cfg
  case eConn of
    Left (e :: SomeException) -> pure $ Left $ KVDBError KVDBConnectionFailed $ show e
    Right conn                -> pure $ Right conn

disconnect :: SqlConn beM ->   IO ()
disconnect (PostgresPool _ pool) = DP.destroyAllResources pool
disconnect (MySQLPool _ pool)    = DP.destroyAllResources pool
disconnect (SQLitePool _ pool)   = DP.destroyAllResources pool
disconnect (MockingPool _)       = pure ()

awaitMVarWithTimeout :: MVar (Either Text a) -> Int -> IO (Either AwaitingError a)
awaitMVarWithTimeout mvar mcs | mcs <= 0  = go 0
                              | otherwise = go mcs
  where
    portion = (mcs `div` 10) + 1
    go rest
      | rest <= 0 = do
        mValue <- tryReadMVar mvar
        pure $ case mValue of
          Nothing          -> Left AwaitingTimeout
          Just (Right val) -> Right val
          Just (Left err)  -> Left $ ForkedFlowError err
      | otherwise = do
          tryReadMVar mvar >>= \case
            Just (Right val) -> pure $ Right val
            Just (Left err)  -> pure $ Left $ ForkedFlowError err
            Nothing          -> threadDelay portion >> go (rest - portion)

-- | Utility function to convert HttpApi HTTPRequests to http-client HTTP
-- requests
getHttpLibRequest :: MonadThrow m => Bool -> HTTPRequest -> m HTTP.Request
getHttpLibRequest shouldCompress request = do
  let url = Text.unpack $ getRequestURL request
      (compressionConfig, mHeaderValForCompression) = mkCompressionConfig 
  httpLibRequest <- HTTP.parseRequest url
  let
    requestMethod = case getRequestMethod request of
      Get     -> "GET"
      Put     -> "PUT"
      Post    -> "POST"
      Delete  -> "DELETE"
      Head    -> "HEAD"
      Trace   -> "TRACE"
      Connect -> "CONNECT"
      Options -> "OPTIONS"
      Patch   -> "PATCH"
  let
    setBody = case getRequestBody request of
      Just body ->
        let body' = bool (getLBinaryString body) (getLBinaryString (LBinaryString $ compress compressionConfig (getLBinaryString body))) shouldCompress
        in  \req -> req { HTTP.requestBody = HTTP.RequestBodyLBS body' }
      Nothing   -> id

  -- TODO: Respect "Content-Transfer-Encoding" header
  let
    headers :: HTTP.RequestHeaders = getRequestHeaders request
      & Map.toList
      & map (\(x, y) -> (CI.mk (Encoding.encodeUtf8 x), Encoding.encodeUtf8 y))

  let
    setTimeout = case getRequestTimeout request <|> getFromCustomTimeoutHeader of
      Just x  -> setRequestTimeout x
      Nothing -> setRequestTimeout defaultTimeout

  let
    setRedirects = case getRequestRedirects request of
      Just x  -> \req -> req {HTTP.redirectCount = x}
      Nothing -> id

  pure $ setRedirects . setTimeout . setBody $
      httpLibRequest
        { HTTP.method         = requestMethod
        , HTTP.requestHeaders = maybe (headers) (\x -> headers <> [(compressionHeader,(Encoding.encodeUtf8 . show) x)]) mHeaderValForCompression
        }
  where
    getFromCustomTimeoutHeader =
      (A.decodeStrict' . Encoding.encodeUtf8) =<< (Map.lookup "x-custom-timeout-millis" $ getRequestHeaders request)
-- | Set timeout in microseconds
setRequestTimeout :: Int -> HTTP.Request -> HTTP.Request
setRequestTimeout x req = req {HTTP.responseTimeout = HTTP.responseTimeoutMicro x}


-- | Utility function to translate http-client HTTP responses back to HttpAPI
-- responses
translateHttpResponse :: HTTP.Response Lazy.ByteString -> Either Text HTTPResponse
translateHttpResponse response = do
  headers <- translateResponseHeaders $ HTTP.responseHeaders response
  status <-  translateResponseStatusMessage . HTTP.statusMessage . HTTP.responseStatus $ response
  pure $ HTTPResponse
    { getResponseBody    = LBinaryString $ HTTP.responseBody response
    , getResponseCode    = HTTP.statusCode $ HTTP.responseStatus response
    , getResponseHeaders = headers
    , getResponseStatus  = status
    }

modify302RedirectionResponse :: HTTPResponse -> HTTPResponse
modify302RedirectionResponse resp = do
  let contentType = Map.lookup "content-type" (getResponseHeaders resp)
  case (getResponseCode resp, contentType) of
    (302 , Just "text/plain") -> do
      let lbs = getLBinaryString $ getResponseBody resp
      case A.decode lbs :: Maybe Text of
        Nothing -> resp
        Just val -> maybe resp (\correctUrl -> resp { getResponseBody =  (LBinaryString . A.encode) correctUrl })  (modifyRedirectingUrl val)
    (_  , _           )   -> resp

  where
    status = getResponseStatus resp
    modifyRedirectingUrl = Text.stripPrefix (status <> ". Redirecting to ")


translateResponseHeaders
  :: [(CI.CI Strict.ByteString, Strict.ByteString)]
  -> Either Text (Map.Map Text.Text Text.Text)
translateResponseHeaders httpLibHeaders = do
  let
    result = do
      headerNames <- mapM  (Encoding.decodeUtf8' . CI.original . fst) httpLibHeaders
      headerValues <- mapM (Encoding.decodeUtf8' . snd) httpLibHeaders
      return $ zip (map Text.toLower headerNames) headerValues

  -- TODO: Look up encoding and use some thread-safe unicode package to decode
  --       headers
  -- let encoding
  --   = List.findIndex (\name -> name == "content-transfer-encoding") headerNames
  headers <- displayEitherException "Error decoding HTTP response headers: " result
  pure $ Map.fromList headers

translateResponseStatusMessage :: Strict.ByteString -> Either Text Text
translateResponseStatusMessage = displayEitherException "Error decoding HTTP response status message: " . Encoding.decodeUtf8'

displayEitherException :: Exception e => Text -> Either e a -> Either Text a
displayEitherException prefix = either (Left . (prefix <>) . Text.pack . Exception.displayException) Right

getMerchantIDFromLoggerCtxOrOptLocal :: R.FlowRuntime -> IO (Maybe Text)
getMerchantIDFromLoggerCtxOrOptLocal flowRt =
  runFlow flowRt $ (L.getLoggerContext "merchant_id") >>= maybe (L.getOptionLocal MerchantID) (pure . Just)

getArtRecEnabledFlagFromOptLocal :: R.FlowRuntime -> IO (Maybe Text)
getArtRecEnabledFlagFromOptLocal flowRt = do
  mIsArtRecordingEnabled <- runFlow flowRt $ L.getOptionLocal ArtRecordingEnabled
  pure $ case mIsArtRecordingEnabled of
    Just True -> Just "true"
    _         -> Nothing


-- translateHeaderName :: CI.CI Strict.ByteString -> Text.Text
-- translateHeaderName = Encoding.decodeUtf8' . CI.original

interpretFlowMethod :: Maybe FlowGUID -> R.FlowRuntime -> L.FlowMethod a -> IO a
interpretFlowMethod _ R.FlowRuntime {_httpClientManagers, _defaultHttpClientManager} (L.LookupHTTPManager mbMgrSel next) =
    pure $ next $ case mbMgrSel of
      Just (ManagerSelector mngrName) -> HM.lookup mngrName _httpClientManagers
      Nothing                         -> Just _defaultHttpClientManager

interpretFlowMethod mbFlowGuid flowRt@R.FlowRuntime {..} (L.CallServantAPI mngr bUrl apiTag errFunc (EulerClient f) next) = do
 let S.ClientEnv manager baseUrl cookieJar makeClientRequest = S.mkClientEnv mngr bUrl
     clientE = S.ClientEnv manager baseUrl cookieJar (\url -> getResponseTimeout . makeClientRequest url)
     shouldMock = ARTUtils.shouldMockAPICall . Just . Text.pack . S.baseUrlHost $ baseUrl
 tenantObj <- runFlow flowRt $ L.getOptionLocal L.TenantConfigObj
 let mHostValue = join $ L.tenantHost <$> tenantObj
 isLoadTestingOptionLocal <- runFlow flowRt $ L.getOptionLocal IsLoadTesting
 let mLoadTesting = maybe (Nothing) (\x -> if (x && ((maybe "" (\y -> L.tenantIdV2 y) tenantObj) `elem` Env.tenantIdListForLoadTesting)) then (Just "True") else Nothing) isLoadTestingOptionLocal
 mMerchId' <- getMerchantIDFromLoggerCtxOrOptLocal flowRt
 void $ runFlow flowRt $ L.setOptionLocal OutApiTag (show apiTag)
 let isInternalAPICall = shouldBypassProxy . Just . Text.pack . S.baseUrlHost $ baseUrl
     mMerchId = bool Nothing mMerchId' isInternalAPICall
 mbArtRecEnabled <- bool (pure Nothing) (getArtRecEnabledFlagFromOptLocal flowRt) isInternalAPICall
 trackingHeaders' <- bool (pure []) (getTrackingIdsFromLogContext lc) isInternalAPICall
 requestId <- runFlow flowRt $ L.getLoggerContext "x-request-id"
 mEulerRequestId <- if isInternalAPICall then runFlow flowRt $ L.getLoggerContext "euler-request-id" else pure Nothing
 let trackingHeaders = case find (\(name, _) -> name == "x-request-id") trackingHeaders' of
      Just _ -> trackingHeaders'
      Nothing -> addHeaderIfJust trackingHeaders' ("x-request-id", requestId <|> (snd <$> find (\(name, _) -> name == "x-euler-sessionid") trackingHeaders'))
 let additionalHeaders = foldl addHeaderIfJust trackingHeaders $ [("x-tenant-host", mHostValue), ("x-merchant-id", mMerchId), ("x-art-recording", mbArtRecEnabled), ("x-load-testing", mLoadTesting), ("euler-request-id", mEulerRequestId)]
 isArtV2ReplayEnabledWithSessId <- runFlow flowRt ARTUtils.shouldARTV2Replay
 if isArtReplayEnabled || (isArtV2ReplayEnabledWithSessId && shouldMock)
      then do
        fmap next $ do
          void $ runFlow flowRt $ L.setOptionLocal OutApiTag (show apiTag)
          eitherResult <- tryRunClient $! S.runClientM (runEulerClientReplay (EulerClient f) additionalHeaders isArtV2ReplayEnabledWithSessId isInternalAPICall) clientE
          case eitherResult of
            Left err -> do
              pure $ Left err
            Right response ->
              pure $ Right response
      else
        fmap next $ do
              shouldRecord <- shouldARTRecord _optionsLocal
              shouldRecordV2 <- ARTUtils.shouldARTV2Record _optionsLocal
              mbLogEncKey <- runFlow flowRt $ L.getOption LogEncryptionKey
              mbLogEncIv <- runFlow flowRt $ L.getOption LogEncryptionIV
              eitherResult <- tryRunClient $! S.runClientM (runEulerClient mbFlowGuid flowRt (if shouldLogAPI
                                                                                then dbgLogger (show apiTag)
                                                                                else emptyLogger
                                                                          ) errFunc getLoggerMaskConfig _optionsLocal _recordingLocal bUrl (EulerClient f) shouldRecord shouldRecordV2 producePayloadIO apiTag additionalHeaders mbLogEncKey mbLogEncIv isInternalAPICall) clientE
              case eitherResult of
                Left err -> do
                  pure $ Left err
                Right response ->
                  pure $ Right response
  where
    producePayloadIO :: KUtils.ValueType -> ByteString -> IO()
    producePayloadIO k v = runFlow flowRt $ R.producePayload k v

    runEulerClientReplay :: EulerClient a -> [(Text, Text)] -> Bool -> Bool -> SCIHC.ClientM a
    runEulerClientReplay (EulerClient re) additionalHeaders isArtV2Replay isInternalAPICall = foldFree (interpretClientFReplay additionalHeaders isArtV2Replay isInternalAPICall) re

    interpretClientFReplay :: [(Text, Text)] -> Bool -> Bool -> SCF.ClientF a -> SC.ClientM a
    interpretClientFReplay _ _ _ (SCF.Throw e) = throwM e
    interpretClientFReplay additionalHeaders isArtV2Replay isInternalAPICall (SCF.RunRequest req' next') = do
        let additionalHeaders' = bool [] additionalHeaders isArtV2Replay
            modifiedReq = foldl (\req (h, headerValue) -> addHeaderIfNotPresent (CI.mk $ encodeUtf8 h) headerValue req) req' additionalHeaders'
            modifiedFilteredReq = if isInternalAPICall then modifiedReq else requestWithheaderFilter modifiedReq
            replayKVDBEntry = CallAPIEntryT $ CallAPIEntry {
                        jsonRequest = fromServantRequest modifiedFilteredReq bUrl,
                        jsonResult = Right A.Null,
                        apiTag = show apiTag
                      }
        (resp, msg) <- liftIO $ runFlow flowRt $ do
          mSessId <- L.getLoggerContext "x-request-id"
          ER.callBrahmaReplayA replayKVDBEntry mSessId
        let res = (A.eitherDecode resp) :: Either String HTTPResponse
        case res of
          Right (httpResponse) -> do
            if (getResponseCode httpResponse >= 200) && (getResponseCode httpResponse < 300)
              then
                pure $ next' $ toServantResponse httpResponse{getResponseStatus = decodeUtf8 msg}
              else if isArtV2Replay
                then throwM (SCC.FailureResponse (mkServantFailureReq modifiedReq) (toServantResponse httpResponse))
                else throwM (SCC.DecodeFailure "" (toServantResponse httpResponse{getResponseStatus = decodeUtf8 msg}))
          Left (err) -> do
            when shouldLogCallStackART $ do
              liftIO $ runFlow flowRt $ do
                L.logErrorV @Text "CALLSTACK_ART_ERROR_CALLAPI" $ GHC.prettyCallStack $ GHC.callStack
                L.logErrorV @Text "CALLAPI_ERROR" $ err
            if isArtV2Replay
              then do
                liftIO $ runFlow flowRt $ L.logErrorV @Text "SERVANT_API_RESP_DECODE_ERROR" err
                throwM (SCC.ConnectionError . toException . ER.CallServantAPIReplayException $ Text.pack $ decodeUtf8 resp)
              else throwM (ER.CallServantAPIReplayException $ Text.pack err)
      where
        mkServantFailureReq :: SCC.Request -> SCC.RequestF () (SCC.BaseUrl, ByteString)
        mkServantFailureReq servantReq =
          SCC.Request {
            requestPath = (bUrl, Lazy.toStrict . BB.toLazyByteString $ SCC.requestPath servantReq)
          , requestQueryString = SCC.requestQueryString servantReq
          , requestBody = Nothing
          , requestAccept = SCC.requestAccept servantReq
          , requestHeaders = SCC.requestHeaders servantReq
          , requestHttpVersion = SCC.requestHttpVersion servantReq
          , requestMethod = SCC.requestMethod servantReq
          }
        
        requestWithheaderFilter :: SCC.Request -> SCC.Request
        requestWithheaderFilter req = req { SCC.requestHeaders = Seq.filter (\(key, _) -> ((Encoding.decodeUtf8 . CI.original) key) `notElem` getHeadersToFilterFromOutgoingRequest) (SCC.requestHeaders req) }
  
        addHeaderIfNotPresent headerName headerValue request =
          let allHeaders = SCC.requestHeaders request
              headerPresent = headerName `elem` (map fst allHeaders)
            in if headerPresent
                  then request
                  else SCC.addHeader headerName headerValue request

    lc = R._logContext . R._loggerRuntime . R._coreRuntime $ flowRt

    emptyLogger _ _ _ _ _ _ _  = return ()

    customHeader :: CI.CI ByteString
    customHeader = CI.mk $ encodeUtf8 @Text "x-custom-timeout-millis"

    addHeaderIfJust :: [(Text, Text)] -> (Text, Maybe Text) -> [(Text, Text)]
    addHeaderIfJust headers (hName, hVal) = maybe headers (\hValue -> (hName, hValue):headers) hVal

    getResponseTimeout req = do
      let (modHeaders, maybeCustomTimeOut) = foldl (\(arr, m) (headerName, v) -> if customHeader == headerName then (arr, Just (headerName, v)) else ([(headerName, v)] <> arr, m)) ([], Nothing) $ requestHeaders req
      case maybeCustomTimeOut >>= convertMilliSecondToMicro of
        Just value -> req {HTTP.responseTimeout = HTTP.responseTimeoutMicro value, HTTP.requestHeaders = modHeaders}
        Nothing -> if HTTP.responseTimeout req == HTTP.responseTimeoutNone
                    then setRequestTimeout defaultTimeout req
                    else req {HTTP.responseTimeout = mResponseTimeout mngr}

    convertMilliSecondToMicro :: (a, ByteString) -> Maybe Int
    convertMilliSecondToMicro (_, value) = (*) 1000  <$> A.decodeStrict value

    dbgLogger :: forall msg . A.ToJSON msg => Entity -> Category -> LogLevel -> Action -> Maybe ErrorL -> Maybe Latency -> Maybe RespCode -> msg -> IO()
    dbgLogger entity category logLevel action maybeError maybeLatency maybeRespCode msg =
      runLogger mbFlowGuid (R._loggerRuntime . R._coreRuntime $ flowRt) 
        $ L.masterLogger logLevel ("CallServantAPI impl" :: String) category (Just action) Nothing Nothing (Just entity) maybeError maybeLatency Nothing Nothing Nothing Nothing maybeRespCode (Message Nothing (Just $ A.toJSON msg)) Nothing

    shouldLogAPI =
      R.shouldLogAPI . R._loggerRuntime . R._coreRuntime $ flowRt
    getLoggerMaskConfig =
      R.getLogMaskingConfig . R._loggerRuntime . R._coreRuntime $ flowRt
    tryRunClient :: IO (Either S.ClientError a) -> IO (Either S.ClientError a)
    tryRunClient act = do
      res :: Either S.ClientError (Either S.ClientError a) <- try act
      pure $ join res

interpretFlowMethod _ R.FlowRuntime {..} (L.GetHTTPManager settings next) =
  fmap next $ do
    modifyMVar _dynHttpClientManagers $ \_cache -> do
      let mCacheMgr = LRU.lookup settings _cache
      case mCacheMgr of
        Just (mgr, cache) -> pure (cache, mgr)
        Nothing  -> do
          mgr <- HTTP.newManager $ buildSettings settings
          pure (LRU.insert settings mgr _cache, mgr)


interpretFlowMethod mbFlowGuid flowRt@R.FlowRuntime {..} (L.CallHTTP request' apiTag errFunc manager mbMaskReqResBody mbDigestDetails next) = do
    tenantObj <- runFlow flowRt $ L.getOptionLocal L.TenantConfigObj
    let mHostValue = join $ L.tenantHost <$> tenantObj
    mMerchId <- getMerchantIDFromLoggerCtxOrOptLocal flowRt
    mbArtRecEnabled <- getArtRecEnabledFlagFromOptLocal flowRt
    let isInternalAPICall = shouldBypassProxy . Just . getRequestURL $ request'
    void $ runFlow flowRt $ L.setOptionLocal OutApiTag (show apiTag)
    isLoadTestingOptionLocal <- runFlow flowRt $ L.getOptionLocal IsLoadTesting
    let mLoadTesting = maybe (Nothing) (\x -> if (x && ((maybe "" (\y -> L.tenantIdV2 y) tenantObj) `elem` Env.tenantIdListForLoadTesting)) then (Just "True") else Nothing) isLoadTestingOptionLocal
    additionalHeaders <- bool (pure []) (getTrackingIdsFromLogContext lc) isInternalAPICall
    let reqWithMerchantIdHeader = bool request' (addHeaderIfNotPresent "x-art-recording" mbArtRecEnabled $ addHeaderIfNotPresent "x-merchant-id" mMerchId request') isInternalAPICall
        reqWithTrackingHeaders = withOptionalHeaders additionalHeaders reqWithMerchantIdHeader
    requestId <- runFlow flowRt $ L.getLoggerContext "x-request-id"
    mEulerRequestId <- if isInternalAPICall then runFlow flowRt $ L.getLoggerContext "euler-request-id" else pure Nothing
    let eulerSessionId = Map.lookup "x-euler-sessionid" $ getRequestHeaders reqWithTrackingHeaders
    let requestWithId = addHeaderIfNotPresent "x-request-id" (requestId <|> eulerSessionId) reqWithTrackingHeaders
        request = (if isInternalAPICall then addHeaderIfNotPresent "euler-request-id" mEulerRequestId else id) $ addHeaderIfNotPresent "x-load-testing" mLoadTesting (addHeaderIfNotPresent "x-tenant-host" mHostValue requestWithId)
        requestWithHeaderFilter = if isInternalAPICall then request else request { getRequestHeaders = Map.filterWithKey (\k _ -> k `notElem` getHeadersToFilterFromOutgoingRequest) (getRequestHeaders request) }
    isArtV2ReplayEnabledWithSessId <- runFlow flowRt ARTUtils.shouldARTV2Replay
    if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
      then do
        let replayKVDBEntry = CallAPIEntryT $ CallAPIEntry {
                        jsonRequest = bool (request' {getRequestHeaders = Map.empty}) requestWithHeaderFilter isArtV2ReplayEnabledWithSessId, -- removing headers as request body limit is maxed out.,
                        jsonResult = Right A.Null,
                        apiTag = show apiTag}
        resp <- runFlow flowRt $ do
          mSessId <- L.getLoggerContext "x-request-id"
          ER.callBrahmaReplayR replayKVDBEntry mSessId
        let res = (A.eitherDecode resp) :: Either String HTTPResponse
        case res of
          Right (httpResponse) -> do
            fmap next $ pure $ Right httpResponse
          Left (err) -> do
            let errMsg = Text.pack err
            when shouldLogCallStackART $ do
              runFlow flowRt $ do
                L.logErrorV @Text "CALLSTACK_ART_ERROR_CALLHTTP" $ GHC.prettyCallStack $ GHC.callStack
                L.logErrorV @Text "CALLHTTP_ERROR" $ err
            fmap next $ pure $ Left (errMsg)
      else do
        val <- fmap next $ do
          shouldRecord <- shouldARTRecord _optionsLocal
          shouldRecordV2 <- ARTUtils.shouldARTV2Record _optionsLocal
          start <- systemToTAITime <$> getSystemTime
          httpLibRequest <- getHttpLibRequest (compressionDecider (show apiTag) (getRequestURL request)) requestWithHeaderFilter
          eResponse <- try $! do
            httpLibRequest' <-
              case mbDigestDetails of
                Just (username, password) -> do
                  httpLibRequestDAuth <- (\r -> NC.applyDigestAuth (encodeUtf8 username) (encodeUtf8 password) r manager) httpLibRequest
                  httpLibRequestDAuth
                Nothing -> pure httpLibRequest
            HTTP.httpLbs httpLibRequest' manager
          end <- liftIO $ systemToTAITime <$> getSystemTime
          let lat = div (diffTimeToPicoseconds $ diffAbsoluteTime end start) picoMilliDiff
              httpRequestMethod = decodeUtf8 $ method httpLibRequest
          eresp <- case eResponse of
            Left (err :: SomeException) -> do
              mbLogEncKey <- runFlow flowRt $ L.getOption LogEncryptionKey
              mbLogEncIv <- runFlow flowRt $ L.getOption LogEncryptionIV
              let errMsg = getErrReason err
                  errVal = exceptionToValue err
              when shouldLogAPI $ do
                let httpResponse           = HTTPResponse {getResponseBody = (LBinaryString . A.encode) errVal, getResponseCode = 500, getResponseHeaders = Map.empty, getResponseStatus = "IOException"}
                    logEntry               = mkHttpApiCallLogEntry lat (Just $ maskHTTPRequest getLoggerMaskConfig request mbMaskReqResBody mbLogEncKey mbLogEncIv) (Just $ maskHTTPResponse getLoggerMaskConfig httpResponse mbMaskReqResBody Nothing Nothing) (Bool.bool EXTERNAL INTERNAL ( (shouldBypassProxy . Just . decodeUtf8 . host $ httpLibRequest) || isArtReplayEnabled ) ) apiTag Nothing
                logJsonError "OUTGOING_API" errMsg httpRequestMethod 500 lat logEntry
                when (shouldLogEncryptedLogs) $ logEncLogsJSONError request httpResponse errMsg httpRequestMethod 500 lat mbLogEncKey mbLogEncIv
              EEMF.updateApiLatencyMonitoring _optionsLocal Nothing lat
              pure $ Left errMsg
            Right httpResponse -> do
              mbLogEncKey <- runFlow flowRt $ L.getOption LogEncryptionKey
              mbLogEncIv <- runFlow flowRt $ L.getOption LogEncryptionIV
              case (modify302RedirectionResponse <$> translateHttpResponse httpResponse) of
                Left errMsg -> do
                  when shouldLogAPI $ do
                    let httpRes  = HTTPResponse {getResponseBody = LBinaryString $ HTTP.responseBody httpResponse, getResponseCode = HTTP.statusCode . HTTP.responseStatus $ httpResponse, getResponseHeaders = Map.empty, getResponseStatus = "HTTPException"}
                        logEntry = mkHttpApiCallLogEntry lat (Just $ maskHTTPRequest getLoggerMaskConfig request mbMaskReqResBody mbLogEncKey mbLogEncIv) (Just $ maskHTTPResponse getLoggerMaskConfig httpRes mbMaskReqResBody mbLogEncKey mbLogEncIv) (Bool.bool EXTERNAL INTERNAL ( (shouldBypassProxy . Just . decodeUtf8 . host $ httpLibRequest) || isArtReplayEnabled ) ) apiTag Nothing
                    logJsonError "OUTGOING_API" errMsg httpRequestMethod (HTTP.statusCode . HTTP.responseStatus $ httpResponse) lat logEntry
                    when (shouldLogEncryptedLogs) $ logEncLogsJSONError request httpRes errMsg httpRequestMethod (HTTP.statusCode . HTTP.responseStatus $ httpResponse) lat mbLogEncKey mbLogEncIv
                  EEMF.updateApiLatencyMonitoring _optionsLocal Nothing lat
                  pure $ Left errMsg
                Right response -> do
                  let errInfo = errFunc response
                      errLog = mkErrorLog =<< errInfo
                      updatedRes = addErrorInfoToResponseHeaders response errInfo
                      mbDownstreamOutgoingLat = Map.lookup EEMF.internalApiLatencyHeader (getResponseHeaders response)
                  EEMF.updateApiLatencyMonitoring _optionsLocal mbDownstreamOutgoingLat lat
                  when shouldLogAPI $ do
                    let logEntry = mkHttpApiCallLogEntry lat (Just $ maskHTTPRequest getLoggerMaskConfig request mbMaskReqResBody mbLogEncKey mbLogEncIv) (Just $ maskHTTPResponse getLoggerMaskConfig updatedRes mbMaskReqResBody mbLogEncKey mbLogEncIv) (Bool.bool EXTERNAL INTERNAL ( (shouldBypassProxy . Just . decodeUtf8 . host $ httpLibRequest) || isArtReplayEnabled ) ) apiTag errInfo
                    logJson "OUTGOING_API" Info httpRequestMethod (show apiTag) errLog lat (getResponseCode updatedRes) logEntry
                    when (shouldLogEncryptedLogs) $ logEncLogsJSON request updatedRes httpRequestMethod apiTag errLog lat mbLogEncKey mbLogEncIv
                  pure $ Right updatedRes
          when (shouldRecord || shouldRecordV2) $ do
            let apiEntry = case eresp of
                            Right resp -> CallAPIEntryT $ CallAPIEntry {
                                    jsonRequest = requestWithHeaderFilter,
                                    jsonResult = Right $ A.toJSON resp,
                                    apiTag = show apiTag}
                            Left errMsg -> CallAPIEntryT $ CallAPIEntry {
                                  jsonRequest = requestWithHeaderFilter,
                                  jsonResult = Left $ toErrorPayload errMsg,
                                  apiTag = show apiTag}
            when (shouldRecord) $ do
              m <- takeMVar _recordingLocal
              putMVar _recordingLocal $ m <> [apiEntry]
            when (shouldRecordV2) $ runFlow flowRt $ R.producePayload KUtils.OUTGOING_API (Lazy.toStrict $ A.encode apiEntry)
          pure eresp
        pure val
  where
    encryptedCompressedLogEntryWithoutMasking encKMSDecAESKey encKMSDecAESIV compressedLogEntryWithoutMasking = 
      either ("encryption failed :: " <>) id $ (aesEncryptText encKMSDecAESKey encKMSDecAESIV . decodeUtf8) compressedLogEntryWithoutMasking

    logEncLogsJSONError request response errMsg httpRequestMethod statusCode lat mbLogEncKey mbLogEncIv = do
      case (mbLogEncKey, mbLogEncIv) of
        (Just encKMSDecAESKey, Just encKMSDecAESIV) -> do
          let logEntryWithoutMasking = mkHttpApiCallLogEntryWithoutMasking request response
          !mCompressedLogEntryWithoutMasking <- logCompressionHelper mbFlowGuid flowRt ((Lazy.toStrict . A.encode) logEntryWithoutMasking)
          case mCompressedLogEntryWithoutMasking of
            Just compressedLogEntryWithoutMasking ->
              logJsonError "ENCRYPTED_OUTGOING_API" errMsg httpRequestMethod statusCode lat (encryptedCompressedLogEntryWithoutMasking encKMSDecAESKey encKMSDecAESIV compressedLogEntryWithoutMasking)
            _ -> pure ()
        _ -> pure ()

    logEncLogsJSON request response httpRequestMethod apitag errLog lat mbLogEncKey mbLogEncIv = do
      case (mbLogEncKey, mbLogEncIv) of
        (Just encKMSDecAESKey, Just encKMSDecAESIV) -> do
          let logEntryWithoutMasking = mkHttpApiCallLogEntryWithoutMasking request response
          !mCompressedLogEntryWithoutMasking <- logCompressionHelper mbFlowGuid flowRt ((Lazy.toStrict . A.encode) logEntryWithoutMasking)
          case mCompressedLogEntryWithoutMasking of
            Just compressedLogEntryWithoutMasking -> 
              logJson "ENCRYPTED_OUTGOING_API" Info httpRequestMethod (show apitag) errLog lat (getResponseCode response) (encryptedCompressedLogEntryWithoutMasking encKMSDecAESKey encKMSDecAESIV compressedLogEntryWithoutMasking)
            _ -> pure ()
        _ -> pure ()

    lc = R._logContext . R._loggerRuntime . R._coreRuntime $ flowRt
    picoMilliDiff :: Integer
    picoMilliDiff = 1000000000
    getErrReason :: SomeException -> Text
    getErrReason ex =
      case fromException ex of
        Just (HC.HttpExceptionRequest _req reason) -> show reason
        Just (HC.InvalidUrlException _ reason) -> Text.pack reason
        Nothing -> Text.pack $ displayException ex
    exceptionToValue :: SomeException -> A.Value
    exceptionToValue ex =
      case (fromException ex :: Maybe HC.HttpException) of
        Just httpEx -> toJSON httpEx
        Nothing -> toJSON $ displayException ex
    logJsonError :: (ToJSON a) => Text -> Text -> Text -> Int -> Integer -> a -> IO ()
    logJsonError tag err method statusCode latency msg =
      let errM = ErrorL Nothing "API_ERROR" err
        in logJson tag Error method (show apiTag) (Just errM) latency statusCode msg
    logJson :: ToJSON a => Text -> LogLevel -> Action -> Entity -> Maybe ErrorL -> Latency -> RespCode -> a -> IO ()
    logJson tag level action entity maybeError lat respCode msg =
      runLogger mbFlowGuid (R._loggerRuntime . R._coreRuntime $ flowRt)
        $ L.masterLogger level ("callHTTP" :: String) tag (Just action) Nothing Nothing (Just entity) maybeError (Just lat) Nothing Nothing Nothing Nothing (Just respCode) (Message Nothing (Just $ A.toJSON msg)) Nothing

    shouldLogAPI =
      R.shouldLogAPI . R._loggerRuntime . R._coreRuntime $ flowRt
    getLoggerMaskConfig =
      R.getLogMaskingConfig . R._loggerRuntime . R._coreRuntime $ flowRt

    mkErrorLog :: ErrorInfo -> Maybe ErrorL
    mkErrorLog errInfo = Just (ErrorL (Just errInfo.error_code) errInfo.error_category errInfo.error_message)

    addHeaderIfNotPresent headerName mHeaderValue request =
      if hasHeader headerName request
        then request
        else withOptionalHeader headerName mHeaderValue request

interpretFlowMethod mbFlowGuid R.FlowRuntime {..} (L.EvalLogger loggerAct next) =
  next <$> runLogger mbFlowGuid (R._loggerRuntime _coreRuntime) loggerAct

interpretFlowMethod _ _ (L.RunIO _ ioAct next) =
  next <$> ioAct

interpretFlowMethod _ flowRt (L.WithRunFlow ioAct) =
  ioAct (runFlow flowRt)

interpretFlowMethod _ rt@R.FlowRuntime {..} (L.GetOption k (next :: Maybe v -> next)) = do
  isArtV2ReplayEnabledWithSessId <- runFlow rt ARTUtils.shouldARTV2Replay
  if isArtV2ReplayEnabledWithSessId && ARTUtils.shouldReplayGlobalOption @v k
    then do
      let replayGetOptionEntry = GlobalOptionsEntryT $ GlobalOptionsEntry
                                  {
                                    functionName = "getOption",
                                    key = Text.pack $ show k,
                                    value = Nothing
                                  }
      eResp <- runFlow rt $ do
        mSessId <- L.getLoggerContext "x-request-id"
        ER.callBrahmaReplayWithRespCodeCheck replayGetOptionEntry mSessId
      case eResp of
        Left err -> do
          when shouldLogCallStackART $ do
            runFlow rt $ do
              L.logErrorV @Text "CALLSTACK_ART_ERROR_GET_OPTION" $ GHC.prettyCallStack $ GHC.callStack
              L.logErrorV @Text "CALLTIME_ERROR" $ err
          fmap next $ recordForART
        Right resp -> do
          let maybeReply = A.eitherDecode resp
          next <$> case maybeReply of
            Left err -> do
              let errMsg = encodeUtf8 err
              when shouldLogCallStackART $ do
                runFlow rt $ do
                  L.logErrorV @Text "CALLSTACK_ART_ERROR_GET_OPTION" $ GHC.prettyCallStack $ GHC.callStack
                  L.logErrorV @Text "CALLTIME_ERROR" $ err
              runFlow rt $ L.throwException $ S.err500 {S.errBody = errMsg}
            Right reply -> case fromArtRecordedValue reply of
                            Left _ -> recordForART
                            Right val -> pure $ Just $ val
    else fmap next $ recordForART

    where
      recordForART = do
        shouldRecordV2 <- ARTUtils.shouldARTV2Record _optionsLocal
        m <- readMVar _options
        maybeVal <- pure $ do
          valAny <- Map.lookup k m
          pure $ unsafeCoerce valAny
        when (shouldRecordV2 && ARTUtils.shouldRecordGlobalOption @v k) $ do
          let optionsEntry = GlobalOptionsEntryT $ GlobalOptionsEntry
                              {
                                functionName = "getOption",
                                key = Text.pack $ show k,
                                value = maybeVal >>= \v -> either (const Nothing) Just (toArtRecordingValue v)
                              }
          runFlow rt $ R.producePayload KUtils.GET_OPTIONS (Lazy.toStrict $ A.encode optionsEntry)
        pure maybeVal

interpretFlowMethod _ rt@R.FlowRuntime {..} (L.SetOption k (v :: v) next) =
  fmap next $ do
      shouldRecordV2 <- ARTUtils.shouldARTV2Record _optionsLocal
      m <- takeMVar _options
      let newMap = Map.insert k (unsafeCoerce @_ @Any v) m
      when (shouldRecordV2 && shouldRecordSetOption && ARTUtils.shouldRecordGlobalOption @v k) $ do
        let optionsEntry = GlobalOptionsEntryT $ GlobalOptionsEntry
                            {
                              functionName = "setOption",
                              key = Text.pack $ show k,
                              value = either (const Nothing) Just (toArtRecordingValue v)
                            }
        runFlow rt $ R.producePayload KUtils.SET_OPTIONS (Lazy.toStrict $ A.encode optionsEntry)
      putMVar _options newMap

interpretFlowMethod _ R.FlowRuntime {..} (L.SetLoggerContext k v next) =
  fmap next $ do
    m <- readIORef $ R._logContext . R._loggerRuntime $ _coreRuntime
    let newMap = HM.insert k v m
    writeIORef (R._logContext . R._loggerRuntime $ _coreRuntime) newMap

interpretFlowMethod _ R.FlowRuntime {..} (L.GetLoggerContext k next) =
  fmap next $ do
    m <- readIORef $ R._logContext . R._loggerRuntime $ _coreRuntime
    pure $ HM.lookup k m

interpretFlowMethod _ R.FlowRuntime {..} (L.SetLoggerContextMap newMap next) =
  fmap next $ do
    oldMap <- readIORef $ R._logContext . R._loggerRuntime $ _coreRuntime
    writeIORef (R._logContext . R._loggerRuntime $ _coreRuntime) (HM.union newMap oldMap)

interpretFlowMethod _ R.FlowRuntime {..} (L.GetLoggerContextMap next) =
  fmap next $ do
    logmap <- readIORef $ R._logContext . R._loggerRuntime $ _coreRuntime
    pure logmap

interpretFlowMethod _ R.FlowRuntime {..} (L.IncrementArtCounter next) =
  fmap next $ do
    cnt <- takeMVar (_artCounter)
    putMVar _artCounter (cnt + 1)
    pure $ cnt + 1

interpretFlowMethod _ R.FlowRuntime {..} (L.ModifyOption k fn next) =
  fmap next $ do
    modifyMVar _options modifyAndCallFn
    where
      modifyAndCallFn curOptions = do
        let valAny = Map.lookup k curOptions
        case valAny of
          Nothing -> pure (curOptions,(Nothing,Nothing))
          Just val -> do
            let oldVal = unsafeCoerce val
                modifiedVal = fn oldVal
            pure (Map.insert k (unsafeCoerce @_ @Any modifiedVal) curOptions,
                  (Just oldVal, Just modifiedVal)
                )

interpretFlowMethod _ R.FlowRuntime {..} (L.DelOption k next) =
  fmap next $ do
    m <- takeMVar _options
    let newMap = Map.delete k m
    putMVar _options newMap

interpretFlowMethod _ R.FlowRuntime {..} (L.GetOptionLocal k next) =
  fmap next $ do
    m <- readMVar _optionsLocal
    pure $ do
      valAny <- Map.lookup k m
      pure $ unsafeCoerce valAny

interpretFlowMethod _ R.FlowRuntime {..} (L.SetOptionLocal k v next) =
  fmap next $ do
    m <- takeMVar _optionsLocal
    let newMap = Map.insert k (unsafeCoerce @_ @Any v) m
    putMVar _optionsLocal newMap

interpretFlowMethod _ R.FlowRuntime {..} (L.ModifyOptionLocal k fn next) =
  fmap next $ do
    modifyMVar _optionsLocal modifyAndCallFn
    where
      modifyAndCallFn curOptions = do
        let valAny = Map.lookup k curOptions
        case valAny of
          Nothing -> pure (curOptions, (Nothing,Nothing))
          Just val -> do
            let oldVal = unsafeCoerce val
                modifiedVal = fn oldVal
            pure (Map.insert k (unsafeCoerce @_ @Any modifiedVal) curOptions,
                  (Just oldVal, Just modifiedVal)
                )

interpretFlowMethod _ R.FlowRuntime {..} (L.DelOptionLocal k next) =
  fmap next $ do
    m <- takeMVar _optionsLocal
    let newMap = Map.delete k m
    putMVar _optionsLocal newMap

interpretFlowMethod _ flowRT (L.GetFlowRuntime next) = pure (next flowRT)

interpretFlowMethod _ R.FlowRuntime {..} (L.GetRecordingLocal next) =
  fmap next $ do
    m <- readMVar _recordingLocal
    pure m

interpretFlowMethod _ R.FlowRuntime {..} (L.AppendRecordingLocal v next) =
  fmap next $ do
    shouldRecord <- shouldARTRecord _optionsLocal
    when shouldRecord $ do
      m <- takeMVar _recordingLocal
      putMVar _recordingLocal (m <> [v])

interpretFlowMethod _ R.FlowRuntime {..} (L.DelRecordingLocal next) =
  fmap next $ do
    _ <- takeMVar _recordingLocal
    putMVar _recordingLocal mempty

interpretFlowMethod _ R.FlowRuntime {..} (L.GetConfig k queryType next) =
  fmap next $ do
    let configCache = case queryType of
                        FIND_ONE -> _configCache
                        FIND_ALL -> _configCacheFindAll
    config <- readIORef configCache
    let res = snd $ SimpleLRU.lookup k config
    return res

interpretFlowMethod _ R.FlowRuntime {..} (L.SetConfig k v queryType next) =
  fmap next $ do
    let configCache = case queryType of
                        FIND_ONE -> _configCache
                        FIND_ALL -> _configCacheFindAll
    atomicModifyIORef' configCache (modifyConf k v)

  where
    modifyConf :: W8Arr -> R.ConfigEntry -> SimpleLRU.LRU W8Arr R.ConfigEntry -> (SimpleLRU.LRU W8Arr R.ConfigEntry, ())
    modifyConf key val configLRU =
      let m' = SimpleLRU.insert key val configLRU
      in (m', ())

interpretFlowMethod _ R.FlowRuntime {..} (L.ModifyConfig k entryMod queryType next) = do
  fmap next $ do
    let configCache = case queryType of
                        FIND_ONE -> _configCache
                        FIND_ALL -> _configCacheFindAll
    atomicModifyIORef' configCache (modifyConf k entryMod)

  where
    modifyConf :: W8Arr -> (Maybe R.ConfigEntry -> R.ConfigEntry) -> SimpleLRU.LRU W8Arr R.ConfigEntry -> (SimpleLRU.LRU W8Arr R.ConfigEntry, ())
    modifyConf key modification configLRU =
      let
        (lru', val) = SimpleLRU.lookup k configLRU
        lru'' = flip (SimpleLRU.insert key) lru' $ modification val
      in (, ()) lru''

interpretFlowMethod _ R.FlowRuntime {..} (L.DelConfig k queryType next) =
  fmap next $ do
    let configCache = case queryType of
                        FIND_ONE -> _configCache
                        FIND_ALL -> _configCacheFindAll
    atomicModifyIORef' configCache (deleteConfig k)
  where
    deleteConfig :: W8Arr -> SimpleLRU.LRU W8Arr R.ConfigEntry -> (SimpleLRU.LRU W8Arr R.ConfigEntry, ())
    deleteConfig key configLRU =
      let m' = SimpleLRU.delete key configLRU
      in (fst m', ())

interpretFlowMethod _ R.FlowRuntime {..} (L.TrySetConfig k v queryType next) =
  fmap next $ do
    let configCache = case queryType of
                        FIND_ONE -> _configCache
                        FIND_ALL -> _configCacheFindAll
    atomicModifyIORef' configCache (modifyConfig k v)
  where
    modifyConfig :: W8Arr -> R.ConfigEntry -> SimpleLRU.LRU W8Arr R.ConfigEntry -> (SimpleLRU.LRU W8Arr R.ConfigEntry, Maybe ())
    modifyConfig key val configLRU =
      let m' = SimpleLRU.insert key val configLRU
      in (m', Just ())

interpretFlowMethod _ R.FlowRuntime {..} (L.AcquireConfigLock k next) =
  fmap next $ do
    m <- takeMVar _configCacheLock
    didAcquire <- CMap.insertIfAbsent k () m
    putMVar _configCacheLock m
    return didAcquire

interpretFlowMethod _ R.FlowRuntime {..} (L.ReleaseConfigLock k next) =
  fmap next $ do
    m <- takeMVar _configCacheLock
    didDelete <- CMap.delete k m
    putMVar _configCacheLock m
    return didDelete

interpretFlowMethod _ rt@R.FlowRuntime {..} (L.GenerateGUID tag next) = do
  isArtV2ReplayEnabledWithSessId <- runFlow rt ARTUtils.shouldARTV2Replay
  if isArtV2ReplayEnabledWithSessId
    then do
      let replayUUIDEntry = UuidEntryT $ UuidEntry {
                              functionName = tag,
                              uuid = A.Null
                            }
      resp <- runFlow rt $ do
        mSessId <- L.getLoggerContext "x-request-id"
        ER.callBrahmaReplayR replayUUIDEntry mSessId
      let maybeReply = A.eitherDecode resp
      next <$> case maybeReply of
        Left err -> do
          let errMsg = encodeUtf8 err
          when shouldLogCallStackART $ do
            runFlow rt $ do
              L.logErrorV @Text "CALLSTACK_ART_ERROR_UUID" $ GHC.prettyCallStack $ GHC.callStack
              L.logErrorV @Text "CALLTIME_ERROR" $ err
          runFlow rt $ L.throwException $ S.err500 {S.errBody = errMsg}
        Right (reply :: UUID.UUID) -> pure reply
    else do
      shouldRecordV2 <- ARTUtils.shouldARTV2Record _optionsLocal
      uuidValue <- UUID.nextRandom
      when (shouldRecordV2) $ do
        let uuidEntry = UuidEntryT $ UuidEntry {
                          functionName = tag,
                          uuid = (A.toJSON uuidValue)
                        }
        runFlow rt $ ARTFlowUtils.producePayload KUtils.UUID (Lazy.toStrict $ A.encode uuidEntry)
      pure $ next uuidValue

interpretFlowMethod _ rt@R.FlowRuntime {..} (L.GetCurrentTime next) = do
  isArtV2ReplayEnabledWithSessId <- runFlow rt ARTUtils.shouldARTV2Replay
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
      then do
        let replayTimeStampEntry = TimeStampEntryT $ TimeStampEntry {
          functionName = "getCurrentTime",
          timestamp = A.Null
        }
        resp <- runFlow rt $ do
          mSessId <- L.getLoggerContext "x-request-id"
          ER.callBrahmaReplayR replayTimeStampEntry mSessId
        let maybeReply = A.eitherDecode resp
        next <$> case maybeReply of
          Left err -> do
            let errMsg = encodeUtf8 err
            when shouldLogCallStackART $ do
              runFlow rt $ do
                L.logErrorV @Text "CALLSTACK_ART_ERROR_CURRENT_TIME" $ GHC.prettyCallStack $ GHC.callStack
                L.logErrorV @Text "CALLTIME_ERROR" $ err
            runFlow rt $ L.throwException $ S.err500 {S.errBody = errMsg}
          Right (reply :: DT.UTCTime) -> pure reply
      else do
        time <- DT.getCurrentTime
        shouldRecord <- shouldARTRecord _optionsLocal
        shouldRecordV2 <- ARTUtils.shouldARTV2Record _optionsLocal
        when (shouldRecord || (shouldRecordV2 && shouldRecordTimestamp)) $ do
          let timeEntry = TimeStampEntryT $ TimeStampEntry {
                              functionName = "getCurrentTime",
                              timestamp = (A.toJSON time)
                            }
          when (shouldRecord) $ do
              m <- takeMVar _recordingLocal
              putMVar _recordingLocal $ m <> [timeEntry]
          when (shouldRecordV2 && shouldRecordTimestamp) $ runFlow rt $ R.producePayload KUtils.TIMESTAMP (Lazy.toStrict $ A.encode timeEntry)
        pure $ next time

interpretFlowMethod _ rt@R.FlowRuntime {..} (L.GetPOSIXTime next) = do
  isArtV2ReplayEnabledWithSessId <- runFlow rt ARTUtils.shouldARTV2Replay
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
      then do
        let replayTimeStampEntry = TimeStampEntryT $ TimeStampEntry {
          functionName = "getPOSIXTime",
          timestamp = A.Null
        }
        resp <- runFlow rt $ do
          mSessId <- L.getLoggerContext "x-request-id"
          ER.callBrahmaReplayR replayTimeStampEntry mSessId
        let maybeReply = A.eitherDecode resp
        next <$> case maybeReply of
          Left err -> do
            let errMsg = encodeUtf8 err
            when shouldLogCallStackART $ do
              runFlow rt $ do
                L.logErrorV @Text "CALLSTACK_ART_ERROR_POSIX_TIME" $ GHC.prettyCallStack $ GHC.callStack
                L.logErrorV @Text "CALLTIME_ERROR" $ err
            runFlow rt $ L.throwException $ S.err500 {S.errBody = errMsg}
          Right (reply :: DT.POSIXTime) -> pure reply
      else do
        time <- DT.getPOSIXTime
        shouldRecord <- shouldARTRecord _optionsLocal
        shouldRecordV2 <- ARTUtils.shouldARTV2Record _optionsLocal
        when (shouldRecord || (shouldRecordV2 && shouldRecordTimestamp)) $ do
          let timeEntry = TimeStampEntryT $ TimeStampEntry {
                              functionName = "getPOSIXTime",
                              timestamp = (A.toJSON time)
                            }
          when (shouldRecord) $ do
              m <- takeMVar _recordingLocal
              putMVar _recordingLocal $ m <> [timeEntry]
          when (shouldRecordV2 && shouldRecordTimestamp) $ runFlow rt $ R.producePayload KUtils.TIMESTAMP (Lazy.toStrict $ A.encode timeEntry)
        pure $ next time

interpretFlowMethod _ rt@R.FlowRuntime {..} (L.RandomRIO k p next) = do
  isArtV2ReplayEnabledWithSessId <- runFlow rt ARTUtils.shouldARTV2Replay
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
      then do
        let replayRandomRIOEntry = RandomRIOEntryT $ RandomRIOEntry {
          functionName = "randomRIO",
          range = A.toJSON p,
          value = A.Null,
          tag = k
        }
        resp <- runFlow rt $ do
          mSessId <- L.getLoggerContext "x-request-id"
          ER.callBrahmaReplayR replayRandomRIOEntry mSessId
        let maybeReply = A.eitherDecode resp
        next <$> case maybeReply of
          Left err -> do
            let errMsg = encodeUtf8 err
            when shouldLogCallStackART $ do
              runFlow rt $ do
                L.logErrorV @Text "CALLSTACK_ART_ERROR_RANDOM" $ GHC.prettyCallStack $ GHC.callStack
                L.logErrorV @Text "CALLTIME_ERROR" $ err
            runFlow rt $ L.throwException $ S.err500 {S.errBody = errMsg}
          Right reply -> pure reply
      else do
        generatedVal <- SR.randomRIO p
        shouldRecord <- shouldARTRecord _optionsLocal
        shouldRecordV2 <- ARTUtils.shouldARTV2Record _optionsLocal
        when (shouldRecord || shouldRecordV2) $ do
          let randomRIOEntry = RandomRIOEntryT $ RandomRIOEntry {
                              functionName = "randomRIO",
                              range = A.toJSON p,
                              value = A.toJSON generatedVal,
                              tag = k
                            }
          when (shouldRecord) $ do
              m <- takeMVar _recordingLocal
              putMVar _recordingLocal $ m <> [randomRIOEntry]
          when (shouldRecordV2) $ runFlow rt $ R.producePayload KUtils.RANDOM_RIO (Lazy.toStrict $ A.encode randomRIOEntry)
        pure $ next generatedVal

interpretFlowMethod _ rt@R.FlowRuntime {..} (L.GetRandomBytes k p next) = do
  isArtV2ReplayEnabledWithSessId <- runFlow rt ARTUtils.shouldARTV2Replay
  if isArtV2ReplayEnabledWithSessId
      then do
        let replayRandomBytesEntry = RandomBytesEntryT $ RandomBytesEntry {
          functionName = "getRandomBytes",
          input = p,
          value = A.Null,
          tag = k
        }
        eResp <- runFlow rt $ do
          mSessId <- L.getLoggerContext "x-request-id"
          ER.callBrahmaReplayWithRespCodeCheck replayRandomBytesEntry mSessId
        case eResp of
          Left err -> do
            when shouldLogCallStackART $ do
              runFlow rt $ do
                L.logErrorV @Text "CALLSTACK_ART_ERROR_RANDOM_BYTES" $ GHC.prettyCallStack $ GHC.callStack
                L.logErrorV @Text "CALLTIME_ERROR" $ err
            generatedVal <- CryptoRandom.getRandomBytes p
            pure $ next generatedVal
          Right resp -> do
            let maybeReply = A.eitherDecode resp
            next <$> case maybeReply of
              Left err -> do
                let errMsg = encodeUtf8 err
                when shouldLogCallStackART $ do
                  runFlow rt $ do
                    L.logErrorV @Text "CALLSTACK_ART_ERROR_RANDOM_BYTES" $ GHC.prettyCallStack $ GHC.callStack
                    L.logErrorV @Text "CALLTIME_ERROR" $ err
                runFlow rt $ L.throwException $ S.err500 {S.errBody = errMsg}
              Right reply -> case fromArtRecordedValue reply of
                                Left err -> do
                                  runFlow rt $ do
                                    L.logErrorV @Text "CALLSTACK_ART_ERROR_RANDOM_BYTES : fromArtRecordedValue " err
                                    L.throwException $ S.err500 {S.errBody = fromString err}
                                Right val -> pure val
      else do
        generatedVal <- CryptoRandom.getRandomBytes p
        shouldRecordV2 <- ARTUtils.shouldARTV2Record _optionsLocal
        when shouldRecordV2 $ do
          case toArtRecordingValue generatedVal of
            Right val -> do
              let randomBytesEntry = RandomBytesEntryT $ RandomBytesEntry {
                                  functionName = "getRandomBytes",
                                  input = p,
                                  value = val,
                                  tag = k
                                }
              runFlow rt $ R.producePayload KUtils.RANDOM_BYTES (Lazy.toStrict $ A.encode randomBytesEntry)
            Left err ->
              runFlow rt $ L.logErrorV @Text "CALLSTACK_ART_ERROR_RANDOM_BYTES : toArtRecordingValue " err
        pure $ next generatedVal

interpretFlowMethod _ _ (L.RunSysCmd cmd next) =
  next <$> readCreateProcess (shell cmd) ""

----------------------------------------------------------------------
interpretFlowMethod mbFlowGuid rt@R.FlowRuntime {..} (L.Fork desc newFlowGUID flow next) = do
  awaitableMVar <- newEmptyMVar
  shouldRecord <- shouldARTRecord _optionsLocal
  shouldRecordV2 <- ARTUtils.shouldARTV2Record _optionsLocal
  when (shouldRecord) $ do
    m <- takeMVar _recordingLocal
    putMVar _recordingLocal $ m <> [ForkFlowEntryT $ ForkFlowEntry desc newFlowGUID]
  when (not isArtReplayEnabled) $ do
      tid <- forkIO $ do
        rt' <- EEMF.refreshIOMetricLocalOptions =<< refreshLocalOptionsForART shouldRecord
        res <- runFlow' (Just newFlowGUID) rt' $ do
          res' <- L.runSafeFlow flow
          when isForkFlowLatencyMetricEnabled logLatencyMetricLog
          when (shouldRecordForkFLow && shouldRecord) $ readRecordingsAndWriteToFileForkFLow desc newFlowGUID
          when (shouldRecordV2) $ R.producePayload KUtils.FORK_FLOW (Lazy.toStrict $ A.encode $ A.String "fork flow")
          pure res'
        case res of
          Left (err :: Text) ->
              runLogger mbFlowGuid (R._loggerRuntime . R._coreRuntime $ rt) $
                L.masterLogger
                  Error
                  ("Exception while executing Fork function" :: Text)
                  "ERROR"
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  (Just $ ErrorL Nothing "FORK_ERROR" $ "Exception : " <> err)
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  (Message (Just $ A.toJSON ("Exception : " <> err <> (" , Stack Trace ") <> (Text.pack $ prettyCallStack callStack))) Nothing)
                  Nothing
          Right _ -> pure ()
        putMVar awaitableMVar res
      labelThread tid $ "euler-Fork:" ++ Text.unpack desc
  pure $ next $ Awaitable awaitableMVar
  where
    refreshLocalOptionsForART shouldRec = if shouldRec
          then do
            newRecordingLocal <- newMVar ([])
            R.forkFlowWithNewRecordingLocal rt newRecordingLocal
          else pure rt

----------------------------------------------------------------------

interpretFlowMethod _ _ (L.Await mbMcs (Awaitable awaitableMVar) next) = do
  let act = case mbMcs of
        Nothing -> do
          val <- readMVar awaitableMVar
          case val of
            Left err  -> pure $ Left $ ForkedFlowError err
            Right res -> pure $ Right res
        Just (Microseconds mcs) -> awaitMVarWithTimeout awaitableMVar $ fromIntegral mcs
  next <$> act

interpretFlowMethod _ _ (L.ThrowException ex _) = do
  throwIO ex

interpretFlowMethod mbFlowGuid rt (L.CatchException comp handler cont) =
  cont <$> catch (runFlow' mbFlowGuid rt comp) (runFlow' mbFlowGuid rt . handler)

-- Lack of impredicative polymorphism in GHC makes me sad. - Koz
interpretFlowMethod mbFlowGuid rt (L.Mask cb cont) =
  cont <$> mask (\cb' -> runFlow' mbFlowGuid rt (cb (dimap (runFlow' mbFlowGuid rt) (L.runIO' "Mask") cb')))

interpretFlowMethod mbFlowGuid rt (L.UninterruptibleMask cb cont) =
  cont <$> uninterruptibleMask
    (\cb' -> runFlow' mbFlowGuid rt (cb (dimap (runFlow' mbFlowGuid rt) (L.runIO' "UninterruptibleMask") cb')))

interpretFlowMethod mbFlowGuid rt (L.GeneralBracket acquire release use' cont) =
  cont <$> generalBracket
    (runFlow' mbFlowGuid rt acquire)
    (\x -> runFlow' mbFlowGuid rt . release x)
    (runFlow' mbFlowGuid rt . use')

interpretFlowMethod mbFlowGuid rt (L.RunSafeFlow _ flow next) = fmap next $ do
  fl <- try @_ @SomeException $ runFlow' mbFlowGuid rt flow
  pure $ mapLeft show fl

----------------------------------------------------------------------

interpretFlowMethod _ R.FlowRuntime {..} (L.InitSqlDBConnection cfg next) =
  fmap next $ do
    let connTag = dbConfigToTag cfg
    connMap <- takeMVar _sqldbConnections
    res <- case Map.lookup connTag connMap of
      Just _ -> pure $ Left $ DBError ConnectionAlreadyExists $ "Connection for " <> connTag <> " already created."
      Nothing -> connect cfg
    case res of
      Right conn -> putMVar _sqldbConnections $ Map.insert connTag (bemToNative conn) connMap
      Left _     -> putMVar _sqldbConnections connMap
    pure res

interpretFlowMethod _ R.FlowRuntime {..} (L.DeInitSqlDBConnection conn next) =
  fmap next $ do
    let connTag = sqlConnToTag conn
    connMap <- takeMVar _sqldbConnections
    case Map.lookup connTag connMap of
      Nothing -> putMVar _sqldbConnections connMap
      Just _ -> do
        disconnect conn
        putMVar _sqldbConnections $ Map.delete connTag connMap

interpretFlowMethod _ R.FlowRuntime {..} (L.GetSqlDBConnection cfg next) =
  fmap next $ do
    let connTag = dbConfigToTag cfg
    connMap <- readMVar _sqldbConnections
    pure $ case Map.lookup connTag connMap of
      Just conn -> Right $ nativeToBem connTag conn
      Nothing   -> Left $ DBError ConnectionDoesNotExist $ "Connection for " <> connTag <> " does not exists."

interpretFlowMethod _ R.FlowRuntime {..} (L.InitKVDBConnection cfg next) =
  fmap next $ do
    let connTag = kvdbConfigToTag cfg
    connections <- takeMVar _kvdbConnections
    res <- case Map.lookup connTag connections of
      Just _  -> pure $ Left $ KVDBError KVDBConnectionAlreadyExists $ "Connection for " +|| connTag ||+ " already created."
      Nothing -> connectRedis cfg
    case res of
      Left _  -> putMVar _kvdbConnections connections
      Right conn -> putMVar _kvdbConnections
        $ Map.insert connTag (kvdbToNative conn) connections
    pure res

interpretFlowMethod _ R.FlowRuntime {..} (L.DeInitKVDBConnection conn next) =
  fmap next $ do
    let connTag = kvdbConnToTag conn
    connections <- takeMVar _kvdbConnections
    case Map.lookup connTag connections of
      Nothing -> putMVar _kvdbConnections connections
      Just _ -> do
        R.kvDisconnect $ kvdbToNative conn
        putMVar _kvdbConnections $ Map.delete connTag connections

interpretFlowMethod _ R.FlowRuntime {..} (L.GetKVDBConnection cfg next) =
  fmap next $ do
    let connTag = kvdbConfigToTag cfg
    connMap <- readMVar _kvdbConnections
    pure $ case Map.lookup connTag connMap of
      Just conn -> Right $ nativeToKVDB connTag conn
      Nothing   -> Left $ KVDBError KVDBConnectionDoesNotExist $ "Connection for " +|| connTag ||+ " does not exists."

interpretFlowMethod mbFlowGuid flowRt (L.RunDB conn (L.SqlDBFlow sqlDbMethod runInTransaction) next) = do
    let dbgLogger msg =
          if R.shouldFlowLogRawSql flowRt
          then runLogger mbFlowGuid (R._loggerRuntime . R._coreRuntime $ flowRt)
              $ L.masterLogger Debug ("RunDB Impl" :: String) "DB" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Message (Just $ A.toJSON msg) Nothing) Nothing
          else pure ()
    rawSqlTVar <- newTVarIO mempty
    -- This function would be used inside beam and write raw sql, generated by beam backend, in TVar.
    let dbgLogAction = \rawSqlStr -> atomically (modifyTVar' rawSqlTVar (`DL.snoc` rawSqlStr)) *> dbgLogger rawSqlStr
    EEMF.withMonitoringIO mbFlowGuid EEMT.DB flowRt $ fmap (next . fst . connPoolExceptionWrapper) $ tryAny $ if runInTransaction
      then do
        eRes <- R.withTransaction conn $ \nativeConn -> runSqlDB nativeConn dbgLogAction sqlDbMethod
        eRes' <- case eRes of
                  Left exception -> Left <$> wrapException mbFlowGuid flowRt exception
                  Right x        -> pure $ Right x
        rawSql <- DL.toList <$> readTVarIO rawSqlTVar
        pure (eRes', rawSql)
      else do
        eRes <- try @_ @SomeException $
          case conn of
            PostgresPool _ pool ->
              DP.withResource pool $ \conn' ->
                runSqlDB (NativePGConn conn') dbgLogAction $ sqlDbMethod
            MySQLPool _ pool ->
              DP.withResource pool $ \conn' ->
                runSqlDB (NativeMySQLConn conn') dbgLogAction $ sqlDbMethod
            SQLitePool _ pool ->
              DP.withResource pool $ \conn' ->
                runSqlDB (NativeSQLiteConn conn') dbgLogAction $ sqlDbMethod
            MockingPool _ -> throw $ MockingError "interpretFlowMethod SqlDBFlow called in RunDB interpreter"
        wrapAndSend rawSqlTVar eRes
  where
      wrapAndSend rawSqlLoc eResult = do
        rawSql <- DL.toList <$> readTVarIO rawSqlLoc
        eResult' <- case eResult of
          Left exception -> Left <$> wrapException mbFlowGuid flowRt exception
          Right x        -> pure $ Right x
        pure (eResult', rawSql)

interpretFlowMethod mbFlowGuid flowRt (L.RunDB conn (L.TransactionFlow f) next) = do
    fmap next $ do
      eRes <- R.withTransaction conn $ runFlow' mbFlowGuid flowRt . f
      case eRes of
          Left exception -> Left <$> wrapException mbFlowGuid flowRt exception
          Right x        -> pure $ Right x

interpretFlowMethod mbFlowGuid flowRt (L.RunDBWithRetry conn (L.SqlDBFlow sqlDbMethod runInTransaction) next) = do
    EEMF.withMonitoringIO mbFlowGuid EEMT.DB flowRt $ do
      let dbgLogger msg =
            if R.shouldFlowLogRawSql flowRt
            then runLogger mbFlowGuid (R._loggerRuntime . R._coreRuntime $ flowRt)
                $ L.masterLogger Debug ("RunDB Impl" :: String) "DB" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Message (Just $ A.toJSON msg) Nothing) Nothing
            else pure ()
      res <- fmap (next . fst . connPoolExceptionWrapper) $ tryAny $ (runDBHelper dbgLogger dbRetryAttemps)
      pure res

  where
      wrapAndSend rawSqlLoc eResult = do
        rawSql <- DL.toList <$> readTVarIO rawSqlLoc
        eResult' <- case eResult of
          Left exception -> Left <$> wrapException mbFlowGuid flowRt exception
          Right x        -> pure $ Right x
        pure (eResult', rawSql)

      isTextPresent :: Text -> Text -> Bool
      isTextPresent searchText textToSearchIn = Text.isInfixOf searchText textToSearchIn

      runDBHelper dbLogger attempt = do
        rawSqlTVar <- newTVarIO mempty
        -- This function would be used inside beam and write raw sql, generated by beam backend, in TVar.
        let dbgLogAction = \rawSqlStr -> atomically (modifyTVar' rawSqlTVar (`DL.snoc` rawSqlStr)) *> dbLogger rawSqlStr
        res <- if runInTransaction
          then do
            eRes <- R.withTransaction conn $ \nativeConn -> runSqlDB nativeConn dbgLogAction sqlDbMethod
            eRes' <- case eRes of
                      Left exception -> Left <$> wrapException mbFlowGuid flowRt exception
                      Right x        -> pure $ Right x
            rawSql <- DL.toList <$> readTVarIO rawSqlTVar
            pure (eRes', rawSql)
          else do
            eRes <- try @_ @SomeException $
              -- Only Retries for Postgres as of now TODO: should handle for Mysql and redis
              case conn of
                PostgresPool _ pool ->
                  DP.withResourceAndRetry pool $ \conn' ->
                    runSqlDB (NativePGConn conn') dbgLogAction $ sqlDbMethod
                MySQLPool _ pool ->
                  DP.withResource pool $ \conn' ->
                    runSqlDB (NativeMySQLConn conn') dbgLogAction $ sqlDbMethod
                SQLitePool _ pool ->
                  DP.withResource pool $ \conn' ->
                    runSqlDB (NativeSQLiteConn conn') dbgLogAction $ sqlDbMethod
                MockingPool _ -> throw $ MockingError "interpretFlowMethod SqlDBFlow called in RunDBWithRetry interpreter"
            wrapAndSend rawSqlTVar eRes
        case (conn,res) of
          (PostgresPool _ _,(Left (DBError _ _),_)) ->
            if ((attempt :: Integer) > 0)
              then runDBHelper dbLogger (attempt - 1)
              else pure $ res
          (_, (Left (DBError _ txt),_)) ->
            if ((attempt :: Integer) > 0 && (isTextPresent "resource vanished" txt))
              then do
                  (threadDelay dbRetryTime)
                  (runDBHelper dbLogger (attempt - 1))
              else pure $ res
          _ -> pure res


interpretFlowMethod mbFlowGuid flowRt (L.RunDBWithRetry conn (L.TransactionFlow f) next) = do
    fmap next $ do
      eRes <- R.withTransaction conn $ runFlow' mbFlowGuid flowRt . f
      case eRes of
          Left exception -> Left <$> wrapException mbFlowGuid flowRt exception
          Right x        -> pure $ Right x


interpretFlowMethod mbFlowGuid flowRt@(R.FlowRuntime {..}) (L.RunKVDB cName act next) = do
    shouldRecord <- shouldARTRecord _optionsLocal
    shouldRecordV2 <- ARTUtils.shouldARTV2Record _optionsLocal
    EEMF.withMonitoringIO mbFlowGuid EEMT.REDIS flowRt $ next <$> runKVDB mbFlowGuid shouldRecord shouldRecordV2 producePayloadIO flowRt cName _kvdbConnections act
  where
    producePayloadIO :: KUtils.ValueType -> ByteString -> IO()
    producePayloadIO k v = runFlow flowRt $ R.producePayload k v


interpretFlowMethod mbFlowGuid rt@R.FlowRuntime {_pubSubController, _pubSubConnection} (L.RunPubSub act next) =
    case _pubSubConnection of
      Nothing -> go $ error "Connection to pubSub is not set in FlowRuntime"
      Just cn -> go cn
  where
    go conn = next <$> runPubSub _pubSubController conn
      (L.unpackLanguagePubSub act $ runFlow' mbFlowGuid rt)

interpretFlowMethod mbFlowGuid flowRt (L.RunDBWithConn conn sqlDbMethod next) = do
    EEMF.withMonitoringIO mbFlowGuid EEMT.DB flowRt $ do
      let dbgLogger msg =
            if R.shouldFlowLogRawSql flowRt
            then runLogger mbFlowGuid (R._loggerRuntime . R._coreRuntime $ flowRt)
                $ L.masterLogger Debug ("RunDB Impl" :: String) "DB" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Message (Just $ A.toJSON msg) Nothing) Nothing
            else pure ()
      rawSqlTVar <- newTVarIO mempty
      -- This function would be used inside beam and write raw sql, generated by beam backend, in TVar.
      let dbgLogAction = \rawSqlStr -> atomically (modifyTVar' rawSqlTVar (`DL.snoc` rawSqlStr)) *> dbgLogger rawSqlStr
      val <- fmap (next . fst . connPoolExceptionWrapper) $ tryAny $ do
        eRes <- try @_ @SomeException $ runSqlDB conn dbgLogAction $ sqlDbMethod
        wrapAndSend rawSqlTVar eRes
      pure val
  where
      wrapAndSend rawSqlLoc eResult = do
        rawSql <- DL.toList <$> readTVarIO rawSqlLoc
        eResult' <- case eResult of
          Left exception -> Left <$> wrapException mbFlowGuid flowRt exception
          Right x        -> pure $ Right x
        pure (eResult', rawSql)

interpretFlowMethod _ rt (L.WithModifiedRuntime f flow next) = next <$> runFlow (f rt) flow

interpretFlowMethod _ R.FlowRuntime {..} (L.GetSnowflakeID sId pId k next) = next . either handleException id <$> (try @_ @SomeException $ generateSnowflake' sId pId k _snowflakeGenerator)
  where
    handleException :: SomeException -> Either SnowflakeError Snowflake
    handleException = Left . Fatal . Text.pack . show

runFlow' :: Maybe FlowGUID -> R.FlowRuntime -> L.Flow a -> IO a
runFlow' mbFlowGuid flowRt (L.Flow comp) = foldF (interpretFlowMethod mbFlowGuid flowRt) comp

runFlow :: R.FlowRuntime -> L.Flow a -> IO a
runFlow = runFlow' Nothing

-- Helpers

wrapException :: HasCallStack => Maybe Text -> R.FlowRuntime -> SomeException -> IO DBError
wrapException mbFlowGuid flowRt exception = do
  let exception' = (wrapException' exception)
  runFlow' mbFlowGuid flowRt $ L.logException exception
  pure exception'

wrapException' :: SomeException -> DBError
wrapException' e = fromMaybe (DBError UnrecognizedError $ show e)
  (sqliteErrorToDbError   (show e) <$> fromException e <|>
    mysqlErrorToDbError    (show e) <$> fromException  e <|>

      postgresErrorToDbError (show e) <$> fromException e)
connPoolExceptionWrapper :: Either SomeException (Either DBError _a1, [Text]) -> (Either DBError _a1, [Text])
connPoolExceptionWrapper (Left e) = (Left $ DBError ConnectionFailed $ show e, [])
connPoolExceptionWrapper (Right r) = r

dbConfigToTag :: DBConfig beM -> ConnTag
dbConfigToTag = \case
  PostgresPoolConf t _ _ -> t
  MySQLPoolConf t _ _    -> t
  SQLitePoolConf t _ _   -> t
  MockingConf t _ _      -> t

sqlConnToTag :: SqlConn beM -> ConnTag
sqlConnToTag = \case
  PostgresPool t _ -> t
  MySQLPool t _    -> t
  SQLitePool t _   -> t
  MockingPool t    -> t

kvdbConfigToTag :: KVDBConfig -> Text
kvdbConfigToTag = \case
  KVDBConfig t _        -> t
  KVDBClusterConfig t _ -> t

kvdbConnToTag :: KVDBConn -> Text
kvdbConnToTag (Redis t _) = t

addErrorInfoToResponseHeaders :: HTTPResponse  -> Maybe ErrorInfo -> HTTPResponse
addErrorInfoToResponseHeaders validRes (Just errorInfo) =
  let errorHeaders = Map.fromList [("x-error_code", errorInfo.error_code)
                      , ("x-error_message", errorInfo.error_message)
                      , ("x-error_category", errorInfo.error_category)
                      , ("x-unified_error_code", errorInfo.unified_error_code)
                      , ("x-unified_error_message", errorInfo.unified_error_message)]
  in validRes {getResponseHeaders = validRes.getResponseHeaders <> errorHeaders}
addErrorInfoToResponseHeaders validRes Nothing = validRes

getConnTagFromDbName :: ModelDBConfig beM -> DBName -> Text
getConnTagFromDbName dbConf dbName = case dbConf of
  WithFallbackDB oldDbCfg newDbCfg -> if dbName == ECRDB then dbConfigToTag oldDbCfg  else dbConfigToTag newDbCfg
  WithoutFallbackDB oldDbCfg -> dbConfigToTag oldDbCfg