{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE ScopedTypeVariables     #-}
module EulerHS.ApiHelpers where

import EulerHS.Masking
import Control.Monad.Error.Class (catchError)
import Data.Aeson as A
import Data.ByteString.Builder (toLazyByteString)
import Data.List (init, isSuffixOf)
import Data.Time.Clock (diffTimeToPicoseconds)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (diffAbsoluteTime)
import EulerHS.Api
import EulerHS.ART.Types
import EulerHS.ART.Utils
import EulerHS.Prelude
import EulerHS.Options (OptionKey)
import qualified Control.Exception as Exception
import qualified Data.Bool as Bool
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LBS hiding (length, take, drop)
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Encoding as TE
import qualified EulerHS.BinaryString as T
import qualified EulerHS.HttpAPI as InternalHttp
import qualified EulerHS.Logger.Types as Log
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Status as HttpStatus
import qualified Servant.API as SAPI
import qualified Servant.Client as SC
import qualified Servant.Client.Core as SCC
import qualified Servant.Client.Free as SCF
import qualified Servant.Client.Internal.HttpClient as SCIHC
import qualified Data.Sequence as Seq
import qualified EulerHS.Extra.Monitoring.Flow as Monitoring
import qualified Network.HTTP.Client as HC
import Data.Maybe (fromJust)
import Servant.Types.SourceT
import EulerHS.Compression (compressionDecider, compress, mkCompressionConfig, compressionHeader, getChunkSizeForStream
                          , logCompressionHelper)
import qualified EulerHS.Extra.KafkaClient.Utils as KUtils
import qualified Data.ByteString.Char8 as BS8
import EulerHS.EnvVars (shouldLogEncryptedLogs, getHeadersToFilterFromOutgoingRequest)
import EulerHS.Encryption (aesEncryptText)
import qualified EulerHS.Framework.Runtime as R
import           EulerHS.Common (FlowGUID)


mkServantApiCallLogEntry :: (Show apiTag) => Maybe Log.LogMaskingConfig -> SCF.BaseUrl -> SCC.Request -> SCC.Response -> A.Value -> Integer -> apiTag -> Maybe Log.ErrorInfo -> Maybe Text -> Maybe Text -> ServantApiCallLogEntry
mkServantApiCallLogEntry mbMaskConfig bUrl req res res_headers' lat apiTag errInfo mbLogEncKey mbLogEncIv = do
  let (url, req_headers', req_body', queryParams) = getRequestInfoToLog mbMaskConfig bUrl req mbLogEncKey mbLogEncIv
      res_body' = parseRequestResponseBody (shouldMaskKey mbMaskConfig) getMaskText mbLogEncKey mbLogEncIv (getContentTypeForServant . toList $ SCC.responseHeaders res) (LBS.toStrict $ SCC.responseBody res)
  ServantApiCallLogEntry
    { url = url
    , method = method'
    , req_headers = req_headers'
    , req_body = req_body'
    , res_code = res_code'
    , res_body = res_body'
    , res_headers = res_headers'
    , latency = Just lat
    , req_query_params = queryParams
    , api_tag = Just $ show apiTag
    , req_type = (Bool.bool InternalHttp.EXTERNAL InternalHttp.INTERNAL (InternalHttp.shouldBypassProxy $ Just $ Text.pack host))
    , error_info = errInfo
    }
  where 
    method' = TE.decodeUtf8 $ SCC.requestMethod req

    res_code' = HttpStatus.statusCode $ SCC.responseStatusCode res

    host = SCF.baseUrlHost bUrl

    getMaskText :: Text
    getMaskText = maybe defaultMaskText (fromMaybe defaultMaskText . Log._maskText) mbMaskConfig

mkServantApiCallLogEntryWithoutMasking :: SCC.Request -> SCC.Response -> EncryptedServantApiCallLogEntry
mkServantApiCallLogEntryWithoutMasking req res = do
  EncryptedServantApiCallLogEntry
    {
      req_body = req_body_without_masking
    , res_body = resp_body_without_masking
    }
  where
    resp_body_without_masking = fromMaybe (A.String "") $ A.decode $ SCC.responseBody res

    req_body_without_masking = fromMaybe (A.String "") $ A.decode $
     case SCC.requestBody req of
      Just (reqbody, _) -> do
        case reqbody of
          SCC.RequestBodyBS s  -> LBS.fromStrict s
          SCC.RequestBodyLBS s -> s
          SCC.RequestBodySource sr -> (LBS.fromStrict . BS8.pack . show . SCC.RequestBodySource) sr
      Nothing -> (A.encode . A.String) "body = (empty)"

client :: SC.HasClient EulerClient api => Proxy api -> SC.Client EulerClient api
client api = SCC.clientIn api $ Proxy @EulerClient

interpretClientF :: (Show apiTag) => Maybe FlowGUID -> R.FlowRuntime -> (forall msg. A.ToJSON msg => Log.Category -> Log.LogLevel -> Log.Action -> Maybe Log.ErrorL -> Maybe Log.Latency -> Maybe Log.RespCode -> msg -> IO()) -> (LBS.ByteString  -> Maybe Log.ErrorInfo)
  -> Maybe Log.LogMaskingConfig -> MVar (Map OptionKey Any) -> MVar ([RecordingEntry]) -> SCC.BaseUrl -> Bool -> Bool -> (KUtils.ValueType -> ByteString -> IO ()) -> apiTag -> [(Text, Text)] -> Maybe Text -> Maybe Text -> Bool -> SCF.ClientF a -> SC.ClientM a
interpretClientF _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (SCF.Throw e) = throwM e
interpretClientF mbFlowGuid flowRt log errFunc mbMaskConfig optionsLocal recordingLocal bUrl shouldRecord shouldRecordV2 producePayloadIO apiTag additionalHeaders mbLogEncKey mbLogEncIv isInternalAPICall (SCF.RunRequest req' next) = do 
  start <- liftIO $ systemToTAITime <$> getSystemTime
  let modifiedFilteredReq = if isInternalAPICall then modifiedReq else requestWithheaderFilter modifiedReq -- no filter on headers for internal calls
  mightBeCompressedRequest <- liftIO $ compressAndAddHeader modifiedReq
  mightBeCompressedRequestWithHeaderFilter <- liftIO $ compressAndAddHeader modifiedFilteredReq
  validRes <- catchError (SCC.runRequestAcceptStatus Nothing mightBeCompressedRequestWithHeaderFilter) (errorHandler start)
  when (shouldRecord || shouldRecordV2) $ do
    let apiEntry = CallAPIEntryT $ CallAPIEntry {
          jsonRequest = fromServantRequest mightBeCompressedRequestWithHeaderFilter bUrl, 
          jsonResult = Right $ A.toJSON $ fromServantResponse validRes,
          apiTag = show apiTag}
    when (shouldRecord) $ do
      m <- takeMVar recordingLocal
      putMVar recordingLocal $ m <> [apiEntry]
    when (shouldRecordV2) $ liftIO $ producePayloadIO KUtils.OUTGOING_API (LBS.toStrict $ A.encode apiEntry)
  end <- liftIO $ systemToTAITime <$> getSystemTime
  let errInfo = errFunc $ SCC.responseBody validRes
      lat = div (diffTimeToPicoseconds $ diffAbsoluteTime end start) picoMilliDiff
      updatedRes = addErrorInfoToResponseHeaders validRes errInfo
      resHeadersHM = getHMResHeaders updatedRes
      mbDownstreamOutgoingLat = HM.lookup Monitoring.internalApiLatencyHeader resHeadersHM
      logEntry = mkServantApiCallLogEntry mbMaskConfig bUrl mightBeCompressedRequest updatedRes (A.toJSON resHeadersHM) lat apiTag errInfo mbLogEncKey mbLogEncIv
      logEntryWithoutMasking = mkServantApiCallLogEntryWithoutMasking mightBeCompressedRequest updatedRes
      errLog = mkErrorLog =<< errInfo
  liftIO $ Monitoring.updateApiLatencyMonitoring optionsLocal mbDownstreamOutgoingLat lat 
  liftIO $ log "OUTGOING_API" Log.Info (decodeUtf8 $ SCC.requestMethod mightBeCompressedRequest) errLog (Just lat) (Just $ HttpStatus.statusCode $ SCC.responseStatusCode updatedRes) logEntry
  when (shouldLogEncryptedLogs) $
    case (mbLogEncKey, mbLogEncIv) of
      (Just kmsDecEncKey, Just kmsDecEncIV) -> do
        !mCompressedLogEntryWithoutMasking <- liftIO $ logCompressionHelper mbFlowGuid flowRt ((LBS.toStrict . A.encode) logEntryWithoutMasking)
        case mCompressedLogEntryWithoutMasking of
          Just compressedLogEntryWithoutMasking -> do
            let encryptedCompressedLogEntryWithoutMasking = either ("encryption failed :: " <>) id $ (aesEncryptText kmsDecEncKey kmsDecEncIV . decodeUtf8) compressedLogEntryWithoutMasking
            liftIO $ log "ENCRYPTED_OUTGOING_API" Log.Info (decodeUtf8 $ SCC.requestMethod mightBeCompressedRequest) errLog (Just lat) (Just $ HttpStatus.statusCode $ SCC.responseStatusCode updatedRes) encryptedCompressedLogEntryWithoutMasking
          _ -> pure ()
      _ -> pure()
  pure $ next updatedRes
  where
    getHMResHeaders res = (foldl' (\m (k,v) -> HM.insert k v m) HM.empty)
      $ fmap (bimap (TE.decodeUtf8 . CI.original) TE.decodeUtf8)
      $ maskServantHeaders (shouldMaskKey mbMaskConfig) getMaskText
      $ SCC.responseHeaders res
    
    getMaskText :: Text
    getMaskText = maybe defaultMaskText (fromMaybe defaultMaskText . Log._maskText) mbMaskConfig
    
    chunksOf :: Int64 -> LBS.ByteString -> [LBS.ByteString]
    chunksOf i ls = map (LBS.take i) (build (splitter ls))
      where
        build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
        build g = g (:) []

        splitter :: LBS.ByteString -> (LBS.ByteString -> a -> a) -> a -> a
        splitter "" _ n = n
        splitter l c n = l `c` splitter (LBS.drop i l) c n

    compressAndAddHeader :: SCC.Request -> IO SCC.Request
    compressAndAddHeader uncompressedReq = do
      let (compressionConfig, mHeaderValForCompression) = mkCompressionConfig
          !shouldCompress = compressionDecider (show apiTag) (getBaseUrl bUrl)
      (mReqBody,mMime,rbType) <- 
            case SCC.requestBody uncompressedReq of
              (Just (SCC.RequestBodyLBS reqBody,mime)) -> pure (Just reqBody,Just mime, "LBS" :: Text)
              (Just (SCC.RequestBodyBS reqBody,mime))  -> pure (Just $ LBS.fromStrict reqBody,Just mime, "SBS" :: Text)
              (Just (SCC.RequestBodySource streamByteString, mime)) -> do
                reqBody <- (runExceptT . runSourceT) (SAPI.fromSourceIO streamByteString)
                case reqBody of
                  Right r ->
                    pure (Just $ LBS.concat r , Just mime ,"S_BS" :: Text)
                  Left err -> do 
                    liftIO $ log "OUTGOING_API" Log.Error ("Unable to extract request body [fromSourceIO] to compress" :: Text) Nothing Nothing (Just (-1)) err
                    pure (Nothing, Nothing, "S_BS" :: Text)
              (_) -> pure (Nothing, Nothing, "" :: Text)
      case mReqBody of
        Just rBody -> do 
          let !mCompressedBody = bool (rBody) (compress compressionConfig rBody) shouldCompress
              !mCompressedReq =
                case rbType of 
                  "LBS"  -> SCC.setRequestBodyLBS mCompressedBody (fromJust mMime) uncompressedReq
                  "SBS"  -> SCC.setRequestBodyLBS mCompressedBody (fromJust mMime) uncompressedReq
                  "S_BS" -> SCC.setRequestBody (SCC.RequestBodySource (SAPI.toSourceIO (chunksOf getChunkSizeForStream mCompressedBody))) (fromJust mMime) uncompressedReq
                  _ -> uncompressedReq
          pure $ bool (mCompressedReq) (SCC.addHeader compressionHeader (show mHeaderValForCompression :: Text) mCompressedReq) shouldCompress
        Nothing -> pure $ uncompressedReq
       

    modifiedReq = foldl (\req (h, headerValue) -> addHeaderIfNotPresent (CI.mk $ encodeUtf8 h) headerValue req) req' additionalHeaders

    requestWithheaderFilter :: SCC.Request -> SCC.Request
    requestWithheaderFilter req = req { SCC.requestHeaders = Seq.filter (\(key, _) -> ((TE.decodeUtf8 . CI.original) key) `notElem` getHeadersToFilterFromOutgoingRequest) (SCC.requestHeaders req) }

    addHeaderIfNotPresent headerName headerValue request =
      let allHeaders = SCC.requestHeaders request
          headerPresent = headerName `elem` (map fst allHeaders)
        in if headerPresent
              then request
              else SCC.addHeader headerName headerValue request

    errorHandler startTime err = do
      endTime <- liftIO $ systemToTAITime <$> getSystemTime
      let lat = div (diffTimeToPicoseconds $ diffAbsoluteTime endTime startTime) picoMilliDiff
      case err of
        SC.FailureResponse _ resp ->
            either (defaultErrorLogger reqMethod lat) (\x -> logJsonError ("FailureResponse" :: Text) reqMethod lat (InternalHttp.getResponseCode x) (getErrorResponseWithRequest x)) (translateResponseFHttpResponse resp)
        SC.DecodeFailure txt resp ->
            either (defaultErrorLogger reqMethod lat) (\x -> logJsonError (("DecodeFailure: " :: Text) <> txt) reqMethod lat (InternalHttp.getResponseCode x) (getErrorResponseWithRequest x)) (translateResponseFHttpResponse resp)
        SC.UnsupportedContentType mediaType resp ->
            either (defaultErrorLogger reqMethod lat) (\x -> logJsonError (("UnsupportedContentType: " :: Text) <> (show @Text mediaType)) reqMethod lat (InternalHttp.getResponseCode x) (getErrorResponseWithRequest x)) (translateResponseFHttpResponse resp)
        SC.InvalidContentTypeHeader resp ->
            either (defaultErrorLogger reqMethod lat) (\x -> logJsonError ("InvalidContentTypeHeader" :: Text) reqMethod lat (InternalHttp.getResponseCode x) (getErrorResponseWithRequest x)) (translateResponseFHttpResponse resp)
        SC.ConnectionError exception -> defaultErrorLogger reqMethod lat $ exceptionToValue exception
      when (shouldRecord || shouldRecordV2) $ do
        let apiEntry = CallAPIEntryT $ CallAPIEntry {
            jsonRequest = fromServantRequest modifiedReq bUrl, 
            jsonResult = Right $ getRespFromClientError err,
            apiTag = show apiTag}
        when (shouldRecord) $ do
          m <- takeMVar recordingLocal
          putMVar recordingLocal $ m <> [apiEntry]
        when (shouldRecordV2) $ liftIO $ producePayloadIO KUtils.OUTGOING_API (LBS.toStrict $ A.encode apiEntry)
      throwM err

    getRespFromClientError err =
        case err of
          SC.FailureResponse _ resp -> A.toJSON $ fromServantResponse resp
          SC.DecodeFailure _ resp -> A.toJSON $ fromServantResponse resp
          SC.UnsupportedContentType _ resp -> A.toJSON $ fromServantResponse resp
          SC.InvalidContentTypeHeader resp -> A.toJSON $ fromServantResponse resp
          SC.ConnectionError exception -> exceptionToValue exception

    picoMilliDiff :: Integer
    picoMilliDiff = 1000000000

    exceptionToValue :: SomeException -> Value
    exceptionToValue ex = 
      case (fromException ex :: Maybe HC.HttpException) of
        Just httpEx -> toJSON httpEx
        Nothing -> toJSON $ displayException ex        

    getBaseUrl = (Text.pack . SCF.baseUrlHost)

    reqMethod = decodeUtf8 $ SCC.requestMethod modifiedReq

    logJsonError :: Text -> Text -> Integer -> Int -> ServantApiCallLogEntry -> SC.ClientM ()
    logJsonError err method latency responseCode res = 
      let errorBody = Log.ErrorL Nothing "API_ERROR" err
        in liftIO $ log "OUTGOING_API" Log.Error method (Just errorBody) (Just latency) (Just responseCode) res

    defaultErrorLogger :: forall msg. A.ToJSON msg => Text -> Integer ->  msg -> SC.ClientM ()
    defaultErrorLogger method latency msg = liftIO $ log "OUTGOING_API" Log.Error method Nothing (Just latency) (Just (-1)) (getErrorReasonWithRequest latency msg)

    getErrorReasonWithRequest :: forall msg. A.ToJSON msg => Integer -> msg -> ServantApiCallLogEntry
    getErrorReasonWithRequest latency msg =
      let (url, req_headers, req_body, req_query_params) = getRequestInfoToLog mbMaskConfig bUrl req' mbLogEncKey mbLogEncIv
          host = SCF.baseUrlHost bUrl
          in
            ServantApiCallLogEntry
              { url = url
              , method = TE.decodeUtf8 $ SCC.requestMethod modifiedReq
              , req_headers = req_headers
              , req_body = req_body
              , req_query_params = req_query_params
              , res_code = (-1)
              , res_body = A.toJSON msg
              , res_headers = A.Null
              , error_info = Nothing
              , latency = (Just latency)
              , api_tag = (Just $ show apiTag)
              , req_type = (Bool.bool InternalHttp.EXTERNAL InternalHttp.INTERNAL (InternalHttp.shouldBypassProxy $ Just $ Text.pack host))
              }

    mkErrorLog :: Log.ErrorInfo -> Maybe Log.ErrorL
    mkErrorLog errInfo = Just $ Log.ErrorL (Just errInfo.error_code) errInfo.error_category errInfo.error_message

    getErrorResponseWithRequest x =
      let (url, req_headers, req_body, req_query_params) = getRequestInfoToLog mbMaskConfig bUrl req' mbLogEncKey mbLogEncIv
          response = InternalHttp.maskHTTPResponse mbMaskConfig x Nothing mbLogEncKey mbLogEncIv
          host = SCF.baseUrlHost bUrl
          in
            ServantApiCallLogEntry
              { url = url
              , method = TE.decodeUtf8 $ SCC.requestMethod modifiedReq
              , req_headers = req_headers
              , req_body = req_body
              , req_query_params = req_query_params
              , res_code = response.getResponseCode
              , res_body = response.getResponseBody
              , res_headers = A.toJSON $ response.getResponseHeaders
              , error_info = Nothing
              , latency = Nothing
              , api_tag = Nothing
              , req_type = (Bool.bool InternalHttp.EXTERNAL InternalHttp.INTERNAL (InternalHttp.shouldBypassProxy $ Just $ Text.pack host))
              }

getRequestInfoToLog :: Maybe Log.LogMaskingConfig -> SCF.BaseUrl -> SCC.Request -> Maybe Text -> Maybe Text -> (String,A.Value, A.Value,A.Value)
getRequestInfoToLog mbMaskConfig bUrl req mbLogEncKey mbLogEncIv =
  (baseUrl <> LBS.toString (toLazyByteString (SCC.requestPath req))
  , req_headers'
  , req_body'
  , queryParams
  )
  where
    queryParams = queryToJson
      $ fmap (bimap TE.decodeUtf8 (TE.decodeUtf8 <$>))
      $ maskQueryStrings (shouldMaskKey mbMaskConfig) getMaskText
      $ SCC.requestQueryString req

    req_headers' = headersToJson
      $ fmap (bimap (TE.decodeUtf8 . CI.original) TE.decodeUtf8)
      $ maskServantHeaders (shouldMaskKey mbMaskConfig) getMaskText
      $ SCC.requestHeaders req

    req_body' = case SCC.requestBody req of
      Just (reqbody, _) -> do
        let contentType = getContentTypeForServant . toList $ SCC.requestHeaders req
        case reqbody of
          SCC.RequestBodyBS s  -> parseRequestResponseBody (shouldMaskKey mbMaskConfig) getMaskText mbLogEncKey mbLogEncIv contentType s
          SCC.RequestBodyLBS s -> parseRequestResponseBody (shouldMaskKey mbMaskConfig) getMaskText mbLogEncKey mbLogEncIv contentType $ LBS.toStrict s
          SCC.RequestBodySource sr -> A.String $ show $ SCC.RequestBodySource sr
      Nothing -> A.String "body = (empty)"

    baseUrlString = SCF.showBaseUrl bUrl
    baseUrl = if "/" `isSuffixOf` baseUrlString then init baseUrlString else baseUrlString

    getMaskText :: Text
    getMaskText = maybe defaultMaskText (fromMaybe defaultMaskText . Log._maskText) mbMaskConfig

    queryToJson :: Seq (Text, Maybe Text) -> A.Value
    queryToJson = A.toJSON . foldl' (\m (k,v) -> HM.insert k v m) HM.empty

    headersToJson :: Seq (Text, Text) -> A.Value
    headersToJson = A.toJSON . foldl' (\m (k,v) -> HM.insert k v m) HM.empty


addErrorInfoToResponseHeaders :: SCC.Response -> Maybe Log.ErrorInfo -> SCC.Response
addErrorInfoToResponseHeaders validRes@SCC.Response{..} (Just errorInfo) =
  let errorHeaders = [("x-error_code", encodeUtf8 $ errorInfo.error_code)
                      , ("x-error_message", encodeUtf8 errorInfo.error_message)
                      , ("x-error_category", encodeUtf8 errorInfo.error_category)
                      , ("x-unified_error_code", encodeUtf8 errorInfo.unified_error_code)
                      , ("x-unified_error_message", encodeUtf8 $ errorInfo.unified_error_message)]
      modifiedHeaders = Seq.fromList errorHeaders
  in validRes {SCC.responseHeaders = responseHeaders <> modifiedHeaders}
addErrorInfoToResponseHeaders responseHeaders Nothing = responseHeaders

runEulerClient :: (Show apiTag) => Maybe FlowGUID -> R.FlowRuntime -> (forall msg. A.ToJSON msg => Log.Category -> Log.LogLevel -> Log.Action -> Maybe Log.ErrorL -> Maybe Log.Latency -> Maybe Log.RespCode -> msg -> IO())-> (LBS.ByteString  -> Maybe Log.ErrorInfo)
              -> Maybe Log.LogMaskingConfig -> MVar (Map OptionKey Any) -> MVar ([RecordingEntry]) -> SCC.BaseUrl -> EulerClient a -> Bool -> Bool -> (KUtils.ValueType -> ByteString -> IO()) -> apiTag -> [(Text, Text)] -> Maybe Text -> Maybe Text -> Bool -> SCIHC.ClientM a
runEulerClient mbFlowGuid flowRt log errFunc mbMaskConfig optionsLocal recordingLocal bUrl (EulerClient f) shouldRecord shouldRecordV2 producePayloadIO apiTag additionalHeaders mbLogEncKey mbLogEncIv isInternalAPICall = foldFree (interpretClientF mbFlowGuid flowRt log errFunc mbMaskConfig optionsLocal recordingLocal bUrl shouldRecord shouldRecordV2 producePayloadIO apiTag additionalHeaders mbLogEncKey mbLogEncIv isInternalAPICall) f

translateResponseFHttpResponse :: SC.Response -> Either Text InternalHttp.HTTPResponse
translateResponseFHttpResponse SC.Response{..} = do
  headers <- translateResponseHeaders $ toList responseHeaders
  status <-  translateResponseStatusMessage $ HTTP.statusMessage responseStatusCode
  pure $ InternalHttp.HTTPResponse
    { getResponseBody    = T.LBinaryString responseBody
    , getResponseCode    = HTTP.statusCode responseStatusCode
    , getResponseHeaders = headers
    , getResponseStatus  = status
    }

translateResponseHeaders
  :: [(CI.CI Strict.ByteString, Strict.ByteString)]
  -> Either Text (Map.Map Text.Text Text.Text)
translateResponseHeaders httpLibHeaders = do
  let
    result = do
      headerNames <- mapM  (Encoding.decodeUtf8' . CI.original . fst) httpLibHeaders
      headerValues <- mapM (Encoding.decodeUtf8' . snd) httpLibHeaders
      return $ zip (map Text.toLower headerNames) headerValues
  headers <- displayEitherException "Error decoding HTTP response headers: " result
  pure $ Map.fromList headers

translateResponseStatusMessage :: Strict.ByteString -> Either Text Text
translateResponseStatusMessage = displayEitherException "Error decoding HTTP response status message: " . Encoding.decodeUtf8'

displayEitherException :: Exception e => Text -> Either e a -> Either Text a
displayEitherException prefix = either (Left . (prefix <>) . Text.pack . Exception.displayException) Right
