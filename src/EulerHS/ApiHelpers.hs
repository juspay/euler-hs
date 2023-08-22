{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE RecordWildCards     #-}
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
import EulerHS.ART.EnvVars
import EulerHS.Prelude
import qualified Control.Exception as Exception
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Encoding as TE
import qualified EulerHS.BinaryString as T
import qualified EulerHS.HttpAPI as InternalHttp
import qualified EulerHS.Logger.Types as Log
import qualified EulerHS.Options ()
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Status as HttpStatus
import qualified Servant.Client as SC
import qualified Servant.Client.Core as SCC
import qualified Servant.Client.Free as SCF
import qualified Servant.Client.Internal.HttpClient as SCIHC
import qualified Data.Sequence as Seq

mkServantApiCallLogEntry :: (Show apiTag) => Maybe Log.LogMaskingConfig -> SCF.BaseUrl -> SCC.Request -> SCC.Response -> Integer -> apiTag -> Maybe Log.ErrorInfo -> ServantApiCallLogEntry
mkServantApiCallLogEntry mbMaskConfig bUrl req res lat apiTag errInfo = do
  let (url, req_headers', req_body', queryParams) = getRequestInfoToLog mbMaskConfig bUrl req
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
    , error_info = errInfo
    }
  where 
    method' = TE.decodeUtf8 $ SCC.requestMethod req
    
    res_code' = HttpStatus.statusCode $ SCC.responseStatusCode res
    res_body' = parseRequestResponseBody (shouldMaskKey mbMaskConfig) getMaskText (getContentTypeForServant . toList $ SCC.responseHeaders res)
        . LBS.toStrict
        $ SCC.responseBody res
    res_headers' = headersToJson
      $ fmap (bimap (TE.decodeUtf8 . CI.original) TE.decodeUtf8)
      $ maskServantHeaders (shouldMaskKey mbMaskConfig) getMaskText
      $ SCC.responseHeaders res
    getMaskText :: Text
    getMaskText = maybe defaultMaskText (fromMaybe defaultMaskText . Log._maskText) mbMaskConfig

    headersToJson :: Seq (Text, Text) -> A.Value
    headersToJson = A.toJSON . foldl' (\m (k,v) -> HM.insert k v m) HM.empty

client :: SC.HasClient EulerClient api => Proxy api -> SC.Client EulerClient api
client api = SCC.clientIn api $ Proxy @EulerClient

interpretClientF :: (Show apiTag) => (forall msg. A.ToJSON msg => Log.LogLevel -> Log.Action -> Maybe Log.ErrorL -> Maybe Log.Latency -> Maybe Log.RespCode -> msg -> IO()) -> (LBS.ByteString  -> Maybe Log.ErrorInfo)
  -> Maybe Log.LogMaskingConfig -> MVar (Map Text Any) -> MVar ([RecordingEntry]) -> SCC.BaseUrl -> apiTag -> Maybe Text -> SCF.ClientF a -> SC.ClientM a
interpretClientF _ _  _  _ _ _ _ _ (SCF.Throw e) = throwM e
interpretClientF log errFunc mbMaskConfig _ recordingLocal bUrl apiTag mHostValue (SCF.RunRequest req' next) = do 
  start <- liftIO $ systemToTAITime <$> getSystemTime
  validRes <- catchError (SCC.runRequestAcceptStatus Nothing req) (errorHandler start)
  when (isArtRecEnabled) $ do
    m <- takeMVar recordingLocal
    let apiEntry = CallAPIEntryT $ CallAPIEntry {
        jsonRequest = fromServantRequest req, 
        jsonResult = Right $ A.toJSON $ fromServantResponse validRes}
    putMVar recordingLocal $ m <> [apiEntry]
  end <- liftIO $ systemToTAITime <$> getSystemTime
  let errInfo = errFunc $ SCC.responseBody validRes
      lat = div (diffTimeToPicoseconds $ diffAbsoluteTime end start) picoMilliDiff
      updatedRes = addErrorInfoToResponseHeaders validRes errInfo
      logEntry = mkServantApiCallLogEntry mbMaskConfig bUrl req updatedRes lat apiTag errInfo
      errLog = mkErrorLog =<< errInfo
  liftIO $ log Log.Info (decodeUtf8 $ SCC.requestMethod req) errLog (Just lat) (Just $ HttpStatus.statusCode $ SCC.responseStatusCode updatedRes) logEntry
  pure $ next updatedRes
  where
    req = maybe req' (\ hostValue -> SCC.addHeader "x-tenant-host" hostValue req') mHostValue
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
        SC.ConnectionError exception -> defaultErrorLogger reqMethod lat $ displayException exception
      throwM err

    picoMilliDiff :: Integer
    picoMilliDiff = 1000000000

    reqMethod = decodeUtf8 $ SCC.requestMethod req

    logJsonError :: Text -> Text -> Integer -> Int -> ServantApiCallLogEntry -> SC.ClientM ()
    logJsonError err method latency responseCode res = 
      let errorBody = Log.ErrorL Nothing "API_ERROR" err
        in liftIO $ log Log.Error method (Just errorBody) (Just latency) (Just responseCode) res

    defaultErrorLogger :: forall msg. A.ToJSON msg => Text -> Integer ->  msg -> SC.ClientM ()
    defaultErrorLogger method latency msg = liftIO $ log Log.Error method Nothing (Just latency) (Just (-1)) msg

    mkErrorLog :: Log.ErrorInfo -> Maybe Log.ErrorL
    mkErrorLog errInfo = Just $ Log.ErrorL (Just errInfo.error_code) errInfo.error_category errInfo.error_message

    getErrorResponseWithRequest x =
      let (url, req_headers, req_body, req_query_params) = getRequestInfoToLog mbMaskConfig bUrl req'
          response = InternalHttp.maskHTTPResponse mbMaskConfig x Nothing
          in
            ServantApiCallLogEntry
              { url = url
              , method = TE.decodeUtf8 $ SCC.requestMethod req
              , req_headers = req_headers
              , req_body = req_body
              , req_query_params = req_query_params
              , res_code = response.getResponseCode
              , res_body = response.getResponseBody
              , res_headers = A.toJSON $ response.getResponseHeaders
              , error_info = Nothing
              , latency = Nothing
              , api_tag = Nothing
              }

getRequestInfoToLog :: Maybe Log.LogMaskingConfig -> SCF.BaseUrl -> SCC.Request -> (String,A.Value, A.Value,A.Value)
getRequestInfoToLog mbMaskConfig bUrl req =
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
      Just (reqbody, _) ->
        case reqbody of
          SCC.RequestBodyBS s -> parseRequestResponseBody (shouldMaskKey mbMaskConfig) getMaskText (getContentTypeForServant . toList $ SCC.requestHeaders req) s
          SCC.RequestBodyLBS s -> parseRequestResponseBody (shouldMaskKey mbMaskConfig) getMaskText (getContentTypeForServant . toList $ SCC.requestHeaders req) $ LBS.toStrict s
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

runEulerClient :: (Show apiTag) => (forall msg. A.ToJSON msg => Log.LogLevel -> Log.Action -> Maybe Log.ErrorL -> Maybe Log.Latency -> Maybe Log.RespCode -> msg -> IO())-> (LBS.ByteString  -> Maybe Log.ErrorInfo)
              -> Maybe Log.LogMaskingConfig -> MVar (Map Text Any) -> MVar ([RecordingEntry]) -> SCC.BaseUrl -> EulerClient a -> apiTag -> Maybe Text -> SCIHC.ClientM a
runEulerClient log errFunc mbMaskConfig optionsLocal recordingLocal bUrl (EulerClient f) apiTag mHostValue = foldFree (interpretClientF log errFunc mbMaskConfig optionsLocal recordingLocal bUrl apiTag mHostValue) f

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
