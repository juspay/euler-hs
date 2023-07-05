{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE  RankNTypes #-}
{-# LANGUAGE  ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}

module EulerHS.Api where

import qualified Control.Exception as Exception
import qualified Data.ByteString as Strict
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS (toStrict)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy.UTF8 as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import           Data.Time.Clock (diffTimeToPicoseconds)
import           Data.Time.Clock.System (getSystemTime, systemToTAITime)
import           Data.Time.Clock.TAI (diffAbsoluteTime)
import           Data.List (init, isSuffixOf)
import qualified Data.Text.Encoding as TE (decodeUtf8)
import           GHC.Generics ()
import qualified EulerHS.Logger.Types as Log
import           EulerHS.Masking (defaultMaskText, getContentTypeForServant,
                                  maskServantHeaders,
                                  maskQueryStrings,
                                  parseRequestResponseBody, shouldMaskKey)
import           EulerHS.Prelude
import qualified EulerHS.BinaryString as T
import qualified Servant.Client as SC
import qualified Servant.Client.Core as SCC
import           Servant.Client.Core.RunClient (RunClient)
import qualified Servant.Client.Free as SCF
import qualified Servant.Client.Internal.HttpClient as SCIHC
import qualified Network.HTTP.Types.Status as HttpStatus
import qualified EulerHS.Options as T
import           Control.Monad.Error.Class (catchError)

import qualified Network.HTTP.Types as HTTP
import qualified EulerHS.HttpAPI as InternalHttp

newtype EulerClient a = EulerClient (Free SCF.ClientF a)
    deriving newtype (Functor, Applicative, Monad, RunClient)

data LogServantRequest
  = LogServantRequest
    { url         :: String
    , method      :: Text
    , body        :: A.Value
    , headers     :: Seq (Text, Text)
    , queryString :: Seq (Text, Maybe Text)
    }
    deriving stock (Show,Generic)
    deriving anyclass A.ToJSON

data LogServantResponse
  = LogServantResponse
    { statusCode  :: String
    , headers     :: Seq (Text,Text)
    , httpVersion :: String
    , body        :: A.Value
    }
    deriving stock (Show,Generic)
    deriving anyclass A.ToJSON

data ServantApiCallLogEntry = ServantApiCallLogEntry
  { url :: String
  , method :: Text
  , req_headers :: A.Value
  , req_body :: A.Value
  , res_code :: Int
  , res_body :: A.Value
  , res_headers :: A.Value
  , latency :: Integer
  , req_query_params :: A.Value
  }
    deriving stock (Show,Generic)
    deriving anyclass A.ToJSON

data ApiTag = ApiTag
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (ToJSON, FromJSON)

instance T.OptionEntity ApiTag Text

mkServantApiCallLogEntry :: Maybe Log.LogMaskingConfig -> SCF.BaseUrl -> SCC.Request -> SCC.Response -> Integer -> ServantApiCallLogEntry
mkServantApiCallLogEntry mbMaskConfig bUrl req res lat = ServantApiCallLogEntry
    { url = baseUrl <> (LBS.toString $ toLazyByteString (SCC.requestPath req))
    , method = method'
    , req_headers = req_headers'
    , req_body = req_body'
    , res_code = res_code'
    , res_body = res_body'
    , res_headers = res_headers'
    , latency = lat
    , req_query_params = queryParams
    }
  where
    queryParams = queryToJson
      $ fmap (bimap TE.decodeUtf8 (TE.decodeUtf8 <$>))
      $ maskQueryStrings (shouldMaskKey mbMaskConfig) getMaskText
      $ SCC.requestQueryString req
    method' = TE.decodeUtf8 $ SCC.requestMethod req
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

    queryToJson :: Seq (Text, Maybe Text) -> A.Value
    queryToJson = A.toJSON . foldl' (\m (k,v) -> HM.insert k v m) HM.empty

    headersToJson :: Seq (Text, Text) -> A.Value
    headersToJson = A.toJSON . foldl' (\m (k,v) -> HM.insert k v m) HM.empty

    baseUrlString = SCF.showBaseUrl bUrl
    baseUrl = if isSuffixOf "/" baseUrlString then init baseUrlString else baseUrlString

client :: SC.HasClient EulerClient api => Proxy api -> SC.Client EulerClient api
client api = SCC.clientIn api $ Proxy @EulerClient

interpretClientF :: (forall msg. A.ToJSON msg => Log.LogLevel -> Log.Action -> Log.Entity -> Maybe (Log.ErrorL) -> Maybe Log.Latency -> Maybe Log.RespCode -> msg -> IO()) 
  -> Maybe Log.LogMaskingConfig -> SCC.BaseUrl -> SCF.ClientF a -> SC.ClientM a
interpretClientF _   _   _ (SCF.Throw e) = throwM e
interpretClientF log mbMaskConfig bUrl (SCF.RunRequest req next) = do
  start <- liftIO $ systemToTAITime <$> getSystemTime
  validRes <- catchError (SCC.runRequestAcceptStatus Nothing req) (errorHandler start)
  end <- liftIO $ systemToTAITime <$> getSystemTime
  let lat = div (diffTimeToPicoseconds $ diffAbsoluteTime end start) picoMilliDiff
  let logEntry = mkServantApiCallLogEntry mbMaskConfig bUrl req validRes lat
  liftIO $ log Log.Info (show $ SCC.requestMethod req) "EXT_TAG" Nothing (Just lat) (Just $ HttpStatus.statusCode $ SCC.responseStatusCode validRes) logEntry -- log_todo : extract external tag
  pure $ next validRes
  where
    errorHandler startTime err = do
      endTime <- liftIO $ systemToTAITime <$> getSystemTime
      let lat = div (diffTimeToPicoseconds $ diffAbsoluteTime endTime startTime) picoMilliDiff
      case err of
        SC.FailureResponse _ resp ->
            either (defaultErrorLogger reqMethod lat) (\x -> logJsonError ("FailureResponse" :: Text) reqMethod lat (InternalHttp.getResponseCode x) (InternalHttp.maskHTTPResponse mbMaskConfig x Nothing)) (translateResponseFHttpResponse resp)
        SC.DecodeFailure txt resp -> 
            either (defaultErrorLogger reqMethod lat) (\x -> logJsonError (("DecodeFailure: " :: Text) <> txt) reqMethod lat (InternalHttp.getResponseCode x) (InternalHttp.maskHTTPResponse mbMaskConfig x Nothing)) (translateResponseFHttpResponse resp)
        SC.UnsupportedContentType mediaType resp -> 
            either (defaultErrorLogger reqMethod lat) (\x -> logJsonError (("UnsupportedContentType: " :: Text) <> (show @Text mediaType)) reqMethod lat (InternalHttp.getResponseCode x) (InternalHttp.maskHTTPResponse mbMaskConfig x Nothing)) (translateResponseFHttpResponse resp)
        SC.InvalidContentTypeHeader resp -> 
            either (defaultErrorLogger reqMethod lat) (\x -> logJsonError ("InvalidContentTypeHeader" :: Text) reqMethod lat (InternalHttp.getResponseCode x) (InternalHttp.maskHTTPResponse mbMaskConfig x Nothing)) (translateResponseFHttpResponse resp)
        SC.ConnectionError exception -> defaultErrorLogger reqMethod lat $ displayException exception
      throwM err
        
    picoMilliDiff :: Integer
    picoMilliDiff = 1000000000

    reqMethod = decodeUtf8 $ SCC.requestMethod req
    
    logJsonError :: Text -> Text -> Integer -> Int -> InternalHttp.HTTPResponseMasked -> SC.ClientM ()
    logJsonError err method latency responseCode res = 
      let errorBody = Log.ErrorL Nothing err err
        in liftIO $ log Log.Error method "EXT_TAG" (Just errorBody) (Just latency) (Just responseCode) (InternalHttp.HTTPResponseException err res)
    
    defaultErrorLogger :: forall msg. A.ToJSON msg => Text -> Integer ->  msg -> SC.ClientM ()
    defaultErrorLogger method latency msg = liftIO $ log Log.Error method "EXT_TAG" Nothing (Just latency) (Just (-1)) msg

runEulerClient :: (forall msg. A.ToJSON msg => Log.LogLevel -> Log.Action -> Log.Entity -> Maybe (Log.ErrorL) -> Maybe Log.Latency -> Maybe Log.RespCode -> msg -> IO()) 
              -> Maybe Log.LogMaskingConfig -> SCC.BaseUrl -> EulerClient a -> SCIHC.ClientM a
runEulerClient log mbMaskConfig bUrl (EulerClient f) = foldFree (interpretClientF log mbMaskConfig bUrl) f

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