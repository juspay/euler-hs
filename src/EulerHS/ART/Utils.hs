{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module EulerHS.ART.Utils
  ( fromServantRequest
  , toServantResponse
  , fromServantResponse
  , toErrorPayload
  , isArtRecEnabled
  , shouldARTRecord
  ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import           EulerHS.ART.Types
import qualified EulerHS.Framework.Language as L
import           EulerHS.Options (OptionKey, mkOptionKey)
import           EulerHS.Prelude hiding (fromList)
import qualified Servant.Client.Core as SCC
import qualified Servant.Client.Free as SCF
import qualified Data.Text as T
import Data.Sequence (fromList)
import qualified Data.ByteString.Builder as BB
import qualified Data.Map as Map
import qualified Data.Text.Lazy.Encoding as TL
import           EulerHS.BinaryString (LBinaryString (LBinaryString),getLBinaryString)
import           EulerHS.HttpAPI
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Status as HTTPS
import qualified Network.HTTP.Types.Version as HTTPV
import           Unsafe.Coerce (unsafeCoerce)

fromServantRequest :: SCC.Request -> SCC.BaseUrl -> HTTPRequest
fromServantRequest req' bUrl = let
      method' = case decodeUtf8 $ SCC.requestMethod req' of
              ("GET"     :: Text) -> Get
              ("PUT"     :: Text) -> Put
              ("POST"    :: Text) -> Post
              ("DELETE"  :: Text) -> Delete
              ("HEAD"    :: Text) -> Head
              ("TRACE"   :: Text) -> Trace
              ("CONNECT" :: Text) -> Connect
              ("OPTIONS" :: Text) -> Options
              ("PATCH"   :: Text) -> Patch
              _         -> error "Invalid HTTP method"
      in HTTPRequest method' req_headers' req_body' (getBaseUrl <> req_path' <> req_query_param') Nothing Nothing
  where
    getBaseUrl :: Text
    getBaseUrl = T.pack . SCF.showBaseUrl $ bUrl

    req_body' :: Maybe LBinaryString
    req_body' = case SCC.requestBody req' of
      Just (reqbody, _) ->
        case reqbody of
          SCC.RequestBodyBS s -> Just $ LBinaryString $ LBS.fromStrict s
          SCC.RequestBodyLBS s -> Just $ LBinaryString s
          SCC.RequestBodySource sr -> Just $ LBinaryString $ TL.encodeUtf8 $ show $ SCC.RequestBodySource sr
      Nothing -> Just $ LBinaryString $ TL.encodeUtf8 "body = (empty)"

    req_headers' :: Map.Map Text Text
    req_headers' = headersToJson
      $ (bimap (decodeUtf8 . CI.original) decodeUtf8) <$> SCC.requestHeaders req'

    req_path' :: Text
    req_path' = decodeUtf8 . LBS.toStrict . BB.toLazyByteString $ SCC.requestPath req'

    req_query_param' :: Text
    req_query_param' = decodeUtf8 . HTTP.renderQuery True . toList . SCC.requestQueryString $ req'

    headersToJson :: Seq (Text, Text) -> Map.Map Text Text
    headersToJson = foldl' (\m (k,v) -> Map.insert k v m) Map.empty

fromServantResponse :: SCC.Response -> HTTPResponse
fromServantResponse res = HTTPResponse res_body' res_code' res_headers' res_status'
  where
    res_body' :: LBinaryString
    res_body' = LBinaryString $ SCC.responseBody res

    res_code' :: Int
    res_code' = HTTP.statusCode $ SCC.responseStatusCode res

    res_status' :: Text
    res_status' = decodeUtf8 $ HTTP.statusMessage $ SCC.responseStatusCode res

    res_headers' :: Map.Map Text Text
    res_headers' = headersToJson
      $ (bimap (decodeUtf8 . CI.original) decodeUtf8) <$> SCC.responseHeaders res

    headersToJson :: Seq (Text, Text) -> Map.Map Text Text
    headersToJson = foldl' (\m (k,v) -> Map.insert k v m) Map.empty

toServantResponse ::  HTTPResponse -> SCC.Response
toServantResponse httpResponse =
  SCC.Response
            { responseStatusCode  = HTTPS.mkStatus (httpResponse.getResponseCode) (encodeUtf8 httpResponse.getResponseStatus)
            , responseHeaders     = fromList ((\(headerName, val) -> (CI.mk (encodeUtf8 headerName), encodeUtf8 val))<$> Map.toList httpResponse.getResponseHeaders)
            , responseHttpVersion = HTTPV.http11
            , responseBody        = getLBinaryString $ getResponseBody httpResponse
            }

toErrorPayload :: Text -> ErrorPayload
toErrorPayload errMsg = ErrorPayload
  { isError = True
  , errorMessage = errMsg
  , userMessage = " Something went wrong."
  }

isArtRecEnabled :: L.MonadFlow m => m Bool
isArtRecEnabled = do
  mShouldRecord <- L.getOptionLocal ShouldRecordSession
  pure $ fromMaybe False mShouldRecord

shouldARTRecord :: MVar (Map OptionKey Any) -> IO Bool
shouldARTRecord optionLocalMVar = do
    m <- readMVar optionLocalMVar
    maybeValue <- pure $ do
      valAny <- Map.lookup (mkOptionKey ShouldRecordSession) m
      pure $ unsafeCoerce valAny
    return $ fromMaybe False maybeValue

