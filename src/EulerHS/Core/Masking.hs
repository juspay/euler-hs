module EulerHS.Core.Masking where


import qualified Data.Aeson as Aeson
import           EulerHS.Prelude
import qualified Network.HTTP.Types as HTTP
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet (member)
import qualified EulerHS.Core.Types.Logger as Log (LogMaskingConfig(..), MaskKeyType (..))
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.List as List

shouldMaskKey :: Maybe Log.LogMaskingConfig -> Text -> Bool
shouldMaskKey Nothing _ = False
shouldMaskKey (Just Log.LogMaskingConfig{..}) key =
  case _keyType of
    Log.WhiteListKey -> not $ member key _maskKeys
    Log.BlackListKey -> member key _maskKeys

defaultMaskText :: Text
defaultMaskText = "***"


maskHTTPHeaders :: (Text -> Bool) -> Text -> Map.Map Text Text ->  Map.Map Text Text
maskHTTPHeaders shouldMask maskText = Map.mapWithKey maskHeader
  where
    maskHeader :: Text -> Text -> Text
    maskHeader key value  = if shouldMask key then maskText else value

maskServantHeaders :: (Text -> Bool) -> Text -> Seq HTTP.Header -> Seq HTTP.Header
maskServantHeaders shouldMask maskText headers = maskHeader <$> headers
  where
    maskHeader :: HTTP.Header -> HTTP.Header
    maskHeader (headerName,headerValue) =
      if shouldMask (decodeUtf8 $ CI.original headerName)
        then (headerName,encodeUtf8 maskText)
        else (headerName,headerValue)

maskQueryStrings :: (Text -> Bool) -> Text -> Seq HTTP.QueryItem -> Seq HTTP.QueryItem
maskQueryStrings shouldMask maskText queryStrings = maskQueryString <$> queryStrings
  where
    maskQueryString :: HTTP.QueryItem -> HTTP.QueryItem
    maskQueryString (key,value) =
      if shouldMask (decodeUtf8 key)
        then (key,Just $ encodeUtf8 maskText)
        else (key,value)

parseRequestResponseBody :: (Text -> Bool) -> Text -> Maybe ByteString -> ByteString -> Text
parseRequestResponseBody shouldMask maskText mbContentType req
  | isContentTypeBlockedForLogging mbContentType = notSupportedPlaceHolder
  | otherwise = 
      case Aeson.eitherDecodeStrict req of
        Right value ->  decodeUtf8 . Aeson.encode $ maskJSON shouldMask maskText value
        Left _ -> decodeUtf8 . Aeson.encode $ maskJSON shouldMask maskText $ handleQueryString req

maskJSON :: (Text -> Bool) -> Text -> Aeson.Value -> Aeson.Value
maskJSON shouldMask maskText (Aeson.Object r) = Aeson.Object $ handleObject shouldMask maskText r
maskJSON shouldMask maskText (Aeson.Array r) =  Aeson.Array $ maskJSON shouldMask maskText <$> r
maskJSON _ _ value = value

handleObject :: (Text -> Bool) -> Text -> Aeson.Object -> Aeson.Object
handleObject shouldMask maskText = HashMap.mapWithKey maskingFn
  where
    maskingFn key value = maskJSON shouldMask maskText $ updatedValue key value
    updatedValue key fn = if shouldMask key then Aeson.String maskText else fn

handleQueryString :: ByteString -> Aeson.Value
handleQueryString strg = Aeson.Object . fmap (Aeson.String . fromMaybe "") . HashMap.fromList $ HTTP.parseQueryText strg

notSupportedPlaceHolder :: Text
notSupportedPlaceHolder = "Logging Not Support For this content"

isContentTypeBlockedForLogging :: Maybe ByteString -> Bool
isContentTypeBlockedForLogging Nothing = False
isContentTypeBlockedForLogging (Just contentType) =
       Text.isInfixOf "html" (Text.toLower $ decodeUtf8 contentType) 
    || Text.isInfixOf "xml" (Text.toLower $ decodeUtf8 contentType)



getContentTypeForServant :: HTTP.ResponseHeaders -> Maybe ByteString
getContentTypeForServant = List.lookup HTTP.hContentType

getContentTypeForHTTP :: Map.Map Text Text -> Maybe ByteString
getContentTypeForHTTP header = getContentTypeForServant $ getTupleList
  where
    getTupleList = makeHeaderLableCI <$> (Map.assocs header)
    makeHeaderLableCI (headerName,headerValue) = (CI.mk $ encodeUtf8 headerName, encodeUtf8 headerValue)
