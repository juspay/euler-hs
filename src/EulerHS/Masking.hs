{-# LANGUAGE RecordWildCards #-}

module EulerHS.Masking where

import           Data.HashSet (member)
import           EulerHS.Prelude
import Data.String.Conversions hiding ((<>))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson.Key as AKey
import qualified Data.CaseInsensitive as CI
import qualified Data.HashSet as HS
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Bifunctor as Bifunctor
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified EulerHS.Extra.Regex as Regex
import qualified EulerHS.Logger.Types as Log
import qualified Network.HTTP.Types as HTTP

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

parseRequestResponseBody :: (Text -> Bool) -> Text -> Maybe ByteString -> ByteString -> Aeson.Value
parseRequestResponseBody shouldMask maskText mbContentType req
  | isContentTypeBlockedForLogging mbContentType = notSupportedPlaceHolder mbContentType
  | otherwise =
      case Aeson.eitherDecodeStrict req of
        Right value -> maskJSON shouldMask maskText mbContentType value
        Left _ -> maskJSON shouldMask maskText mbContentType $ handleQueryString req

maskJSON :: (Text -> Bool) -> Text -> Maybe ByteString -> Aeson.Value -> Aeson.Value
maskJSON shouldMask maskText mbContentType (Aeson.Object r) = Aeson.Object $ handleObject shouldMask maskText mbContentType r
maskJSON shouldMask maskText mbContentType (Aeson.Array r) =  Aeson.Array $ maskJSON shouldMask maskText mbContentType <$> r
maskJSON shouldMask maskText mbContentType (Aeson.String r) =
  bool (Aeson.String r) (decodeToObject) (doesContentTypeHaveNestedStringifiedJSON mbContentType)
  where
    decodeToObject =
      case Aeson.eitherDecodeStrict $ encodeUtf8 $ r of
        Right val ->
          case val of
            (Aeson.Object v) -> Aeson.Object $ handleObject shouldMask maskText Nothing v
            (Aeson.Array _) -> maskJSON shouldMask maskText Nothing val
            _ -> val
        Left _ -> Aeson.String r
maskJSON _ _ _ value = value

handleObject :: (Text -> Bool) -> Text -> Maybe ByteString -> Aeson.Object -> Aeson.Object
handleObject shouldMask maskText mbContentType = AKM.mapWithKey maskingFn
  where
    maskingFn key value = bool (maskJSON shouldMask maskText mbContentType value) (Aeson.String maskText) $ shouldMask (AKey.toText key)

handleQueryString :: ByteString -> Aeson.Value
handleQueryString strg = Aeson.Object . fmap (Aeson.String . fromMaybe "") . KeyMap.fromList . map (Bifunctor.first AesonKey.fromText) $ HTTP.parseQueryText strg

notSupportedPlaceHolder :: Maybe ByteString -> Aeson.Value
notSupportedPlaceHolder (Just bs) = Aeson.String $ "Logging Not Support For this content " <> decodeUtf8 bs
notSupportedPlaceHolder Nothing = Aeson.String "Logging Not Support For this content "

isContentTypeBlockedForLogging :: Maybe ByteString -> Bool
isContentTypeBlockedForLogging Nothing = False
isContentTypeBlockedForLogging (Just contentType) =
       Text.isInfixOf "html" (Text.toLower $ decodeUtf8 contentType)
    || Text.isInfixOf "xml" (Text.toLower $ decodeUtf8 contentType)


-- NOTE: This logic is added because we are sending stringified JSON as Value
-- TODO: Can we convert these into application/json api calls ?
doesContentTypeHaveNestedStringifiedJSON :: Maybe ByteString -> Bool
doesContentTypeHaveNestedStringifiedJSON Nothing = False
doesContentTypeHaveNestedStringifiedJSON (Just contentType) = (("application/x-www-form-urlencoded" :: ByteString) == contentType)

getContentTypeForServant :: HTTP.ResponseHeaders -> Maybe ByteString
getContentTypeForServant = List.lookup HTTP.hContentType

getContentTypeForHTTP :: Map.Map Text Text -> Maybe ByteString
getContentTypeForHTTP header = getContentTypeForServant getTupleList
  where
    getTupleList = makeHeaderLableCI <$> Map.assocs header
    makeHeaderLableCI (headerName,headerValue) = (CI.mk $ encodeUtf8 headerName, encodeUtf8 headerValue)

-- PS Implemention for masking XML [blacklisting]
-- TODO: move away from regex
maskXMLText :: Maybe (HS.HashSet Text) -> Text.Text -> Text.Text
maskXMLText (Just customMaskingKeys) xml = foldl' (\acc x -> maskXMLForTAG x $ maskXMLForAttribute x acc) xml customMaskingKeys
maskXMLText Nothing xml = foldl' (\acc x -> maskXMLForTAG x $ maskXMLForAttribute x acc) xml defaultMaskingKeys

maskXMLForAttribute :: Text.Text -> Text.Text -> Text.Text
maskXMLForAttribute key xmlToMask =
  case (Regex.regex ("(" <> key <> ")=\"[^>]*(\")" :: Text.Text)) of
    Left _ -> "[HIDDEN]" -- "ISSUE WITH REGEX"
    Right cRegex -> Regex.replace cRegex (((toSBSFromText key) <>  "=\"FILTERED\"") :: SBS) xmlToMask 

maskXMLForTAG :: Text.Text -> Text.Text -> Text.Text
maskXMLForTAG key xmlToMask = 
  case (Regex.regex ("<(" <> key <> ")>[^</]*</(" <> key <> ")>" :: Text.Text)) of
    Left _ -> "[HIDDEN]" -- "ISSUE WITH REGEX"
    Right cRegex -> Regex.replace cRegex (("<"  <> (toSBSFromText key) <>  ">FILTERED</" <> (toSBSFromText key) <> ">") :: SBS) xmlToMask 

toSBSFromText :: Text.Text -> ByteString
toSBSFromText = encodeUtf8

defaultMaskingKeys :: HS.HashSet Text 
defaultMaskingKeys = HS.fromList []
