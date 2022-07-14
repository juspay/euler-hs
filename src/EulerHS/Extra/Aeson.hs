module EulerHS.Extra.Aeson
( stripLensPrefixOptions
, stripAllLensPrefixOptions
, jsonSetField
, encodeJSON
, decodeJSON
) where

import           Prelude

import           Data.Aeson (FromJSON, ToJSON, Options, defaultOptions, fieldLabelModifier)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LazyText


stripLensPrefixOptions :: Options
stripLensPrefixOptions = defaultOptions { fieldLabelModifier = drop 1 }

stripAllLensPrefixOptions :: Options
stripAllLensPrefixOptions = defaultOptions { fieldLabelModifier = dropPrefix}
  where
    dropPrefix :: String -> String
    dropPrefix field = if length field > 0
                         then dropWhile (== head field) field
                         else field

-- utility functions

-- | Set a field inside a JSON Object
jsonSetField :: ToJSON a => Text -> a -> Aeson.Value -> Aeson.Value
jsonSetField fieldName fieldValue obj = case obj of
  Aeson.Object fields ->
    Aeson.Object $ HashMap.insert fieldName (Aeson.toJSON fieldValue) fields
  _ ->
    error $ "This should be an object... got " <> show obj

-- | Encode a value to JSON Text
--
-- Note: the name `jsonEncode` is already taken by Aeson
encodeJSON :: ToJSON a => a -> Text
encodeJSON = LazyText.toStrict . Aeson.encodeToLazyText

-- | Parse JSON Text into a value
decodeJSON :: FromJSON a => Text -> Maybe a
decodeJSON = Aeson.decode . LazyByteString.fromStrict . Text.encodeUtf8
