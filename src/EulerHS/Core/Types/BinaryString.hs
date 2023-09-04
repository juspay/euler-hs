{- |
Module      :  EulerHS.Core.Types.BinaryString
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

module EulerHS.Core.Types.BinaryString
( BinaryString(..)
, LBinaryString(..)
, base64Encode
, base64Decode
) where

import           EulerHS.Prelude

import qualified Control.Monad.Fail as MonadFail
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.String.Conversions as Conversions
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding



--------------------------------------------------------------------------
-- Base64 encoding/decoding helpers
--------------------------------------------------------------------------

newtype BinaryString
  = BinaryString
    { getBinaryString :: Strict.ByteString }
    deriving (Show, Eq, Ord)

instance ToJSON BinaryString where
  toJSON val = toJSON ((show $ getBinaryString val) :: Text)

instance FromJSON BinaryString where
  parseJSON val = do
    bs <- parseJSON val
    pure $ BinaryString $ read bs

instance Conversions.ConvertibleStrings Strict.ByteString BinaryString where
  convertString = BinaryString

instance Conversions.ConvertibleStrings BinaryString Strict.ByteString where
  convertString = getBinaryString

instance Conversions.ConvertibleStrings Lazy.ByteString BinaryString where
  convertString = BinaryString . Conversions.convertString

instance Conversions.ConvertibleStrings BinaryString Lazy.ByteString where
  convertString = Conversions.convertString . getBinaryString

--------------------------------------------------------------------------
-- Lazy BinaryString
--------------------------------------------------------------------------

newtype LBinaryString
  = LBinaryString
    { getLBinaryString :: Lazy.ByteString }
    deriving (Show, Eq, Ord)

instance ToJSON LBinaryString where
  toJSON val = toJSON (show (getLBinaryString val) :: Text)

instance FromJSON LBinaryString where
  parseJSON val = do
    lbs <- parseJSON val
    pure $ LBinaryString $ read lbs

instance Conversions.ConvertibleStrings Lazy.ByteString LBinaryString where
  convertString = LBinaryString

instance Conversions.ConvertibleStrings LBinaryString Lazy.ByteString where
  convertString = getLBinaryString

instance Conversions.ConvertibleStrings Strict.ByteString LBinaryString where
  convertString = LBinaryString . Conversions.convertString

instance Conversions.ConvertibleStrings LBinaryString Strict.ByteString where
  convertString = Conversions.convertString . getLBinaryString

--------------------------------------------------------------------------
-- Base64 encoding/decoding helpers
--------------------------------------------------------------------------

-- | Base64 encode a bytestring
--
-- NOTE: Decoding to UTF-8 cannot fail so is safe
--
base64Encode :: Strict.ByteString -> Text.Text
base64Encode = Encoding.decodeUtf8 . B64.encode

-- | Base64 decode a Base64-encoded string
--
-- NOTE: This may fail if the string is malformed using MonadFail
--
base64Decode :: MonadFail.MonadFail m => Text.Text -> m Strict.ByteString
base64Decode s = case B64.decode (Encoding.encodeUtf8 s) of
  Left err  -> fail err
  Right res -> return res
