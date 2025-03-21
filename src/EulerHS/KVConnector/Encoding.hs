{-# LANGUAGE OverloadedStrings #-}
module EulerHS.KVConnector.Encoding
  (
    encode_,
    encodeDead,
    decodeLiveOrDead
  )
 where

import           EulerHS.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Serialize as Cereal
import qualified Data.ByteString.Lazy as BSL
import           Data.Cereal.Instances ()
import           EulerHS.KVConnector.Types (KVEntry(..))
import qualified Juspay.Extra.Env as Env


encode_ :: (Aeson.ToJSON (table Identity), Cereal.Serialize (table Identity)) => Bool -> KVEntry table -> BSL.ByteString
encode_ isEnabled val =
  if isEnabled
     then BSL.fromStrict $ "CBOR" <> Cereal.encode val
     else case getKVEncodingFromat of
        "JSV1" -> "JSV1" <> Aeson.encode val
        _      -> "JSON" <> Aeson.encode val.row

getKVEncodingFromat :: Text
getKVEncodingFromat =
    let envType =  Env.JuspayEnv
                    { key = "KV_ENCODING_FORMAT"
                    , actionLeft = Env.mkDefaultEnvAction ("JSON" :: Text)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

-- LIVE/DEAD marker for values

encodeDead :: BSL.ByteString -> BSL.ByteString
encodeDead val = "DEAD" <> val

decodeLiveOrDead :: BSL.ByteString -> (Bool, BSL.ByteString)
decodeLiveOrDead val =
  let (h, v) = BSL.splitAt 4 val
    in case h of
      "DEAD" -> (False, v)
      _      -> (True , val)
