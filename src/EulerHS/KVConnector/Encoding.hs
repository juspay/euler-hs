{- |
Module      :  EulerHS.KVConnector.Encoding
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

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
import Data.Cereal.Instances ()

encode_ :: (Aeson.ToJSON a, Cereal.Serialize a) => Bool -> a -> BSL.ByteString
encode_ isEnabled val =
  if isEnabled
     then BSL.fromStrict $ "CBOR" <> Cereal.encode val
     else "JSON" <> Aeson.encode val


-- LIVE/DEAD marker for values

encodeDead :: BSL.ByteString -> BSL.ByteString
encodeDead val = "DEAD" <> val

decodeLiveOrDead :: BSL.ByteString -> (Bool, BSL.ByteString)
decodeLiveOrDead val =
  let (h, v) = BSL.splitAt 4 val
    in case h of
      "DEAD" -> (False, v)
      _      -> (True , val)
