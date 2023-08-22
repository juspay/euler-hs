{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module EulerHS.Extra.Orphans where

import Data.Aeson as A
import Prelude
import Data.Text
import Data.ByteString
import Universum
import qualified Data.HashMap.Strict as HM
import qualified Database.Redis as R
import qualified Data.Text as T
import Data.Scientific

instance ToJSON ByteString where -- Need to check is this right way
  toJSON x = A.String (decodeUtf8 x)

instance FromJSON ByteString where -- Need to check is this right way
  parseJSON (A.String val) = pure $ encodeUtf8 val
  parseJSON val = (pure . encodeUtf8) (decodeUtf8 . A.encode $ val :: Text)

instance ToJSON R.XReadOpts where
  toJSON R.XReadOpts{..} = A.Object $ HM.fromList [("block", toJSON block),("recordCount",toJSON recordCount)]

instance FromJSON R.XReadOpts where
  parseJSON (A.Object hm) = do
    let block =  toInteger' (HM.lookup "block" hm)
        recordCount = toInteger' (HM.lookup "recordCount" hm)
        noAck' = toBoolean (HM.lookup "recordCount" hm)
    pure (R.XReadOpts block recordCount noAck')
    where
      toBoolean (Just (A.String v)) = Just True == (readMaybe $ T.unpack v)
      toBoolean (Just (A.Bool v)) = v
      toBoolean _ = False

      toInteger' (Just (A.String v)) = readMaybe $ T.unpack v
      toInteger' (Just (A.Number v)) = Just $ coefficient v
      toInteger' _ = Nothing
  parseJSON _ = pure R.defaultXreadOpts
