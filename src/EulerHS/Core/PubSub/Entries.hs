{-# LANGUAGE DeriveAnyClass #-}

module EulerHS.Core.PubSub.Entries where


import           EulerHS.Prelude

import qualified Data.Aeson as A
import qualified EulerHS.Types as T

----------------------------------------------------------------------

data PublishEntry = PublishEntry
    { jsonChannel :: A.Value
    , jsonPayload :: A.Value
    , jsonResult  :: A.Value
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance T.RRItem PublishEntry where
    getTag _ = "PublishEntry"

instance T.MockedResult PublishEntry (Either T.KVDBReply Integer) where
    getMock PublishEntry {jsonResult} = T.jsonDecode jsonResult

mkPublishEntry :: ByteString -> ByteString -> Either T.KVDBReply Integer -> PublishEntry
mkPublishEntry c p r = PublishEntry
    (T.jsonEncode c)
    (T.jsonEncode p)
    (T.jsonEncode r)

----------------------------------------------------------------------

data SubscribeEntry = SubscribeEntry
    { jsonChannels :: A.Value
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance T.RRItem SubscribeEntry where
    getTag _ = "SubscribeEntry"

instance T.MockedResult SubscribeEntry (IO ()) where
    getMock _ = Just $ pure ()

mkSubscribeEntry :: [ByteString] -> IO () -> SubscribeEntry
mkSubscribeEntry c _ = SubscribeEntry $ T.jsonEncode c

----------------------------------------------------------------------

data PSubscribeEntry = PSubscribeEntry
    { jsonPatterns :: A.Value
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance T.RRItem PSubscribeEntry where
    getTag _ = "PSubscribeEntry"

instance T.MockedResult PSubscribeEntry (IO ()) where
    getMock _ = Just $ pure ()

mkPSubscribeEntry :: [ByteString] -> IO () -> PSubscribeEntry
mkPSubscribeEntry p _ = PSubscribeEntry $ T.jsonEncode p

----------------------------------------------------------------------
