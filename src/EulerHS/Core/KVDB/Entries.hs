{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module EulerHS.Core.KVDB.Entries where


import qualified Data.Aeson as A
import           EulerHS.Prelude
import           EulerHS.Types (MockedResult (..), RRItem (..))
import qualified EulerHS.Types as T
import qualified EulerHS.Core.KVDB.Language as L

data SetEntry = SetEntry
  { jsonKey    :: A.Value
  , jsonValue  :: A.Value
  , jsonResult :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance RRItem SetEntry where
  getTag _ = "SetEntry"

instance MockedResult SetEntry (Either T.KVDBReply T.KVDBStatus) where
  getMock SetEntry {jsonResult} = T.jsonDecode jsonResult

mkSetEntry :: ByteString -> ByteString -> Either T.KVDBReply T.KVDBStatus -> SetEntry
mkSetEntry k v r = SetEntry
  (T.jsonEncode k)
  (T.jsonEncode v)
  (T.jsonEncode r)

----------------------------------------------------------------------

data SetExEntry = SetExEntry
  { jsonKey    :: A.Value
  , jsonTtl    :: A.Value
  , jsonValue  :: A.Value
  , jsonResult :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance RRItem SetExEntry where
  getTag _ = "SetExEntry"

instance MockedResult SetExEntry (Either T.KVDBReply T.KVDBStatus) where
  getMock SetExEntry {jsonResult} = T.jsonDecode jsonResult

mkSetExEntry :: ByteString -> Integer -> ByteString -> Either T.KVDBReply T.KVDBStatus -> SetExEntry
mkSetExEntry k e v r = SetExEntry
  (T.jsonEncode k)
  (toJSON e)
  (T.jsonEncode v)
  (T.jsonEncode r)

----------------------------------------------------------------------

data SetOptsEntry = SetOptsEntry
  { jsonKey    :: A.Value
  , jsonValue  :: A.Value
  , jsonTTL    :: A.Value
  , jsonCond   :: A.Value
  , jsonResult :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance RRItem SetOptsEntry where
  getTag _ = "SetOptsEntry"

instance MockedResult SetOptsEntry (Either T.KVDBReply Bool) where
  getMock SetOptsEntry {jsonResult} = T.jsonDecode jsonResult

mkSetOptsEntry :: ByteString -> ByteString -> L.KVDBSetTTLOption -> L.KVDBSetConditionOption -> Either T.KVDBReply Bool -> SetOptsEntry
mkSetOptsEntry k v ttl cond r = SetOptsEntry
  (T.jsonEncode k)
  (T.jsonEncode v)
  (toJSON ttl)
  (toJSON cond)
  (T.jsonEncode r)

----------------------------------------------------------------------

data GetEntry = GetEntry
  { jsonKey    :: A.Value
  , jsonResult :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance RRItem GetEntry where
  getTag _ = "GetEntry"

instance MockedResult GetEntry (Either T.KVDBReply (Maybe ByteString)) where
  getMock GetEntry {jsonResult} = T.jsonDecode jsonResult


mkGetEntry :: ByteString -> Either T.KVDBReply (Maybe ByteString) -> GetEntry
mkGetEntry k r = GetEntry
  (T.jsonEncode k)
  (T.jsonEncode r)

----------------------------------------------------------------------

data ExistsEntry = ExistsEntry
  { jsonKey    :: A.Value
  , jsonResult :: A.Value
  } deriving (Show, Eq,  Generic, ToJSON, FromJSON)

instance RRItem ExistsEntry where
  getTag _ = "ExistsEntry"

instance MockedResult ExistsEntry (Either T.KVDBReply Bool) where
  getMock ExistsEntry {jsonResult} = T.jsonDecode jsonResult

mkExistsEntry :: ByteString -> Either T.KVDBReply Bool -> ExistsEntry
mkExistsEntry k r = ExistsEntry
  (T.jsonEncode k)
  (T.jsonEncode r)

-- ----------------------------------------------------------------------

data DelEntry = DelEntry
  { jsonKeys   :: A.Value
  , jsonResult :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance RRItem DelEntry where
  getTag _ = "DelEntry"

instance MockedResult DelEntry (Either T.KVDBReply Integer) where
  getMock DelEntry {jsonResult} = T.jsonDecode jsonResult

mkDelEntry :: [ByteString] -> Either T.KVDBReply Integer -> DelEntry
mkDelEntry k r = DelEntry
  (T.jsonEncode k)
  (T.jsonEncode r)


-- ----------------------------------------------------------------------

data ExpireEntry = ExpireEntry
  { jsonKey    :: A.Value
  , duration   :: Integer
  , jsonResult :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance RRItem ExpireEntry where
  getTag _ = "ExpireEntry"

instance MockedResult ExpireEntry (Either T.KVDBReply Bool) where
  getMock ExpireEntry {jsonResult} = T.jsonDecode jsonResult

mkExpireEntry :: ByteString -> Integer -> Either T.KVDBReply Bool -> ExpireEntry
mkExpireEntry k d r = ExpireEntry
  (T.jsonEncode k)
  d
  (T.jsonEncode r)

-- ----------------------------------------------------------------------

data IncrEntry = IncrEntry
  { jsonKey    :: A.Value
  , jsonResult :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance RRItem IncrEntry where
  getTag _ = "IncrEntry"

instance MockedResult IncrEntry (Either T.KVDBReply Integer) where
  getMock IncrEntry {jsonResult} = T.jsonDecode jsonResult

mkIncrEntry :: ByteString -> Either T.KVDBReply Integer -> IncrEntry
mkIncrEntry k r = IncrEntry
  (T.jsonEncode k)
  (T.jsonEncode r)

-- ----------------------------------------------------------------------

data HSetEntry = HSetEntry
  { jsonKey    :: A.Value
  , jsonField  :: A.Value
  , jsonValue  :: A.Value
  , jsonResult :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance RRItem HSetEntry where
  getTag _ = "HSetEntry"

instance MockedResult HSetEntry (Either T.KVDBReply Bool) where
  getMock HSetEntry {jsonResult} = T.jsonDecode jsonResult

mkHSetEntry :: ByteString -> ByteString -> ByteString -> Either T.KVDBReply Bool -> HSetEntry
mkHSetEntry k f v r = HSetEntry
  (T.jsonEncode k)
  (T.jsonEncode f)
  (T.jsonEncode v)
  (T.jsonEncode r)

-- ----------------------------------------------------------------------

data HGetEntry = HGetEntry
  { jsonKey    :: A.Value
  , jsonField  :: A.Value
  , jsonResult :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance RRItem HGetEntry where
  getTag _ = "HGetEntry"

instance MockedResult HGetEntry (Either T.KVDBReply (Maybe ByteString)) where
  getMock HGetEntry {jsonResult} = T.jsonDecode jsonResult

mkHGetEntry :: ByteString -> ByteString -> Either T.KVDBReply (Maybe ByteString) -> HGetEntry
mkHGetEntry k f r = HGetEntry
  (T.jsonEncode k)
  (T.jsonEncode f)
  (T.jsonEncode r)

-- ----------------------------------------------------------------------

data XAddEntry = XAddEntry
  { jsonStream  :: A.Value
  , jsonEntryId :: A.Value
  , jsonItems   :: A.Value
  , jsonResult  :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance RRItem XAddEntry where
  getTag _ = "XAddEntry"

instance MockedResult XAddEntry (Either T.KVDBReply L.KVDBStreamEntryID) where
  getMock XAddEntry {jsonResult} = T.jsonDecode jsonResult

mkXAddEntry :: ByteString -> L.KVDBStreamEntryIDInput -> [L.KVDBStreamItem] -> Either T.KVDBReply L.KVDBStreamEntryID -> XAddEntry
mkXAddEntry s e i r = XAddEntry
  (T.jsonEncode s)
  (toJSON e)
  (T.jsonEncode i)
  (T.jsonEncode r)

-- ----------------------------------------------------------------------

data XLenEntry = XLenEntry
  { jsonStream  :: A.Value
  , jsonResult  :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance RRItem XLenEntry where
  getTag _ = "XLenEntry"

instance MockedResult XLenEntry (Either T.KVDBReply Integer) where
  getMock XLenEntry {jsonResult} = T.jsonDecode jsonResult

mkXLenEntry :: ByteString -> Either T.KVDBReply Integer -> XLenEntry
mkXLenEntry s r = XLenEntry
  (T.jsonEncode s)
  (T.jsonEncode r)

-- ----------------------------------------------------------------------

jsonExDecode :: forall a . T.JSONEx a => A.Value -> Maybe a
jsonExDecode = T.resolveJSONEx @a T.jsonDecode T.fromJSONMaybe

data MultiExecEntry = MultiExecEntry
  { jsonResult :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance RRItem MultiExecEntry where
  getTag _ = "MultiExecEntry"

instance T.JSONEx a => MockedResult MultiExecEntry (Either T.KVDBReply (T.TxResult a)) where
  getMock MultiExecEntry {jsonResult} =
    case temp of
      Nothing                             -> Nothing
      Just (Left e)                       -> Just $ Left e
      Just (Right (T.TxSuccess Nothing )) -> Nothing
      Just (Right (T.TxSuccess (Just a))) -> Just $ Right $ T.TxSuccess a
      Just (Right (T.TxAborted         )) -> Just $ Right $ T.TxAborted
      Just (Right (T.TxError s         )) -> Just $ Right $ T.TxError s
    where
      temp :: Maybe (Either T.KVDBReply (T.TxResult (Maybe a)))
      temp = fmap (fmap (fmap jsonExDecode)) $ jsonExDecode jsonResult


mkMultiExecEntry :: forall a . T.JSONEx a => Either T.KVDBReply (T.TxResult a) -> MultiExecEntry
mkMultiExecEntry r = MultiExecEntry $
    A.toJSON $ fmap (A.toJSON1 . fmap (T.resolveJSONEx @a T.jsonEncode toJSON)) r


-- MultiExecWithHash

data MultiExecWithHashEntry = MultiExecWithHashEntry
  { hashValue :: A.Value
  , jsonResult :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance RRItem MultiExecWithHashEntry where
  getTag _ = "MultiExecWithHashEntry"

instance T.JSONEx a => MockedResult MultiExecWithHashEntry (Either T.KVDBReply (T.TxResult a)) where
  getMock MultiExecWithHashEntry {jsonResult} =
    case temp of
      Nothing                             -> Nothing
      Just (Left e)                       -> Just $ Left e
      Just (Right (T.TxSuccess Nothing )) -> Nothing
      Just (Right (T.TxSuccess (Just a))) -> Just $ Right $ T.TxSuccess a
      Just (Right (T.TxAborted         )) -> Just $ Right $ T.TxAborted
      Just (Right (T.TxError s         )) -> Just $ Right $ T.TxError s
    where
      temp :: Maybe (Either T.KVDBReply (T.TxResult (Maybe a)))
      temp = fmap (fmap (fmap jsonExDecode)) $ jsonExDecode jsonResult


mkMultiExecWithHashEntry :: forall a . T.JSONEx a => ByteString -> Either T.KVDBReply (T.TxResult a) -> MultiExecWithHashEntry
mkMultiExecWithHashEntry h r = MultiExecWithHashEntry (T.jsonEncode h) $
    A.toJSON $ fmap (A.toJSON1 . fmap (T.resolveJSONEx @a T.jsonEncode toJSON)) r