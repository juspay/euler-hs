{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

module EulerHS.Core.Playback.Entries where

import qualified Data.Aeson as A
import           Data.Generics.Product.Positions (getPosition)
import qualified Data.Text as Text
import           EulerHS.Core.Types.Playback (MockedResult (..), RRItem (..))
import           EulerHS.Prelude
import qualified EulerHS.Types as T
import qualified Servant.Client as S

----------------------------------------------------------------------

data RunDBEntry = RunDBEntry
    { jsonResult :: A.Value
    , rawSql     :: [Text]
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkRunDBEntry :: T.JSONEx a => (T.DBResult a, [Text]) -> RunDBEntry
mkRunDBEntry (res, sql) = RunDBEntry  (T.jsonEncode res) sql

instance RRItem RunDBEntry where
  getTag _ = "RunDBEntry"

instance T.JSONEx a => MockedResult RunDBEntry (T.DBResult a, [Text]) where
  getMock RunDBEntry {jsonResult, rawSql} = fmap (\x -> (x,rawSql)) (T.jsonDecode jsonResult)

data ThrowExceptionEntry = ThrowExceptionEntry
  { exMessage :: String
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkThrowExceptionEntry :: Exception e => e -> a -> ThrowExceptionEntry
mkThrowExceptionEntry e _ = ThrowExceptionEntry $ show e

instance RRItem ThrowExceptionEntry where
  getTag _ = "ThrowExceptionEntry"

instance MockedResult ThrowExceptionEntry a where
  getMock _ = Just $ error "This shold not be evaluated: throw exception result"

----------------------------------------------------------------------

data CallServantAPIEntry = CallServantAPIEntry
  { baseUrl    :: S.BaseUrl
  , jsonResult :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkCallServantAPIEntry
  :: T.JSONEx a
  => S.BaseUrl
  -> Either S.ClientError a
  -> CallServantAPIEntry
mkCallServantAPIEntry burl = CallServantAPIEntry burl . T.jsonEncode

instance RRItem CallServantAPIEntry where
  getTag _ = "CallServantAPIEntry"

instance T.JSONEx a => MockedResult CallServantAPIEntry (Either S.ClientError a) where
    getMock CallServantAPIEntry {jsonResult} = T.jsonDecode jsonResult

----------------------------------------------------------------------

data CallHttpAPIEntry = CallHttpAPIEntry
  { request   :: T.HTTPRequest
  , eResponse :: Either Text.Text T.HTTPResponse
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkCallHttpAPIEntry
  :: T.HTTPRequest
  -> Either Text.Text T.HTTPResponse
  -> CallHttpAPIEntry
mkCallHttpAPIEntry = CallHttpAPIEntry

instance RRItem CallHttpAPIEntry where
  getTag _ = "CallHttpAPIEntry"

instance MockedResult CallHttpAPIEntry (Either Text.Text T.HTTPResponse) where
    getMock (CallHttpAPIEntry {eResponse}) = Just eResponse

-- ----------------------------------------------------------------------

data SetOptionEntry = SetOptionEntry
  { key   :: Text
  , value :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkSetOptionEntry :: ToJSON v => Text -> v -> () -> SetOptionEntry
mkSetOptionEntry k v _ = SetOptionEntry k (toJSON v)

instance RRItem SetOptionEntry where
  getTag _ = "SetOptionEntry"

instance MockedResult SetOptionEntry () where
  getMock _ = Just ()
-------------------------------------------------------------------------

data DelOptionEntry = DelOptionEntry { key :: Text }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkDelOptionEntry :: Text -> () -> DelOptionEntry
mkDelOptionEntry k _ = DelOptionEntry k

instance RRItem DelOptionEntry where
  getTag _ = "DelOptionEntry"

instance MockedResult DelOptionEntry () where
  getMock _ = Just ()

-- ----------------------------------------------------------------------

data GetOptionEntry = GetOptionEntry
  { key   :: Text
  , value :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkGetOptionEntry :: ToJSON v => Text -> Maybe v -> GetOptionEntry
mkGetOptionEntry k mv = GetOptionEntry k (toJSON mv)

instance RRItem GetOptionEntry where
  getTag _ = "GetOptionEntry"

instance FromJSON v => MockedResult GetOptionEntry v where
  getMock GetOptionEntry{value} = T.fromJSONMaybe value

-- ----------------------------------------------------------------------

data RunSysCmdEntry = RunSysCmdEntry
  { cmd    :: String
  , result :: String
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkRunSysCmdEntry :: String -> String -> RunSysCmdEntry
mkRunSysCmdEntry cmd result = RunSysCmdEntry cmd result

instance RRItem RunSysCmdEntry where
  getTag _ = "RunSysCmdEntry"

instance MockedResult RunSysCmdEntry String where
  getMock RunSysCmdEntry {..} = Just result

-- ----------------------------------------------------------------------

data ForkEntry = ForkEntry
  { description :: Text
  , guid        :: Text
  } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkForkEntry :: Text -> Text -> () -> ForkEntry
mkForkEntry desc guid _ = ForkEntry desc guid

instance RRItem ForkEntry where
  getTag _ = "ForkEntry"

instance MockedResult ForkEntry () where
  getMock _ = Just ()

-- ----------------------------------------------------------------------

data GenerateGUIDEntry = GenerateGUIDEntry
  { guid :: Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkGenerateGUIDEntry :: Text -> GenerateGUIDEntry
mkGenerateGUIDEntry = GenerateGUIDEntry

instance RRItem GenerateGUIDEntry where
  getTag _ = "GenerateGUIDEntry"

instance MockedResult GenerateGUIDEntry Text where
  getMock (GenerateGUIDEntry g) = Just g

-- ----------------------------------------------------------------------

data RunIOEntry = RunIOEntry
  { description :: Text
  , jsonResult  :: A.Value
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkRunIOEntry
  :: forall a
   . T.JSONEx a
  => Text
  -> a
  -> RunIOEntry
mkRunIOEntry descr a = RunIOEntry descr $
  (T.resolveJSONEx @a T.jsonEncode toJSON) a

instance RRItem RunIOEntry where
  getTag _ = "RunIOEntry"

instance T.JSONEx a => MockedResult RunIOEntry a where
    getMock (RunIOEntry _ r) =
      T.resolveJSONEx @a T.jsonDecode T.fromJSONMaybe r


-- ----------------------------------------------------------------------

data RunUntracedIOEntry = RunUntracedIOEntry
  { description :: Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkRunUntracedIOEntry :: Text -> a -> RunUntracedIOEntry
mkRunUntracedIOEntry descr _ = RunUntracedIOEntry descr

instance RRItem RunUntracedIOEntry where
  getTag _ = "RunUntracedIOEntry"

-- Not possible to mock these values, you have to re-run the IO action
-- instance MockedResult RunUntracedIOEntry () where
--   getMock (RunUntracedIOEntry _) = Just ()

------------------------------------------------------------------------

data InitSqlDBConnectionEntry beM = InitSqlDBConnectionEntry
  { dBConfig       :: T.DBConfig beM
  , initConnResult :: Either T.DBError ()
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkInitSqlDBConnectionEntry :: T.DBConfig beM -> Either T.DBError a -> InitSqlDBConnectionEntry beM
mkInitSqlDBConnectionEntry dbcfg res = case res of
  Left err -> InitSqlDBConnectionEntry dbcfg (Left err)
  Right _  -> InitSqlDBConnectionEntry dbcfg (Right ())

instance RRItem (InitSqlDBConnectionEntry beM)  where
  getTag _ = "InitSqlDBConnectionEntry"

instance MockedResult (InitSqlDBConnectionEntry beM) (T.DBResult (T.SqlConn beM)) where
  getMock  (InitSqlDBConnectionEntry _ res) =
    case res of
      Left err -> Just $ Left err
      Right _  -> Just $ Right $ T.MockedPool ""


----------------------------------------------------------------------

data DeInitSqlDBConnectionEntry (beM :: Type -> Type) = DeInitSqlDBConnectionEntry
  { connTag :: Text
  } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkDeInitSqlDBConnectionEntry :: T.SqlConn beM -> a -> DeInitSqlDBConnectionEntry beM
mkDeInitSqlDBConnectionEntry cfg _ = DeInitSqlDBConnectionEntry (getPosition @1 cfg)

instance RRItem (DeInitSqlDBConnectionEntry beM) where
  getTag _ = "DeInitSqlDBConnectionEntry"

instance MockedResult (DeInitSqlDBConnectionEntry beM) () where
  getMock (DeInitSqlDBConnectionEntry _) = Just ()

------------------------------------------------------------------------

data GetSqlDBConnectionEntry beM = GetSqlDBConnectionEntry
  { dBConfig      :: T.DBConfig beM
  , getConnResult :: Either T.DBError ()
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkGetSqlDBConnectionEntry :: T.DBConfig beM -> Either T.DBError a -> GetSqlDBConnectionEntry beM
mkGetSqlDBConnectionEntry dbcfg res = case res of
  Left err -> GetSqlDBConnectionEntry dbcfg (Left err)
  Right _  -> GetSqlDBConnectionEntry dbcfg (Right ())

instance RRItem (GetSqlDBConnectionEntry beM)  where
  getTag _ = "GetSqlDBConnectionEntry"

instance MockedResult (GetSqlDBConnectionEntry beM) (T.DBResult (T.SqlConn beM)) where
  getMock (GetSqlDBConnectionEntry _ res) =
    case res of
      Left err -> Just $ Left err
      Right _  -> Just $ Right $ T.MockedPool ""

-------------------------------------------------------------------------

data InitKVDBConnectionEntry = InitKVDBConnectionEntry
  { kvdbConfig :: T.KVDBConfig
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkInitKVDBConnectionEntry :: T.KVDBConfig -> a -> InitKVDBConnectionEntry
mkInitKVDBConnectionEntry dbcfg _ = InitKVDBConnectionEntry dbcfg

instance RRItem InitKVDBConnectionEntry  where
  getTag _ = "InitKVDBConnectionEntry"

instance MockedResult InitKVDBConnectionEntry (T.KVDBAnswer T.KVDBConn) where
  getMock (InitKVDBConnectionEntry _) = Just $ Right $ T.Mocked ""

-------------------------------------------------------------------------

data DeInitKVDBConnectionEntry = DeInitKVDBConnectionEntry
  { connTag :: Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkDeInitKVDBConnectionEntry :: T.KVDBConn -> a -> DeInitKVDBConnectionEntry
mkDeInitKVDBConnectionEntry conn _ = DeInitKVDBConnectionEntry (getPosition @1 conn)

instance RRItem DeInitKVDBConnectionEntry  where
  getTag _ = "DeInitKVDBConnectionEntry"

instance MockedResult DeInitKVDBConnectionEntry () where
  getMock (DeInitKVDBConnectionEntry _) = Just ()

-------------------------------------------------------------------------

data GetKVDBConnectionEntry = GetKVDBConnectionEntry
  { kvdbConfig :: T.KVDBConfig
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkGetKVDBConnectionEntry :: T.KVDBConfig -> a -> GetKVDBConnectionEntry
mkGetKVDBConnectionEntry dbcfg _ = GetKVDBConnectionEntry dbcfg

instance RRItem GetKVDBConnectionEntry  where
  getTag _ = "GetKVDBConnectionEntry"

instance MockedResult GetKVDBConnectionEntry (T.KVDBAnswer T.KVDBConn) where
  getMock (GetKVDBConnectionEntry _) = Just $ Right $ T.Mocked ""

----------------------------------------------------------------------

data AwaitEntry = AwaitEntry
  { timeout    :: Maybe Int
  , jsonResult :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkAwaitEntry :: ToJSON v => Maybe T.Microseconds -> Either T.AwaitingError v -> AwaitEntry
mkAwaitEntry mbMcs val = AwaitEntry (unwrapMcs <$> mbMcs) (toJSON val)
  where
    unwrapMcs (T.Microseconds mcs) = fromIntegral mcs

instance RRItem AwaitEntry  where
  getTag _ = "AwaitEntry"

instance FromJSON v => MockedResult AwaitEntry v where
  getMock (AwaitEntry _ jsonValue) = T.fromJSONMaybe jsonValue

-------------------------------------------------------------------------------------

data RunSafeFlowEntry = RunSafeFlowEntry
  { guid       :: Text
  , jsonResult :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkRunSafeFlowEntry :: ToJSON v => Text -> Either Text v -> RunSafeFlowEntry
mkRunSafeFlowEntry guid val = RunSafeFlowEntry guid (toJSON val)

instance RRItem RunSafeFlowEntry  where
  getTag _ = "RunSafeFlowEntry"

instance (FromJSON v) => MockedResult RunSafeFlowEntry v where
  getMock (RunSafeFlowEntry _ jsonValue) = T.fromJSONMaybe jsonValue
