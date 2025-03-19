{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# OPTIONS_GHC -Wno-star-is-type #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module EulerHS.KVConnector.Types 
  (
    module EulerHS.KVConnector.Types
  ) where


import EulerHS.Prelude
import qualified Data.Aeson as A
import           Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import           Data.Time (UTCTime)
import qualified Text.Show
import qualified EulerHS.KVDB.Language as L
import qualified EulerHS.Framework.Language as L
import qualified Database.Beam as B
import           Database.Beam.MySQL (MySQL)
import           Database.Beam.Backend (BeamSqlBackend, HasSqlValueSyntax (sqlValueSyntax), autoSqlValueSyntax)
import qualified Database.Beam.Backend.SQL as B
import           Database.Beam.Schema (FieldModification, TableField)
import           Sequelize (Column, Set)
import qualified EulerHS.Types as T
import           EulerHS.KVDB.Types (MeshError(..))
import           Data.Aeson ((.=))
import qualified Streamly.Data.Serialize.Instances ()
import qualified Streamly.Data.MutByteArray as MBA

------------ TYPES AND CLASSES ------------

data PrimaryKey = PKey [(Text,Text)]
data SecondaryKey = SKey [(Text,Text)]

data PKvKey = PKvKey {_key :: Text, prefixedKey :: Text, _shard :: Text}
  deriving (Generic, Eq, Show)

data SKvKey = SKvKey {sKey :: Text, prefixedSKey :: Text}
  deriving (Generic, Eq, Show)

data DBName = ECRDB | TRACKERDB
  deriving (Generic, Eq, Show, ToJSON, FromJSON, Serialize)

data KVEntry table =
  KVEntry
    {
      db :: DBName,
      row :: table Identity,
      updatedAt :: Maybe Integer 
    } deriving (Generic)

  
deriving instance Eq (table Identity) => Eq (KVEntry table)
deriving instance Show (table Identity) => Show (KVEntry table)
deriving instance A.ToJSON (table Identity) => A.ToJSON (KVEntry table)
deriving instance A.FromJSON (table Identity) =>  A.FromJSON (KVEntry table)
deriving instance Serialize (table Identity) =>  Serialize (KVEntry table)

data StreamMetadata = StreamMetadata
  { keyID :: Maybe Text
  , kvTag :: Text
  , mid :: Maybe Text
  , dbPrefix :: Maybe Text
  , schemaName :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

class KVConnector table where
  tableName :: Text
  keyMap :: KM.KeyMap Bool
  primaryKey :: Bool -> table -> PrimaryKey
  secondaryKeys :: Bool -> table -> [SecondaryKey]
  inMemPrefix :: Text -- TODO: Remove once duplicate types are removed from application
  inMemPrefix = ""
  getCreateCounterKey :: table -> Maybe Text
  getCreateCounterKey = const Nothing
  
--------------- EXISTING DB MESH ---------------
class MeshState a where
  getShardedHashTag :: a -> Maybe Text
  getKVKey          :: a -> Maybe Text
  getKVDirtyKey     :: a -> Maybe Text
  isDBMeshEnabled   :: a -> Bool

class MeshMeta be table where
  meshModelFieldModification :: table (FieldModification (TableField table))
  valueMapper :: Map.Map Text (A.Value -> A.Value)
  parseFieldAndGetClause :: A.Value -> Text -> Parser (TermWrap be table)
  parseSetClause :: [(Text, A.Value)] -> Parser [Set be table]
  columnized :: table (B.Columnar' (B.TableField table))

data TermWrap be (table :: (* -> *) -> *) where
  TermWrap :: (B.BeamSqlBackendCanSerialize be a, A.ToJSON a, Ord a, B.HasSqlEqualityCheck be a, Show a, MBA.Serialize a)
              => Column table a -> a -> TermWrap be table

type MeshResult a = Either MeshError a

newtype AutoPrimaryId = AutoPrimaryId { toIntegerId :: Maybe Integer}

-- data MeshError
--   = MKeyNotFound Text
--   | MDBError T.DBError
--   | MRedisError T.KVDBReply
--   | MDecodingError Text
--   | MUpdateFailed Text
--   | MMultipleKeysFound Text
--   | UnexpectedError Text
--   deriving (Show, Generic, Exception, Data)
--   deriving anyclass (ToJSON,FromJSON)
-- data MeshError
--   = MKeyNotFound Text
--   | MDBError T.DBError
--   | MRedisError T.KVDBReply
--   | MDecodingError Text
--   | MUpdateFailed Text
--   | MMultipleKeysFound Text
--   | UnexpectedError Text
--   deriving (Show, Generic, Exception, Data)

-- instance ToJSON MeshError where
--   toJSON (MRedisError r) = A.object
--     [
--       "contents" A..= (show r :: Text),
--       "tag" A..= ("MRedisError" :: Text)
--     ]
--   toJSON a = A.toJSON a

data QueryPath = KVPath | SQLPath

data MeshConfig = MeshConfig
  { meshEnabled           :: Bool
  , cerealEnabled         :: Bool
  , memcacheEnabled       :: Bool
  , memcacheFindAllEnabled :: Bool
  , snowFlakeEnabled      :: Bool
  , meshDBName            :: Text
  , ecRedisDBStream       :: Text
  , kvRedis               :: Text
  , reconRedis            :: Text
  , redisTtl              :: L.KVDBDuration
  , kvHardKilled          :: Bool
  , shouldPushToETLStream :: Bool
  , shouldPushToReconRedis :: Bool
  , disablePartitionKey    :: Bool
  , disableFallBackToCommonDb :: Bool
  , cleanDBHardKilled      :: Bool
  , cleanDBEnabled         :: Bool
  , shouldRecacheFind      :: Bool
  , shouldPushToSQLWriteLogsStream :: Bool
  , cachePrefix            :: Maybe Text
  , dbPrefix               :: Maybe Text
  , prefixMigrationMode    :: L.PrefixMigrationMode
  , shouldSkipDBForRecency :: Bool
  }
  deriving (Generic, Eq, Show, A.ToJSON)

-- meshConfig :: MeshConfig
-- meshConfig = MeshConfig
--   { meshEnabled = True
--   , memcacheEnabled = False
--   , meshDBName = "ECRDB"
--   , ecRedisDBStream = "db-sync-stream"
--   , kvRedis = "KVRedis"
--   , redisTtl = 43200
--   }

instance HasSqlValueSyntax MySQL String => HasSqlValueSyntax MySQL UTCTime where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend MySQL => B.HasSqlEqualityCheck MySQL UTCTime

instance HasSqlValueSyntax MySQL String => HasSqlValueSyntax MySQL A.Value where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend MySQL => B.HasSqlEqualityCheck MySQL A.Value

instance HasSqlValueSyntax MySQL String => HasSqlValueSyntax MySQL (Vector Int) where
  sqlValueSyntax = autoSqlValueSyntax

instance HasSqlValueSyntax MySQL String => HasSqlValueSyntax MySQL (Vector Text) where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend MySQL => B.HasSqlEqualityCheck MySQL (Vector Int)

instance BeamSqlBackend MySQL => B.HasSqlEqualityCheck MySQL (Vector Text)

data MerchantID = MerchantID 
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Source = KV | SQL | KV_AND_SQL | IN_MEM
    deriving (Generic, Show, Eq, ToJSON)

data IdSource = REDIS | SQL' | SNOWFLAKE
    deriving (Generic, Show, Eq, ToJSON)


data DBLogEntry a = DBLogEntry
  { _log_type             :: Text
  , _action               :: Text
  , _operation            :: T.Operation
  , _data                 :: a
  , _latency              :: Maybe Int
  , _model                :: Text
  , _source               :: Source
  , _apiTag               :: Maybe Text
  , _merchant_id          :: Maybe Text
  , _whereDiffCheckRes    :: Maybe [[Text]]
  , _idSource             :: Maybe IdSource
  , _connTag              :: Maybe Text
  }
  deriving stock (Generic)
  -- deriving anyclass (ToJSON)
instance (ToJSON a) => ToJSON (DBLogEntry a) where
  toJSON val = A.object [ "log_type" .= _log_type val
                        , "action" .= _action val
                        , "operation" .= _operation val
                        , "latency" .= _latency val
                        , "model" .= _model val
                        , "data" .= _data val
                        , "source" .= _source val
                        , "api_tag" .= _apiTag val
                        , "merchant_id" .= _merchant_id val
                        , "whereDiffCheckRes" .= _whereDiffCheckRes val
                        , "idSource" .= _idSource val
                      ]

data ETLStreamKeys = ETLCreate | ETLUpdate
    deriving (Generic, ToJSON)

instance Show ETLStreamKeys where
  show ETLCreate = "create"
  show ETLUpdate = "update"

--------------------------------------------------------------------------------
-- TH Instances
--------------------------------------------------------------------------------

$(MBA.deriveSerialize [d|instance MBA.Serialize MerchantID|])
instance T.OptionEntity MerchantID Text
