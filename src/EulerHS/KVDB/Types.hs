{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass     #-}

module EulerHS.KVDB.Types
  (
    -- * Core KVDB
    -- ** Types
     KVDBKey, KVDBValue, KVDBDuration
  , KVDBSetTTLOption(..), KVDBSetConditionOption(..)
  , KVDBField, KVDBChannel, KVDBMessage
  , KVDBStream, KVDBStreamItem, KVDBStreamEntryID (..), KVDBStreamEntryIDInput (..)
  , RecordID, KVDBStreamReadResponse (..), KVDBStreamReadResponseRecord (..), KVDBStreamEnd, KVDBStreamStart
  , KVDBGroupName, KVDBConsumerName
  , KVDBConn(..)
  , KVDBAnswer
  , KVDBReply
  , TxResult(..)
  , KVDBStatus
  , KVDBStatusF(..)
  , KVDBReplyF(..)
  , NativeKVDBConn (..)
  , KVDBConfig (..)
  , RedisConfig (..)
  , KVDBError (..)
  -- ** Methods
  , defaultKVDBConnConfig
  , exceptionToKVDBReply
  , fromRdStatus
  , fromRdTxResult
  , hedisReplyToKVDBReply
  , mkKVDBConfig
  , mkKVDBClusterConfig
  , mkRedisConn
  , nativeToKVDB
  , kvdbToNative
  , MeshError(..)
  ) where

import           Data.Data (Data)
import           Data.Time (NominalDiffTime)
import qualified Data.Aeson as A
import qualified Database.Redis as RD
import           EulerHS.Prelude
import qualified GHC.Generics as G
import qualified EulerHS.SqlDB.Types as DBError


data KVDBSetTTLOption
  = NoTTL
  | Seconds Integer
  | Milliseconds Integer
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

data KVDBSetConditionOption
  = SetAlways
  | SetIfExist
  | SetIfNotExist
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

type KVDBKey =ByteString
type KVDBValue = ByteString
type KVDBDuration = Integer
type KVDBField = ByteString
type KVDBChannel = ByteString
type KVDBMessage = ByteString

type KVDBStream = ByteString
type KVDBStreamEnd = ByteString
type KVDBStreamStart = ByteString
type RecordID = ByteString
type KVDBGroupName = ByteString
type KVDBConsumerName = ByteString

data KVDBStreamEntryID = KVDBStreamEntryID Integer Integer
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

data KVDBStreamEntryIDInput
  = EntryID KVDBStreamEntryID
  | AutoID
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

data KVDBStreamReadResponse =
  KVDBStreamReadResponse {
      streamName :: ByteString
    , response :: [KVDBStreamReadResponseRecord]
  }
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

data KVDBStreamReadResponseRecord =
  KVDBStreamReadResponseRecord {
      recordId :: ByteString
    , records :: [(ByteString, ByteString)]
  }
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

type KVDBStreamItem = (ByteString, ByteString)

----------------------------------------------------------------------

-- Key-value database connection
data KVDBConn = Redis {-# UNPACK #-} !Text
                      !RD.Connection
  deriving stock (Generic)

data KVDBError
  = KVDBConnectionAlreadyExists
  | KVDBConnectionDoesNotExist
  | KVDBConnectionFailed
  deriving stock (Eq, Show, Generic, Data)
  deriving anyclass (ToJSON,FromJSON)

data KVDBReplyF bs
  = SingleLine bs
  | Err bs
  | Integer Integer
  | Bulk (Maybe bs)
  | MultiBulk (Maybe [KVDBReplyF bs])
  | ExceptionMessage String
  | KVDBError KVDBError String
  | DecompressError String
  deriving stock (Eq, Show, Generic, Functor, Data)
  deriving anyclass (ToJSON,FromJSON)

type KVDBReply = KVDBReplyF ByteString

data KVDBStatusF bs
  = Ok
  | Pong
  | Status bs
  deriving stock (Eq, Show, Generic, Functor)
  deriving anyclass (ToJSON,FromJSON)

type KVDBStatus = KVDBStatusF ByteString

fromRdStatus :: RD.Status -> KVDBStatus
fromRdStatus = \case
  RD.Ok        -> Ok
  RD.Pong      -> Pong
  RD.Status bs -> Status bs

data TxResult a
  = TxSuccess a
  | TxAborted
  | TxError String
  deriving stock (Eq, Show, Functor, Generic, G.Generic1)
  deriving anyclass (ToJSON,FromJSON)

fromRdTxResult :: RD.TxResult a -> TxResult a
fromRdTxResult = \case
  RD.TxSuccess x -> TxSuccess x
  RD.TxAborted   -> TxAborted
  RD.TxError err -> TxError err

type KVDBAnswer = Either KVDBReply

hedisReplyToKVDBReply :: RD.Reply -> KVDBReply
hedisReplyToKVDBReply = \case
  RD.SingleLine s -> SingleLine s
  RD.Error err    -> Err err
  RD.Integer s    -> Integer s
  RD.Bulk s       -> Bulk s
  RD.MultiBulk s  -> MultiBulk . fmap (fmap hedisReplyToKVDBReply) $ s

exceptionToKVDBReply :: Exception e => e -> KVDBReply
exceptionToKVDBReply = ExceptionMessage . displayException

newtype NativeKVDBConn = NativeKVDB RD.Connection

-- | Transform 'KVDBConn' to 'NativeKVDBConn'
kvdbToNative :: KVDBConn -> NativeKVDBConn
kvdbToNative (Redis _ conn) = NativeKVDB conn

-- | Transforms 'NativeKVDBConn' to 'KVDBConn'
nativeToKVDB :: Text -> NativeKVDBConn -> KVDBConn
nativeToKVDB connTag (NativeKVDB conn) = Redis connTag conn

data KVDBConfig
  = KVDBConfig Text RedisConfig
  | KVDBClusterConfig Text RedisConfig
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

data RedisConfig = RedisConfig
    { connectHost           :: String
    , connectPort           :: Word16
    , connectAuth           :: Maybe Text
    , connectDatabase       :: Integer
    , connectReadOnly       :: Bool
    , connectMaxConnections :: Int
    , connectMaxIdleTime    :: NominalDiffTime
    , connectTimeout        :: Maybe NominalDiffTime
    } deriving stock (Show, Eq, Ord, Generic)
      deriving anyclass (ToJSON, FromJSON)

defaultKVDBConnConfig :: RedisConfig
defaultKVDBConnConfig = RedisConfig
    { connectHost           = "localhost"
    , connectPort           = 6379
    , connectAuth           = Nothing
    , connectDatabase       = 0
    , connectReadOnly       = False
    , connectMaxConnections = 50
    , connectMaxIdleTime    = 30
    , connectTimeout        = Nothing
    }

-- | Transform RedisConfig to the Redis ConnectInfo.
toRedisConnectInfo :: RedisConfig -> RD.ConnectInfo
toRedisConnectInfo RedisConfig {..} = RD.ConnInfo
  { RD.connectHost           = connectHost
  , RD.connectPort           = RD.PortNumber $ toEnum $ fromEnum connectPort
  , RD.connectAuth           = encodeUtf8 <$> connectAuth
  , RD.connectReadOnly       = connectReadOnly
  , RD.connectDatabase       = connectDatabase
  , RD.connectMaxConnections = connectMaxConnections
  , RD.connectMaxIdleTime    = connectMaxIdleTime
  , RD.connectTimeout        = connectTimeout
  , RD.connectTLSParams      = Nothing
  }

-- | Create configuration KVDBConfig for Redis
mkKVDBConfig :: Text -> RedisConfig -> KVDBConfig
mkKVDBConfig = KVDBConfig

-- | Create cluster configuration KVDBConfig for Redis
mkKVDBClusterConfig :: Text -> RedisConfig -> KVDBConfig
mkKVDBClusterConfig = KVDBClusterConfig

-- | Create 'KVDBConn' from 'KVDBConfig'
mkRedisConn :: KVDBConfig -> IO KVDBConn
mkRedisConn = \case
  KVDBConfig connTag cfg        -> Redis connTag <$> createRedisConn cfg
  KVDBClusterConfig connTag cfg -> Redis connTag <$> createClusterRedisConn cfg

-- | Connect with the given config to the database.
createRedisConn :: RedisConfig -> IO RD.Connection
createRedisConn = RD.connect . toRedisConnectInfo 

-- | Connect with the given cluster config to the database.
createClusterRedisConn :: RedisConfig -> IO RD.Connection
createClusterRedisConn = RD.connectCluster . toRedisConnectInfo

data MeshError
  = MKeyNotFound Text
  | MDBError DBError.DBError
  | MRedisError KVDBReply
  | MDecodingError Text
  | MUpdateFailed Text
  | MMultipleKeysFound Text
  | UnexpectedError Text
  deriving (Show, Generic, Exception, Data)
  deriving anyclass (FromJSON)

instance ToJSON MeshError where
  toJSON = \case
    (MKeyNotFound a)       -> convertToJSON "MKeyNotFound" a
    (MDBError a)           -> convertToJSON "MDBError" a
    (MRedisError a)        -> convertToJSON "MRedisError" (show a :: Text)
    (MDecodingError a)     -> convertToJSON "MDecodingError" a
    (MUpdateFailed a)      -> convertToJSON "MUpdateFailed" a
    (MMultipleKeysFound a) -> convertToJSON "MMultipleKeysFound" a
    (UnexpectedError a)    -> convertToJSON "UnexpectedError" a
    where
      convertToJSON :: (ToJSON a) => Text -> a -> A.Value
      convertToJSON tag content = A.object
        [
          "contents" A..= content,
          "tag" A..= tag
        ]