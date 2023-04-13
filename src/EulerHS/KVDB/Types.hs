{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wwarn=missing-fields #-}

module EulerHS.KVDB.Types
  (
    -- * Core KVDB
    -- ** Types
    KVDBKey
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
  ) where

import           Data.Data (Data)
import           Data.Time (NominalDiffTime)
import qualified Database.Redis as RD
import           EulerHS.Prelude
import qualified GHC.Generics as G

type KVDBKey = Text

-- Key-value database connection
data KVDBConn = Redis {-# UNPACK #-} !Text
                      !RD.Connection
  deriving stock (Generic)

data KVDBError
  = KVDBConnectionAlreadyExists
  | KVDBConnectionDoesNotExist
  | KVDBConnectionFailed
  deriving stock (Eq, Show, Generic, Data)

data KVDBReplyF bs
  = SingleLine bs
  | Err bs
  | Integer Integer
  | Bulk (Maybe bs)
  | MultiBulk (Maybe [KVDBReplyF bs])
  | ExceptionMessage String
  | KVDBError KVDBError String
  deriving stock (Eq, Show, Generic, Functor, Data)

type KVDBReply = KVDBReplyF ByteString

data KVDBStatusF bs
  = Ok
  | Pong
  | Status bs
  deriving stock (Eq, Show, Generic, Functor)

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
