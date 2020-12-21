{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

{- |
Module      :  EulerHS.Core.Types.KVDB
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2021
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable

Types and helper functions of the KV DB subsystem.

Currently, highly resembles the `hedis` library interface to Redis.
Other KV DBs are not yet supported.

This module is internal and should not imported in the projects.
Import 'EulerHS.Types' instead.
-}

module EulerHS.Core.Types.KVDB
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

import qualified Data.Aeson as A
import           Data.Time (NominalDiffTime)
import qualified Database.Redis as RD
import           EulerHS.Core.Types.Serializable
import           EulerHS.Prelude
import qualified GHC.Generics as G

-- | Alias for denoting a raw key type.
type KVDBKey = Text

-- | Key-value database connection
data KVDBConn
  = Mocked Text -- TODO swap Text with ConnTag type
    -- ^ Mocked connection. Used for ART and tests.
  | Redis Text RD.Connection
  -- ^ Real Redis connection.
  deriving (Generic)

----------------------------------------------------------------------

-- | Error that may occur when initializing / deinitializing a KV DB connection.
data KVDBError
  = KVDBConnectionAlreadyExists
  -- ^ Connection for a particular config already exist.
  | KVDBConnectionDoesNotExist
  -- ^ Connection for a particular config is not found.
  | KVDBConnectionFailed
  -- ^ Connection procedure failed.
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | A unified parametrizable type for return values of the KV DB subsystem.
--
-- Mostly duplicates the @hedis@ library interface.
data KVDBReplyF bs
  = SingleLine bs
  | Err bs
  | Integer Integer
  | Bulk (Maybe bs)
  | MultiBulk (Maybe [KVDBReplyF bs])
  | ExceptionMessage String
  | KVDBError KVDBError String
  deriving (Eq, Show, Generic, Functor)

instance ToJSON   (KVDBReplyF ByteStringS)
instance FromJSON (KVDBReplyF ByteStringS)

-- | A unified type for return values of the KV DB subsystem.
--
-- Mostly duplicates the @hedis@ library interface.
type KVDBReply = KVDBReplyF ByteString

instance ToJSON KVDBReply where
  toJSON = toJSON . fromKVDBReply

fromKVDBReply :: KVDBReply -> KVDBReplyF ByteStringS
fromKVDBReply = fmap fromByteString

instance FromJSON KVDBReply where
  parseJSON = fmap toKVDBReply . parseJSON

toKVDBReply :: KVDBReplyF ByteStringS -> KVDBReply
toKVDBReply = fmap toByteString

----------------------------------------------------------------------

-- | Status that may be returned by the methods of the KVDB language.
data KVDBStatusF bs
  = Ok
  | Pong
  | Status bs
  deriving (Eq, Show, Generic, Functor)

instance ToJSON   (KVDBStatusF ByteStringS)
instance FromJSON (KVDBStatusF ByteStringS)

-- | Status that may be returned by the methods of the KVDB language.
type KVDBStatus = KVDBStatusF ByteString

instance ToJSON KVDBStatus where
  toJSON = toJSON . fromStatus

fromStatus :: KVDBStatus -> KVDBStatusF ByteStringS
fromStatus Ok          = Ok
fromStatus Pong        = Pong
fromStatus (Status bs) = Status $ fromByteString bs

instance FromJSON KVDBStatus where
  parseJSON = fmap toStatus . parseJSON

toStatus :: KVDBStatusF ByteStringS -> KVDBStatus
toStatus Ok          = Ok
toStatus Pong        = Pong
toStatus (Status bs) = Status $ toByteString bs

-- | Result of a transactional evaluation of KV DB methods.
data TxResult a
  = TxSuccess a
    -- ^ Transaction is successful
  | TxAborted
    -- ^ Transaction is aborted
  | TxError String
    -- ^ Some error happened
  deriving (Eq, Show, Functor, Generic, G.Generic1, A.ToJSON1, A.FromJSON1, ToJSON, FromJSON)

-- | A type that contains either a valid result or a detailed info
-- about the response from the KV DB subsystem.
type KVDBAnswer = Either KVDBReply

-- | A config type used to create a connection with a KV DB.
data KVDBConfig
  = KVDBConfig Text RedisConfig
    -- ^ Regular Redis config
  | KVDBClusterConfig Text RedisConfig
    -- ^ KV DB config for a Redis cluster
  | KVDBMockedConfig Text
    -- ^ Mocked config. Used in ART and tests.
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Redis config
data RedisConfig = RedisConfig
    { connectHost           :: String
      -- ^ Host
    , connectPort           :: Word16
      -- ^ Port
    , connectAuth           :: Maybe Text
      -- ^ Auth credentials
    , connectDatabase       :: Integer
      -- ^ Database number
    , connectMaxConnections :: Int
      -- ^ Max number of connections
    , connectMaxIdleTime    :: NominalDiffTime
      -- ^ Max connection idle time
    , connectTimeout        :: Maybe NominalDiffTime
      -- ^ Timeout for a connection
    } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Default Redis config.
-- connectHost           = "127.0.0.1"
-- connectPort           = 6379
-- connectAuth           = Nothing
-- connectDatabase       = 0
-- connectMaxConnections = 50
-- connectMaxIdleTime    = 30
-- connectTimeout        = Nothing

defaultKVDBConnConfig :: RedisConfig
defaultKVDBConnConfig = RedisConfig
    { connectHost           = "127.0.0.1"
    , connectPort           = 6379
    , connectAuth           = Nothing
    , connectDatabase       = 0
    , connectMaxConnections = 50
    , connectMaxIdleTime    = 30
    , connectTimeout        = Nothing
    }

-- | Create configuration KVDBConfig for Redis
mkKVDBConfig :: Text -> RedisConfig -> KVDBConfig
mkKVDBConfig = KVDBConfig

-- | Create cluster configuration KVDBConfig for Redis
mkKVDBClusterConfig :: Text -> RedisConfig -> KVDBConfig
mkKVDBClusterConfig = KVDBClusterConfig

-- * Internal types and functions

-- | Native connection of a particular KV DB.
--
-- Internal type, should not be used in the BL.
data NativeKVDBConn
  = NativeKVDB (RD.Connection)
  | NativeKVDBMockedConn

-- | Transform 'KVDBConn' to 'NativeKVDBConn'
--
-- Internal function, should not be used in the BL.
kvdbToNative :: KVDBConn -> NativeKVDBConn
kvdbToNative (Mocked _)     = NativeKVDBMockedConn
kvdbToNative (Redis _ conn) = NativeKVDB conn

-- | Transforms 'NativeKVDBConn' to 'KVDBConn'
--
-- Internal function, should not be used in the BL.
nativeToKVDB :: Text -> NativeKVDBConn -> KVDBConn
nativeToKVDB connTag NativeKVDBMockedConn = Mocked connTag
nativeToKVDB connTag (NativeKVDB conn)    = Redis connTag conn

-- | Transforms a Redis-related @Status@ to an abstracted 'KVDBStatus'
--
-- Internal function, should not be used in the BL.
fromRdStatus :: RD.Status -> KVDBStatus
fromRdStatus RD.Ok          = Ok
fromRdStatus RD.Pong        = Pong
fromRdStatus (RD.Status bs) = Status $ bs

-- | Transforms a Redis-related @TxResult@ to an abstracted 'TxResult'
--
-- Internal function, should not be used in the BL.
fromRdTxResult :: RD.TxResult a -> TxResult a
fromRdTxResult (RD.TxSuccess a) = TxSuccess a
fromRdTxResult RD.TxAborted     = TxAborted
fromRdTxResult (RD.TxError s)   = TxError s

-- | Transforms a Redis-related @Reply@ to an abstracted 'KVDBReply'
--
-- Internal function, should not be used in the BL.
hedisReplyToKVDBReply :: RD.Reply -> KVDBReply
hedisReplyToKVDBReply (RD.SingleLine s) = SingleLine s
hedisReplyToKVDBReply (RD.Error s)      = Err s
hedisReplyToKVDBReply (RD.Integer s)    = Integer s
hedisReplyToKVDBReply (RD.Bulk s)       = Bulk s
hedisReplyToKVDBReply (RD.MultiBulk s)  = MultiBulk (map (hedisReplyToKVDBReply <$>) s)

-- | Wraps an exception into 'KVDBReply'
--
-- Internal function, should not be used in the BL.
exceptionToKVDBReply :: Exception e => e -> KVDBReply
exceptionToKVDBReply e = ExceptionMessage $ displayException e

-- | Transform `RedisConfig` to the Redis-related @ConnectInfo@.
--
-- Internal function, should not be used in the BL.
toRedisConnectInfo :: RedisConfig -> RD.ConnectInfo
toRedisConnectInfo RedisConfig {..} = RD.ConnInfo
  { RD.connectHost           = connectHost
  , RD.connectPort           = RD.PortNumber $ toEnum $ fromEnum connectPort
  , RD.connectAuth           = encodeUtf8 <$> connectAuth
  , RD.connectDatabase       = connectDatabase
  , RD.connectMaxConnections = connectMaxConnections
  , RD.connectMaxIdleTime    = connectMaxIdleTime
  , RD.connectTimeout        = connectTimeout
  , RD.connectTLSParams      = Nothing
  }

-- | Create 'KVDBConn' from 'KVDBConfig'
--
-- Internal function, should not be used in the BL.
mkRedisConn :: KVDBConfig -> IO KVDBConn
mkRedisConn (KVDBMockedConfig connTag)        = pure $ Mocked connTag
mkRedisConn (KVDBConfig connTag cfg)          = Redis connTag <$> createRedisConn cfg
mkRedisConn (KVDBClusterConfig connTag cfg)   = Redis connTag <$> createClusterRedisConn cfg

-- | Connect with the given config to the database.
--
-- Internal function, should not be used in the BL.
createRedisConn :: RedisConfig -> IO RD.Connection
createRedisConn = RD.connect . toRedisConnectInfo

-- | Connect with the given cluster config to the database.
--
-- Internal function, should not be used in the BL.
createClusterRedisConn :: RedisConfig -> IO RD.Connection
createClusterRedisConn = RD.connectCluster . toRedisConnectInfo
