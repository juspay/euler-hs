{- |
Module      :  EulerHS.Core.Types.KVDB
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

{- |
Module      :  EulerHS.Core.Types.KVDB
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
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
  , KVDBMockedValues
  , KVDBMockedValues'(..)
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

import           Data.Time (NominalDiffTime)
import qualified Database.Redis as RD
import           EulerHS.Prelude
import qualified GHC.Generics as G


type KVDBKey = Text

-- Key-value database connection
data KVDBConn
  = Mocked Text 
  | Redis Text RD.Connection
  -- ^ Real connection.
  deriving (Generic)

data KVDBMockedValues' = KVDBMockedValues'
  { kvdbSet    :: [KVDBStatus]
  , kvdbGet    :: [(Maybe ByteString)]
  , kvdbExists :: [Bool]
  , kvdbDel    :: [Integer]
  , kvdbExpire :: [Bool]
  , kvdbIncr   :: [Integer]
  , kvdbHSet   :: [Bool]
  , kvdbHGet   :: [(Maybe ByteString)]
  , kvdbTX     :: [TxResult Any]
  } deriving (Generic, Typeable)


type KVDBMockedValues = MVar (KVDBMockedValues')

----------------------------------------------------------------------

data KVDBError
  = KVDBConnectionAlreadyExists
  | KVDBConnectionDoesNotExist
  | KVDBConnectionFailed
  deriving (Eq, Show, Generic)

data KVDBReplyF bs
  = SingleLine bs
  | Err bs
  | Integer Integer
  | Bulk (Maybe bs)
  | MultiBulk (Maybe [KVDBReplyF bs])
  | ExceptionMessage String
  | KVDBError KVDBError String
  deriving (Eq, Show, Generic, Functor)

type KVDBReply = KVDBReplyF ByteString

-- fromKVDBReply :: KVDBReply -> KVDBReplyF ByteStringS
-- fromKVDBReply = fmap fromByteString

-- toKVDBReply :: KVDBReplyF ByteStringS -> KVDBReply
-- toKVDBReply = fmap toByteString

----------------------------------------------------------------------

data KVDBStatusF bs
  = Ok
  | Pong
  | Status bs
  deriving (Eq, Show, Generic, Functor)

type KVDBStatus = KVDBStatusF ByteString

-- fromStatus :: KVDBStatus -> KVDBStatusF ByteStringS
-- fromStatus Ok          = Ok
-- fromStatus Pong        = Pong
-- fromStatus (Status bs) = Status $ fromByteString bs

-- toStatus :: KVDBStatusF ByteStringS -> KVDBStatus
-- toStatus Ok          = Ok
-- toStatus Pong        = Pong
-- toStatus (Status bs) = Status $ toByteString bs

fromRdStatus :: RD.Status -> KVDBStatus
fromRdStatus RD.Ok          = Ok
fromRdStatus RD.Pong        = Pong
fromRdStatus (RD.Status bs) = Status $ bs

----------------------------------------------------------------------

data TxResult a
  = TxSuccess a
  | TxAborted
  | TxError String
  deriving (Eq, Show, Functor, Generic, G.Generic1)

fromRdTxResult :: RD.TxResult a -> TxResult a
fromRdTxResult (RD.TxSuccess a) = TxSuccess a
fromRdTxResult RD.TxAborted     = TxAborted
fromRdTxResult (RD.TxError s)   = TxError s

----------------------------------------------------------------------

type KVDBAnswer = Either KVDBReply

hedisReplyToKVDBReply :: RD.Reply -> KVDBReply
hedisReplyToKVDBReply (RD.SingleLine s) = SingleLine s
hedisReplyToKVDBReply (RD.Error s)      = Err s
hedisReplyToKVDBReply (RD.Integer s)    = Integer s
hedisReplyToKVDBReply (RD.Bulk s)       = Bulk s
hedisReplyToKVDBReply (RD.MultiBulk s)  = MultiBulk (map (hedisReplyToKVDBReply <$>) s)


exceptionToKVDBReply :: Exception e => e -> KVDBReply
exceptionToKVDBReply e = ExceptionMessage $ displayException e

----------------------------------------------------------------------

data NativeKVDBConn
  = NativeKVDB (RD.Connection)
  | NativeKVDBMockedConn

-- | Transform 'KVDBConn' to 'NativeKVDBConn'
kvdbToNative :: KVDBConn -> NativeKVDBConn
kvdbToNative (Mocked _)     = NativeKVDBMockedConn
kvdbToNative (Redis _ conn) = NativeKVDB conn

-- | Transforms 'NativeKVDBConn' to 'KVDBConn'
nativeToKVDB :: Text -> NativeKVDBConn -> KVDBConn
nativeToKVDB connTag NativeKVDBMockedConn = Mocked connTag
nativeToKVDB connTag (NativeKVDB conn)    = Redis connTag conn


data KVDBConfig
  = KVDBConfig Text RedisConfig
  | KVDBClusterConfig Text RedisConfig
  | KVDBMockedConfig Text
  deriving (Show, Eq, Ord, Generic)

data RedisConfig = RedisConfig
    { connectHost           :: String
    , connectPort           :: Word16
    , connectAuth           :: Maybe Text
    , connectDatabase       :: Integer
    , connectMaxConnections :: Int
    , connectMaxIdleTime    :: NominalDiffTime
    , connectTimeout        :: Maybe NominalDiffTime
    } deriving (Show, Eq, Ord, Generic)


defaultKVDBConnConfig :: RedisConfig
defaultKVDBConnConfig = RedisConfig
    { connectHost           = "localhost"
    , connectPort           = 6379
    , connectAuth           = Nothing
    , connectDatabase       = 0
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
mkRedisConn (KVDBMockedConfig connTag)        = pure $ Mocked connTag
mkRedisConn (KVDBConfig connTag cfg)          = Redis connTag <$> createRedisConn cfg
mkRedisConn (KVDBClusterConfig connTag cfg)   = Redis connTag <$> createClusterRedisConn cfg

-- | Connect with the given config to the database.
createRedisConn :: RedisConfig -> IO RD.Connection
createRedisConn = RD.connect . toRedisConnectInfo

-- | Connect with the given cluster config to the database.
createClusterRedisConn :: RedisConfig -> IO RD.Connection
createClusterRedisConn = RD.connectCluster . toRedisConnectInfo
