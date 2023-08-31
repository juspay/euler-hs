{- |
Module      :  EulerHS.Core.KVDB.Language
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable

Language of the KV DB subsystem.

Currently, highly resembles the `hedis` library interface to Redis.
Other KV DBs are not yet supported.

This module is internal and should not imported in the projects.
Import 'EulerHS.Language' instead.
-}

{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EulerHS.Core.KVDB.Language
  (
  -- * KVDB language
  -- ** Types
    KVDB, KVDBTx, KVDBKey, KVDBValue, KVDBDuration
  , KVDBSetTTLOption(..), KVDBSetConditionOption(..)
  , KVDBField, KVDBChannel, KVDBMessage
  , KVDBStream, KVDBStreamItem, KVDBStreamEntryID (..), KVDBStreamEntryIDInput (..)
  , KVDBF(..), KeyValueF(..), TransactionF(..)
  -- ** Methods
  -- *** Regular
  -- **** For simple values
  , set, get, incr, setex, setOpts
  -- **** For hash values
  , hset, hget
  -- **** For streams
  , xadd, xlen
  -- **** For both
  , exists, del, expire
  -- *** Transactional
  -- | Used inside multiExec instead of regular
  , multiExec, multiExecWithHash
  , setTx, getTx, delTx, setexTx
  , hsetTx, hgetTx
  , xaddTx, xlenTx
  , expireTx
  -- *** Set
  , sadd
  , sismember
  -- *** Raw
  , rawRequest
  ) where

import qualified Database.Redis as R
import qualified EulerHS.Core.Types as T
import           EulerHS.Prelude hiding (get)

data KVDBSetTTLOption
  = NoTTL
  | Seconds Integer
  | Milliseconds Integer
  deriving stock Generic

data KVDBSetConditionOption
  = SetAlways
  | SetIfExist
  | SetIfNotExist
  deriving stock Generic

type KVDBKey = ByteString
type KVDBValue = ByteString
type KVDBDuration = Integer
type KVDBField = ByteString
type KVDBChannel = ByteString
type KVDBMessage = ByteString

type KVDBStream = ByteString

data KVDBStreamEntryID = KVDBStreamEntryID Integer Integer
  deriving stock Generic

data KVDBStreamEntryIDInput
  = EntryID KVDBStreamEntryID
  | AutoID
  deriving stock Generic

type KVDBStreamItem = (ByteString, ByteString)

----------------------------------------------------------------------

data KeyValueF f next where
  Set     :: KVDBKey -> KVDBValue -> (f T.KVDBStatus -> next) -> KeyValueF f next
  SetEx   :: KVDBKey -> KVDBDuration -> KVDBValue -> (f T.KVDBStatus -> next) -> KeyValueF f next
  SetOpts :: KVDBKey -> KVDBValue -> KVDBSetTTLOption -> KVDBSetConditionOption -> (f Bool -> next) -> KeyValueF f next
  Get     :: KVDBKey -> (f (Maybe ByteString) -> next) -> KeyValueF f next
  Exists  :: KVDBKey -> (f Bool -> next) -> KeyValueF f next
  Del     :: [KVDBKey] -> (f Integer -> next) -> KeyValueF f next
  Expire  :: KVDBKey -> KVDBDuration -> (f Bool -> next) -> KeyValueF f next
  Incr    :: KVDBKey -> (f Integer -> next) -> KeyValueF f next
  HSet    :: KVDBKey -> KVDBField -> KVDBValue -> (f Bool -> next) -> KeyValueF f next
  HGet    :: KVDBKey -> KVDBField -> (f (Maybe ByteString) -> next) -> KeyValueF f next
  XAdd    :: KVDBStream -> KVDBStreamEntryIDInput -> [KVDBStreamItem] -> (f KVDBStreamEntryID -> next) -> KeyValueF f next
  XLen    :: KVDBStream -> (f Integer -> next) -> KeyValueF f next
  SAdd    :: KVDBKey -> [KVDBValue] -> (f Integer -> next) -> KeyValueF f next
  SMem    :: KVDBKey -> KVDBKey -> (f Bool -> next) -> KeyValueF f next
  Raw     :: (R.RedisResult a) => [ByteString] -> (f a -> next) -> KeyValueF f next

instance Functor (KeyValueF f) where
  fmap f (Set k value next)              = Set k value (f . next)
  fmap f (SetEx k ex value next)         = SetEx k ex value (f . next)
  fmap f (SetOpts k value ttl cond next) = SetOpts k value ttl cond (f . next)
  fmap f (Get k next)                    = Get k (f . next)
  fmap f (Exists k next)                 = Exists k (f . next)
  fmap f (Del ks next)                   = Del ks (f . next)
  fmap f (Expire k sec next)             = Expire k sec (f . next)
  fmap f (Incr k next)                   = Incr k (f . next)
  fmap f (HSet k field value next)       = HSet k field value (f . next)
  fmap f (HGet k field next)             = HGet k field (f . next)
  fmap f (XAdd s entryId items next)     = XAdd s entryId items (f . next)
  fmap f (XLen s next)                   = XLen s (f . next)
  fmap f (SAdd k v next)                 = SAdd k v (f . next)
  fmap f (SMem k v next)                 = SMem k v (f . next)
  fmap f (Raw args next)                 = Raw args (f . next)

type KVDBTx = F (KeyValueF R.Queued)

----------------------------------------------------------------------

data TransactionF next where
  MultiExec
    :: KVDBTx (R.Queued a)
    -> (T.KVDBAnswer (T.TxResult a) -> next)
    -> TransactionF next
  MultiExecWithHash
    :: ByteString
    -> KVDBTx (R.Queued a)
    -> (T.KVDBAnswer (T.TxResult a) -> next)
    -> TransactionF next

instance Functor TransactionF where
  fmap f (MultiExec dsl next)           = MultiExec dsl (f . next)
  fmap f (MultiExecWithHash h dsl next) = MultiExecWithHash h dsl (f . next)

----------------------------------------------------------------------

data KVDBF next
  = KV (KeyValueF T.KVDBAnswer next)
  | TX (TransactionF next)
  deriving Functor

type KVDB next = ExceptT T.KVDBReply (F KVDBF) next

----------------------------------------------------------------------
-- | Set the value of a key. Transaction version.
setTx :: KVDBKey -> KVDBValue -> KVDBTx (R.Queued T.KVDBStatus)
setTx key value = liftFC $ Set key value id

-- | Set the value and ttl of a key. Transaction version.
setexTx :: KVDBKey -> KVDBDuration -> KVDBValue -> KVDBTx (R.Queued T.KVDBStatus)
setexTx key ex value = liftFC $ SetEx key ex value id

-- | Set the value of a hash field. Transaction version.
hsetTx :: KVDBKey -> KVDBField -> KVDBValue -> KVDBTx (R.Queued Bool)
hsetTx key field value = liftFC $ HSet key field value id

-- | Get the value of a key. Transaction version.
getTx :: KVDBKey -> KVDBTx (R.Queued (Maybe ByteString))
getTx key = liftFC $ Get key id

-- | Get the value of a hash field. Transaction version.
hgetTx :: KVDBKey -> KVDBField -> KVDBTx (R.Queued (Maybe ByteString))
hgetTx key field = liftFC $ HGet key field id

-- | Delete a keys. Transaction version.
delTx :: [KVDBKey] -> KVDBTx (R.Queued Integer)
delTx ks = liftFC $ Del ks id

-- | Set a key's time to live in seconds. Transaction version.
expireTx :: KVDBKey -> KVDBDuration -> KVDBTx (R.Queued Bool)
expireTx key sec = liftFC $ Expire key sec id

xaddTx :: KVDBStream -> KVDBStreamEntryIDInput -> [KVDBStreamItem] -> KVDBTx (R.Queued KVDBStreamEntryID)
xaddTx stream entryId items = liftFC $ XAdd stream entryId items id

xlenTx :: KVDBStream -> KVDBTx (R.Queued Integer)
xlenTx stream = liftFC $ XLen stream id

---
-- | Set the value of a key
set :: KVDBKey -> KVDBValue -> KVDB T.KVDBStatus
set key value = ExceptT $ liftFC $ KV $ Set key value id

-- | Set the value and ttl of a key.
setex :: KVDBKey -> KVDBDuration -> KVDBValue -> KVDB T.KVDBStatus
setex key ex value = ExceptT $ liftFC $ KV $ SetEx key ex value id

setOpts :: KVDBKey -> KVDBValue -> KVDBSetTTLOption -> KVDBSetConditionOption -> KVDB Bool
setOpts key value ttl cond = ExceptT $ liftFC $ KV $ SetOpts key value ttl cond id

-- | Get the value of a key
get :: KVDBKey -> KVDB (Maybe ByteString)
get key = ExceptT $ liftFC $ KV $ Get key id

-- | Determine if a key exists
exists :: KVDBKey -> KVDB Bool
exists key = ExceptT $ liftFC $ KV $ Exists key id

-- | Delete a keys
del :: [KVDBKey] -> KVDB Integer
del ks = ExceptT $ liftFC $ KV $ Del ks id

-- | Set a key's time to live in seconds
expire :: KVDBKey -> KVDBDuration -> KVDB Bool
expire key sec = ExceptT $ liftFC $ KV $ Expire key sec id

-- | Increment the integer value of a key by one
incr :: KVDBKey -> KVDB Integer
incr key = ExceptT $ liftFC $ KV $ Incr key id

-- | Set the value of a hash field
hset :: KVDBKey -> KVDBField -> KVDBValue -> KVDB Bool
hset key field value = ExceptT $ liftFC $ KV $ HSet key field value id

-- | Get the value of a hash field
hget :: KVDBKey -> KVDBField -> KVDB (Maybe ByteString)
hget key field = ExceptT $ liftFC $ KV $ HGet key field id

xadd :: KVDBStream -> KVDBStreamEntryIDInput -> [KVDBStreamItem] -> KVDB KVDBStreamEntryID
xadd stream entryId items = ExceptT $ liftFC $ KV $ XAdd stream entryId items id

xlen :: KVDBStream -> KVDB Integer
xlen stream = ExceptT $ liftFC $ KV $ XLen stream id

-- | Add one or more members to a set
sadd :: KVDBKey -> [KVDBValue] -> KVDB Integer
sadd setKey setmem = ExceptT $ liftFC $ KV $ SAdd setKey setmem id

sismember :: KVDBKey -> KVDBKey -> KVDB Bool
sismember key member = ExceptT $ liftFC $ KV $ SMem key member id

-- | Run commands inside a transaction(suited only for standalone redis setup).
multiExec :: KVDBTx (R.Queued a) -> KVDB (T.TxResult a)
multiExec kvtx = ExceptT $ liftFC $ TX $ MultiExec kvtx id

-- | Run commands inside a transaction(suited only for cluster redis setup).
multiExecWithHash :: ByteString -> KVDBTx (R.Queued a) -> KVDB (T.TxResult a)
multiExecWithHash h kvtx = ExceptT $ liftFC $ TX $ MultiExecWithHash h kvtx id

-- | Perform a raw call against the underlying Redis data store. This is
-- definitely unsafe, and should only be used if you know what you're doing.
--
-- /See also:/ The
-- [Hedis function](http://hackage.haskell.org/package/hedis-0.12.8/docs/Database-Redis.html#v:sendRequest) this is based on.
--
-- @since 2.0.3.2
rawRequest :: (R.RedisResult a) => [ByteString] -> KVDB a
rawRequest args = ExceptT . liftFC . KV . Raw args $ id
