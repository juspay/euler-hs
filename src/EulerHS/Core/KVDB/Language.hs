{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveAnyClass        #-}

{- |
Module      :  EulerHS.Core.KVDB.Language
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2021
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
  ) where

import qualified Data.Aeson as A
import qualified Database.Redis as R
import qualified EulerHS.Core.Types as T
import           EulerHS.Prelude hiding (get)

-- | TTL options for the `set` operaion
data KVDBSetTTLOption
  = NoTTL
    -- ^ No TTL
  | Seconds Integer
    -- ^ TTL in seconds
  | Milliseconds Integer
    -- ^ TTL in millisecons
  deriving stock Generic
  deriving anyclass A.ToJSON

-- | Options for the `set` operation
data KVDBSetConditionOption
  = SetAlways
    -- ^ Set value no matter what
  | SetIfExist
    -- ^ Set if exist
  | SetIfNotExist
    -- ^ Set if not exist
  deriving stock Generic
  deriving anyclass A.ToJSON

-- | Raw key value (ByteString)
type KVDBKey = ByteString

-- | Raw value (ByteString)
type KVDBValue = ByteString

-- | Duration (seconds)
type KVDBDuration = Integer

-- | Field
type KVDBField = ByteString

-- | Channel
type KVDBChannel = ByteString

-- | Message
type KVDBMessage = ByteString

-- | Stream
type KVDBStream = ByteString

-- | ID of a stream entity
data KVDBStreamEntryID = KVDBStreamEntryID Integer Integer
  deriving stock Generic
  deriving anyclass (A.ToJSON, A.FromJSON)

-- | Input of a stream entity
data KVDBStreamEntryIDInput
  = EntryID KVDBStreamEntryID
  | AutoID
  deriving stock Generic
  deriving anyclass A.ToJSON

-- | Stream item
type KVDBStreamItem = (ByteString, ByteString)

----------------------------------------------------------------------

-- | Algebra of the KV DB language
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

-- | KV DB transactional monadic language
type KVDBTx = F (KeyValueF R.Queued)

----------------------------------------------------------------------

-- | Algebra of the transactional evaluation
-- ('Exec' in hedis notaion)
data TransactionF next where
  MultiExec
    :: T.JSONEx a
    => KVDBTx (R.Queued a)
    -> (T.KVDBAnswer (T.TxResult a) -> next)
    -> TransactionF next
  MultiExecWithHash
    :: T.JSONEx a
    => ByteString
    -> KVDBTx (R.Queued a)
    -> (T.KVDBAnswer (T.TxResult a) -> next)
    -> TransactionF next

instance Functor TransactionF where
  fmap f (MultiExec dsl next) = MultiExec dsl (f . next)
  fmap f (MultiExecWithHash h dsl next) = MultiExecWithHash h dsl (f . next)

----------------------------------------------------------------------

-- | Top-level algebra combining either a transactional or regular language method
data KVDBF next
  = KV (KeyValueF T.KVDBAnswer next)
  | TX (TransactionF next)
  deriving Functor

-- | Main KV DB language
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

-- | Add entities to a stream
xaddTx :: KVDBStream -> KVDBStreamEntryIDInput -> [KVDBStreamItem] -> KVDBTx (R.Queued KVDBStreamEntryID)
xaddTx stream entryId items = liftFC $ XAdd stream entryId items id

-- | Get length of a stream
xlenTx :: KVDBStream -> KVDBTx (R.Queued Integer)
xlenTx stream = liftFC $ XLen stream id

-- | Set the value of a key
set :: KVDBKey -> KVDBValue -> KVDB T.KVDBStatus
set key value = ExceptT $ liftFC $ KV $ Set key value id

-- | Set the value and ttl of a key.
setex :: KVDBKey -> KVDBDuration -> KVDBValue -> KVDB T.KVDBStatus
setex key ex value = ExceptT $ liftFC $ KV $ SetEx key ex value id

-- | Specify set operation options
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

-- | Add entities to a stream
xadd :: KVDBStream -> KVDBStreamEntryIDInput -> [KVDBStreamItem] -> KVDB KVDBStreamEntryID
xadd stream entryId items = ExceptT $ liftFC $ KV $ XAdd stream entryId items id

-- | Get length of a stream
xlen :: KVDBStream -> KVDB Integer
xlen stream = ExceptT $ liftFC $ KV $ XLen stream id

-- | Run commands inside a transaction(suited only for standalone redis setup).
multiExec :: T.JSONEx a => KVDBTx (R.Queued a) -> KVDB (T.TxResult a)
multiExec kvtx = ExceptT $ liftFC $ TX $ MultiExec kvtx id

-- | Run commands inside a transaction(suited only for cluster redis setup).
multiExecWithHash :: T.JSONEx a => ByteString -> KVDBTx (R.Queued a) -> KVDB (T.TxResult a)
multiExecWithHash h kvtx = ExceptT $ liftFC $ TX $ MultiExecWithHash h kvtx id
