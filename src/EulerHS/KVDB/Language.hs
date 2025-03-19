{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}

module EulerHS.KVDB.Language
  (
  -- * KVDB language
  -- ** Types
    KVDB, KVDBTx, KVDBKey, KVDBValue, KVDBDuration
  , KVDBSetTTLOption(..), KVDBSetConditionOption(..)
  , KVDBField, KVDBChannel, KVDBMessage
  , KVDBStream, KVDBStreamItem, KVDBStreamEntryID (..), KVDBStreamEntryIDInput (..)
  , KVDBF(..), KeyValueF(..), TransactionF(..), RecordID, KVDBStreamReadResponse (..), KVDBStreamReadResponseRecord (..), KVDBStreamEnd, KVDBStreamStart
  , KVDBGroupName, KVDBConsumerName
  -- ** Methods
  -- *** Regular
  -- **** For simple values
  , set, get, incr, decr, setex, setOpts, incrBy, decrBy, incrByFloat
  -- **** For list values
  , lpush, lrange, rpop, llen
  -- **** For hash values
  , hset, hget, hgetAll, hdel, hsetNx, hlen , hincrBy
  -- **** For streams
  , xadd, xlen, xread, xrange, xrevrange, xreadGroup, xdel, xgroupCreate
  -- **** For both
  , exists, del, expire, expireAt
  -- *** Transactional
  -- | Used inside multiExec instead of regular
  , multiExec, multiExecWithHash
  , setTx, getTx, delTx, setexTx
  , lpushTx, lrangeTx, rpopTx, llenTx
  , hsetTx, hgetTx, hsetNxTx,  hmset, hmsetTx , hincrByTx
  , xaddTx, xlenTx , xreadTx , xreadOpts
  , incrTx, decrTx
  , expireTx, expireAtTx
  , saddTx
  , setOptsTx
  , incrByTx, decrByTx, incrByFloatTx
  -- *** Set
  , sadd, srem
  , smembers, smove
  , sismember

  --- *** Ordered Set
  , zadd
  , zrange, zrangewithscores, zrangebyscore, zrangebyscorewithscore, zrangebyscorewithlimit, zrem, zremrangebyscore, zcard
  -- *** Raw
  , rawRequest
  , pingRequest
  ) where

import qualified Database.Redis as R
import           EulerHS.KVDB.Types (KVDBAnswer, KVDBReply, KVDBStatus, TxResult, KVDBKey, KVDBValue, KVDBDuration
                    , KVDBSetTTLOption(..), KVDBSetConditionOption(..)
                    , KVDBField, KVDBChannel, KVDBMessage
                    , KVDBStream, KVDBStreamItem, KVDBStreamEntryID (..), KVDBStreamEntryIDInput (..)
                    , RecordID, KVDBStreamReadResponse (..), KVDBStreamReadResponseRecord (..), KVDBStreamEnd, KVDBStreamStart
                    , KVDBGroupName, KVDBConsumerName)
import           EulerHS.Prelude hiding (get)


data KeyValueF f next where
  Set     :: KVDBKey -> KVDBValue -> (f KVDBStatus -> next) -> KeyValueF f next
  SetEx   :: KVDBKey -> KVDBDuration -> KVDBValue -> (f KVDBStatus -> next) -> KeyValueF f next
  SetOpts :: KVDBKey -> KVDBValue -> KVDBSetTTLOption -> KVDBSetConditionOption -> (f Bool -> next) -> KeyValueF f next
  Get     :: KVDBKey -> (f (Maybe ByteString) -> next) -> KeyValueF f next
  Exists  :: KVDBKey -> (f Bool -> next) -> KeyValueF f next
  Del     :: [KVDBKey] -> (f Integer -> next) -> KeyValueF f next
  Expire  :: KVDBKey -> KVDBDuration -> (f Bool -> next) -> KeyValueF f next
  ExpireAt  :: KVDBKey -> KVDBDuration -> (f Bool -> next) -> KeyValueF f next
  Incr    :: KVDBKey -> (f Integer -> next) -> KeyValueF f next
  IncrBy  :: KVDBKey -> Integer -> (f Integer -> next) -> KeyValueF f next
  IncrByFloat  :: KVDBKey -> Double -> (f Double -> next) -> KeyValueF f next
  Decr    :: KVDBKey -> (f Integer -> next) -> KeyValueF f next
  DecrBy  :: KVDBKey -> Integer -> (f Integer -> next) -> KeyValueF f next
  HSet    :: KVDBKey -> KVDBField -> KVDBValue -> (f Integer -> next) -> KeyValueF f next
  HSetNx  :: KVDBKey -> KVDBField -> KVDBValue -> (f Bool -> next) -> KeyValueF f next
  HmSet   :: KVDBKey -> [(KVDBField, KVDBValue)] -> (f KVDBStatus -> next) -> KeyValueF f next
  HGet    :: KVDBKey -> KVDBField -> (f (Maybe ByteString) -> next) -> KeyValueF f next
  HGetAll :: KVDBKey -> (f ([(ByteString, ByteString)]) -> next) -> KeyValueF f next
  HDel    :: KVDBKey -> [KVDBField] -> (f Integer -> next) -> KeyValueF f next
  HLen    :: KVDBKey -> (f Integer -> next) -> KeyValueF f next
  XAdd    :: KVDBStream -> KVDBStreamEntryIDInput -> [KVDBStreamItem] -> (f KVDBStreamEntryID -> next) -> KeyValueF f next
  XRead   :: KVDBStream -> RecordID -> (f (Maybe [KVDBStreamReadResponse]) -> next) -> KeyValueF f next
  XReadGroup :: KVDBGroupName -> KVDBConsumerName -> [(KVDBStream, RecordID)] -> R.XReadOpts -> (f (Maybe [KVDBStreamReadResponse]) -> next) -> KeyValueF f next
  XReadOpts :: [(KVDBStream, KVDBStreamEntryIDInput)] -> R.XReadOpts -> (f (Maybe [R.XReadResponse]) -> next) -> KeyValueF f next
  XGroupCreate :: KVDBStream -> KVDBGroupName -> RecordID -> (f R.Status -> next) -> KeyValueF f next
  XDel    :: KVDBStream -> [KVDBStreamEntryID] -> (f Integer -> next) -> KeyValueF f next
  XRange :: KVDBStream -> KVDBStreamStart ->  KVDBStreamEnd -> Maybe Integer -> (f [KVDBStreamReadResponseRecord] -> next) -> KeyValueF f next
  XRevRange :: KVDBStream -> KVDBStreamEnd -> KVDBStreamStart -> Maybe Integer -> (f [KVDBStreamReadResponseRecord] -> next) -> KeyValueF f next
  XLen    :: KVDBStream -> (f Integer -> next) -> KeyValueF f next
  SAdd    :: KVDBKey -> [KVDBValue] -> (f Integer -> next) -> KeyValueF f next
  ZAdd  :: KVDBKey -> [(Double, KVDBValue)] -> (f Integer -> next) -> KeyValueF f next
  ZRange :: KVDBKey -> Integer -> Integer -> (f [ByteString] -> next) -> KeyValueF f next
  ZRangeWithScores :: KVDBKey -> Integer -> Integer -> (f [(ByteString,Double)] -> next) -> KeyValueF f next
  ZRangeByScore :: KVDBKey -> Double -> Double -> (f [ByteString] -> next) -> KeyValueF f next
  ZRangeByScoreWithScore :: KVDBKey -> Double -> Double -> (f [(ByteString,Double)] -> next) -> KeyValueF f next
  ZRangeByScoreWithLimit :: KVDBKey -> Double -> Double -> Integer -> Integer -> (f [ByteString] -> next) -> KeyValueF f next
  ZRem :: KVDBKey -> [KVDBValue] -> (f Integer -> next) -> KeyValueF f next
  ZRemRangeByScore :: KVDBKey -> Double -> Double -> (f Integer -> next) -> KeyValueF f next
  ZCard :: KVDBKey -> (f Integer -> next) -> KeyValueF f next
  SRem    :: KVDBKey -> [KVDBValue] -> (f Integer -> next) -> KeyValueF f next
  LPush   :: KVDBKey -> [KVDBValue] -> (f Integer -> next) -> KeyValueF f next
  LRange   :: KVDBKey -> Integer -> Integer -> (f [ByteString] -> next) -> KeyValueF f next
  RPop    :: KVDBKey -> (f (Maybe ByteString) -> next) -> KeyValueF f next
  LLen    :: KVDBKey -> (f Integer -> next) -> KeyValueF f next
  SMembers :: KVDBKey -> (f [ByteString] -> next) -> KeyValueF f next
  SMove   :: KVDBKey -> KVDBKey -> KVDBValue -> (f Bool -> next) -> KeyValueF f next
  SMem    :: KVDBKey -> KVDBKey -> (f Bool -> next) -> KeyValueF f next
  Raw     :: (R.RedisResult a) => [ByteString] -> (f a -> next) -> KeyValueF f next
  Ping    :: (f R.Status -> next) -> KeyValueF f next
  HIncrBy :: KVDBKey -> KVDBField -> Integer -> (f Integer -> next) -> KeyValueF f next

instance Functor (KeyValueF f) where
  fmap f (Set k value next)              = Set k value (f . next)
  fmap f (SetEx k ex value next)         = SetEx k ex value (f . next)
  fmap f (SetOpts k value ttl cond next) = SetOpts k value ttl cond (f . next)
  fmap f (Get k next)                    = Get k (f . next)
  fmap f (Exists k next)                 = Exists k (f . next)
  fmap f (Del ks next)                   = Del ks (f . next)
  fmap f (Expire k sec next)             = Expire k sec (f . next)
  fmap f (ExpireAt k sec next)           = ExpireAt k sec (f . next)
  fmap f (Incr k next)                   = Incr k (f . next)
  fmap f (IncrBy k val next)             = IncrBy k val (f . next)
  fmap f (IncrByFloat k val next)        = IncrByFloat k val (f . next)
  fmap f (Decr k next)                   = Decr k (f . next)
  fmap f (DecrBy k val next)             = DecrBy k val (f . next)
  fmap f (HSet k field value next)       = HSet k field value (f . next)
  fmap f (HSetNx k field value next)     = HSetNx k field value (f . next)
  fmap f (HmSet k values next)           = HmSet k values (f . next)
  fmap f (HGet k field next)             = HGet k field (f . next)
  fmap f (HGetAll k next)                = HGetAll k (f . next)
  fmap f (HDel k field next)             = HDel k field (f . next)
  fmap f (HLen k next)                   = HLen k (f . next)
  fmap f (XAdd s entryId items next)     = XAdd s entryId items (f . next)
  fmap f (XRead s entryId next)          = XRead s entryId (f . next)
  fmap f (XReadGroup gName cName s opts next) = XReadGroup gName cName s opts (f . next)
  fmap f (XGroupCreate s gName startId next)  = XGroupCreate s gName startId (f . next)
  fmap f (XDel s entryId next)               = XDel s entryId (f . next)
  fmap f (XRange strm sstart send count next) = XRange strm sstart send count (f . next)
  fmap f (XRevRange strm send sstart count next) = XRevRange strm send sstart count (f . next)
  fmap f (XLen s next)                   = XLen s (f . next)
  fmap f (SAdd k v next)                 = SAdd k v (f . next)
  fmap f (ZAdd k v next)                 = ZAdd k v (f . next)
  fmap f (ZRange k s1 s2 next)           = ZRange k s1 s2 (f . next)
  fmap f (ZRangeWithScores k s1 s2 next) = ZRangeWithScores k s1 s2 (f . next)
  fmap f (ZRangeByScore k s1 s2 next)    = ZRangeByScore k s1 s2 (f . next)
  fmap f (ZRangeByScoreWithScore k s1 s2 next)    = ZRangeByScoreWithScore k s1 s2 (f . next)
  fmap f (ZRangeByScoreWithLimit k s1 s2 offset count next) = ZRangeByScoreWithLimit k s1 s2 offset count (f . next)
  fmap f (ZRem k v next)                 = ZRem k v (f . next)
  fmap f (ZRemRangeByScore k s1 s2 next) = ZRemRangeByScore k s1 s2 (f . next)
  fmap f (ZCard k next)                  = ZCard k (f . next)
  fmap f (SRem k v next)                 = SRem k v (f . next)
  fmap f (LPush k v next)                = LPush k v (f . next)
  fmap f (LRange k start stop next)      = LRange k start stop (f . next)
  fmap f (RPop k next)                   = RPop k (f . next)
  fmap f (LLen k next)                   = LLen k (f . next)
  fmap f (SMove k1 k2 v next)            = SMove k1 k2 v (f . next)
  fmap f (SMembers k next)               = SMembers k (f . next)
  fmap f (SMem k v next)                 = SMem k v (f . next)
  fmap f (Raw args next)                 = Raw args (f . next)
  fmap f (XReadOpts s readOpts next)     = XReadOpts s readOpts (f . next)
  fmap f (Ping next)                     = Ping (f . next)
  fmap f (HIncrBy k field value next)    = HIncrBy k field value (f . next)

type KVDBTx = F (KeyValueF R.Queued)

----------------------------------------------------------------------

data TransactionF next where
  MultiExec
    :: KVDBTx (R.Queued a)
    -> (KVDBAnswer (TxResult a) -> next)
    -> TransactionF next
  MultiExecWithHash
    :: ByteString
    -> KVDBTx (R.Queued a)
    -> (KVDBAnswer (TxResult a) -> next)
    -> TransactionF next

instance Functor TransactionF where
  fmap f (MultiExec dsl next)           = MultiExec dsl (f . next)
  fmap f (MultiExecWithHash h dsl next) = MultiExecWithHash h dsl (f . next)

----------------------------------------------------------------------

data KVDBF next
  = KV (KeyValueF KVDBAnswer next)
  | TX (TransactionF next)
  deriving Functor

type KVDB next = ExceptT KVDBReply (F KVDBF) next

----------------------------------------------------------------------
-- | Set the value of a key. Transaction version.
setTx :: KVDBKey -> KVDBValue -> KVDBTx (R.Queued KVDBStatus)
setTx key value = liftFC $ Set key value id

-- | Set the value and ttl of a key. Transaction version.
setexTx :: KVDBKey -> KVDBDuration -> KVDBValue -> KVDBTx (R.Queued KVDBStatus)
setexTx key ex value = liftFC $ SetEx key ex value id

-- | Set the value of a hash field. Transaction version.
hsetTx :: KVDBKey -> KVDBField -> KVDBValue -> KVDBTx (R.Queued Integer)
hsetTx key field value = liftFC $ HSet key field value id

hincrByTx :: KVDBKey -> KVDBField -> Integer -> KVDBTx (R.Queued Integer)
hincrByTx key field value = liftFC $ HIncrBy key field value id

-- | Set multiple hash fields to multiple values. Transaction version.
hmsetTx :: KVDBKey -> [(KVDBField, KVDBValue)] -> KVDBTx (R.Queued KVDBStatus)
hmsetTx key values = liftFC $ HmSet key values id

hsetNxTx :: KVDBKey -> KVDBField -> KVDBValue -> KVDBTx (R.Queued Bool)
hsetNxTx key field value = liftFC $ HSetNx key field value id

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

-- | Set a key's time to live in unix-time-seconds. Transaction version.
expireAtTx :: KVDBKey -> KVDBDuration -> KVDBTx (R.Queued Bool)
expireAtTx key sec = liftFC $ ExpireAt key sec id

xaddTx :: KVDBStream -> KVDBStreamEntryIDInput -> [KVDBStreamItem] -> KVDBTx (R.Queued KVDBStreamEntryID)
xaddTx stream entryId items = liftFC $ XAdd stream entryId items id

xreadTx :: KVDBStream -> RecordID -> KVDBTx (R.Queued (Maybe [KVDBStreamReadResponse]))
xreadTx stream entryId = liftFC $ XRead stream entryId id

xlenTx :: KVDBStream -> KVDBTx (R.Queued Integer)
xlenTx stream = liftFC $ XLen stream id

lpushTx :: KVDBKey -> [KVDBValue] -> KVDBTx (R.Queued Integer)
lpushTx key value = liftFC $ LPush key value id

lrangeTx :: KVDBKey -> Integer -> Integer -> KVDBTx (R.Queued [ByteString])
lrangeTx key start stop = liftFC $ LRange key start stop id

rpopTx :: KVDBKey -> KVDBTx (R.Queued (Maybe ByteString))
rpopTx key = liftFC $ RPop key id

llenTx :: KVDBKey -> KVDBTx (R.Queued Integer)
llenTx key = liftFC $ LLen key id

saddTx :: KVDBKey -> [KVDBValue] -> KVDBTx (R.Queued Integer)
saddTx setKey setmem = liftFC $ SAdd setKey setmem id

setOptsTx :: KVDBKey -> KVDBValue -> KVDBSetTTLOption -> KVDBSetConditionOption -> KVDBTx (R.Queued Bool)
setOptsTx key value ttl cond = liftFC $ SetOpts key value ttl cond id

incrTx :: KVDBKey -> KVDBTx (R.Queued Integer)
incrTx key = liftFC $ Incr key id

incrByTx :: KVDBKey -> Integer -> KVDBTx (R.Queued Integer)
incrByTx key val = liftFC $ IncrBy key val id

incrByFloatTx :: KVDBKey -> Double -> KVDBTx (R.Queued Double)
incrByFloatTx key val = liftFC $ IncrByFloat key val id

decrTx :: KVDBKey -> KVDBTx (R.Queued Integer)
decrTx key = liftFC $ Decr key id

decrByTx :: KVDBKey -> Integer -> KVDBTx (R.Queued Integer)
decrByTx key val = liftFC $ DecrBy key val id

---
-- | Set the value of a key
set :: KVDBKey -> KVDBValue -> KVDB KVDBStatus
set key value = ExceptT $ liftFC $ KV $ Set key value id

-- | Set the value and ttl of a key.
setex :: KVDBKey -> KVDBDuration -> KVDBValue -> KVDB KVDBStatus
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

-- | Set a key's time to live in unix-time-seconds
expireAt :: KVDBKey -> KVDBDuration -> KVDB Bool
expireAt key sec = ExceptT $ liftFC $ KV $ ExpireAt key sec id

-- | Increment the integer value of a key by one
incr :: KVDBKey -> KVDB Integer
incr key = ExceptT $ liftFC $ KV $ Incr key id

-- | Increment the integer value of a key by val
incrBy :: KVDBKey -> Integer -> KVDB Integer
incrBy key val = ExceptT $ liftFC $ KV $ IncrBy key val id

-- | Increment the double value of a key by val
incrByFloat :: KVDBKey -> Double -> KVDB Double
incrByFloat key val = ExceptT $ liftFC $ KV $ IncrByFloat key val id

-- | Decrement the integer value of a key by one
decr :: KVDBKey -> KVDB Integer
decr key = ExceptT $ liftFC $ KV $ Decr key id

-- | Decrement the integer value of a key by val
decrBy :: KVDBKey -> Integer -> KVDB Integer
decrBy key val = ExceptT $ liftFC $ KV $ DecrBy key val id

-- | Set the value of a hash field
hset :: KVDBKey -> KVDBField -> KVDBValue -> KVDB Integer
hset key field value = ExceptT $ liftFC $ KV $ HSet key field value id

hsetNx :: KVDBKey -> KVDBField -> KVDBValue -> KVDB Bool
hsetNx key field value = ExceptT $ liftFC $ KV $ HSetNx key field value id

hmset :: KVDBKey -> [(KVDBField, KVDBValue)] -> KVDB KVDBStatus
hmset key values = ExceptT $ liftFC $ KV $ HmSet key values id

-- | Get the value of a hash field
hget :: KVDBKey -> KVDBField -> KVDB (Maybe ByteString)
hget key field = ExceptT $ liftFC $ KV $ HGet key field id

hincrBy :: KVDBKey -> KVDBField -> Integer -> KVDB Integer
hincrBy key field value = ExceptT $ liftFC $ KV $ HIncrBy key field value id

hgetAll :: KVDBKey -> KVDB ([(ByteString, ByteString)])
hgetAll key = ExceptT $ liftFC $ KV $ HGetAll key id

hdel :: KVDBKey -> [KVDBField] -> KVDB Integer
hdel key fields = ExceptT $ liftFC $ KV $ HDel key fields id

hlen :: KVDBKey -> KVDB Integer
hlen key = ExceptT $ liftFC $ KV $ HLen key id

xadd :: KVDBStream -> KVDBStreamEntryIDInput -> [KVDBStreamItem] -> KVDB KVDBStreamEntryID
xadd stream entryId items = ExceptT $ liftFC $ KV $ XAdd stream entryId items id

xread :: KVDBStream -> RecordID -> KVDB (Maybe [KVDBStreamReadResponse])
xread stream entryId = ExceptT $ liftFC $ KV $ XRead stream entryId id

xreadGroup :: KVDBGroupName -> KVDBConsumerName -> [(KVDBStream, RecordID)] -> Maybe Integer -> Maybe Integer -> Bool -> KVDB (Maybe [KVDBStreamReadResponse])
xreadGroup groupName consumerName streamsAndIds mBlock mCount noack = ExceptT $ liftFC $ KV $ XReadGroup groupName consumerName streamsAndIds (R.XReadOpts mBlock mCount noack) id

xreadOpts :: [(KVDBStream, KVDBStreamEntryIDInput)] -> R.XReadOpts -> KVDB (Maybe [R.XReadResponse])
xreadOpts stPair readOpts = ExceptT $ liftFC $ KV $ XReadOpts stPair readOpts id

xgroupCreate :: KVDBStream -> KVDBGroupName -> RecordID -> KVDB R.Status
xgroupCreate stream groupName startId = ExceptT $ liftFC $ KV $ XGroupCreate stream groupName startId id

xdel :: KVDBStream -> [KVDBStreamEntryID] -> KVDB Integer
xdel stream entryId = ExceptT $ liftFC $ KV $ XDel stream entryId id

xrange :: KVDBStream -> KVDBStreamStart -> KVDBStreamEnd -> Maybe Integer -> KVDB ([KVDBStreamReadResponseRecord])
xrange stream sstart send count = ExceptT $ liftFC $ KV $ XRange stream sstart send count id

xrevrange :: KVDBStream -> KVDBStreamEnd -> KVDBStreamStart -> Maybe Integer -> KVDB ([KVDBStreamReadResponseRecord])
xrevrange stream send sstart count = ExceptT $ liftFC $ KV $ XRevRange stream send sstart count id

xlen :: KVDBStream -> KVDB Integer
xlen stream = ExceptT $ liftFC $ KV $ XLen stream id

-- | Add one or more members to a set
sadd :: KVDBKey -> [KVDBValue] -> KVDB Integer
sadd setKey setmem = ExceptT $ liftFC $ KV $ SAdd setKey setmem id

srem :: KVDBKey -> [KVDBValue] -> KVDB Integer
srem setKey setmem = ExceptT $ liftFC $ KV $ SRem setKey setmem id

zadd :: KVDBKey -> [(Double, KVDBValue)] -> KVDB Integer
zadd key values = ExceptT $ liftFC $ KV $ ZAdd key values id

zrange :: KVDBKey -> Integer -> Integer -> KVDB [ByteString]
zrange key startRank stopRank = ExceptT $ liftFC $ KV $ ZRange key startRank stopRank id

zrangewithscores :: KVDBKey -> Integer -> Integer -> KVDB [(ByteString,Double)]
zrangewithscores key startRank stopRank = ExceptT $ liftFC $ KV $ ZRangeWithScores key startRank stopRank id

zrangebyscore :: KVDBKey -> Double -> Double -> KVDB [ByteString]
zrangebyscore key minScore maxScore = ExceptT $ liftFC $ KV $ ZRangeByScore key minScore maxScore id

zrangebyscorewithscore :: KVDBKey -> Double -> Double -> KVDB [(ByteString,Double)]
zrangebyscorewithscore key minScore maxScore = ExceptT $ liftFC $ KV $ ZRangeByScoreWithScore key minScore maxScore id

zrem :: KVDBKey -> [KVDBValue] -> KVDB Integer
zrem key values = ExceptT $ liftFC $ KV $ ZRem key values id

zremrangebyscore :: KVDBKey -> Double -> Double -> KVDB Integer
zremrangebyscore key minScore maxScore = ExceptT $ liftFC $ KV $ ZRemRangeByScore key minScore maxScore id

zrangebyscorewithlimit :: KVDBKey -> Double -> Double -> Integer -> Integer -> KVDB [ByteString]
zrangebyscorewithlimit key minScore maxScore offset count = ExceptT $ liftFC $ KV $ ZRangeByScoreWithLimit key minScore maxScore offset count id

zcard :: KVDBKey -> KVDB Integer
zcard key = ExceptT $ liftFC $ KV $ ZCard key id

lpush :: KVDBKey -> [KVDBValue] -> KVDB Integer
lpush key value = ExceptT $ liftFC $ KV $ LPush key value id

lrange :: KVDBKey -> Integer -> Integer -> KVDB [ByteString]
lrange key start stop = ExceptT $ liftFC $ KV $ LRange key start stop id

rpop :: KVDBKey -> KVDB (Maybe ByteString)
rpop key = ExceptT $ liftFC $ KV $ RPop key id

llen :: KVDBKey -> KVDB Integer
llen key = ExceptT $ liftFC $ KV $ LLen key id

smembers :: KVDBKey -> KVDB [ByteString]
smembers key = ExceptT $ liftFC $ KV $ SMembers key id

smove :: KVDBKey -> KVDBKey -> KVDBValue -> KVDB Bool
smove source destination member = ExceptT $ liftFC $ KV $ SMove source destination member id

sismember :: KVDBKey -> KVDBKey -> KVDB Bool
sismember key member = ExceptT $ liftFC $ KV $ SMem key member id

-- | Run commands inside a transaction(suited only for standalone redis setup).
multiExec :: KVDBTx (R.Queued a) -> KVDB (TxResult a)
multiExec kvtx = ExceptT $ liftFC $ TX $ MultiExec kvtx id

-- | Run commands inside a transaction(suited only for cluster redis setup).
multiExecWithHash :: ByteString -> KVDBTx (R.Queued a) -> KVDB (TxResult a)
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

pingRequest :: KVDB R.Status
pingRequest = ExceptT $ liftFC $ KV $ Ping id