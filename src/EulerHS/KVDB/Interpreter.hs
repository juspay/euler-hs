{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

module EulerHS.KVDB.Interpreter
  (
    -- * KVDB Interpreter
    runKVDB
  ) where

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Database.Redis as R
import qualified EulerHS.KVDB.Language as L
import           EulerHS.KVDB.Types (KVDBError (KVDBConnectionDoesNotExist),
                                     KVDBReply, KVDBReplyF (Bulk, KVDBError),
                                     NativeKVDBConn (NativeKVDB),
                                     exceptionToKVDBReply, fromRdStatus,
                                     fromRdTxResult, hedisReplyToKVDBReply)
import           EulerHS.Prelude
import           Text.Read (read)

interpretKeyValueF
  ::
  (forall b . R.Redis (Either R.Reply b) -> IO (Either KVDBReply b))
  -> L.KeyValueF (Either KVDBReply) a
  -> IO a
interpretKeyValueF runRedis (L.Set k v next) =
  next . second fromRdStatus <$> runRedis (R.set k v)

interpretKeyValueF runRedis (L.SetEx k e v next) =
  next . second fromRdStatus <$> runRedis (R.setex k e v)

interpretKeyValueF runRedis (L.SetOpts k v ttl cond next) =
  fmap next $ do
    result <- runRedis $ R.setOpts k v (makeSetOpts ttl cond)
    pure $ case result of
      Right _             -> Right True
      -- (nil) is ok, app should not fail
      Left (Bulk Nothing) -> Right False
      Left reply          -> Left reply

interpretKeyValueF runRedis (L.Get k next) =
  fmap next $
    runRedis $ R.get k

interpretKeyValueF runRedis (L.Exists k next) =
  fmap next $
    runRedis $ R.exists k

interpretKeyValueF _ (L.Del [] next) =
  pure . next . pure $ 0

interpretKeyValueF runRedis (L.Del ks next) =
  fmap next $
    runRedis $ R.del ks

interpretKeyValueF runRedis (L.Expire k sec next) =
  fmap next $
    runRedis $ R.expire k sec

interpretKeyValueF runRedis (L.Incr k next) =
  fmap next $
    runRedis $ R.incr k

interpretKeyValueF runRedis (L.HSet k field value next) =
  fmap next $ do
    result <- runRedis $ R.hset k field value
    pure $ case result of
      Right 0 -> Right False
      Right _ -> Right True
      Left reply -> Left reply

interpretKeyValueF runRedis (L.HGet k field next) =
  fmap next $
    runRedis $ R.hget k field

interpretKeyValueF runRedis (L.XAdd stream entryId items next) =
  fmap next $
    runRedis $ do
      result <- R.xadd stream (makeStreamEntryId entryId) items
      pure $ parseStreamEntryId <$> result
  where
    makeStreamEntryId (L.EntryID (L.KVDBStreamEntryID ms sq)) = show ms <> "-" <> show sq
    makeStreamEntryId L.AutoID = "*"

    parseStreamEntryId bs =
      -- "number-number" is redis entry id invariant
      let (ms, sq) = bimap (read . T.unpack) (read . T.unpack) .
                      T.breakOn "-" . TE.decodeUtf8With TE.lenientDecode $ bs
      in L.KVDBStreamEntryID ms sq

interpretKeyValueF runRedis (L.XRead stream entryId next) =
  fmap next $
    runRedis $ do
      result <- R.xread [(stream, entryId)]
      pure $ (fmap . fmap $ parseXReadResponse) <$> result
  where
    parseXReadResponseRecord :: R.StreamsRecord -> L.KVDBStreamReadResponseRecord
    parseXReadResponseRecord record =
      L.KVDBStreamReadResponseRecord (R.recordId record) (R.keyValues record)

    parseXReadResponse :: R.XReadResponse -> L.KVDBStreamReadResponse
    parseXReadResponse (R.XReadResponse strm records) = L.KVDBStreamReadResponse strm (parseXReadResponseRecord <$> records)

interpretKeyValueF runRedis (L.XReadGroup groupName consumerName streamsAndIds opt next) =
  fmap next $
    runRedis $ do
      result <- R.xreadGroupOpts groupName consumerName streamsAndIds opt
      pure $ (fmap . fmap $ parseXReadResponse) <$> result
  where
    parseXReadResponseRecord :: R.StreamsRecord -> L.KVDBStreamReadResponseRecord
    parseXReadResponseRecord record =
      L.KVDBStreamReadResponseRecord (R.recordId record) (R.keyValues record)

    parseXReadResponse :: R.XReadResponse -> L.KVDBStreamReadResponse
    parseXReadResponse (R.XReadResponse strm records) = L.KVDBStreamReadResponse strm (parseXReadResponseRecord <$> records)

interpretKeyValueF runRedis (L.XReadOpts strObjs readOpts next) =
  fmap next $
    runRedis $ do
      result <- R.xreadOpts ((\(a, b) -> (a, makeStreamEntryId b)) <$> strObjs) readOpts
      pure result
  where
    makeStreamEntryId (L.EntryID (L.KVDBStreamEntryID ms sq)) = show ms <> "-" <> show sq
    makeStreamEntryId L.AutoID = "*" 


interpretKeyValueF runRedis (L.XGroupCreate stream groupName startId next) =
  fmap next $ runRedis $ R.xgroupCreate stream groupName startId

interpretKeyValueF runRedis (L.XDel stream entryIds next) =
  fmap next $
    runRedis $ R.xdel stream ((\(L.KVDBStreamEntryID ms sq) -> show ms <> "-" <> show sq)  <$> entryIds)

interpretKeyValueF runRedis (L.XRevRange stream send sstart count next) =
  fmap next $
    runRedis $ do
      result <- R.xrevRange stream send sstart count
      pure $ (fmap parseXReadResponseRecord) <$> result
  where
    parseXReadResponseRecord :: R.StreamsRecord -> L.KVDBStreamReadResponseRecord
    parseXReadResponseRecord record =
      L.KVDBStreamReadResponseRecord (R.recordId record) (R.keyValues record)
      

interpretKeyValueF runRedis (L.XLen stream next) =
  fmap next $
    runRedis $ R.xlen stream

interpretKeyValueF runRedis (L.SAdd k v next) =
  fmap next $ runRedis $ R.sadd k v

interpretKeyValueF runRedis (L.ZAdd k v next) = 
  fmap next $ runRedis $ R.zadd k v

interpretKeyValueF runRedis (L.ZRange k start stop next) =
  fmap next $ runRedis $ R.zrange k start stop

interpretKeyValueF runRedis (L.ZRangeByScore k minScore maxScore next) =
  fmap next $ runRedis $  R.zrangebyscore k minScore maxScore

interpretKeyValueF runRedis (L.ZRangeByScoreWithLimit k minScore maxScore offset count next) =
  fmap next $ runRedis $  R.zrangebyscoreLimit k minScore maxScore offset count

interpretKeyValueF runRedis (L.ZRem k v next) =
  fmap next $ runRedis $ R.zrem k v

interpretKeyValueF runRedis (L.ZRemRangeByScore k minScore maxScore next) =
  fmap next $ runRedis $ R.zremrangebyscore k minScore maxScore

interpretKeyValueF runRedis (L.ZCard k next) =
  fmap next $ runRedis $ R.zcard k

interpretKeyValueF runRedis (L.SRem k v next) =
  fmap next $ runRedis $ R.srem k v

interpretKeyValueF runRedis (L.LPush k v next) =
  fmap next $ runRedis $ R.lpush k v

interpretKeyValueF runRedis (L.LRange k start stop next) =
  fmap next $ runRedis $ R.lrange k start stop

interpretKeyValueF runRedis (L.SMembers k next) =
  fmap next $ runRedis $ R.smembers k

interpretKeyValueF runRedis (L.SMove k1 k2 v next) =
  fmap next $ runRedis $ R.smove k1 k2 v

interpretKeyValueF runRedis (L.SMem k v next) =
  fmap next $ runRedis $ R.sismember k v

interpretKeyValueF runRedis (L.Raw args next) = next <$> runRedis (R.sendRequest args)

interpretKeyValueF runRedis (L.Ping next) = fmap next $ runRedis $ R.ping

interpretKeyValueTxF :: L.KeyValueF R.Queued a -> R.RedisTx a
interpretKeyValueTxF (L.Set k v next) =
  next . fmap fromRdStatus <$> R.set k v

interpretKeyValueTxF (L.SetEx k e v next) =
  next . fmap fromRdStatus <$> R.setex k e v

interpretKeyValueTxF (L.SetOpts k v ttl cond next) =
  next . fmap (R.Ok ==) <$> (R.setOpts k v . makeSetOpts ttl $ cond)

interpretKeyValueTxF (L.Get k next) =
  next <$> R.get k

interpretKeyValueTxF (L.Exists k next) =
  next <$> R.exists k

interpretKeyValueTxF (L.Del [] next) =
  pure . next . pure $ 0

interpretKeyValueTxF (L.Del ks next) =
  next <$> R.del ks

interpretKeyValueTxF (L.Expire k sec next) =
  next <$> R.expire k sec

interpretKeyValueTxF (L.Incr k next) =
  next <$> R.incr k

interpretKeyValueTxF (L.HSet k field value next) =
  next . fmap (/= 0) <$> R.hset k field value

interpretKeyValueTxF (L.HGet k field next) =
  next <$> R.hget k field

interpretKeyValueTxF (L.XLen stream next) =
  next <$> R.xlen stream

interpretKeyValueTxF (L.XGroupCreate stream groupName startId next) =
  next <$> R.xgroupCreate stream groupName startId

interpretKeyValueTxF (L.XDel stream entryIds next) =
  next <$> R.xdel stream ((\(L.KVDBStreamEntryID ms sq) -> show ms <> "-" <> show sq)  <$> entryIds)

interpretKeyValueTxF (L.XAdd stream entryId items next) =
  next . fmap parseStreamEntryId <$> R.xadd stream (makeStreamEntryId entryId) items
  where
    makeStreamEntryId (L.EntryID (L.KVDBStreamEntryID ms sq)) = show ms <> "-" <> show sq
    makeStreamEntryId L.AutoID = "*"

    parseStreamEntryId bs =
      -- "number-number" is redis entry id invariant
      let (ms, sq) = bimap (read . T.unpack) (read . T.unpack) .
                      T.breakOn "-" . TE.decodeUtf8With TE.lenientDecode $ bs
      in L.KVDBStreamEntryID ms sq

interpretKeyValueTxF (L.XRead stream entryId next) =
  next . fmap (fmap . fmap $ parseXReadResponse) <$> R.xread [(stream, entryId)]
  where
    parseXReadResponseRecord :: R.StreamsRecord -> L.KVDBStreamReadResponseRecord
    parseXReadResponseRecord record =
      L.KVDBStreamReadResponseRecord (R.recordId record) (R.keyValues record)
               
    parseXReadResponse :: R.XReadResponse -> L.KVDBStreamReadResponse
    parseXReadResponse (R.XReadResponse strm records) = L.KVDBStreamReadResponse strm (parseXReadResponseRecord <$> records)

interpretKeyValueTxF (L.XReadGroup groupName consumerName streamsAndIds opt next) =
  next . fmap (fmap . fmap $ parseXReadResponse) <$> R.xreadGroupOpts groupName consumerName streamsAndIds opt
  where
    parseXReadResponseRecord :: R.StreamsRecord -> L.KVDBStreamReadResponseRecord
    parseXReadResponseRecord record =
      L.KVDBStreamReadResponseRecord (R.recordId record) (R.keyValues record)
               
    parseXReadResponse :: R.XReadResponse -> L.KVDBStreamReadResponse
    parseXReadResponse (R.XReadResponse strm records) = L.KVDBStreamReadResponse strm (parseXReadResponseRecord <$> records)

interpretKeyValueTxF (L.XReadOpts strObjs readOpts next) =
  fmap next $ R.xreadOpts ((\(a, b) -> (a, makeStreamEntryId b)) <$> strObjs) readOpts
  where
    makeStreamEntryId (L.EntryID (L.KVDBStreamEntryID ms sq)) = show ms <> "-" <> show sq
    makeStreamEntryId L.AutoID = "*"

interpretKeyValueTxF (L.XRevRange stream send sstart count next) =
  next . fmap (fmap parseXReadResponseRecord) <$> R.xrevRange stream send sstart count
  where
    parseXReadResponseRecord :: R.StreamsRecord -> L.KVDBStreamReadResponseRecord
    parseXReadResponseRecord record =
      L.KVDBStreamReadResponseRecord (R.recordId record) (R.keyValues record)

interpretKeyValueTxF (L.SAdd k v next) =
  next <$> R.sadd k v

interpretKeyValueTxF (L.ZAdd k v next) =
  next <$> R.zadd k v

interpretKeyValueTxF (L.ZRange k startRank stopRank next) =
  next <$> R.zrange k startRank stopRank

interpretKeyValueTxF (L.ZRangeByScore k minScore maxScore next) =
  next <$> R.zrangebyscore k minScore maxScore

interpretKeyValueTxF (L.ZRangeByScoreWithLimit k minScore maxScore offset count next) =
  next <$> R.zrangebyscoreLimit k minScore maxScore offset count

interpretKeyValueTxF (L.ZRem k v next) =
  next <$> R.zrem k v

interpretKeyValueTxF (L.ZRemRangeByScore k minScore maxScore next) =
  next <$> R.zremrangebyscore k minScore maxScore

interpretKeyValueTxF (L.ZCard k next) =
  next <$> R.zcard k

interpretKeyValueTxF (L.SRem k v next) =
  next <$> R.srem k v

interpretKeyValueTxF (L.LRange k start stop next) =
  next <$> R.lrange k start stop

interpretKeyValueTxF (L.LPush k v next) =
  next <$> R.lpush k v

interpretKeyValueTxF (L.SMembers k next) =
  next <$> R.smembers k

interpretKeyValueTxF (L.SMove k1 k2 v next) =
  next <$> R.smove k1 k2 v

interpretKeyValueTxF (L.SMem k v next) =
  next <$> R.sismember k v

interpretKeyValueTxF (L.Raw args next) = next <$> R.sendRequest args

interpretKeyValueTxF (L.Ping next) = next <$> R.ping

interpretTransactionF
  :: (forall b. R.Redis (Either R.Reply b) -> IO (Either KVDBReply b))
  -> L.TransactionF a
  -> IO a
interpretTransactionF runRedis (L.MultiExec dsl next) =
  fmap next $
    runRedis $ fmap (Right . fromRdTxResult) $ R.multiExec $ foldF interpretKeyValueTxF dsl

interpretTransactionF runRedis (L.MultiExecWithHash _ dsl next) =
  fmap next $
    runRedis $ fmap (Right . fromRdTxResult) $ R.multiExec $ foldF interpretKeyValueTxF dsl


interpretDbF
  :: (forall b. R.Redis (Either R.Reply b) -> IO (Either KVDBReply b))
  -> L.KVDBF a
  -> IO a
interpretDbF runRedis (L.KV f) = interpretKeyValueF    runRedis f
interpretDbF runRedis (L.TX f) = interpretTransactionF runRedis f

runKVDB :: Text -> MVar (Map Text NativeKVDBConn) -> L.KVDB a -> IO (Either KVDBReply a)
runKVDB cName kvdbConnMapMVar =
  fmap (join . first exceptionToKVDBReply) . try @_ @SomeException .
    foldF (interpretDbF runRedis) . runExceptT
  where
    runRedis :: R.Redis (Either R.Reply a) -> IO (Either KVDBReply a)
    runRedis redisDsl = do
      connections <- readMVar kvdbConnMapMVar
      case Map.lookup cName connections of
        Nothing -> pure $ Left $ KVDBError KVDBConnectionDoesNotExist
          $ "Can't find redis connection: " <> T.unpack cName
        Just (NativeKVDB c) -> first hedisReplyToKVDBReply <$> R.runRedis c redisDsl

makeSetOpts :: L.KVDBSetTTLOption -> L.KVDBSetConditionOption -> R.SetOpts
makeSetOpts ttl cond =
  R.SetOpts
    { setSeconds =
        case ttl of
          L.Seconds s -> Just s
          _           -> Nothing
    , setMilliseconds =
        case ttl of
          L.Milliseconds ms -> Just ms
          _                 -> Nothing
    , setCondition =
        case cond of
          L.SetAlways     -> Nothing
          L.SetIfExist    -> Just R.Xx
          L.SetIfNotExist -> Just R.Nx
    }
