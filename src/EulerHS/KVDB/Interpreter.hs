{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EulerHS.KVDB.Interpreter
  (
    -- * KVDB Interpreter
    runKVDB
  ) where

import qualified Data.ByteString.Char8 as BS
import           Data.Either.Extra (mapRight)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Database.Redis as R
import qualified Data.Aeson as A
import EulerHS.ART.Types
import           Data.Time (getCurrentTime, utc, utcToZonedTime,
                            zonedTimeToLocalTime)
import           EulerHS.Runtime (FlowRuntime(..))
import           EulerHS.Common (FlowGUID)
import qualified EulerHS.KVDB.Language as L
import           EulerHS.KVDB.Types (KVDBError (KVDBConnectionDoesNotExist),
                                     KVDBReply, KVDBReplyF (Bulk, KVDBError),
                                     NativeKVDBConn (NativeKVDB),
                                     exceptionToKVDBReply, fromRdStatus,
                                     fromRdTxResult, hedisReplyToKVDBReply)
import           EulerHS.Compression (redisCompressHelper, redisDecompressHelper, redisDecompressHelperPure, RedisZstdDictConf(..))
import           EulerHS.Extra.Monitoring.Flow (getOptionLocalIO)
import           EulerHS.Prelude
import           Text.Read (read)
import qualified Data.ByteString.Lazy as BSL
import qualified EulerHS.Extra.KafkaClient.Utils as KUtils

interpretKeyValueF
  ::
    Maybe FlowGUID
  -> FlowRuntime
  -> (forall b . R.Redis (Either R.Reply b) -> IO (Either KVDBReply b))
  -> L.KeyValueF (Either KVDBReply) a
  -> IO a
interpretKeyValueF mbFlowGuid _flowRt runRedis (L.Set k v next) = do
  compressedV <- liftIO $ redisCompressHelper mbFlowGuid _flowRt v
  next . second fromRdStatus <$> runRedis (R.set k compressedV)

interpretKeyValueF mbFlowGuid _flowRt runRedis (L.SetEx k e v next) = do
  compressedV <- liftIO $ redisCompressHelper mbFlowGuid _flowRt v
  next . second fromRdStatus <$> runRedis (R.setex k e compressedV)

interpretKeyValueF mbFlowGuid _flowRt runRedis (L.SetOpts k v ttl cond next) = do
  compressedV <- liftIO $ redisCompressHelper mbFlowGuid _flowRt v
  fmap next $ do
    result <- runRedis $ R.setOpts k compressedV (makeSetOpts ttl cond)
    pure $ case result of
      Right _             -> Right True
      -- (nil) is ok, app should not fail
      Left (Bulk Nothing) -> Right False
      Left reply          -> Left reply

interpretKeyValueF mbFlowGuid flowRt runRedis (L.Get k next) = do
  res <- runRedis $ R.get k
  r <- case res of
    Right (Just v) -> mapRight Just <$> redisDecompressHelper mbFlowGuid flowRt v
    _ -> pure res
  fmap next $ pure r

interpretKeyValueF _mbFlowGuid _ runRedis (L.Exists k next) =
  fmap next $
    runRedis $ R.exists k

interpretKeyValueF _mbFlowGuid _ _ (L.Del [] next) =
  pure . next . pure $ 0

interpretKeyValueF _mbFlowGuid _ runRedis (L.Del ks next) =
  fmap next $
    runRedis $ R.del ks

interpretKeyValueF _mbFlowGuid _ runRedis (L.Expire k sec next) =
  fmap next $
    runRedis $ R.expire k sec

interpretKeyValueF _mbFlowGuid _ runRedis (L.ExpireAt k sec next) =
  fmap next $
    runRedis $ R.expireat k sec

interpretKeyValueF _mbFlowGuid _ runRedis (L.Incr k next) =
  fmap next $
    runRedis $ R.incr k

interpretKeyValueF _mbFlowGuid _ runRedis (L.IncrBy k val next) =
  fmap next $
    runRedis $ R.incrby k val

interpretKeyValueF _mbFlowGuid _ runRedis (L.IncrByFloat k val next) =
  fmap next $
    runRedis $ R.incrbyfloat k val

interpretKeyValueF _mbFlowGuid _ runRedis (L.Decr k next) =
  fmap next $
    runRedis $ R.decr k

interpretKeyValueF _mbFlowGuid _ runRedis (L.DecrBy k val next) =
  fmap next $
    runRedis $ R.decrby k val

interpretKeyValueF _mbFlowGuid _ runRedis (L.HSet k field value next) =
  fmap next $
    runRedis $ R.hset k field value

interpretKeyValueF _mbFlowGuid _ runRedis (L.HSetNx k field value next) =
  fmap next $
    runRedis $ R.hsetnx k field value

interpretKeyValueF _mbFlowGuid _ runRedis (L.HmSet k values next) =
  next . second fromRdStatus <$>
    runRedis (R.hmset k values)

interpretKeyValueF _mbFlowGuid _ runRedis (L.HGet k field next) =
  fmap next $
    runRedis $ R.hget k field

interpretKeyValueF _mbFlowGuid _ runRedis (L.HIncrBy k field value next) = 
  fmap next $
    runRedis $ R.hincrby k field value 

interpretKeyValueF _mbFlowGuid _ runRedis (L.HDel k fields next) = 
  fmap next $
    runRedis $ R.hdel k fields

interpretKeyValueF _mbFlowGuid _ runRedis (L.HLen k next) = 
  fmap next $
    runRedis $ R.hlen k

interpretKeyValueF _mbFlowGuid _ runRedis (L.HGetAll k next) = 
  fmap next $
    runRedis $ R.hgetall k

interpretKeyValueF _mbFlowGuid _ runRedis (L.XAdd stream entryId items next) =
  fmap next $
    runRedis $ do
      result <- R.xadd stream (makeStreamEntryId entryId) items
      pure $ parseKVDBStreamEntryID <$> result
  where
    makeStreamEntryId (L.EntryID (L.KVDBStreamEntryID ms sq)) = show ms <> "-" <> show sq
    makeStreamEntryId L.AutoID = "*"

interpretKeyValueF _mbFlowGuid _ runRedis (L.XRead stream entryId next) =
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

interpretKeyValueF _mbFlowGuid _ runRedis (L.XReadGroup groupName consumerName streamsAndIds opt next) =
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

interpretKeyValueF _mbFlowGuid _ runRedis (L.XReadOpts strObjs readOpts next) =
  fmap next $
    runRedis $ do
      result <- R.xreadOpts ((\(a, b) -> (a, makeStreamEntryId b)) <$> strObjs) readOpts
      pure result
  where
    makeStreamEntryId (L.EntryID (L.KVDBStreamEntryID ms sq)) = show ms <> "-" <> show sq
    makeStreamEntryId L.AutoID = "*"


interpretKeyValueF _mbFlowGuid _ runRedis (L.XGroupCreate stream groupName startId next) =
  fmap next $ runRedis $ R.xgroupCreate stream groupName startId

interpretKeyValueF _mbFlowGuid _ runRedis (L.XDel stream entryIds next) =
  fmap next $
    runRedis $ R.xdel stream ((\(L.KVDBStreamEntryID ms sq) -> show ms <> "-" <> show sq)  <$> entryIds)

interpretKeyValueF _mbFlowGuid _ runRedis (L.XRange stream sstart send count next) =
  fmap next $
    runRedis $ do
      result <- R.xrange stream sstart send count
      pure $ (fmap parseXReadResponseRecord) <$> result
  where
    parseXReadResponseRecord :: R.StreamsRecord -> L.KVDBStreamReadResponseRecord
    parseXReadResponseRecord record =
      L.KVDBStreamReadResponseRecord (R.recordId record) (R.keyValues record)

interpretKeyValueF _mbFlowGuid _ runRedis (L.XRevRange stream send sstart count next) =
  fmap next $
    runRedis $ do
      result <- R.xrevRange stream send sstart count
      pure $ (fmap parseXReadResponseRecord) <$> result
  where
    parseXReadResponseRecord :: R.StreamsRecord -> L.KVDBStreamReadResponseRecord
    parseXReadResponseRecord record =
      L.KVDBStreamReadResponseRecord (R.recordId record) (R.keyValues record)


interpretKeyValueF _mbFlowGuid _ runRedis (L.XLen stream next) =
  fmap next $
    runRedis $ R.xlen stream

interpretKeyValueF _mbFlowGuid _ runRedis (L.SAdd k v next) =
  fmap next $ runRedis $ R.sadd k v

interpretKeyValueF _mbFlowGuid _ runRedis (L.ZAdd k v next) =
  fmap next $ runRedis $ R.zadd k v

interpretKeyValueF _mbFlowGuid _ runRedis (L.ZRange k start stop next) =
  fmap next $ runRedis $ R.zrange k start stop

interpretKeyValueF _mbFlowGuid _ runRedis (L.ZRangeWithScores k start stop next) =
  fmap next $ runRedis $ R.zrangeWithscores k start stop

interpretKeyValueF _mbFlowGuid _ runRedis (L.ZRangeByScore k minScore maxScore next) =
  fmap next $ runRedis $  R.zrangebyscore k minScore maxScore

interpretKeyValueF _mbFlowGuid _ runRedis (L.ZRangeByScoreWithScore k minScore maxScore next) =
  fmap next $ runRedis $  R.zrangebyscoreWithscores k minScore maxScore

interpretKeyValueF _mbFlowGuid _ runRedis (L.ZRangeByScoreWithLimit k minScore maxScore offset count next) =
  fmap next $ runRedis $  R.zrangebyscoreLimit k minScore maxScore offset count

interpretKeyValueF _mbFlowGuid _ runRedis (L.ZRem k v next) =
  fmap next $ runRedis $ R.zrem k v

interpretKeyValueF _mbFlowGuid _ runRedis (L.ZRemRangeByScore k minScore maxScore next) =
  fmap next $ runRedis $ R.zremrangebyscore k minScore maxScore

interpretKeyValueF _mbFlowGuid _ runRedis (L.ZCard k next) =
  fmap next $ runRedis $ R.zcard k

interpretKeyValueF _mbFlowGuid _ runRedis (L.SRem k v next) =
  fmap next $ runRedis $ R.srem k v

interpretKeyValueF _mbFlowGuid _ runRedis (L.LPush k v next) =
  fmap next $ runRedis $ R.lpush k v

interpretKeyValueF _mbFlowGuid _ runRedis (L.LRange k start stop next) =
  fmap next $ runRedis $ R.lrange k start stop

interpretKeyValueF _mbFlowGuid _ runRedis (L.RPop k next) =
  fmap next $ runRedis $ R.rpop k

interpretKeyValueF _mbFlowGuid _ runRedis (L.LLen k next) =
  fmap next $ runRedis $ R.llen k

interpretKeyValueF _mbFlowGuid _ runRedis (L.SMembers k next) =
  fmap next $ runRedis $ R.smembers k

interpretKeyValueF _mbFlowGuid _ runRedis (L.SMove k1 k2 v next) =
  fmap next $ runRedis $ R.smove k1 k2 v

interpretKeyValueF _mbFlowGuid _ runRedis (L.SMem k v next) =
  fmap next $ runRedis $ R.sismember k v

interpretKeyValueF _mbFlowGuid _ runRedis (L.Raw args next) = next <$> runRedis (R.sendRequest args)

interpretKeyValueF _mbFlowGuid _ runRedis (L.Ping next) = fmap next $ runRedis $ R.ping

addToRecordingLocal :: FlowRuntime -> RecordingEntry -> R.RedisTx ()
addToRecordingLocal FlowRuntime {..} entry = do
    m <- takeMVar _recordingLocal
    putMVar _recordingLocal $ m <> [entry]

interpretKeyValueTxF :: Text -> Maybe FlowGUID -> Bool -> Bool -> (KUtils.ValueType -> ByteString -> IO()) -> FlowRuntime -> L.KeyValueF R.Queued a -> R.RedisTx a
interpretKeyValueTxF cName mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.Set k v next) = do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    when _shouldRecord $ do
      addToRecordingLocal _flowRt $ RunKVDBEntryT $ RSetBT $ RSetB k (toJSON v) (Left A.Null) recTimestamp cName
    compressedV <- liftIO $ redisCompressHelper mbFlowGuid _flowRt v
    _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RSetBT $ RSetB k (toJSON v) (Left A.Null) recTimestamp cName)
    next . fmap fromRdStatus <$> R.set k compressedV

interpretKeyValueTxF cName mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.SetEx k e v next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RSetexBT $ RSetexB (k) e (toJSON v) (Left A.Null) recTimestamp cName
  compressedV <- liftIO $ redisCompressHelper mbFlowGuid _flowRt v
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RSetexBT $ RSetexB (k) e (toJSON v) (Left A.Null) recTimestamp cName)
  next . fmap fromRdStatus <$> R.setex k e compressedV

interpretKeyValueTxF cName mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.SetOpts k v ttl cond next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RSetOptsBT $ RSetOptsB (k) (toJSON v) (toJSON ttl) (toJSON cond) (Left A.Null) recTimestamp cName
  compressedV <- liftIO $ redisCompressHelper mbFlowGuid _flowRt v
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RSetOptsBT $ RSetOptsB (k) (toJSON v) (toJSON ttl) (toJSON cond) (Left A.Null) recTimestamp cName)
  next . fmap (R.Ok ==) <$> (R.setOpts k compressedV . makeSetOpts ttl $ cond)

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.Get k next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RGetBT $ RGetB (k) (Nothing) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RGetBT $ RGetB (k) (Nothing) recTimestamp cName)
  (R.Queued f) <- R.get k
  mbDictMap <- liftIO $ getOptionLocalIO (_options _flowRt) RedisZstdDictConf
  pure $ next (R.Queued (\vr -> (cHelper mbDictMap) (f vr :: Either R.Reply (Maybe ByteString)) ))

  where
    cHelper mbDictMap r = case r of
        Right (Just v) -> bimap (R.Error . BS.pack) Just (redisDecompressHelperPure mbDictMap v)
        _ -> r

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.Exists k next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RExistsBT $ RExistsB (k) (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RExistsBT $ RExistsB (k) (Left A.Null) recTimestamp cName)
  next <$> R.exists k

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.Del [] next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RDelBT $ RDelB ([]) (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RDelBT $ RDelB ([]) (Left A.Null) recTimestamp cName)
  pure . next . pure $ 0

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.Del ks next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RDelBT $ RDelB ks (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RDelBT $ RDelB ks (Left A.Null) recTimestamp cName)
  next <$> R.del ks

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.Expire k sec next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RExpireBT $ RExpireB (k) (sec) (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RExpireBT $ RExpireB (k) (sec) (Left A.Null) recTimestamp cName)
  next <$> R.expire k sec

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.ExpireAt k sec next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RExpireAtBT $ RExpireAtB (k) (sec) (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RExpireAtBT $ RExpireAtB (k) (sec) (Left A.Null) recTimestamp cName)
  next <$> R.expireat k sec

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.Incr k next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RIncrBT $ RIncrB (k) (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RIncrBT $ RIncrB (k) (Left A.Null) recTimestamp cName)
  next <$> R.incr k

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.IncrBy k val next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RIncrByBT $ RIncrByB (k) (val) (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RIncrByBT $ RIncrByB (k) (val) (Left A.Null) recTimestamp cName)
  next <$> R.incrby k val

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.IncrByFloat k val next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RIncrByFloatBT $ RIncrByFloatB (k) (val) (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RIncrByFloatBT $ RIncrByFloatB (k) (val) (Left A.Null) recTimestamp cName)
  next <$> R.incrbyfloat k val

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.Decr k next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RDecrBT $ RDecrB (k) (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RDecrBT $ RDecrB (k) (Left A.Null) recTimestamp cName)
  next <$> R.decr k

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.DecrBy k val next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RDecrByBT $ RDecrByB (k) (val) (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RDecrByBT $ RDecrByB (k) (val) (Left A.Null) recTimestamp cName)
  next <$> R.decrby k val

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.HSet k field value next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RHSetBT $ RHSetB (k) (toJSON field) (toJSON value) (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RHSetBT $ RHSetB (k) (toJSON field) (toJSON value) (Left A.Null) recTimestamp cName)
  next <$> R.hset k field value

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO  _flowRt@FlowRuntime {} (L.HSetNx k field value next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RHSetNxBT $ RHSetNxB (k) (toJSON field) (toJSON value) (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RHSetNxBT $ RHSetNxB (k) (toJSON field) (toJSON value) (Left A.Null) recTimestamp cName)
  next <$> R.hsetnx k field value

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.HmSet k values next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RHmSetBT $ RHmSetB (k) (values) (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RHmSetBT $ RHmSetB (k) values (Left A.Null) recTimestamp cName)
  next . fmap fromRdStatus <$> R.hmset k values

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.HGet k field next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RHGetT $ RHGet (k) (toJSON field) (Nothing) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RHGetT $ RHGet (k) (toJSON field) (Nothing) recTimestamp cName)
  next <$> R.hget k field

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.HIncrBy k field value next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RHIncrByBT $ RHIncrByB (k) (toJSON field) value (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RHIncrByBT $ RHIncrByB (k) (toJSON field) value (Left A.Null) recTimestamp cName)
  next <$> R.hincrby k field value

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.HGetAll k next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RHGetAllT $ RHGetAll (k) (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RHGetAllT $ RHGetAll (k) (Left A.Null) recTimestamp cName)
  next <$> R.hgetall k

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.HDel k fields next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RHDelBT $ RHDel (k) (fields) (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RHDelBT $ RHDel (k) (fields) (Left A.Null) recTimestamp cName)
  next <$> R.hdel k fields

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.HLen k next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS . BSL.toStrict . A.encode $ RunKVDBEntryT $ RHLenBT $ (RHLenB k (Left A.Null) recTimestamp cName)
  next <$> R.hlen k

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.XLen stream next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS . BSL.toStrict . A.encode $ RunKVDBEntryT $ RXLenBT $ (RXLenB stream (Left A.Null) recTimestamp cName)
  next <$> R.xlen stream

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.XGroupCreate stream groupName startId next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS . BSL.toStrict . A.encode $ RunKVDBEntryT $ RXGroupCreateBT $ RXGroupCreateB stream (toJSON groupName) (toJSON startId) (Left A.Null) recTimestamp cName
  next <$> R.xgroupCreate stream groupName startId

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.XDel stream entryIds next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS . BSL.toStrict . A.encode $ RunKVDBEntryT $ RXDelBT $ RXDelB stream (toJSON entryIds) (Left A.Null) recTimestamp cName
  next <$> R.xdel stream ((\(L.KVDBStreamEntryID ms sq) -> show ms <> "-" <> show sq)  <$> entryIds)

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.XAdd stream entryId items next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RXaddBT $ RXaddB (stream) (makeStreamEntryId entryId) (toJSON items) (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RXaddBT $ RXaddB (stream) (makeStreamEntryId entryId) (toJSON items) (Left A.Null) recTimestamp cName)
  next . fmap parseKVDBStreamEntryID <$> R.xadd stream (makeStreamEntryId entryId) items
  where
    makeStreamEntryId (L.EntryID (L.KVDBStreamEntryID ms sq)) = show ms <> "-" <> show sq
    makeStreamEntryId L.AutoID = "*"

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.XRead stream entryId next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RXreadBT $ RXreadB (stream) (entryId) (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RXreadBT $ RXreadB (stream) (entryId) (Left A.Null) recTimestamp cName)
  next . fmap (fmap . fmap $ parseXReadResponse) <$> R.xread [(stream, entryId)]
  where
    parseXReadResponseRecord :: R.StreamsRecord -> L.KVDBStreamReadResponseRecord
    parseXReadResponseRecord record =
      L.KVDBStreamReadResponseRecord (R.recordId record) (R.keyValues record)

    parseXReadResponse :: R.XReadResponse -> L.KVDBStreamReadResponse
    parseXReadResponse (R.XReadResponse strm records) = L.KVDBStreamReadResponse strm (parseXReadResponseRecord <$> records)

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.XReadGroup groupName consumerName streamsAndIds opt next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RXReadGroupBT $ RXReadGroupB (toJSON groupName) (toJSON consumerName) streamsAndIds (toJSON opt) (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RXReadGroupBT $ RXReadGroupB (toJSON groupName) (toJSON consumerName) streamsAndIds (toJSON opt) (Left A.Null) recTimestamp cName)
  next . fmap (fmap . fmap $ parseXReadResponse) <$> R.xreadGroupOpts groupName consumerName streamsAndIds opt
  where
    parseXReadResponseRecord :: R.StreamsRecord -> L.KVDBStreamReadResponseRecord
    parseXReadResponseRecord record =
      L.KVDBStreamReadResponseRecord (R.recordId record) (R.keyValues record)

    parseXReadResponse :: R.XReadResponse -> L.KVDBStreamReadResponse
    parseXReadResponse (R.XReadResponse strm records) = L.KVDBStreamReadResponse strm (parseXReadResponseRecord <$> records)

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.XReadOpts strObjs readOpts next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS . BSL.toStrict . A.encode $ RunKVDBEntryT $ RXReadOptsBT $ RXReadOptsB (toJSON strObjs) (toJSON readOpts) (Left A.Null) recTimestamp cName
  fmap next $ R.xreadOpts ((\(a, b) -> (a, makeStreamEntryId b)) <$> strObjs) readOpts
  where
    makeStreamEntryId (L.EntryID (L.KVDBStreamEntryID ms sq)) = show ms <> "-" <> show sq
    makeStreamEntryId L.AutoID = "*"

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.XRange stream sstart send count next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RXRangeBT $ RXRangeB (stream) (toJSON sstart) (toJSON send) (count) (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RXRangeBT $ RXRangeB (stream) (toJSON sstart) (toJSON send) (count) (Left A.Null) recTimestamp cName)
  next . fmap (fmap parseXReadResponseRecord) <$> R.xrange stream sstart send count
  where
    parseXReadResponseRecord :: R.StreamsRecord -> L.KVDBStreamReadResponseRecord
    parseXReadResponseRecord record =
      L.KVDBStreamReadResponseRecord (R.recordId record) (R.keyValues record)

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.XRevRange stream send sstart count next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RXRevRangeBT $ RXRevRangeB (stream) (toJSON send) (toJSON sstart) (count) (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RXRevRangeBT $ RXRevRangeB (stream) (toJSON send) (toJSON sstart) (count) (Left A.Null) recTimestamp cName)
  next . fmap (fmap parseXReadResponseRecord) <$> R.xrevRange stream send sstart count
  where
    parseXReadResponseRecord :: R.StreamsRecord -> L.KVDBStreamReadResponseRecord
    parseXReadResponseRecord record =
      L.KVDBStreamReadResponseRecord (R.recordId record) (R.keyValues record)

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.SAdd k v next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RSAddBT $ RSAddB (k) (v) (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RSAddBT $ RSAddB (k) (v) (Left A.Null) recTimestamp cName)
  next <$> R.sadd k v

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.ZAdd k v next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RZAddBT $ RZAddB k v (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RZAddBT $ RZAddB k v (Left A.Null) recTimestamp cName)
  next <$> R.zadd k v

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.ZRange k startRank stopRank next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RZRangeBT $ RZRangeB k startRank stopRank (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RZRangeBT $ RZRangeB k startRank stopRank (Left A.Null) recTimestamp cName)
  next <$> R.zrange k startRank stopRank

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.ZRangeWithScores k startRank stopRank next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RZRangeWithScoresBT $ RZRangeWithScoresB (k) startRank stopRank (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RZRangeWithScoresBT $ RZRangeWithScoresB (k) startRank stopRank (Left A.Null) recTimestamp cName)
  next <$> R.zrangeWithscores k startRank stopRank

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.ZRangeByScoreWithScore k minScore maxScore next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RZRangeByScoreWithScoreBT $ RZRangeByScoreWithScoreB (k) minScore maxScore (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RZRangeByScoreWithScoreBT $ RZRangeByScoreWithScoreB (k) minScore maxScore (Left A.Null) recTimestamp cName)
  next <$> R.zrangebyscoreWithscores k minScore maxScore

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.ZRangeByScore k minScore maxScore next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RZRangeByScoreT $ RZRangeByScore k minScore maxScore (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RZRangeByScoreT $ RZRangeByScore k minScore maxScore (Left A.Null) recTimestamp cName)
  next <$> R.zrangebyscore k minScore maxScore

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.ZRangeByScoreWithLimit k minScore maxScore offset count next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RZRangeByScoreWithLimitT $ RZRangeByScoreWithLimit k minScore maxScore offset count (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RZRangeByScoreWithLimitT $ RZRangeByScoreWithLimit k minScore maxScore offset count (Left A.Null) recTimestamp cName)
  next <$> R.zrangebyscoreLimit k minScore maxScore offset count

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.ZRem k v next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RZRemT $ RZRem k v (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RZRemT $ RZRem k v (Left A.Null) recTimestamp cName)
  next <$> R.zrem k v

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.ZRemRangeByScore k minScore maxScore next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RZRemRangeByScoreT $ RZRemRangeByScore k minScore maxScore  (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RZRemRangeByScoreT $ RZRemRangeByScore k minScore maxScore  (Left A.Null) recTimestamp cName)
  next <$> R.zremrangebyscore k minScore maxScore

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.ZCard k next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RZCardT $ RZCard k (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RZCardT $ RZCard k (Left A.Null) recTimestamp cName)
  next <$> R.zcard k

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.SRem k v next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RSRemBT $ RSRemB k v (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RSRemBT $ RSRemB k v (Left A.Null) recTimestamp cName)
  next <$> R.srem k v

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.LRange k start stop next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RLRangeBT $ RLRangeB k start stop (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RLRangeBT $ RLRangeB k start stop (Left A.Null) recTimestamp cName)
  next <$> R.lrange k start stop

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.LPush k v next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RLPushBT $ RLPushB k v (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RLPushBT $ RLPushB k v (Left A.Null) recTimestamp cName)
  next <$> R.lpush k v

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.RPop k next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RRPopBT $ RRPopB k (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RRPopBT $ RRPopB k (Left A.Null) recTimestamp cName)
  next <$> R.rpop k

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.LLen k next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RLLenBT $ RLLenB k (Left A.Null) recTimestamp cName)
  next <$> R.llen k

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.SMembers k next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RSmembersBT $ RSmembersB k (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RSmembersBT $ RSmembersB k (Left A.Null) recTimestamp cName)
  next <$> R.smembers k

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.SMove k1 k2 v next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RSMoveBT $ RSMoveB k1 k2 v (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RSMoveBT $ RSMoveB k1 k2 v (Left A.Null) recTimestamp cName)
  next <$> R.smove k1 k2 v

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.SMem k v next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RSMemBT $ RSMemB k v (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RSMemBT $ RSMemB k v (Left A.Null) recTimestamp cName)
  next <$> R.sismember k v

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.Raw args next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RRawBT $ RRawB args (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RRawBT $ RRawB args (Left A.Null) recTimestamp cName)
  next <$> R.sendRequest args

interpretKeyValueTxF cName _mbFlowGuid _shouldRecord _shouldRecordV2 producePayloadIO _flowRt@FlowRuntime {} (L.Ping next) = do
  recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
  when _shouldRecord $ do
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RPingBT $ RPingB (Left A.Null) recTimestamp cName
  _ <- when _shouldRecordV2 $ liftIO $ producePayloadIO KUtils.REDIS (BSL.toStrict $ A.encode $ RunKVDBEntryT $ RPingBT $ RPingB (Left A.Null) recTimestamp cName)
  next <$> R.ping

interpretTransactionF
  :: Text
  -> Maybe FlowGUID
  -> Bool
  -> Bool
  -> (KUtils.ValueType -> ByteString -> IO())
  -> FlowRuntime
  -> (forall b. R.Redis (Either R.Reply b) -> IO (Either KVDBReply b))
  -> L.TransactionF a
  -> IO a
interpretTransactionF cName mbFlowGuid shouldRecord shouldRecordV2 producePayloadIO flowRt@FlowRuntime{} runRedis (L.MultiExec dsl next) = do
  fmap next $
    runRedis $ fmap (Right . fromRdTxResult) $ R.multiExec $ foldF (interpretKeyValueTxF cName mbFlowGuid shouldRecord shouldRecordV2 producePayloadIO flowRt) dsl

interpretTransactionF cName mbFlowGuid shouldRecord shouldRecordV2 producePayloadIO flowRt@FlowRuntime{} runRedis (L.MultiExecWithHash _ dsl next) = do
  fmap next $
    runRedis $ fmap (Right . fromRdTxResult) $ R.multiExec $ foldF (interpretKeyValueTxF cName mbFlowGuid shouldRecord shouldRecordV2 producePayloadIO flowRt) dsl

interpretDbF
  :: Text -> Maybe FlowGUID -> Bool -> Bool
  -> (KUtils.ValueType -> ByteString -> IO())
  -> FlowRuntime
  -> (forall b. R.Redis (Either R.Reply b) -> IO (Either KVDBReply b))
  -> L.KVDBF a
  -> IO a
interpretDbF _ mbFlowGuid _ _ _ flowRt runRedis (L.KV f) = interpretKeyValueF mbFlowGuid flowRt runRedis f
interpretDbF cName mbFlowGuid shouldRecord shouldRecordV2 producePayloadIO flowRt runRedis (L.TX f) = interpretTransactionF cName mbFlowGuid shouldRecord shouldRecordV2 producePayloadIO flowRt runRedis f

runKVDB :: Maybe FlowGUID -> Bool -> Bool -> (KUtils.ValueType -> ByteString -> IO()) -> FlowRuntime -> Text -> MVar (Map Text NativeKVDBConn) -> L.KVDB a -> IO (Either KVDBReply a)
runKVDB mbFlowGuid shouldRecord shouldRecordV2 producePayloadIO flowRt cName kvdbConnMapMVar =
  fmap (join . first exceptionToKVDBReply) . try @_ @SomeException .
    foldF (interpretDbF cName mbFlowGuid shouldRecord shouldRecordV2 producePayloadIO flowRt runRedis) . runExceptT
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


parseKVDBStreamEntryID :: ByteString -> L.KVDBStreamEntryID
parseKVDBStreamEntryID rawID = case T.splitOn "-" (TE.decodeUtf8With TE.lenientDecode rawID) of
  [e1, e2] -> L.KVDBStreamEntryID (read $ T.unpack e1) (read $ T.unpack e2)
  res -> error ("Expected list with length 2, but found " <> (decodeUtf8 $ A.encode res) <> " in parseKVDBStreamEntryID")