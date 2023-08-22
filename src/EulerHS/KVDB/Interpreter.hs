{- |
Module      :  EulerHS.KVDB.Interpreter
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import qualified Data.Aeson as A
import EulerHS.ART.Types
import EulerHS.ART.EnvVars 
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
  fmap next $
    runRedis $ R.hset k field value

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

addToRecordingLocal :: FlowRuntime -> RecordingEntry -> R.RedisTx ()
addToRecordingLocal FlowRuntime {..} entry = do
    m <- takeMVar _recordingLocal
    putMVar _recordingLocal $ m <> [entry]

interpretKeyValueTxF :: Text -> Maybe FlowGUID -> FlowRuntime -> L.KeyValueF R.Queued a -> R.RedisTx a
interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.Set k v next) = do
    when isArtRecEnabled $ do
      recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
      addToRecordingLocal _flowRt $ RunKVDBEntryT $ RSetBT $ RSetB (k) (toJSON v) (Left A.Null) recTimestamp cName
    next . fmap fromRdStatus <$> R.set k v

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.SetEx k e v next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RSetexBT $ RSetexB (k) e (toJSON v) (Left A.Null) recTimestamp cName
  next . fmap fromRdStatus <$> R.setex k e v

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.SetOpts k v ttl cond next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RSetOptsBT $ RSetOptsB (k) (toJSON v) (toJSON ttl) (toJSON cond) (Left A.Null) recTimestamp cName
  next . fmap (R.Ok ==) <$> (R.setOpts k v . makeSetOpts ttl $ cond)

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.Get k next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RGetBT $ RGetB (k) (Nothing) recTimestamp cName
  next <$> R.get k

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.Exists k next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RExistsBT $ RExistsB (k) (Left A.Null) recTimestamp cName
  next <$> R.exists k

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.Del [] next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RDelBT $ RDelB ([]) (Left A.Null) recTimestamp cName
  pure . next . pure $ 0

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.Del ks next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RDelBT $ RDelB ks (Left A.Null) recTimestamp cName
  next <$> R.del ks

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.Expire k sec next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RExpireBT $ RExpireB (k) (sec) (Left A.Null) recTimestamp cName
  next <$> R.expire k sec

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.Incr k next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RIncrBT $ RIncrB (k) (Left A.Null) recTimestamp cName
  next <$> R.incr k

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.HSet k field value next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RHSetBT $ RHSetB (k) (toJSON field) (toJSON value) (Left A.Null) recTimestamp cName
  next <$> R.hset k field value

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.HGet k field next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RHGetT $ RHGet (k) (toJSON field) (Nothing) recTimestamp cName
  next <$> R.hget k field

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.XLen stream next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RXLenBT $ RXLenB (stream) (Left A.Null) recTimestamp cName
  next <$> R.xlen stream

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.XGroupCreate stream groupName startId next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RXGroupCreateBT $ RXGroupCreateB (stream) (toJSON groupName) (toJSON startId) (Left A.Null) recTimestamp cName
  next <$> R.xgroupCreate stream groupName startId

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.XDel stream entryIds next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RXDelBT $ RXDelB (stream) (toJSON entryIds) (Left A.Null) recTimestamp cName
  next <$> R.xdel stream ((\(L.KVDBStreamEntryID ms sq) -> show ms <> "-" <> show sq)  <$> entryIds)

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.XAdd stream entryId items next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RXaddBT $ RXaddB (stream) (makeStreamEntryId entryId) (toJSON items) (Left A.Null) recTimestamp cName
  next . fmap parseStreamEntryId <$> R.xadd stream (makeStreamEntryId entryId) items
  where
    makeStreamEntryId (L.EntryID (L.KVDBStreamEntryID ms sq)) = show ms <> "-" <> show sq
    makeStreamEntryId L.AutoID = "*"

    parseStreamEntryId bs =
      -- "number-number" is redis entry id invariant
      let (ms, sq) = bimap (read . T.unpack) (read . T.unpack) .
                      T.breakOn "-" . TE.decodeUtf8With TE.lenientDecode $ bs
      in L.KVDBStreamEntryID ms sq

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.XRead stream entryId next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RXreadBT $ RXreadB (stream) (entryId) (Left A.Null) recTimestamp cName
  next . fmap (fmap . fmap $ parseXReadResponse) <$> R.xread [(stream, entryId)]
  where
    parseXReadResponseRecord :: R.StreamsRecord -> L.KVDBStreamReadResponseRecord
    parseXReadResponseRecord record =
      L.KVDBStreamReadResponseRecord (R.recordId record) (R.keyValues record)
               
    parseXReadResponse :: R.XReadResponse -> L.KVDBStreamReadResponse
    parseXReadResponse (R.XReadResponse strm records) = L.KVDBStreamReadResponse strm (parseXReadResponseRecord <$> records)

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.XReadGroup groupName consumerName streamsAndIds opt next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RXReadGroupBT $ RXReadGroupB (toJSON groupName) (toJSON consumerName) streamsAndIds (toJSON opt) (Left A.Null) recTimestamp cName
  next . fmap (fmap . fmap $ parseXReadResponse) <$> R.xreadGroupOpts groupName consumerName streamsAndIds opt
  where
    parseXReadResponseRecord :: R.StreamsRecord -> L.KVDBStreamReadResponseRecord
    parseXReadResponseRecord record =
      L.KVDBStreamReadResponseRecord (R.recordId record) (R.keyValues record)
               
    parseXReadResponse :: R.XReadResponse -> L.KVDBStreamReadResponse
    parseXReadResponse (R.XReadResponse strm records) = L.KVDBStreamReadResponse strm (parseXReadResponseRecord <$> records)

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.XReadOpts strObjs readOpts next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RXReadOptsBT $ RXReadOptsB (toJSON strObjs) (toJSON readOpts) (Left A.Null) recTimestamp cName
  fmap next $ R.xreadOpts ((\(a, b) -> (a, makeStreamEntryId b)) <$> strObjs) readOpts
  where
    makeStreamEntryId (L.EntryID (L.KVDBStreamEntryID ms sq)) = show ms <> "-" <> show sq
    makeStreamEntryId L.AutoID = "*"

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.XRevRange stream send sstart count next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RXRevRangeBT $ RXRevRangeB (stream) (toJSON send) (toJSON sstart) (count) (Left A.Null) recTimestamp cName
  next . fmap (fmap parseXReadResponseRecord) <$> R.xrevRange stream send sstart count
  where
    parseXReadResponseRecord :: R.StreamsRecord -> L.KVDBStreamReadResponseRecord
    parseXReadResponseRecord record =
      L.KVDBStreamReadResponseRecord (R.recordId record) (R.keyValues record)

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.SAdd k v next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RSAddBT $ RSAddB (k) (v) (Left A.Null) recTimestamp cName
  next <$> R.sadd k v

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.ZAdd k v next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RZAddBT $ RZAddB (k) (v) (Left A.Null) recTimestamp cName
  next <$> R.zadd k v

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.ZRange k startRank stopRank next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RZRangeBT $ RZRangeB (k) startRank stopRank (Left A.Null) recTimestamp cName
  next <$> R.zrange k startRank stopRank

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.ZRangeByScore k minScore maxScore next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RZRangeByScoreT $ RZRangeByScore k minScore maxScore (Left A.Null) recTimestamp cName
  next <$> R.zrangebyscore k minScore maxScore

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.ZRangeByScoreWithLimit k minScore maxScore offset count next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RZRangeByScoreWithLimitT $ RZRangeByScoreWithLimit k minScore maxScore offset count (Left A.Null) recTimestamp cName
  next <$> R.zrangebyscoreLimit k minScore maxScore offset count

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.ZRem k v next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RZRemT $ RZRem k v (Left A.Null) recTimestamp cName
  next <$> R.zrem k v

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.ZRemRangeByScore k minScore maxScore next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RZRemRangeByScoreT $ RZRemRangeByScore k minScore maxScore  (Left A.Null) recTimestamp cName
  next <$> R.zremrangebyscore k minScore maxScore

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.ZCard k next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RZCardT $ RZCard k (Left A.Null) recTimestamp cName
  next <$> R.zcard k

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.SRem k v next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RSRemBT $ RSRemB k v (Left A.Null) recTimestamp cName
  next <$> R.srem k v

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.LRange k start stop next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RLRangeBT $ RLRangeB k start stop (Left A.Null) recTimestamp cName
  next <$> R.lrange k start stop

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.LPush k v next) = do 
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RLPushBT $ RLPushB k v (Left A.Null) recTimestamp cName
  next <$> R.lpush k v

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.SMembers k next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RSmembersBT $ RSmembersB k (Left A.Null) recTimestamp cName
  next <$> R.smembers k

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.SMove k1 k2 v next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RSMoveBT $ RSMoveB k1 k2 v (Left A.Null) recTimestamp cName
  next <$> R.smove k1 k2 v

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.SMem k v next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RSMemBT $ RSMemB k v (Left A.Null) recTimestamp cName
  next <$> R.sismember k v

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.Raw args next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RRawBT $ RRawB args (Left A.Null) recTimestamp cName
  next <$> R.sendRequest args

interpretKeyValueTxF cName _mbFlowGuid _flowRt@FlowRuntime {} (L.Ping next) = do
  when isArtRecEnabled $ do
    recTimestamp <- liftIO $ zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime
    addToRecordingLocal _flowRt $ RunKVDBEntryT $ RPingBT $ RPingB (Left A.Null) recTimestamp cName
  next <$> R.ping

interpretTransactionF
  :: Text 
  -> Maybe FlowGUID 
  -> FlowRuntime 
  -> (forall b. R.Redis (Either R.Reply b) -> IO (Either KVDBReply b))
  -> L.TransactionF a
  -> IO a
interpretTransactionF cName mbFlowGuid flowRt@FlowRuntime{} runRedis (L.MultiExec dsl next) = do
  fmap next $
    runRedis $ fmap (Right . fromRdTxResult) $ R.multiExec $ foldF (interpretKeyValueTxF cName mbFlowGuid flowRt) dsl

interpretTransactionF cName mbFlowGuid flowRt@FlowRuntime{} runRedis (L.MultiExecWithHash _ dsl next) = do
  fmap next $
    runRedis $ fmap (Right . fromRdTxResult) $ R.multiExec $ foldF (interpretKeyValueTxF cName mbFlowGuid flowRt) dsl


interpretDbF
  :: Text -> Maybe FlowGUID 
  -> FlowRuntime 
  -> (forall b. R.Redis (Either R.Reply b) -> IO (Either KVDBReply b))
  -> L.KVDBF a
  -> IO a
interpretDbF _ _ _ runRedis (L.KV f) = interpretKeyValueF    runRedis f
interpretDbF cName mbFlowGuid flowRt runRedis (L.TX f) = interpretTransactionF cName mbFlowGuid flowRt runRedis f

runKVDB :: Maybe FlowGUID -> FlowRuntime -> Text -> MVar (Map Text NativeKVDBConn) -> L.KVDB a -> IO (Either KVDBReply a)
runKVDB mbFlowGuid flowRt cName kvdbConnMapMVar =
  fmap (join . first exceptionToKVDBReply) . try @_ @SomeException .
    foldF (interpretDbF cName mbFlowGuid flowRt runRedis) . runExceptT
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
