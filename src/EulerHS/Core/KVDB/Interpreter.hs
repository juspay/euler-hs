{- |
Module      :  EulerHS.Core.KVDB.Interpreter
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable

This module contains interpreter for KVDB language and used in KVDB runtime
Also this is to interpret the KVDB language and convert it to
redis commands and execute them
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
module EulerHS.Core.KVDB.Interpreter
  (
    -- * KVDB Interpreter
    runKVDB
  ) where

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Database.Redis as R
import qualified EulerHS.Core.KVDB.Language as L
import qualified EulerHS.Core.Types as D
import           EulerHS.Core.Types.KVDB
import           EulerHS.Prelude

interpretKeyValueF
  :: HasCallStack
  => (forall b . R.Redis (Either R.Reply b) -> IO (Either KVDBReply b))
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
      let [ms, sq] = read . T.unpack <$> T.splitOn "-" (TE.decodeUtf8With TE.lenientDecode bs)
      in L.KVDBStreamEntryID ms sq

interpretKeyValueF runRedis (L.XLen stream next) =
  fmap next $
    runRedis $ R.xlen stream

interpretKeyValueF runRedis (L.SAdd k v next) =
  fmap next $ runRedis $ R.sadd k v

interpretKeyValueF runRedis (L.SMem k v next) =
  fmap next $ runRedis $ R.sismember k v

interpretKeyValueF runRedis (L.Raw args next) = next <$> runRedis (R.sendRequest args)

interpretKeyValueTxF :: HasCallStack => L.KeyValueF R.Queued a -> R.RedisTx a
interpretKeyValueTxF (L.Set k v next) =
  next . fmap D.fromRdStatus <$> R.set k v

interpretKeyValueTxF (L.SetEx k e v next) =
  next . fmap D.fromRdStatus <$> R.setex k e v

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

interpretKeyValueTxF (L.XAdd stream entryId items next) =
  next . fmap parseStreamEntryId <$> R.xadd stream (makeStreamEntryId entryId) items
  where
    makeStreamEntryId (L.EntryID (L.KVDBStreamEntryID ms sq)) = show ms <> "-" <> show sq
    makeStreamEntryId L.AutoID = "*"

    parseStreamEntryId bs =
      let [ms, sq] = read . T.unpack <$> T.splitOn "-" (TE.decodeUtf8With TE.lenientDecode bs)
      in L.KVDBStreamEntryID ms sq

interpretKeyValueTxF (L.SAdd k v next) =
  next <$> R.sadd k v

interpretKeyValueTxF (L.SMem k v next) =
  next <$> R.sismember k v

interpretKeyValueTxF (L.Raw args next) = next <$> R.sendRequest args


interpretTransactionF
  :: HasCallStack
  => (forall b. R.Redis (Either R.Reply b) -> IO (Either KVDBReply b))
  -> L.TransactionF a
  -> IO a
interpretTransactionF runRedis (L.MultiExec dsl next) =
  fmap next $
    runRedis $ fmap (Right . fromRdTxResult) $ R.multiExec $ foldF interpretKeyValueTxF dsl

interpretTransactionF runRedis (L.MultiExecWithHash _ dsl next) =
  fmap next $
    runRedis $ fmap (Right . fromRdTxResult) $ R.multiExec $ foldF interpretKeyValueTxF dsl -- according to https://github.com/informatikr/hedis/commit/947b31a4696df81b267fe9a1a33050625374a208 


interpretDbF
  :: (forall b. R.Redis (Either R.Reply b) -> IO (Either KVDBReply b))
  -> L.KVDBF a
  -> IO a
interpretDbF runRedis (L.KV f) = interpretKeyValueF    runRedis f
interpretDbF runRedis (L.TX f) = interpretTransactionF runRedis f


runKVDB :: HasCallStack => Text -> MVar (Map Text NativeKVDBConn) -> L.KVDB a -> IO (Either KVDBReply a)
runKVDB cName kvdbConnMapMVar =
  fmap (join . first exceptionToKVDBReply) . try @_ @SomeException .
    foldF (interpretDbF runRedis) . runExceptT
  where
    runRedis :: R.Redis (Either R.Reply a) -> IO (Either KVDBReply a)
    runRedis redisDsl = do
      connections <- readMVar kvdbConnMapMVar
      case Map.lookup cName connections of
        Nothing   -> pure $ Left $ KVDBError KVDBConnectionDoesNotExist "Can't find redis connection"
        Just conn ->
          case conn of
            NativeKVDB c         -> first hedisReplyToKVDBReply <$> R.runRedis c redisDsl
            NativeKVDBMockedConn -> pure $ Right $
              error "Result of runRedis with mocked connection should not ever be evaluated"


makeSetOpts :: HasCallStack => L.KVDBSetTTLOption -> L.KVDBSetConditionOption -> R.SetOpts
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
