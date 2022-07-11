module EulerHS.Extra.Language
  ( getOrInitSqlConn
  , getOrInitKVDBConn
  , rExpire
  , rExpireB
  , rDel
  , rDelB
  , rExists
  , rExistsB
  , rExistsT -- alias for rExists (back compat)
  , rHget
  , rHgetB
  , rHset
  , rHsetB
  , rIncr
  , rIncrB
  , rSet
  , rSetT  -- alias for rSet (back compat)
  , rSetB
  , rGet
  , rGetB
  , rGetT  -- alias for rGet (back compat)
  , rSetex
  , rSetexB
  , rSetexT  -- alias for rSetex (back compat)
  , rSetOpts
  , rSetOptsB
  , rSetOptsT
  , keyToSlot
  , rSadd
  , rSismember
  , updateLoggerContext
  , withLoggerContext
  ) where

import           EulerHS.Prelude hiding (get, id)

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as TE
import           Database.Redis (keyToSlot)
import qualified EulerHS.Core.KVDB.Language as L
import qualified EulerHS.Core.Types as T
import qualified EulerHS.Framework.Language as L
import           EulerHS.Runtime (CoreRuntime (..), FlowRuntime (..),
                                  LoggerRuntime (..))

type RedisName = Text
type TextKey = Text
type TextField = Text
type ByteKey = ByteString
type ByteField = ByteString
type ByteValue = ByteString

-- | Get existing SQL connection, or init a new connection.
getOrInitSqlConn :: (HasCallStack, L.MonadFlow m) =>
  T.DBConfig beM -> m (T.DBResult (T.SqlConn beM))
getOrInitSqlConn cfg = do
  eConn <- L.getSqlDBConnection cfg
  case eConn of
    Left (T.DBError T.ConnectionDoesNotExist _) -> L.initSqlDBConnection cfg
    res                                         -> pure res

-- | Get existing Redis connection, or init a new connection.
getOrInitKVDBConn :: (HasCallStack, L.MonadFlow m) => T.KVDBConfig -> m (T.KVDBAnswer T.KVDBConn)
getOrInitKVDBConn cfg = do
  conn <- L.getKVDBConnection cfg
  case conn of
    Left (T.KVDBError T.KVDBConnectionDoesNotExist _) -> L.initKVDBConnection cfg
    res -> pure res

-- KVDB convenient functions

-- ----------------------------------------------------------------------------

rExpire :: (HasCallStack, Integral t, L.MonadFlow m) =>
  RedisName -> TextKey -> t -> m (Either T.KVDBReply Bool)
rExpire cName k t = rExpireB cName (TE.encodeUtf8 k) t

rExpireB :: (HasCallStack, Integral t, L.MonadFlow m) =>
  RedisName -> ByteKey -> t -> m (Either T.KVDBReply Bool)
rExpireB cName k t = do
  res <- L.runKVDB cName $ L.expire k $ toInteger t
  case res of
    Right _ -> do
      -- L.logInfo @Text "Redis expire" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis expire" $ show err
      pure res

-- ----------------------------------------------------------------------------

rDel :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> [TextKey] -> m (Either T.KVDBReply Integer)
rDel cName ks = rDelB cName (TE.encodeUtf8 <$> ks)

rDelB :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> [ByteKey] -> m (Either T.KVDBReply Integer)
rDelB cName ks = do
  res <- L.runKVDB cName $ L.del ks
  case res of
    Right _ -> do
      -- L.logInfo @Text "Redis del" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis del" $ show err
      pure res

-- ----------------------------------------------------------------------------

rExists :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> TextKey -> m (Either T.KVDBReply Bool)
rExists cName k = rExistsB cName $ TE.encodeUtf8 k

rExistsB :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> ByteKey -> m (Either T.KVDBReply Bool)
rExistsB cName k = do
  res <- L.runKVDB cName $ L.exists k
  case res of
    Right _ -> do
      -- L.logInfo @Text "Redis exists" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis exists" $ show err
      pure res

rExistsT :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> TextKey -> m (Either T.KVDBReply Bool)
rExistsT = rExists

-- ----------------------------------------------------------------------------

rHget :: (HasCallStack, FromJSON v, L.MonadFlow m)
  => RedisName -> TextKey -> TextField -> m (Maybe v)
rHget cName k f = do
  let k' = TE.encodeUtf8 k
  let f' = TE.encodeUtf8 f
  r <- L.runKVDB cName $ L.hget k' f'
  case r of
    Right (Just val) -> do
      let v = A.eitherDecode $ BSL.fromStrict val
      case v of
        Left err -> do
          L.logError @Text "Decoding error: " $ show err
          pure Nothing
        Right v' -> do
          -- L.logDebug @Text "Decoded value" $ show v'
          pure $ Just v'
    Right Nothing -> pure Nothing
    Left err -> do
      L.logError @Text "Redis rHget" $ show err
      pure Nothing

rHgetB :: (HasCallStack, L.MonadFlow m) =>
  Text -> ByteKey -> ByteField -> m (Maybe ByteValue)
rHgetB cName k f = do
  res <- L.runKVDB cName $ L.hget k f
  case res of
    Right (Just val) -> pure $ Just val
    Right Nothing -> pure Nothing
    Left err -> do
      L.logError @Text "Redis hget" $ show err
      pure Nothing

-- ----------------------------------------------------------------------------

rHset :: (HasCallStack, ToJSON v, L.MonadFlow m)
  => RedisName -> TextKey -> TextField -> v -> m (Either T.KVDBReply Bool)
rHset cName k f v = rHsetB cName k' f' v'
  where
    k' = TE.encodeUtf8 k
    f' = TE.encodeUtf8 f
    v' = BSL.toStrict $ A.encode v

rHsetB :: (HasCallStack, L.MonadFlow m)
  => RedisName -> ByteKey -> ByteField -> ByteValue -> m (Either T.KVDBReply Bool)
rHsetB cName k f v = do
  res <- L.runKVDB cName $
    L.hset k f v
  case res of
    Right _ -> do
      -- L.logInfo @Text "Redis hset" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis hset" $ show err
      pure res

-- ----------------------------------------------------------------------------

rIncr :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> TextKey -> m (Either T.KVDBReply Integer)
rIncr cName k = rIncrB cName (TE.encodeUtf8 k)

rIncrB :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> ByteKey -> m (Either T.KVDBReply Integer)
rIncrB cName k = do
  res <- L.runKVDB cName $ L.incr k
  case res of
    Right _ -> do
      -- L.logInfo @Text "Redis incr" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis incr" $ show err
      pure res

-- ----------------------------------------------------------------------------

rSet :: (HasCallStack, ToJSON v, L.MonadFlow m) =>
  RedisName -> TextKey -> v -> m (Either T.KVDBReply T.KVDBStatus)
rSet cName k v = rSetB cName k' v'
  where
    k' = TE.encodeUtf8 k
    v' = BSL.toStrict $ A.encode v

rSetB :: (HasCallStack, L.MonadFlow m) =>
  Text -> ByteKey -> ByteValue -> m (Either T.KVDBReply T.KVDBStatus)
rSetB cName k v = do
  res <- L.runKVDB cName $ L.set k v
  case res of
    Right _ -> do
      -- L.logInfo @Text "Redis set" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis set" $ show err
      pure res

rSetT :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> TextKey -> Text -> m (Either T.KVDBReply T.KVDBStatus)
rSetT cName k v = rSetB cName k' v'
  where
    k' = TE.encodeUtf8 k
    v' = TE.encodeUtf8 v

-- ----------------------------------------------------------------------------

rGetB :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> ByteKey -> m (Maybe ByteValue) -- Binary.decode?
rGetB cName k = do
  mv <- L.runKVDB cName $ L.get k
  case mv of
    Right mval -> pure mval
    Left err -> do
      L.logError @Text "Redis get" $ show err
      pure Nothing

rGet :: (HasCallStack, FromJSON v, L.MonadFlow m) =>
  RedisName -> TextKey -> m (Maybe v)
rGet cName k = do
  mv <- rGetB cName (TE.encodeUtf8 k)
  case mv of
    Just val -> case A.eitherDecode' $ BSL.fromStrict val of
      Left err -> do
        L.logError @Text "Redis rGet json decodeEither error" $ show err
        pure Nothing
      Right resp -> pure $ Just resp
    Nothing -> pure Nothing

rGetT :: (HasCallStack, L.MonadFlow m) =>
  Text -> Text -> m (Maybe Text)
rGetT cName k = do
  mv <- rGetB cName (TE.encodeUtf8 k)
  case mv of
    Just val ->
      case TE.decodeUtf8' val of
        Left err -> do
          L.logError @Text "Redis rGetT unicode decode error" (show err)
          pure Nothing
        Right x ->
          pure $ Just x
    Nothing -> pure Nothing

-- ----------------------------------------------------------------------------

rSetex :: (HasCallStack, ToJSON v, Integral t, L.MonadFlow m) =>
  RedisName -> TextKey -> v -> t -> m (Either T.KVDBReply T.KVDBStatus)
rSetex cName k v t = rSetexB cName k' v' t
  where
    k' = TE.encodeUtf8 k
    v' = BSL.toStrict $ A.encode v

rSetexB :: (HasCallStack, Integral t, L.MonadFlow m) =>
  RedisName -> ByteKey -> ByteValue -> t -> m (Either T.KVDBReply T.KVDBStatus)
rSetexB cName k v t = do
  res <- L.runKVDB cName $ L.setex k (toInteger t) v
  case res of
    Right _ -> do
      -- L.logInfo @Text "Redis setex" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis setex" $ show err
      pure res

rSetexT :: (HasCallStack, ToJSON v, Integral t, L.MonadFlow m) =>
  RedisName -> TextKey -> v -> t -> m (Either T.KVDBReply T.KVDBStatus)
rSetexT = rSetex

-- ----------------------------------------------------------------------------

rSetOpts
  :: (HasCallStack, ToJSON v, L.MonadFlow m)
  => RedisName
  -> TextKey
  -> v
  -> L.KVDBSetTTLOption
  -> L.KVDBSetConditionOption
  -> m (Either T.KVDBReply Bool)
rSetOpts cName k v ttl cond = rSetOptsB cName k' v' ttl cond
  where
    k' = TE.encodeUtf8 k
    v' = BSL.toStrict $ A.encode v

rSetOptsB
  :: (HasCallStack, L.MonadFlow m)
  => RedisName
  -> ByteKey
  -> ByteValue
  -> L.KVDBSetTTLOption
  -> L.KVDBSetConditionOption
  -> m (Either T.KVDBReply Bool)
rSetOptsB cName k v ttl cond = do
  res <- L.runKVDB cName $ L.setOpts k v ttl cond
  case res of
    Right _ -> pure res
    Left err -> do
      L.logError @Text "Redis setOpts" $ show err
      pure res

rSetOptsT
  :: (HasCallStack, L.MonadFlow m)
  => RedisName
  -> TextKey
  -> Text
  -> L.KVDBSetTTLOption
  -> L.KVDBSetConditionOption
  -> m (Either T.KVDBReply Bool)
rSetOptsT cName k v ttl cond = rSetOptsB cName k' v' ttl cond
  where
    k' = TE.encodeUtf8 k
    v' = TE.encodeUtf8 v

-- ------------------------------------------------------------------------------

rSadd :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> L.KVDBKey -> [L.KVDBValue] -> m (Either T.KVDBReply Integer)
rSadd cName k v = do
  res <- L.runKVDB cName $ L.sadd k v
  case res of
    Right _ -> pure res
    Left err -> do
      L.logError @Text "Redis sadd" $ show err
      pure res

rSismember :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> L.KVDBKey -> L.KVDBValue -> m (Either T.KVDBReply Bool)
rSismember cName k v = do
  res <- L.runKVDB cName $ L.sismember k v
  case res of
    Right _ -> pure res
    Left err -> do
      L.logError @Text "Redis sismember" $ show err
      pure res

withLoggerContext :: (HasCallStack, L.MonadFlow m) => (T.LogContext -> T.LogContext) -> L.Flow a -> m a
withLoggerContext updateLCtx = L.withModifiedRuntime (updateLoggerContext updateLCtx)


updateLoggerContext :: HasCallStack => (T.LogContext -> T.LogContext) -> FlowRuntime -> FlowRuntime
updateLoggerContext updateLCtx rt@FlowRuntime{..} = rt {_coreRuntime = _coreRuntime {_loggerRuntime = newLrt}}
  where
    newLrt :: LoggerRuntime
    newLrt = case _loggerRuntime _coreRuntime of
               MemoryLoggerRuntime a lc b c d -> MemoryLoggerRuntime a (updateLCtx lc) b c d
               LoggerRuntime { _flowFormatter, _logContext, _logLevel, _logRawSql, _logCounter, _logMaskingConfig, _logLoggerHandle}
                 -> LoggerRuntime  _flowFormatter (updateLCtx _logContext) _logLevel _logRawSql _logCounter _logMaskingConfig _logLoggerHandle