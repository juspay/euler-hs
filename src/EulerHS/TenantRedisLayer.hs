module EulerHS.TenantRedisLayer
  ( module EulerHS.TenantRedisLayer
  , L.KVDBTx
  ) where

import qualified Data.Map.Internal as Map
import           EulerHS.Language
import qualified EulerHS.Extra.Redis as R
import           EulerHS.Prelude
import qualified EulerHS.Types as T
import qualified EulerHS.KVDB.Language as L
import  qualified Database.Redis as R ( Queued, Status, XReadOpts, XReadResponse )
import qualified EulerHS.ART.EnvVars as Env
import           EulerHS.ART.V2.Types (HasArtRecOptions)
import           EulerHS.ART.V2.Utils (getArtReplayRedisPrefix)

tenantKVDelimeter :: Text
tenantKVDelimeter = ":::"

prefixARTKeyT :: MonadFlow m => Text -> m Text
prefixARTKeyT key =
  if Env.isArtV2ReplayEnabled
    then do
      artReplayPrefix <- getArtReplayRedisPrefix
      pure $ artReplayPrefix <> key
    else pure key

prefixARTKeyB :: MonadFlow m => ByteString -> m ByteString
prefixARTKeyB key =
  if Env.isArtV2ReplayEnabled
    then do
      artReplayPrefix <- getArtReplayRedisPrefix
      pure $ (encodeUtf8 artReplayPrefix) <> key
    else pure key

prefixARTKeyMap :: MonadFlow m => Map R.TextKey v -> m (Map R.TextKey v)
prefixARTKeyMap kv =
  if Env.isArtV2ReplayEnabled
    then do
      artReplayPrefix <- getArtReplayRedisPrefix
      pure $ Map.mapKeysMonotonic (artReplayPrefix <>) kv
    else pure kv

getKeyWithTenantT :: MonadFlow m => Text -> m Text
getKeyWithTenantT key = do
  tConfObj <- getOptionLocal TenantConfigObj
  let prefix = join (cacheKeyPrefix <$> tConfObj)
  prefixARTKeyT =<< maybe (if Env.logTenantErrors && isNothing tConfObj
    then do
      (logInfoT "TENANT_CONTEXT_MISSING" $ "Missing tenant context redis , key : " <> key) *> pure key
    else pure key) (\x -> pure $ if x == "" then key else x <> tenantKVDelimeter <> key)  prefix

getKeyWithTenantB :: MonadFlow m => ByteString -> m ByteString
getKeyWithTenantB key = do
  tConfObj <- getOptionLocal TenantConfigObj
  let prefix = join (cacheKeyPrefix <$> tConfObj)
  prefixARTKeyB =<< maybe (if Env.logTenantErrors && isNothing tConfObj
    then (logInfoT "TENANT_CONTEXT_MISSING" $ "Missing tenant context for redis, key " <> (decodeUtf8 key)) *> pure key 
    else pure key) (\x -> pure $ if x == "" then key else x <> (encodeUtf8 tenantKVDelimeter) <> key) (encodeUtf8 <$> prefix)

getKeysWithTenantMap :: MonadFlow m => Map R.TextKey v -> m (Map R.TextKey v)
getKeysWithTenantMap kvMap = do
  tConfObj <- getOptionLocal TenantConfigObj
  let prefix = join (cacheKeyPrefix <$> tConfObj)
  prefixARTKeyMap =<< maybe (if Env.logTenantErrors && isNothing tConfObj 
    then do
      (logInfoV @Text "TENANT_CONTEXT_MISSING" $ ("Missing tenant context redis , kvMap : " :: Text, Map.keys kvMap)) *> pure kvMap
    else pure kvMap) (\x -> pure $ if x == "" then kvMap else Map.mapKeysMonotonic (\k -> x <> tenantKVDelimeter <> k) kvMap) prefix

getMExecWithTenantPrefix :: MonadFlow m => (ByteString -> L.KVDBTx (R.Queued a)) -> m (L.KVDBTx (R.Queued a))
getMExecWithTenantPrefix opts = do
  tConfObj <- getOptionLocal TenantConfigObj
  let prefix = join (cacheKeyPrefix <$> tConfObj)
  if Env.isArtV2ReplayEnabled
    then do
      artPrefix <- getArtReplayRedisPrefix
      let prefix' = maybe artPrefix (\p -> artPrefix <> p) prefix 
      when (isNothing prefix && Env.logTenantErrors && isNothing tConfObj) $
          (logInfoT "TENANT_CONTEXT_MISSING" $ "Missing tenant context redis in MultiExec")
      pure $ opts (encodeUtf8 prefix' <> (encodeUtf8 tenantKVDelimeter))
    else 
      maybe (if Env.logTenantErrors && isNothing tConfObj 
        then do
          (logInfoT "TENANT_CONTEXT_MISSING" $ "Missing tenant context redis in MultiExec") *> pure (opts "")
        else pure (opts "")) (\x -> pure $ if x == "" then (opts "") else opts (encodeUtf8 x <> (encodeUtf8 tenantKVDelimeter))) prefix

class RedisKey a where
  getKey :: a -> Text

withRedisMigrationRead :: MonadFlow m => t -> t -> (t -> m b) -> m b
withRedisMigrationRead  inputWithoutPrefix inputWithPrefix act = do
  migrationMode <- fromMaybe NEW_WRITE_DISABLED <$> getOptionLocal CachePrefixMigrationMode
  if migrationMode `elem` [NEW_READ_ENABLED, OLD_WRITE_DISABLED, OLD_WRITE_ENABLED]
    then act inputWithPrefix
    else act inputWithoutPrefix

withTenantPrefix :: MonadFlow m => Text -> (Text -> m a) -> m a
withTenantPrefix key act = do
  prefixedKey <- getKeyWithTenantT key
  withRedisMigrationRead key prefixedKey act

withTenantPrefixB :: MonadFlow m => ByteString -> (ByteString -> m a) -> m a
withTenantPrefixB key act = do
  prefixedKey <- getKeyWithTenantB key
  withRedisMigrationRead key prefixedKey act

withTenantPrefixL :: MonadFlow m => [Text] -> ([Text] -> m a) -> m a
withTenantPrefixL ks act = do
  ksWithPrefix <- mapM getKeyWithTenantT ks
  withRedisMigrationRead ks ksWithPrefix act

withTenantPrefixBL :: MonadFlow m => [ByteString] -> ([ByteString] -> m a) -> m a
withTenantPrefixBL ks act = do
  ksWithPrefix <- mapM (\k -> withTenantPrefixB k pure) ks
  withRedisMigrationRead ks ksWithPrefix act


withRedisMigrationWrite :: MonadFlow m => t -> t -> (t -> m (Either e a)) -> m (Either e a)
withRedisMigrationWrite inputWithoutPrefix inputWithPrefix act = do
  migrationMode <- fromMaybe NEW_WRITE_DISABLED <$> getOptionLocal CachePrefixMigrationMode
  case migrationMode of
    NEW_WRITE_DISABLED -> act inputWithoutPrefix
    OLD_WRITE_DISABLED -> act inputWithPrefix
    DUAL_WRITE -> runExceptT $ do
      _ <- ExceptT $ act inputWithPrefix
      ExceptT $ act inputWithoutPrefix
    _ -> runExceptT $ do -- NEW_READ_ENABLED || OLD_WRITE_ENABLED
      _ <- ExceptT $ act inputWithoutPrefix
      ExceptT $ act inputWithPrefix


withTenantPrefixWrite :: MonadFlow m => Text -> (Text -> m (Either T.KVDBReply a)) -> m (Either T.KVDBReply a)
withTenantPrefixWrite key act = do
  prefixedKey <- getKeyWithTenantT key
  withRedisMigrationWrite key prefixedKey act

withTenantPrefixBWrite :: MonadFlow m => ByteString -> (ByteString -> m (Either e a)) -> m (Either e a)
withTenantPrefixBWrite key act = do
  prefixedKey <- getKeyWithTenantB key
  withRedisMigrationWrite key prefixedKey act

withTenantPrefixLWrite :: (Traversable t, MonadFlow m) => t Text -> (t Text -> m (Either e a)) -> m (Either e a)
withTenantPrefixLWrite ks act = do
  ksWithPrefix <- mapM getKeyWithTenantT ks
  withRedisMigrationWrite ks ksWithPrefix act

withTenantPrefixBLWrite :: (Traversable t, MonadFlow m) => t ByteString -> (t ByteString -> m (Either e a)) -> m (Either e a)
withTenantPrefixBLWrite ks act = do
  ksWithPrefix <- mapM (\k -> withTenantPrefixB k pure) ks
  withRedisMigrationWrite ks ksWithPrefix act

rGet :: (HasCallStack, FromJSON v, MonadFlow m) =>
  R.RedisName -> R.TextKey -> m (Maybe v)
rGet redis key = withTenantPrefix key (R.rGet redis)

rGetB :: (HasCallStack, MonadFlow m) =>
  R.RedisName -> R.ByteKey -> m (Maybe R.ByteValue)
rGetB redis key = withTenantPrefixB key (R.rGetB redis)

rGetT :: (HasCallStack, MonadFlow m) =>
  R.RedisName -> R.TextKey -> m (Maybe Text)
rGetT redis key = withTenantPrefix key (R.rGetT redis)

get :: MonadFlow m => R.RedisName -> R.ByteKey -> m (T.KVDBAnswer (Maybe ByteString))
get redis key = withTenantPrefixB key (R.rGetBEither redis)

rSetex :: (HasCallStack, ToJSON v, Integral t, MonadFlow m) =>
  R.RedisName -> R.TextKey -> v -> t -> m (Either T.KVDBReply T.KVDBStatus)
rSetex redis key value ttl = withTenantPrefixWrite key (\k -> R.rSetex redis k value ttl)

rSet :: (ToJSON v, MonadFlow m) => R.RedisName -> Text -> v -> m (Either T.KVDBReply T.KVDBStatus)
rSet redis key value = withTenantPrefixWrite key (\k -> R.rSet redis k value)

rSetB :: MonadFlow m => Text -> ByteString -> R.ByteValue -> m (Either T.KVDBReply T.KVDBStatus)
rSetB redis key value = withTenantPrefixBWrite key (\k -> R.rSetB redis k value)

rSetexB :: (HasCallStack, Integral t, MonadFlow m) =>
  R.RedisName -> R.ByteKey -> R.ByteValue -> t -> m (Either T.KVDBReply T.KVDBStatus)
rSetexB redis key value ttl = withTenantPrefixBWrite key (\k -> R.rSetexB redis k value ttl)

rSetT :: MonadFlow m => R.RedisName -> Text -> Text -> m (Either T.KVDBReply T.KVDBStatus)
rSetT redis key value = withTenantPrefixWrite key (\k -> R.rSetT redis k value)

rSetexBulk :: (HasCallStack, ToJSON v, Integral t, MonadFlow m) => R.RedisName -> Map R.TextKey v -> t -> m (Either T.KVDBReply ())
rSetexBulk redis kvMap t = do
  kvMapWithPrefix <- getKeysWithTenantMap kvMap
  withRedisMigrationWrite kvMap kvMapWithPrefix (\kvMap' -> R.rSetexBulk redis kvMap' t)

setOpts :: MonadFlow m => Text -> ByteString -> L.KVDBValue -> L.KVDBSetTTLOption -> L.KVDBSetConditionOption -> m (T.KVDBAnswer Bool)
setOpts redis key value ttl con = withTenantPrefixBWrite key (\k -> R.rSetOptsB redis k value ttl con)

rSetOptsB :: MonadFlow m => R.RedisName -> ByteString -> R.ByteValue -> L.KVDBSetTTLOption -> L.KVDBSetConditionOption -> m (Either T.KVDBReply Bool)
rSetOptsB redis key value ttlOption condition = withTenantPrefixBWrite key $ \k -> R.rSetOptsB redis k value ttlOption condition

rSetOptsT :: MonadFlow m => R.RedisName -> Text -> Text -> L.KVDBSetTTLOption -> L.KVDBSetConditionOption -> m (Either T.KVDBReply Bool)
rSetOptsT redis key value ttlOption condition = withTenantPrefixWrite key $ \k -> R.rSetOptsT redis k value ttlOption condition

rDel :: (HasCallStack, MonadFlow m) =>
  R.RedisName -> [R.TextKey] -> m (Either T.KVDBReply Integer)
rDel redis ks = withTenantPrefixLWrite ks (R.rDel redis)

rIncr :: MonadFlow m => R.RedisName -> R.TextKey -> m (Either T.KVDBReply Integer)
rIncr redis key = withTenantPrefixWrite key (R.rIncr redis)

rIncrWithART :: MonadFlow m => R.RedisName -> R.TextKey -> m (Either T.KVDBReply Integer)
rIncrWithART redis key = withTenantPrefixWrite key (R.rIncrWithART redis)

incr :: MonadFlow m => R.RedisName -> R.ByteKey -> m (T.KVDBAnswer Integer)
incr redis key = withTenantPrefixBWrite key (R.rIncrB redis)

rIncrBy :: MonadFlow m => R.RedisName -> R.TextKey -> Integer -> m (Either T.KVDBReply Integer)
rIncrBy redis key val = withTenantPrefixWrite key (\k -> R.rIncrBy redis k val)

rIncrByFloat :: MonadFlow m => R.RedisName -> R.TextKey -> Double -> m (Either T.KVDBReply Double)
rIncrByFloat redis key val = withTenantPrefixWrite key (\k -> R.rIncrByFloat redis k val)

llen :: MonadFlow m => R.RedisName -> R.ByteKey -> m (T.KVDBAnswer Integer)
llen redis key = withTenantPrefixB key (R.rLLenB redis)

rLRange :: MonadFlow m => R.RedisName -> R.ByteKey -> Integer -> Integer ->  m (Either T.KVDBReply [ByteString])
rLRange redis key start stop = withTenantPrefixB key (\k -> R.rLRangeB redis k start stop)

decr :: MonadFlow m => R.RedisName -> R.ByteKey -> m (T.KVDBAnswer Integer)
decr redis key = withTenantPrefixBWrite key (R.rDecrB redis)

rDecrBy :: MonadFlow m => R.RedisName -> R.TextKey -> Integer -> m (Either T.KVDBReply Integer)
rDecrBy redis key val = withTenantPrefixWrite key (\k -> R.rDecrBy redis k val)

exists :: MonadFlow m => R.RedisName -> R.ByteKey -> m (T.KVDBAnswer Bool)
exists redis key = withTenantPrefixB key (R.rExistsB redis)

rExpire :: (Integral t, MonadFlow m) => R.RedisName -> R.TextKey -> t -> m (Either T.KVDBReply Bool)
rExpire redis key ttl = withTenantPrefixWrite key (\k -> R.rExpire redis k ttl)

rExpireWithART :: (Integral t, MonadFlow m, HasArtRecOptions) => R.RedisName -> R.TextKey -> t -> m (Either T.KVDBReply Bool)
rExpireWithART redis key ttl = withTenantPrefixWrite key (\k -> R.rExpireWithART redis k ttl)

rExpireAt :: (Integral t, MonadFlow m) => R.RedisName -> R.TextKey -> t -> m (Either T.KVDBReply Bool)
rExpireAt redis key ttl = withTenantPrefixWrite key (\k -> R.rExpireAt redis k ttl)

rSadd :: MonadFlow m => R.RedisName -> ByteString -> [L.KVDBValue] -> m (Either T.KVDBReply Integer)
rSadd redis key v = withTenantPrefixBWrite key (\k -> R.rSadd redis k v)

rSismember :: MonadFlow m => R.RedisName -> ByteString -> L.KVDBValue -> m (Either T.KVDBReply Bool)
rSismember redis key v = withTenantPrefixB key (\k -> R.rSismember redis k v)

rSmembersB :: MonadFlow m => R.RedisName -> ByteString -> m (T.KVDBAnswer [ByteString])
rSmembersB redis key = withTenantPrefixB key (R.rSmembersB redis)

sRemB :: MonadFlow m => R.RedisName -> ByteString -> [L.KVDBValue] -> m (T.KVDBAnswer Integer)
sRemB redis key pKeyList = withTenantPrefixBWrite key (\k -> R.sRemB redis k pKeyList)

rZAdd :: (MonadFlow m) => R.RedisName -> L.KVDBKey -> [(Double, R.ByteValue)] -> m (Either T.KVDBReply Integer)
rZAdd redis key val = withTenantPrefixBWrite key $ \k -> R.rZAdd redis k val

rZRemRangeByScore :: (MonadFlow m) => R.RedisName -> L.KVDBKey -> Double -> Double -> m (Either T.KVDBReply Integer)
rZRemRangeByScore redis key minScore maxScore = withTenantPrefixBWrite key $ \k -> R.rZRemRangeByScore redis k minScore maxScore

rZRangeByScore :: (MonadFlow m) => R.RedisName -> ByteString -> Double -> Double -> m (Either T.KVDBReply [L.KVDBValue])
rZRangeByScore redis key minScore maxScore = withTenantPrefixB key $ \k -> R.rZRangeByScore redis k minScore maxScore

rZCard :: MonadFlow m => R.RedisName -> L.KVDBKey -> m (Either T.KVDBReply Integer)
rZCard redis key = withTenantPrefixB key (R.rZCard redis)

rHget :: (MonadFlow m, ToJSON v, FromJSON v) => R.RedisName -> Text -> R.TextField -> m (Maybe v)
rHget redis key f = withTenantPrefix key (\k -> R.rHget redis k f)

rHset :: (MonadFlow m, ToJSON v) => R.RedisName -> Text -> R.TextField -> v -> m (Either T.KVDBReply Integer)
rHset redis key f v = withTenantPrefixWrite key (\k -> R.rHset redis k f v)

rHsetB :: MonadFlow m => R.RedisName -> ByteString -> R.ByteField -> R.ByteValue -> m (Either T.KVDBReply Integer)
rHsetB redis key f v = withTenantPrefixBWrite key (\k -> R.rHsetB redis k f v)

rHincrBy :: MonadFlow m => R.RedisName -> Text -> R.TextField -> Integer -> m (Either T.KVDBReply Integer)
rHincrBy redis key field value = withTenantPrefixWrite key (\k -> R.rHincrBy redis k field value)

rHsetNx :: (HasCallStack, MonadFlow m) => R.RedisName -> R.ByteKey -> R.ByteField -> R.ByteValue -> m (Either T.KVDBReply Bool)
rHsetNx redis key f v = withTenantPrefixBWrite key (\k -> R.rHsetNx redis k f v)

rHmset :: (MonadFlow m, ToJSON v) => R.RedisName -> Text -> [(R.TextField, v)] -> m (Either T.KVDBReply T.KVDBStatus)
rHmset redis key v = withTenantPrefixWrite key (\k -> R.rHmset redis k v)

rHmsetB :: MonadFlow m => R.RedisName -> ByteString -> [(R.ByteField, R.ByteValue)] -> m (Either T.KVDBReply T.KVDBStatus)
rHmsetB redis key v = withTenantPrefixBWrite key (\k -> R.rHmsetB redis k v)

rHGetAll :: (HasCallStack, MonadFlow m) => R.RedisName -> L.KVDBKey -> m (Either T.KVDBReply [(ByteString, ByteString)])
rHGetAll redis key = withTenantPrefixB key (R.rHGetAll redis)

rHDelB :: (HasCallStack, MonadFlow m) => R.RedisName -> L.KVDBKey -> [L.KVDBField] -> m (Either T.KVDBReply Integer)
rHDelB redis key f = withTenantPrefixBWrite key (\k -> R.rHDelB redis k f)

hlen :: MonadFlow m => R.RedisName -> R.ByteKey -> m (T.KVDBAnswer Integer)
hlen redis key = withTenantPrefixB key (R.rHLenB redis)

multiExec :: (MonadFlow m, ToJSON a ,FromJSON a) => Text -> (ByteString -> L.KVDBTx (R.Queued a)) -> m (T.KVDBAnswer (T.TxResult a))
multiExec redis opts = do
  optsWithPrefix <- getMExecWithTenantPrefix opts
  withRedisMigrationWrite (opts "") optsWithPrefix (R.rMultiExec redis) 

------------------ Tx functions ------------------

setTx :: L.KVDBKey -> L.KVDBValue -> ByteString -> L.KVDBTx (R.Queued T.KVDBStatus)
setTx key value prefix = L.setTx (prefix <> key) value

setexTx :: L.KVDBKey -> L.KVDBDuration -> L.KVDBValue -> ByteString -> L.KVDBTx (R.Queued T.KVDBStatus)
setexTx key ex val prefix = L.setexTx (prefix <> key) ex val

setOptsTx :: L.KVDBKey -> L.KVDBValue -> L.KVDBSetTTLOption -> L.KVDBSetConditionOption -> ByteString -> F (L.KeyValueF R.Queued) (R.Queued Bool)
setOptsTx key value ttl cond prefix = L.setOptsTx (prefix <> key) value ttl cond

incrTx :: L.KVDBKey -> ByteString -> L.KVDBTx (R.Queued Integer)
incrTx key prefix = L.incrTx (prefix <> key)

incrByTx :: L.KVDBKey -> Integer -> ByteString -> L.KVDBTx (R.Queued Integer)
incrByTx key val prefix = L.incrByTx (prefix <> key) val

incrByFloatTx :: L.KVDBKey -> Double -> ByteString -> L.KVDBTx (R.Queued Double)
incrByFloatTx key val prefix = L.incrByFloatTx (prefix <> key) val

decrTx :: L.KVDBKey -> ByteString -> L.KVDBTx (R.Queued Integer)
decrTx key prefix = L.decrTx (prefix <> key)

decrByTx :: L.KVDBKey -> Integer -> ByteString -> L.KVDBTx (R.Queued Integer)
decrByTx key val prefix = L.decrByTx (prefix <> key) val

expireTx :: L.KVDBKey -> L.KVDBDuration -> ByteString -> L.KVDBTx (R.Queued Bool)
expireTx key sec prefix = L.expireTx (prefix <> key) sec

expireAtTx :: L.KVDBKey -> L.KVDBDuration -> ByteString -> L.KVDBTx (R.Queued Bool)
expireAtTx key sec prefix = L.expireAtTx (prefix <> key) sec

delTx :: [L.KVDBKey] -> ByteString -> L.KVDBTx (R.Queued Integer)
delTx ks prefix = L.delTx (map (prefix <>) ks)

lpushTx :: L.KVDBKey -> [L.KVDBValue] -> ByteString -> L.KVDBTx (R.Queued Integer)
lpushTx key value prefix = L.lpushTx (prefix <> key) value

rpopTx :: L.KVDBKey -> ByteString -> L.KVDBTx (R.Queued (Maybe ByteString))
rpopTx key prefix = L.rpopTx (prefix <> key)

llenTx :: L.KVDBKey -> ByteString -> L.KVDBTx (R.Queued Integer)
llenTx key prefix = L.llenTx (prefix <> key)

hincrByTx :: L.KVDBKey -> L.KVDBField -> Integer -> ByteString -> KVDBTx (R.Queued Integer)
hincrByTx key field value prefix = L.hincrByTx (prefix <> key) field value

hsetTx :: L.KVDBKey -> L.KVDBField -> L.KVDBValue -> ByteString -> KVDBTx (R.Queued Integer)
hsetTx key field value prefix = L.hsetTx (prefix <> key) field value

hgetTx :: L.KVDBKey -> L.KVDBField -> ByteString -> KVDBTx (R.Queued (Maybe ByteString))
hgetTx key field prefix = L.hgetTx (prefix <> key) field

hsetNxTx :: L.KVDBKey -> L.KVDBField -> L.KVDBValue -> ByteString -> KVDBTx (R.Queued Bool)
hsetNxTx key field value prefix = L.hsetNxTx (prefix <> key) field value

pingRequest :: MonadFlow m => Text -> m (T.KVDBAnswer R.Status)
pingRequest redis = R.rPingRequest redis


---------------- Tenant agnostic functions ---------------------
-- instance RedisKey LockSync where
--   getKey (LockSync a) = "lock_sync_" <> a

getKeyWithART :: (RedisKey k, MonadFlow m) => k -> m Text
getKeyWithART key = 
  if Env.isArtV2ReplayEnabled
    then do
      artReplayPrefix <- getArtReplayRedisPrefix
      pure $ artReplayPrefix <> getKey key
    else pure $ getKey key

xadd :: (MonadFlow m, RedisKey k) => Text -> k -> L.KVDBStreamEntryIDInput -> [L.KVDBStreamItem] -> m (T.KVDBAnswer L.KVDBStreamEntryID)
xadd redis stream entryID items = getKeyWithART stream >>= \stream' -> R.rXaddB redis (encodeUtf8 stream') items entryID

xlen :: (MonadFlow m, RedisKey k) => Text -> k -> m (T.KVDBAnswer Integer)
xlen redis stream = getKeyWithART stream >>= \stream' -> R.rXLenB redis (encodeUtf8 stream')

xdel :: (MonadFlow m, RedisKey k) => Text -> k -> [L.KVDBStreamEntryID] -> m (T.KVDBAnswer Integer)
xdel redis stream ids = getKeyWithART stream >>= \stream' -> R.rXDelB redis (encodeUtf8 stream') ids

xgroupCreate :: (MonadFlow m, RedisKey k) => Text -> k -> L.KVDBGroupName -> L.RecordID -> m (T.KVDBAnswer R.Status)
xgroupCreate redis stream groupName startId = getKeyWithART stream >>= \stream' -> R.rXgroupCreate redis (encodeUtf8 stream') groupName startId

xrangeC :: (MonadFlow m, RedisKey k) => Text -> k -> L.KVDBStreamStart -> L.KVDBStreamEnd -> Maybe Integer -> m (T.KVDBAnswer [L.KVDBStreamReadResponseRecord])
xrangeC redis stream sstart send count = getKeyWithART stream >>= \stream' -> R.rXrangeB redis (encodeUtf8 stream') sstart send count

xreadOptsOneC :: (MonadFlow m, RedisKey k) => Text -> k -> L.KVDBStreamEntryIDInput -> R.XReadOpts -> m (Either T.KVDBReply (Maybe [R.XReadResponse]))
xreadOptsOneC redis stream streamEntryInput readOpts = getKeyWithART stream >>= \stream' -> R.rXreadOpts redis [(encodeUtf8 stream', streamEntryInput)] readOpts

xreadGroup :: (MonadFlow m, RedisKey k) => Text -> L.KVDBGroupName -> k -> [(L.KVDBStream, L.RecordID)] -> Maybe Integer -> Maybe Integer -> Bool -> m (T.KVDBAnswer (Maybe [L.KVDBStreamReadResponse]))
xreadGroup redis groupName consumerName streamsAndIds mBlock mCount noack = getKeyWithART consumerName >>= \consumerName' -> R.rXreadGroup redis groupName (encodeUtf8 consumerName') streamsAndIds mBlock mCount noack

rZAddC :: (MonadFlow m, RedisKey k) => R.RedisName -> k -> [(Double, R.ByteValue)] -> m (Either T.KVDBReply Integer)
rZAddC redis k val = getKeyWithART k >>= \k' -> R.rZAdd redis (encodeUtf8 k') val

rZCardC :: (MonadFlow m, RedisKey k) => R.RedisName -> k -> m (Either T.KVDBReply Integer)
rZCardC redis k = getKeyWithART k >>= \k' -> R.rZCard redis (encodeUtf8 k')

rZRangeByScoreC :: (MonadFlow m, RedisKey k) => R.RedisName -> k -> Double -> Double -> m (Either T.KVDBReply [L.KVDBValue])
rZRangeByScoreC redis k minScore maxScore = getKeyWithART k >>= \k' -> R.rZRangeByScore redis (encodeUtf8 k') minScore maxScore

rZRangeWithScoresC :: (MonadFlow m, RedisKey k) => R.RedisName -> k -> Integer -> Integer -> m (Either T.KVDBReply [(L.KVDBValue, Double)])
rZRangeWithScoresC redis k minScore maxScore = getKeyWithART k >>= \k' -> R.rZRangeWithScores redis (encodeUtf8 k') minScore maxScore

rZRangeByScoreWithLimit :: (MonadFlow m, RedisKey k) => R.RedisName -> k -> Double -> Double -> Integer -> Integer -> m (Either T.KVDBReply [L.KVDBValue])
rZRangeByScoreWithLimit redis k minScore maxScore minLimit maxLimit = getKeyWithART k >>= \k' -> R.rZRangeByScoreWithLimit redis (encodeUtf8 k') minScore maxScore minLimit maxLimit

rZRangeByScoreWithScoresC :: (MonadFlow m, RedisKey k) => R.RedisName -> k -> Double -> Double -> m (Either T.KVDBReply [(L.KVDBValue, Double)])
rZRangeByScoreWithScoresC redis k minScore maxScore = getKeyWithART k >>= \k' -> R.rZRangeByScoreWithScores redis (encodeUtf8 k') minScore maxScore

rZRem :: (MonadFlow m, RedisKey k) => R.RedisName -> k -> [L.KVDBValue] -> m (Either T.KVDBReply Integer)
rZRem redis k vals = getKeyWithART k >>= \k' -> R.rZRem redis (encodeUtf8 k') vals

rZRemRangeByScoreC :: (MonadFlow m, RedisKey k) => R.RedisName -> k -> Double -> Double -> m (Either T.KVDBReply Integer)
rZRemRangeByScoreC redis k minScore maxScore = getKeyWithART k >>= \k' -> R.rZRemRangeByScore redis (encodeUtf8 k') minScore maxScore

rSetC :: (ToJSON v, MonadFlow m, RedisKey k) => R.RedisName -> k -> v -> m (Either T.KVDBReply T.KVDBStatus)
rSetC redis k value = getKeyWithART k >>= \k' -> R.rSet redis k' value

rSetexC :: (HasCallStack, ToJSON v, Integral t, MonadFlow m, RedisKey k) =>
  R.RedisName -> k -> v -> t -> m (Either T.KVDBReply T.KVDBStatus)
rSetexC redis k value ttl = getKeyWithART k >>= \k' -> R.rSetex redis k' value ttl

setOptsC :: (MonadFlow m, RedisKey k) => R.RedisName -> k -> L.KVDBValue -> L.KVDBSetTTLOption -> L.KVDBSetConditionOption -> m (T.KVDBAnswer Bool)
setOptsC redis k value ttl con = getKeyWithART k >>= \k' -> R.rSetOptsB redis (encodeUtf8 k') value ttl con

rGetC :: (HasCallStack, FromJSON v, MonadFlow m, RedisKey k) =>
  R.RedisName -> k -> m (Maybe v)
rGetC redis k = getKeyWithART k >>= \k' -> R.rGet redis k'

rDelC :: (HasCallStack, MonadFlow m, RedisKey k) =>
  R.RedisName -> [k] -> m (Either T.KVDBReply Integer)
rDelC redis ks = mapM getKeyWithART ks >>= \ks' -> R.rDel redis ks'

incrC :: (MonadFlow m, RedisKey k) => R.RedisName -> k -> m (T.KVDBAnswer Integer)
incrC redis k = getKeyWithART k >>= \k' -> R.rIncrB redis (encodeUtf8 k')

rExpireC :: (Integral t, MonadFlow m, RedisKey k) => R.RedisName -> k -> t -> m (Either T.KVDBReply Bool)
rExpireC redis k ttl = getKeyWithART k >>= \k' -> R.rExpire redis k' ttl