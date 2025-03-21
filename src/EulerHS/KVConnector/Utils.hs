{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module EulerHS.KVConnector.Utils where

import Streamly.Data.Serialize.Instances ()
import qualified Data.Aeson as A
-- import           Data.Aeson (Value (Object), object)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as AK
import           Data.List (findIndices, intersect)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Database.Beam as B
import qualified Database.Beam.Schema.Tables as B
import qualified EulerHS.KVConnector.Encoding as Encoding
import           EulerHS.KVConnector.Metrics (incrementMetric, KVMetric(..),incrementRedisCallMetric)
import           EulerHS.KVConnector.Types (AutoPrimaryId(..), MeshMeta(..), MeshResult, MeshConfig(..), KVConnector(..), PrimaryKey(..), SecondaryKey(..),
                    DBLogEntry(..), Source(..), MerchantID(..), IdSource(..), ETLStreamKeys, KVEntry(..), DBName(..), PKvKey(..), SKvKey(..))
import qualified EulerHS.Language as L
#if defined(REDIS_CORE_EXPORT)
#else
import qualified EulerHS.KVDB.Language as L
import qualified EulerHS.Extra.Redis as L
#endif
import qualified EulerHS.TenantRedisLayer as TRL
import           EulerHS.Prelude
import           Text.Casing (quietSnake)
-- import           Servant (err500)
import           Data.Either.Extra (mapLeft, mapRight)
import           Data.Time.Clock.POSIX
import qualified EulerHS.Logger.Types as Log
import           EulerHS.Types (Operation(..), ApiTag(..), BeamRuntime, BeamRunner, DBConfig (MySQLPoolConf, MockingConf), KVDBAnswer, KVDBReply, TxResult)
import           EulerHS.Extra.Snowflakes.Types (SnowflakeError(..))
import           Sequelize (fromColumnar', columnize, Model, Where, Clause(..), Term(..), Set(..), ModelMeta(..), TableType(..))
import           System.Random (randomRIO)
import           Unsafe.Coerce (unsafeCoerce)
import qualified Data.Fixed as Fixed
import qualified Data.Serialize as Serialize
import qualified Data.Serialize as Cereal
import           Safe (atMay)
import EulerHS.KVDB.Types (MeshError(..), KVDBStreamItem)
import           EulerHS.SqlDB.Types (ModelDBConfig(..), getSchemaName )
import Streamly.Data.Array (Array)
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Data.MutByteArray as SSerialize
    (Serialize(..))
import qualified EulerHS.TenantRedisLayer as EHT
import           EulerHS.ART.V2.Types (HasArtRecOptions)
import qualified Juspay.Extra.Env as Env

#include "MachDeps.h"

fnvHash :: Int -> Array Word8 -> Int
fnvHash fnvOffsetBasis arr = go fnvOffsetBasis 0 (Array.length arr)

    where

#if WORD_SIZE_IN_BITS == 64
    fnvPrime = 1099511628211
#else
    fnvPrime = 16777619
#endif
    go hash i end | i >= end = hash
    go hash i end =
        let val = fromIntegral (Array.getIndexUnsafe i arr)
            next = hash * fnvPrime `xor` val
         in go next (i + 1) end

type W8Arr = Array Word8

instance Hashable W8Arr where
    hashWithSalt salt arr = fnvHash salt arr

{-# INLINE jsonKeyValueUpdates #-}
jsonKeyValueUpdates ::
  forall be table. (HasCallStack, Model be table, MeshMeta be table)
  => [Set be table] -> [(Text, A.Value)]
jsonKeyValueUpdates = fmap jsonSet

{-# INLINE jsonSet #-}
jsonSet ::
  forall be table.
  (HasCallStack, Model be table, MeshMeta be table) =>
  Set be table -> (Text, A.Value)
jsonSet (Set column value) = (key, modifiedValue)
  where
    key = B._fieldName . fromColumnar' . column $ columnized @be @table
    modifiedValue = A.toJSON value

jsonSet (SetDefault _) = error "Default values are not supported"

-- | Update the model by setting it's fields according the given
--   key value mapping.
updateModel :: forall be table.
  ( MeshMeta be table,
    ToJSON (table Identity)
  ) =>
  table Identity -> [(Text, A.Value)] -> MeshResult A.Value
updateModel model updVals = do
  let updVals' = map (\(key,v) -> (AK.fromText key, Map.findWithDefault id key (valueMapper @be @table) v)) updVals
  case A.toJSON model of
    A.Object o -> Right (A.Object $ foldr (uncurry KM.insert) o updVals')
    o -> Left $ MUpdateFailed
      ("Failed to update a model. Expected a JSON object but got '" <>
        (decodeUtf8 . BSL.toStrict . encodePretty $ o) <>
        "'.")

updateModel' :: forall table be.
  ( HasCallStack,
    Model be table,
    MeshMeta be table,
    FromJSON (table Identity),
    ToJSON (table Identity)
  ) =>
  [Set be table] ->
  table Identity ->
  MeshResult (table Identity)
updateModel' setClause model = do
  let resp = (updateModel @be @table) model (jsonKeyValueUpdates setClause)
  case resp of
    Left err -> Left err
    Right updatedModel ->
      case resultToEither $ A.fromJSON updatedModel of
        Right val -> Right val
        Left err -> Left $ MDecodingError err

getDataFromPKeysRedis :: forall table m. (
    HasArtRecOptions,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ModelMeta table,
    Serialize.Serialize (table Identity),
    L.MonadFlow m) => MeshConfig -> [ByteString] -> m (MeshResult ([KVEntry table], [KVEntry table]))
getDataFromPKeysRedis _ [] = pure $ Right ([], [])
getDataFromPKeysRedis meshCfg (pKey : pKeys) = do
  res <- L.rGetBEitherWithART meshCfg.kvRedis (fromString $ T.unpack $ decodeUtf8 pKey)
  case res of
    Right (Just r) -> do
      let (decodeResult, isLive) = decodeToField $ BSL.fromChunks [r]
      case decodeResult of
        Right decodeRes -> do
          remainingPKeysResult <- getDataFromPKeysRedis meshCfg pKeys
          case remainingPKeysResult of
            Right remainingResult -> do
              if isLive
                then return $ Right (decodeRes : (fst remainingResult), snd remainingResult)
                else return $ Right (fst remainingResult, decodeRes : (snd remainingResult))
            Left err -> return $ Left err
        Left e -> return $ Left e
    Right Nothing -> do
      getDataFromPKeysRedis meshCfg pKeys
    Left e -> return $ Left $ MRedisError e

------------- KEY UTILS ------------------

keyDelim:: Text
keyDelim = "_"

withCacheKeyPrefix :: MeshConfig -> Text -> Text
withCacheKeyPrefix meshCfg k = case meshCfg.cachePrefix of
  Just "" -> k
  Just prefix -> prefix <> EHT.tenantKVDelimeter <> k
  Nothing -> k

getKvKey :: MeshConfig -> Text -> PKvKey
getKvKey meshCfg k = case (meshCfg.prefixMigrationMode, meshCfg.cachePrefix) of
  (L.NEW_WRITE_DISABLED, _) -> PKvKey {_key = pKeyWithShard, prefixedKey = pKeyWithShard, _shard = shard}
  (_,              Nothing) -> PKvKey {_key = pKeyWithShard, prefixedKey = pKeyWithShard, _shard = shard}
  (_,               Just p) -> PKvKey {_key = pKeyWithShard, prefixedKey = p <> EHT.tenantKVDelimeter <> pKeyWithShard, _shard = shard}
  where
    pKeyWithShard = k <> shard
    shard = getShardedHashTag k

getKvSKey :: MeshConfig -> Text -> SKvKey
getKvSKey meshCfg k = case (meshCfg.prefixMigrationMode, meshCfg.cachePrefix) of
  (L.NEW_WRITE_DISABLED, _) -> SKvKey {sKey = k, prefixedSKey = k}
  (_,              Nothing) -> SKvKey {sKey = k, prefixedSKey = k}
  (_,               Just p) -> SKvKey {sKey = k, prefixedSKey = p <> EHT.tenantKVDelimeter <> k}


getLookupKeyByPKey :: forall table. (KVConnector (table Identity)) => Bool -> MeshConfig -> table Identity -> PKvKey
getLookupKeyByPKey isMySQL meshCfg table = do
  let tName = tableName @(table Identity)
  let (PKey k) = primaryKey isMySQL table
  let lookupKey = getSortedKey k
  getKvKey meshCfg $ tName <> keyDelim <> lookupKey

getSecondaryLookupKeys :: forall table. (KVConnector (table Identity)) => Bool -> MeshConfig -> table Identity -> [SKvKey]
getSecondaryLookupKeys isMySQL meshCfg table = do
  let tName = tableName @(table Identity)
  let skeys = secondaryKeysFiltered isMySQL table
  let tupList = map (\(SKey s) -> s) skeys
  let list = map (\x -> getKvSKey meshCfg $ tName <> keyDelim <> getSortedKey x ) tupList
  list

secondaryKeysFiltered :: forall table. (KVConnector (table Identity)) => Bool -> table Identity -> [SecondaryKey]
secondaryKeysFiltered isMySQL table = filter filterEmptyValues (secondaryKeys isMySQL table)
  where
    filterEmptyValues :: SecondaryKey -> Bool
    filterEmptyValues (SKey sKeyPairs) = not $ any (\p -> snd p == "") sKeyPairs

applyFPair :: (t -> b) -> (t, t) -> (b, b)
applyFPair f (x, y) = (f x, f y)

getPKeyAndValueList :: forall table beM. (HasCallStack, KVConnector (table Identity), A.ToJSON (table Identity)) => ModelDBConfig beM -> table Identity -> [(Text, A.Value)]
getPKeyAndValueList dbConf table = do
  let (PKey k) = primaryKey (isMySQLConfig dbConf) table
      keyValueList = map convertTuple $ sortBy (compare `on` fst) k
        where
          convertTuple :: (Text, Text) -> (AK.Key, Text)
          convertTuple (key, value) = (AK.fromText key, value)
      rowObject = A.toJSON table
  case rowObject of
    A.Object hm -> foldl' (\ acc x -> (go hm x) : acc) [] keyValueList
    _           -> error "Cannot work on row that isn't an Object"

  where
    go hm x = case KM.lookup (fst x) hm of
      Just val -> (AK.toText $ fst x, val)
      Nothing  -> error $ "Cannot find " <> AK.toText (fst x) <> " field in the row"

getSortedKey :: [(Text,Text)] -> Text
getSortedKey kvTup = do
  let sortArr = sortBy (compare `on` fst) kvTup
  let (appendedKeys, appendedValues) = applyFPair (T.intercalate "_") $ unzip sortArr
  appendedKeys <> "_" <> appendedValues

getShardedHashTag :: Text -> Text
getShardedHashTag key = do
  let slot = unsafeCoerce @_ @Word16 $ L.keyToSlot $ encodeUtf8 key
      streamShard = slot `mod` 128
  "{shard-" <> show streamShard <> "}"

addToETLSAndSQLLogStream :: forall table m. ((KVConnector (table Identity)), L.MonadFlow m, HasArtRecOptions) => MeshConfig -> Bool -> ETLStreamKeys -> KVEntry table -> m ()
addToETLSAndSQLLogStream meshCfg isMySQLConf key entry = do
  let kvKey = getLookupKeyByPKey isMySQLConf meshCfg entry.row
      pKey  = fromString . T.unpack $ kvKey.prefixedKey
  when meshCfg.shouldPushToSQLWriteLogsStream $ do
    void $ L.rXaddBWithART meshCfg.reconRedis (encodeUtf8 (getSQLWriteLogsStreamName <> kvKey._shard)) [(show key, pKey)] L.AutoID
  when meshCfg.shouldPushToETLStream $ do
    void $ L.rXaddBWithART meshCfg.kvRedis (encodeUtf8 (getETLStreamName <> kvKey._shard)) (getETLStreamVal meshCfg key kvKey) L.AutoID


addToDBSyncStreamETLStreamAndRedis :: forall table m. 
  ( KVConnector (table Identity)
  , Serialize (table Identity)
  , ToJSON (table Identity)
  , L.MonadFlow m
  , HasArtRecOptions
  )
  => MeshConfig
  -> Text
  -> [ByteString]
  -> ETLStreamKeys
  -> PKvKey
  -> KVEntry table
  -> m (KVDBAnswer (TxResult [L.KVDBStreamEntryID]))
addToDBSyncStreamETLStreamAndRedis meshCfg shard qCmds key pKvKey val = L.rMultiExecWithHashWithART meshCfg.kvRedis (encodeUtf8 shard) $ do
    when (meshCfg.prefixMigrationMode /= L.NEW_WRITE_DISABLED) $ void $
      L.setexTx (encodeUtf8 pKvKey.prefixedKey) meshCfg.redisTtl (BSL.toStrict $ Encoding.encode_ meshCfg.cerealEnabled val)
    when (meshCfg.prefixMigrationMode /= L.OLD_WRITE_DISABLED) $ void $
      L.setexTx (encodeUtf8 pKvKey._key) meshCfg.redisTtl (BSL.toStrict $ Encoding.encode_ meshCfg.cerealEnabled val)
    when meshCfg.shouldPushToETLStream $ void $ L.xaddTx
          (encodeUtf8 (getETLStreamName <> shard))
          L.AutoID
          (getETLStreamVal meshCfg key pKvKey)
    sequence <$> mapM (\qCmd -> (L.xaddTx (encodeUtf8 (meshCfg.ecRedisDBStream <> shard)) L.AutoID [("command", qCmd)])) qCmds

getETLStreamVal :: MeshConfig -> ETLStreamKeys -> PKvKey -> [KVDBStreamItem]
getETLStreamVal meshCfg key kvKey = case (meshCfg.prefixMigrationMode, meshCfg.cachePrefix) of
  (L.NEW_WRITE_DISABLED, _) -> [(show key, encodeUtf8 kvKey._key)]
  (_,          Nothing) -> [(show key, encodeUtf8 kvKey._key)]
  (_,          Just val) ->
    let cacheKeyPrefixB = encodeUtf8 val
    in [(show key, encodeUtf8 kvKey.prefixedKey),("tenant", cacheKeyPrefixB)]

incRedisCounterForCreates :: forall table m. 
  ( KVConnector (table Identity)
  , HasArtRecOptions
  , L.MonadFlow m
  )
  => table Identity
  -> MeshConfig
  -> m ()
incRedisCounterForCreates val meshCfg = when (meshCfg.shouldPushToETLStream && tableName @(table Identity) `elem` redisCounterEnabledTables) $
  case getCreateCounterKey @(table Identity) val of
    Just createCounterSuffix -> do
      let createCounterKey = tableName @(table Identity) <> "_counter_" <> createCounterSuffix
      incrRes <- TRL.rIncrWithART meshCfg.kvRedis createCounterKey
      case incrRes of
        Right 1 -> void $ TRL.rExpireWithART meshCfg.kvRedis createCounterKey redisCreateCounterTTLSec
        Right _ -> pure ()
        Left err -> L.logErrorV @Text "REDIS_CREATE_COUNTER_ERROR" (err)
    Nothing ->  L.logErrorT "REDIS_CREATE_COUNTER_ERROR" "Got empty res for create counter suffix"

------------------------------------------

getTableRowWithPrimaryId :: forall (table :: (Type -> Type) -> Type) m beM.
  (ToJSON (table Identity), FromJSON (table Identity), KVConnector (table Identity), L.MonadFlow m, HasArtRecOptions) 
  => ModelDBConfig beM -> MeshConfig -> Text -> table Identity ->  m (MeshResult (table Identity, IdSource))
getTableRowWithPrimaryId dbConf meshCfg tName tableRow = do
  let (PKey p) = primaryKey (isMySQLConfig dbConf) tableRow
  case p of
    [(field, _)] ->
      case A.toJSON tableRow of
        A.Object jsonObject ->
          if KM.member (AK.fromText field) jsonObject
            then pure $ Right (tableRow, SQL')
            else do
              (eitherPrimaryId, idSource) <- first (mapRight toIntegerId) <$>
                if meshCfg.snowFlakeEnabled
                  then (, SNOWFLAKE) <$> getSnowflakeValue tName
                  else
                    if (meshCfg.meshEnabled && not meshCfg.kvHardKilled) || (meshCfg.cleanDBEnabled && not meshCfg.cleanDBHardKilled)
                      then (, REDIS) <$> getPrimaryIdFromRedis meshCfg tName
                      else pure (Right . AutoPrimaryId $ Nothing, SQL')
              pure $ case eitherPrimaryId of
                Left err -> Left err
                Right (Just primaryId) -> mapRight (, idSource) (decodeJsonToTableRow (A.Object (KM.insert (AK.fromText field) (A.toJSON primaryId) jsonObject)))
                Right Nothing -> Right (tableRow, SQL')
        _ ->  pure $ Left $ MDecodingError "Can't set AutoIncId value of JSON which isn't a object."
    _ ->  pure $ Right (tableRow, SQL')
  where
    decodeJsonToTableRow :: A.Value -> MeshResult (table Identity)
    decodeJsonToTableRow = either (Left . MDecodingError) Right . resultToEither . A.fromJSON  

getSnowflakeValue :: forall m .(L.MonadFlow m) => Text -> m (MeshResult AutoPrimaryId)
getSnowflakeValue tName = go snowflakeGenerationRetryLimit
  where
    snowflakeGenerationRetryLimit :: Int
    snowflakeGenerationRetryLimit =
      let envType = Env.JuspayEnv
                      { key = "SNOWFLAKE_GENERATION_RETRY_LIMIT"
                      , actionLeft = Env.mkDefaultEnvAction (3 :: Int)
                      , decryptFunc = pure
                      , logWhenThrowException = Nothing
                      }
      in Env.lookupEnv envType

    snowflakeGenerationRetryDelay :: Integer
    snowflakeGenerationRetryDelay =
      let envType = Env.JuspayEnv
                      { key = "SNOWFLAKE_GENERATION_RETRY_DELAY"
                      , actionLeft = Env.mkDefaultEnvAction (500 :: Integer)
                      , decryptFunc = pure
                      , logWhenThrowException = Nothing
                      }
      in Env.lookupEnv envType

    go :: Int -> m (MeshResult AutoPrimaryId)
    go 0           = pure . Left . UnexpectedError $ "Could not generate snowflake value"
    go retriesLeft = do
      eitherSnowflakeId <- L.generateSnowflake $ T.unpack tName
      case eitherSnowflakeId of
        Left (Fatal err)           -> pure . Left . UnexpectedError $ err
        Left (NonFatal err) -> do
          L.logWarningT "getSnowflakeValue" err
          void $ ((L.runIOWithART "EulerHS.KVConnector.Utils::go::threadDelayMilisec" threadDelayMilisec) snowflakeGenerationRetryDelay :: m ())
          go $ retriesLeft - 1
        Right snowflake ->
          let
            autoPrimaryId :: Integer
            autoPrimaryId = toInteger snowflake
          in pure . Right . AutoPrimaryId $ if autoPrimaryId < 0
              then
                Nothing
              else
                Just autoPrimaryId

foldEither :: [Either a b] -> Either a [b]
foldEither []               = Right []
foldEither ((Left a) : _)   = Left a
foldEither ((Right b) : xs) = mapRight ((:) b) (foldEither xs)

resultToEither :: A.Result a -> Either Text a
resultToEither (A.Success res) = Right res
resultToEither (A.Error e)     = Left $ T.pack e

getUniqueDBRes :: KVConnector (table Identity) => ModelDBConfig beM -> MeshConfig -> [KVEntry table] -> [KVEntry table] -> [KVEntry table]
getUniqueDBRes dbConf meshCfg dbRows kvRows = do
  let isMySQL = isMySQLConfig dbConf
      kvPkeys = map (\r -> getLookupKeyByPKey isMySQL meshCfg r.row) kvRows
  filter (\r -> getLookupKeyByPKey isMySQL meshCfg r.row `notElem` kvPkeys) dbRows

getLatencyInMicroSeconds :: Integer -> Integer
getLatencyInMicroSeconds execTime = execTime `div` 1000000

isMySQLConfig :: ModelDBConfig beM -> Bool
isMySQLConfig (WithoutFallbackDB (MySQLPoolConf {})) = True
isMySQLConfig (WithFallbackDB (MySQLPoolConf {}) _ ) = True
isMySQLConfig (WithoutFallbackDB (MockingConf _ db _)) = db == "MOCK_SQL"
isMySQLConfig (WithFallbackDB (MockingConf _ db _) _ ) = db == "MOCK_SQL"
isMySQLConfig _ = False

---------------- Match where clauses -------------
findOneMatching :: (B.Beamable table, BeamRuntime be beM, BeamRunner beM) => Where be table -> ModelDBConfig beM -> [KVEntry table] -> Maybe (KVEntry table)
findOneMatching whereClause dbConf = find (matchWhereClause (isMySQLConfig dbConf) whereClause)

findAllMatching :: (B.Beamable table, BeamRuntime be beM, BeamRunner beM) => Where be table -> ModelDBConfig beM -> [KVEntry table] -> [KVEntry table]
findAllMatching whereClause dbConf = filter (matchWhereClause (isMySQLConfig dbConf) whereClause)

matchWhereClause :: B.Beamable table => Bool -> [Clause be table] -> KVEntry table -> Bool
matchWhereClause isMySQL whereClause entry = all matchClauseQuery whereClause
  where
  matchClauseQuery = \case
    And queries     -> all matchClauseQuery queries
    Or queries      -> any matchClauseQuery queries
    Is column' term ->
      let column = fromColumnar' . column' . columnize
        in termQueryMatch isMySQL (column entry.row) term

termQueryMatch :: (Ord value, ToJSON value) => Bool -> value -> Term be value -> Bool
termQueryMatch isMySQL columnVal = \case
  In literals             -> any (matchWithCaseInsensitive columnVal) literals
  Null                    -> isNothing columnVal
  Eq literal              -> matchWithCaseInsensitive columnVal literal
  GreaterThan literal     -> columnVal > literal
  GreaterThanOrEq literal -> columnVal >= literal
  LessThan literal        -> columnVal < literal
  LessThanOrEq literal    -> columnVal <= literal
  Not Null                -> isJust columnVal
  Not (Eq literal)        -> not $ matchWithCaseInsensitive columnVal literal
  Not term                -> not (termQueryMatch isMySQL columnVal term)
  _                       -> error "Term query not supported"

  where
    matchWithCaseInsensitive c1 c2 =
      if c1 == c2
        then True
        else if isMySQL -- Fallback to case insensitive check (MySQL supports this)
          then
            case (toJSON c1, toJSON c2) of
              (A.String s1, A.String s2) -> T.toLower s1 == T.toLower s2
              _ -> False
        else False

toPico :: Int -> Fixed.Pico
toPico value = Fixed.MkFixed $ ((toInteger value) * 1000000000000)

getStreamName :: String -> Text
getStreamName shard = getConfigStreamBasename <> "-" <> (T.pack shard) <> ""

getRandomStream :: forall m. (L.MonadFlow m) => m Text
getRandomStream = do
  streamShard <- L.randomRIO "EulerHS.KVConnector.Utils::getRandomStream::randomRIO" (1, getConfigStreamMaxShards)
  return $ getStreamName (show streamShard)

getConfigStreamNames :: [Text]
getConfigStreamNames = fmap (\shardNo -> getStreamName (show shardNo) ) [1..getConfigStreamMaxShards]

-- Adding negative offset in looper to handle replica lag case.
-- It cannot handle when replica lag is very high, which should be solved by config-kv.
getLooperCurrIdWithOffset :: (L.MonadFlow m) => m Text
getLooperCurrIdWithOffset = T.pack . show . (\t -> t - getConfigStreamOffsetInSeconds * 1000) <$> L.getCurrentDateInMillis

getConfigStreamBasename :: Text
getConfigStreamBasename = 
  let envType = Env.JuspayEnv
                 { key = "CONFIG_STREAM_BASE_NAME"
                 , actionLeft = Env.mkDefaultEnvAction ("ConfigStream" :: Text)
                 , decryptFunc = pure
                 , logWhenThrowException = Nothing
                 }
  in Env.lookupEnv envType

getConfigStreamMaxShards :: Int
getConfigStreamMaxShards =
  let envType = Env.JuspayEnv
                 { key = "CONFIG_STREAM_MAX_SHARDS"
                 , actionLeft = Env.mkDefaultEnvAction (20 :: Int)
                 , decryptFunc = pure
                 , logWhenThrowException = Nothing
                 }
  in Env.lookupEnv envType

getConfigStreamLooperDelayInSec :: Int
getConfigStreamLooperDelayInSec =
  let envType = Env.JuspayEnv
                 { key = "CONFIG_STREAM_LOOPER_DELAY_IN_SEC"
                 , actionLeft = Env.mkDefaultEnvAction (10 :: Int)
                 , decryptFunc = pure
                 , logWhenThrowException = Nothing
                 }
  in Env.lookupEnv envType

getConfigStreamOffsetInSeconds :: Int
getConfigStreamOffsetInSeconds =
  let envType = Env.JuspayEnv
                 { key = "CONFIG_STREAM_LOOPER_OFFSET_IN_SEC"
                 , actionLeft = Env.mkDefaultEnvAction (5 :: Int)
                 , decryptFunc = pure
                 , logWhenThrowException = Nothing
                 }
  in Env.lookupEnv envType

getConfigStreamFetchLimit :: Integer
getConfigStreamFetchLimit =
  let envType = Env.JuspayEnv
                 { key = "CONFIG_STREAM_LOOPER_FETCH_LIMIT"
                 , actionLeft = Env.mkDefaultEnvAction (500 :: Integer)
                 , decryptFunc = pure
                 , logWhenThrowException = Nothing
                 }
  in Env.lookupEnv envType

getConfigEntryTtlJitterInSeconds :: Int
getConfigEntryTtlJitterInSeconds =
  let envType = Env.JuspayEnv
                 { key = "CONFIG_STREAM_TTL_JITTER_IN_SEC"
                 , actionLeft = Env.mkDefaultEnvAction (5 :: Int)
                 , decryptFunc = pure
                 , logWhenThrowException = Nothing
                 }
  in Env.lookupEnv envType

getConfigEntryBaseTtlInSeconds :: Int
getConfigEntryBaseTtlInSeconds =
  let envType = Env.JuspayEnv
                 { key = "CONFIG_STREAM_BASE_TTL_IN_SEC"
                 , actionLeft = Env.mkDefaultEnvAction (300 :: Int)
                 , decryptFunc = pure
                 , logWhenThrowException = Nothing
                 }
  in Env.lookupEnv envType

getConfigEntryNullBaseTtlInSeconds :: Int
getConfigEntryNullBaseTtlInSeconds =
  let envType = Env.JuspayEnv
                 { key = "CONFIG_STREAM_NULL_BASE_TTL_IN_SEC"
                 , actionLeft = Env.mkDefaultEnvAction (240 :: Int)
                 , decryptFunc = pure
                 , logWhenThrowException = Nothing
                 }
  in Env.lookupEnv envType

recacheEnabledConfigTables :: [Text]
recacheEnabledConfigTables =
  let envType = Env.JuspayEnv
                 { key = "KV_RECACHE_ENABLED_CONFIG_TABLES"
                 , actionLeft = Env.mkDefaultEnvAction ([] :: [Text])
                 , decryptFunc = pure
                 , logWhenThrowException = Nothing
                 }
  in Env.lookupEnv envType

kvReadBlacklistedTables :: [Text]
kvReadBlacklistedTables = 
  let envType = Env.JuspayEnv
                 { key = "KV_READ_BLACKLISTED_TABLES"
                 , actionLeft = Env.mkDefaultEnvAction ([] :: [Text])
                 , decryptFunc = pure
                 , logWhenThrowException = Nothing
                 }
  in Env.lookupEnv envType

configKvTtl :: Integer
configKvTtl = 
  let envType = Env.JuspayEnv
                 { key = "CONFIG_KV_TTL"
                 , actionLeft = Env.mkDefaultEnvAction (10800 :: Integer)
                 , decryptFunc = pure
                 , logWhenThrowException = Nothing
                 }
  in Env.lookupEnv envType

secondaryKeyExpiryBuffer:: Integer -- Default 1 hours
secondaryKeyExpiryBuffer =
  let envType = Env.JuspayEnv
                 { key = "SECONDARY_KEY_EXPIRY_BUFFER"
                 , actionLeft = Env.mkDefaultEnvAction (3600 :: Integer)
                 , decryptFunc = pure
                 , logWhenThrowException = Nothing
                 }
  in Env.lookupEnv envType

getConfigEntryNewTtlIO :: IO POSIXTime
getConfigEntryNewTtlIO = do
    currentTime <- getPOSIXTime
    let
      jitterInSec = getConfigEntryTtlJitterInSeconds
      baseTtlInSec = getConfigEntryBaseTtlInSeconds
      t = round currentTime  :: Int
    noise <- randomRIO (1, jitterInSec)
    return $ fromIntegral (baseTtlInSec + noise +  t)

getConfigNullEntryNewTtlIO :: IO POSIXTime
getConfigNullEntryNewTtlIO = do
    currentTime <- getPOSIXTime
    let
      baseTtlInSec = getConfigEntryNullBaseTtlInSeconds
      t = round currentTime  :: Int
    return $ fromIntegral (baseTtlInSec +  t)

getConfigEntryNewTtl :: (L.MonadFlow m) => m POSIXTime
getConfigEntryNewTtl = L.runIOWithART "EulerHS.KVConnector.Utils::getConfigEntryNewTtl::getConfigEntryNewTtlIO" getConfigEntryNewTtlIO

getConfigNullEntryNewTtl :: (L.MonadFlow m) => m POSIXTime
getConfigNullEntryNewTtl = L.runIOWithART "EulerHS.KVConnector.Utils::getConfigNullEntryNewTtl::getConfigNullEntryNewTtlIO" getConfigNullEntryNewTtlIO

threadDelayMilisec :: Integer -> IO ()
threadDelayMilisec ms = threadDelay $ fromIntegral ms * 1000

meshModelTableEntityDescriptor ::
  forall table be.
  (Model be table, MeshMeta be table) =>
  B.DatabaseEntityDescriptor be (B.TableEntity table)
meshModelTableEntityDescriptor = let B.DatabaseEntity x = (meshModelTableEntity @table) in x

meshModelTableEntity ::
  forall table be db.
  (Model be table, MeshMeta be table) =>
  B.DatabaseEntity be db (B.TableEntity table)
meshModelTableEntity =
  let B.EntityModification modification = B.modifyTableFields (meshModelFieldModification @be @table)
  in appEndo modification $ B.DatabaseEntity $ B.dbEntityAuto (modelTableName @table)

toPSJSON :: forall be table. MeshMeta be table => (Text, A.Value) -> (Text, A.Value)
toPSJSON (k, v) = (k, Map.findWithDefault id k (valueMapper @be @table) v)

decodeToField :: forall table. (FromJSON (table Identity), Serialize.Serialize (table Identity)) => BSL.ByteString -> (MeshResult (KVEntry table), Bool)
decodeToField val =
  let decodeRes = Encoding.decodeLiveOrDead val
    in  case decodeRes of
          (isLive, byteString) ->
            let decodedMeshResult =
                        let (h, v) = BSL.splitAt 4 byteString
                          in case h of
                                "CBOR" -> case Cereal.decodeLazy v of
                                            Right r' -> Right r'
                                            Left _ -> case Cereal.decodeLazy v of
                                                        Right r'' -> Right r''
                                                        Left _ -> case Cereal.decodeLazy v of
                                                                      Right r''' -> Right r'''
                                                                      Left err' -> Left $ MDecodingError $ T.pack err'
                                "JSON" ->
                                  case A.eitherDecode v of
                                    Right r' -> Right $ getKVEntryWithDefaultDB r'
                                    Left e   -> Left $ MDecodingError $ T.pack e
                                  
                                "JSV1" -> mapLeft (MDecodingError . T.pack) (A.eitherDecode v)

                                _      -> mapLeft (MDecodingError . T.pack) (A.eitherDecode val)
              in (decodedMeshResult, isLive)

getKVEntryWithDefaultDB :: table Identity -> KVEntry table
getKVEntryWithDefaultDB row = KVEntry ECRDB row Nothing

getKVEntryWithTrackerDB :: table Identity -> KVEntry table
getKVEntryWithTrackerDB row = KVEntry TRACKERDB row Nothing

getDBName :: MeshConfig -> DBName -- This function is only to be used for creation with clean db
getDBName meshCfg = if meshCfg.cleanDBEnabled && (not meshCfg.cleanDBHardKilled) then TRACKERDB else ECRDB

getDualWriteSchemaName :: L.MonadFlow m =>  Text -> Maybe L.TenantDBMigrationMode -> Text -> m (Maybe Text)
getDualWriteSchemaName currentSchemaName dbMigrationMode dbPrefix = do
  pure $ case dbMigrationMode of
    Just mode | mode `elem` [L.DB_DUAL_WRITE] -> Just $ dbPrefix <> dbPrefixDelimeter <> currentSchemaName
    Just mode | mode `elem` [L.NEW_DB_READ_ENABLED] -> T.stripPrefix (dbPrefix <> dbPrefixDelimeter) currentSchemaName
    _ -> Nothing

dbPrefixDelimeter :: Text
dbPrefixDelimeter = "_"

getSchemaNameWithMigrationDbConf 
  :: L.MonadFlow m 
  => ModelDBConfig beM 
  -> DBName 
  -> MeshConfig 
  -> m ([Text], Maybe Text)
getSchemaNameWithMigrationDbConf dbConf dbName meshConfig = do
  let schemaName = case dbConf of
        WithFallbackDB oldDbConf newDbConf -> if dbName == TRACKERDB then getSchemaName newDbConf else getSchemaName oldDbConf
        WithoutFallbackDB dbConfig          -> getSchemaName dbConfig
  tenantVal <- L.getOptionLocal L.TenantConfigObj
  let dbMigrationMode = L.databaseMigrationMode =<< (L.migrationConfig =<< tenantVal)
      dbPrefixForRead = if dbMigrationMode `elem` [Just L.NEW_DB_DISABLED, Just L.DB_DUAL_WRITE] then Nothing else meshConfig.dbPrefix
  migSchemaName <- maybe (pure Nothing) (\val -> getDualWriteSchemaName schemaName dbMigrationMode val) meshConfig.dbPrefix
  pure $ ((schemaName : catMaybes [migSchemaName]), dbPrefixForRead)

instance SSerialize.Serialize a => SSerialize.Serialize (Term be a) where
    addSizeTo i (In val) = 1 + SSerialize.addSizeTo i val
    addSizeTo i (Eq val) = 1 + SSerialize.addSizeTo i val
    addSizeTo i Null = i + 1
    addSizeTo i (GreaterThan val) = 1 + SSerialize.addSizeTo i val
    addSizeTo i (GreaterThanOrEq val) = 1 + SSerialize.addSizeTo i val
    addSizeTo i (LessThan val) = 1 + SSerialize.addSizeTo i val
    addSizeTo i (LessThanOrEq val) = 1 + SSerialize.addSizeTo i val
    addSizeTo i (Like val) = 1 + SSerialize.addSizeTo i val
    addSizeTo i (Not val) = 1 + SSerialize.addSizeTo i val

    serializeAt i marr (In val) = do
        i1 <- SSerialize.serializeAt i marr (0 :: Word8)
        SSerialize.serializeAt i1 marr val
    serializeAt i marr (Eq val) = do
        i1 <- SSerialize.serializeAt i marr (1 :: Word8)
        SSerialize.serializeAt i1 marr val
    serializeAt i marr Null = SSerialize.serializeAt i marr (2 :: Word8)
    serializeAt i marr (GreaterThan val) = do
        i1 <- SSerialize.serializeAt i marr (3 :: Word8)
        SSerialize.serializeAt i1 marr val
    serializeAt i marr (GreaterThanOrEq val) = do
        i1 <- SSerialize.serializeAt i marr (4 :: Word8)
        SSerialize.serializeAt i1 marr val
    serializeAt i marr (LessThan val) = do
        i1 <- SSerialize.serializeAt i marr (5 :: Word8)
        SSerialize.serializeAt i1 marr val
    serializeAt i marr (LessThanOrEq val) = do
        i1 <- SSerialize.serializeAt i marr (6 :: Word8)
        SSerialize.serializeAt i1 marr val
    serializeAt i marr (Like val) = do
        i1 <- SSerialize.serializeAt i marr (7 :: Word8)
        SSerialize.serializeAt i1 marr val
    serializeAt i marr (Not val) = do
        i1 <- SSerialize.serializeAt i marr (8 :: Word8)
        SSerialize.serializeAt i1 marr val

    deserializeAt _ _ _ = error "deserialize Term is Unimplemented"

instance MeshMeta be table => SSerialize.Serialize (Clause be table) where

    addSizeTo i (And clauseList) = 1 + SSerialize.addSizeTo i clauseList
    addSizeTo i (Or clauseList) = 1 + SSerialize.addSizeTo i clauseList
    addSizeTo i (Is column term) =
      let key = B._fieldName . fromColumnar' . column $ columnized @be @table
         in 1 + SSerialize.addSizeTo (SSerialize.addSizeTo i key) term

    serializeAt i marr (And val) = do
        i1 <- SSerialize.serializeAt i marr (0 :: Word8)
        SSerialize.serializeAt i1 marr val
    serializeAt i marr (Or val) = do
        i1 <- SSerialize.serializeAt i marr (1 :: Word8)
        SSerialize.serializeAt i1 marr val
    serializeAt i marr (Is column term) = do
        let key = B._fieldName . fromColumnar' . column $ columnized @be @table
        i1 <- SSerialize.serializeAt i marr (2 :: Word8)
        i2 <- SSerialize.serializeAt i1 marr key
        SSerialize.serializeAt i2 marr term

    deserializeAt _ _ _ = error "deserialize Clause is Unimplemented"

{-# INLINE getInMemCacheKeyFromWhereClause #-}
getInMemCacheKeyFromWhereClause :: forall table be beM. (Model be table, MeshMeta be table) =>
  ModelDBConfig beM -> Text -> B.DatabaseEntityDescriptor be (B.TableEntity table) -> Clause be table -> W8Arr
getInMemCacheKeyFromWhereClause _ pre _ clause = Array.serialize (pre, clause)

{-# INLINABLE mkTrackerKey #-}
mkTrackerKey :: Text -> Text
mkTrackerKey kVal = "key_" <> kVal

-- getFieldsAndValuesFromClause dt (And [Is DBS.id (Eq (Just 1)), Or [Is DBS.merchantId (Eq (Just "123")), Is DBS.orderId (Eq (Just "oid"))]])
-- [[("id","Just 1"),("merchantId","Just \"123\"")],[("id","Just 1"),("orderId","Just \"oid\"")]]

{-# INLINE getFieldsAndValuesFromClause #-}
getFieldsAndValuesFromClause :: forall table be beM. (Model be table, MeshMeta be table) =>
  ModelDBConfig beM -> B.DatabaseEntityDescriptor be (B.TableEntity table) -> Clause be table -> [[(Text, Text)]]
getFieldsAndValuesFromClause dbConf dt = \case
  And cs -> foldl' processAnd [[]] $ map (getFieldsAndValuesFromClause dbConf dt) cs
  Or cs -> processOr cs
  Is column (Eq val) -> do
    let !key = B._fieldName . fromColumnar' . column $ columnized @be @table
    [[(key, showVal . snd $ (toPSJSON @be @table) (key, A.toJSON val))]]
  Is column (In vals) -> do
    let !key = B._fieldName . fromColumnar' . column $ columnized @be @table
    map (\val -> [(key, showVal . snd $ (toPSJSON @be @table) (key, A.toJSON val))]) vals
  _ -> []

  where
    processAnd xs [] = xs
    processAnd [] ys = ys
    processAnd xs ys = [x ++ y | x <-xs, y <- ys]
    processOr xs = concatMap (getFieldsAndValuesFromClause dbConf dt) xs

    showVal res = case res of
      A.String r -> if (isMySQLConfig dbConf) then T.toLower r else r
      A.Number n -> T.pack $ show n
      A.Array l  -> T.pack $ show l
      A.Object o -> T.pack $ show o
      A.Bool b -> T.pack $ show b
      A.Null -> T.pack ""

getPrimaryKeyFromFieldsAndValues :: (L.MonadFlow m, HasCallStack, HasArtRecOptions) => Text -> MeshConfig -> KM.KeyMap Bool -> [(Text, Text)] -> m (MeshResult [ByteString])
getPrimaryKeyFromFieldsAndValues _ _ _ [] = pure $ Right []
getPrimaryKeyFromFieldsAndValues modelName meshCfg keyHashMap fieldsAndValues = do
  res <- foldEither <$> mapM getPrimaryKeyFromFieldAndValueHelper fieldsAndValues
  pure $ mapRight (intersectList . catMaybes) res
  where

    getPrimaryKeyFromFieldAndValueHelper (k, v) = do
      let constructedKey = modelName <> "_" <> k <> "_" <> v
      case KM.lookup (AK.fromText k) keyHashMap of
        Just True -> do
          let pKvKey = getKvKey meshCfg constructedKey
              finalKey = if meshCfg.prefixMigrationMode `elem` [L.NEW_READ_ENABLED, L.OLD_WRITE_DISABLED, L.OLD_WRITE_ENABLED]
                then pKvKey.prefixedKey
                else pKvKey._key
          pure $ Right $ Just [fromString $ T.unpack finalKey]
        Just False -> do
          let sKvKey = getKvSKey meshCfg constructedKey
              finalKey = if meshCfg.prefixMigrationMode `elem` [L.NEW_READ_ENABLED, L.OLD_WRITE_DISABLED, L.OLD_WRITE_ENABLED]
                then sKvKey.prefixedSKey
                else sKvKey.sKey
          res <- L.rSmembersBWithART meshCfg.kvRedis (fromString $ T.unpack finalKey)
          case res of
            Right [] -> pure $ Right Nothing
            Right r -> pure $ Right $ Just r
            Left e  -> pure $ Left $ MRedisError e
        _ -> pure $ Right Nothing

    intersectList (x : y : xs) = intersectList (intersect x y : xs)
    intersectList (x : [])     = x
    intersectList []           = []

filterPrimaryAndSecondaryKeys :: KM.KeyMap Bool -> [(Text, Text)] -> [(Text, Text)]
filterPrimaryAndSecondaryKeys keyHashMap = filter (\(k, _) -> KM.member (AK.fromText k) keyHashMap)

getSecondaryKeys :: KM.KeyMap Bool -> [(Text, Text)] -> [(Text, Text)]
getSecondaryKeys keyHashMap =  filter (\(k, _) -> KM.lookup (AK.fromText k) keyHashMap == Just False)

mkUniq :: Ord a => [a] -> [a] -- O(n log n)
mkUniq = Set.toList . Set.fromList

-- >>> map (T.intercalate "_") (nonEmptySubsequences ["id", "id2", "id3"])
-- ["id","id2","id_id2","id3","id_id3","id2_id3","id_id2_id3"]
nonEmptySubsequences         :: [Text] -> [[Text]]
nonEmptySubsequences []      =  []
nonEmptySubsequences (x:xs)  =  [x]: foldr f [] (nonEmptySubsequences xs)
  where f ys r = ys : (x : ys) : r

whereClauseDiffCheck :: forall be table m beM. 
  ( L.MonadFlow m
  , Model be table
  , MeshMeta be table
  , KVConnector (table Identity)
  ) =>
  ModelDBConfig beM -> Where be table -> m (Maybe [[Text]])
whereClauseDiffCheck dbConf whereClause = 
  if isWhereClauseDiffCheckEnabled then do
    let keyAndValueCombinations = getFieldsAndValuesFromClause dbConf meshModelTableEntityDescriptor (And whereClause)
        andCombinations = map (map convertTuple) $ map (uncurry zip . applyFPair (map (T.intercalate "_") . sortOn (Down . length) . nonEmptySubsequences) . unzip . sort) keyAndValueCombinations
          where
            convertTuple :: (Text, Text) -> (AK.Key, Text)
            convertTuple (key, value) = (AK.fromText key, value)
        keyHashMap = keyMap @(table Identity)
        failedKeys = catMaybes $ map (atMay keyAndValueCombinations) $ findIndices (checkForPrimaryOrSecondary keyHashMap) andCombinations
    if (not $ null failedKeys)
      then do
        let diffRes = map (map fst) failedKeys
        if null $ concat diffRes
          then pure Nothing
          else L.logInfoV @Text "WHERE_DIFF_CHECK" (tableName @(table Identity) <> ": ", diffRes) $> Just diffRes
      else pure Nothing
  else pure Nothing
  where
    checkForPrimaryOrSecondary _ [] = True
    checkForPrimaryOrSecondary keyHashMap ((k, _) : xs) =
      case KM.member k keyHashMap of
        True -> False
        _    -> checkForPrimaryOrSecondary keyHashMap xs

isWhereClauseDiffCheckEnabled :: Bool
isWhereClauseDiffCheckEnabled =
  let envType = Env.JuspayEnv
                 { key = "IS_WHERE_CLAUSE_DIFF_CHECK_ENABLED"
                 , actionLeft = Env.mkDefaultEnvAction (True :: Bool)
                 , decryptFunc = pure
                 , logWhenThrowException = Nothing
                 }
  in Env.lookupEnv envType

isRecachingEnabled :: Bool
isRecachingEnabled =
  let envType = Env.JuspayEnv
                 { key = "IS_RECACHING_ENABLED"
                 , actionLeft = Env.mkDefaultEnvAction (False :: Bool)
                 , decryptFunc = pure
                 , logWhenThrowException = Nothing
                 }
  in Env.lookupEnv envType

shouldLogFindDBCallLogs :: Bool
shouldLogFindDBCallLogs = 
  let envType = Env.JuspayEnv
                 { key = "IS_FIND_DB_LOGS_ENABLED"
                 , actionLeft = Env.mkDefaultEnvAction (False :: Bool)
                 , decryptFunc = pure
                 , logWhenThrowException = Nothing
                 }
  in Env.lookupEnv envType

logAndIncrementIMCMetric :: Bool
logAndIncrementIMCMetric =
  let envType = Env.JuspayEnv
                 { key = "IS_LOG_AND_INC_IMC_METRIC_ENABLED"
                 , actionLeft = Env.mkDefaultEnvAction (False :: Bool)
                 , decryptFunc = pure
                 , logWhenThrowException = Nothing
                 }
  in Env.lookupEnv envType

shouldLogAndIncrementKVMetric :: Source -> Bool
shouldLogAndIncrementKVMetric = \case
  IN_MEM -> logAndIncrementIMCMetric
  _ -> True

isLogsEnabledForModel :: Text -> Bool
isLogsEnabledForModel modelName =
    let envType = Env.JuspayEnv
            { key = "NODE_ENV"
            , actionLeft = Env.mkDefaultEnvAction ("development" :: Text)
            , decryptFunc = pure
            , logWhenThrowException = Nothing
            }
        env = Env.lookupEnv envType
    in if env == ("production" :: Text)
        then
          let modelListType = Env.JuspayEnv
                    { key = "IS_LOGS_ENABLED_FOR_MODEL"
                    , actionLeft = Env.mkDefaultEnvAction ([] :: [Text])
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
              enableModelList :: [Text] = Env.lookupEnv modelListType
          in modelName `elem` enableModelList
        else True

logAndIncrementKVMetric :: (L.MonadFlow m, ToJSON a) => Maybe Text -> Bool -> Text -> Operation -> MeshResult a -> Maybe Int -> Text -> Source -> Maybe IdSource ->  Maybe [[Text]] -> m ()
logAndIncrementKVMetric connTag shouldLogData action operation res latency model source idSource mbDiffCheckRes = do
  apiTag <- L.getOptionLocal ApiTag
  mid    <- L.getOptionLocal MerchantID
  let shouldLogData_  = isLogsEnabledForModel model && shouldLogData
  let dblog = DBLogEntry {
      _log_type     = "DB"
    , _action       = action -- For logprocessor
    , _operation    = operation
    , _data         = case res of
                        Left err -> A.String (T.pack $ show err)
                        Right m  -> if shouldLogData_ then A.toJSON m else A.Null
    , _latency      = latency
    , _model        = model
    , _source       = source
    , _apiTag       = apiTag
    , _merchant_id  = mid
    , _whereDiffCheckRes = mbDiffCheckRes
    , _idSource    =  idSource
    , _connTag = connTag
    }
  case res of
    Left err ->
      logDb Log.Error ("DB" :: Text) source action model latency dblog (Just err)
    Right _ -> 
      if action == "FIND" then
        when shouldLogFindDBCallLogs $ logDb Log.Debug ("DB" :: Text) source action model latency dblog Nothing
        else logDb Log.Info ("DB" :: Text) source action model latency dblog Nothing
  when (source == KV) $ L.setLoggerContext "PROCESSED_THROUGH_KV" "True"
  incrementMetric KVAction dblog (isLeft res) (fromMaybe "" connTag)

logDb :: (L.MonadFlow m, ToJSON val) => Log.LogLevel -> Text -> Source -> Log.Action -> Log.Entity -> Maybe Int -> val -> Maybe MeshError -> m ()
logDb logLevel tag source action entity latency message maybeMeshError =
  L.evalLogger' $ L.masterLogger logLevel tag category (Just action) Nothing Nothing (Just entity) (getErrorLog <$> maybeMeshError) (toInteger <$> latency) Nothing Nothing Nothing Nothing Nothing (Log.Message Nothing (Just $ A.toJSON message)) Nothing
  where
    getErrorLog (MKeyNotFound k) = Log.ErrorL Nothing "MKeyNotFound" k
    getErrorLog (MDBError err) = Log.ErrorL Nothing "DBError" (T.pack $ show err)
    getErrorLog (MRedisError err) = Log.ErrorL Nothing "RedisError" (T.pack $ show err)
    getErrorLog (MDecodingError v) = Log.ErrorL Nothing "MDecodingError" v
    getErrorLog (MUpdateFailed v) = Log.ErrorL Nothing "MUpdateFailed" v
    getErrorLog (MMultipleKeysFound v) = Log.ErrorL Nothing "MMultipleKeysFound" v
    getErrorLog (UnexpectedError v) = Log.ErrorL Nothing "UnknownMeshError" v
    category
      | source == KV = "REDIS"
      | source == SQL = "DB"
      | source == KV_AND_SQL = "REDIS_AND_DB"
      | source == IN_MEM = "INMEM"
      | otherwise = ""

lengthOfLists :: [[a]] -> Int
lengthOfLists = foldl' (\acc el -> acc + length el) 0

withRedisLimit :: (L.MonadFlow m, Show b, ToJSON b) => Text -> Text -> [[b]] -> Maybe TableType -> m (MeshResult a) -> m (MeshResult a)
withRedisLimit tag modelName sKeys tableType redisFunc = do
  let expectedRedisCalls = if modelName `elem` tablesWithoutRedisLimit then 0 else sum $ length <$> sKeys
  if expectedRedisCalls > redisCallsHardLimit
    then do
      when logSecondaryKeys $ L.logDebugV ("Secondary Keys - Hard limit exceeded" :: Text) (toJSON sKeys)
      incrementRedisLimitMetric expectedRedisCalls
      pure $ Left $ UnexpectedError ("Redis Calls Limit Exceeded with length " <> show expectedRedisCalls)
    else if expectedRedisCalls > redisCallsSoftLimit
      then do
      when logSecondaryKeys $ L.logDebugV ("Secondary Keys - Soft limit exceeded" :: Text) (toJSON sKeys)
      incrementRedisLimitMetric expectedRedisCalls
      if isConfigTable tableType then pure . Left $ UnexpectedError ("Redis Calls Soft Exceeded with length " <> show expectedRedisCalls) else redisFunc
    else redisFunc
  where
    incrementRedisLimitMetric expectedRedisCall = incrementRedisCallMetric tag modelName expectedRedisCall (expectedRedisCall > redisCallsSoftLimit) (expectedRedisCall > redisCallsHardLimit)

getETLStreamName :: Text 
getETLStreamName = 
  let envType = Env.JuspayEnv
                 { key = "ETL_STREAM_NAME"
                 , actionLeft = Env.mkDefaultEnvAction ("etl-stream" :: Text)
                 , decryptFunc = pure
                 , logWhenThrowException = Nothing
                 }
  in Env.lookupEnv envType

logSecondaryKeys :: Bool 
logSecondaryKeys = 
  let envType = Env.JuspayEnv
                  { key = "LOG_SECONDARY_KEYS"
                  , actionLeft = Env.mkDefaultEnvAction (False :: Bool)
                  , decryptFunc = pure
                  , logWhenThrowException = Nothing
                  }
  in Env.lookupEnv envType
  

getSQLWriteLogsStreamName :: Text 
getSQLWriteLogsStreamName =
  let envType = Env.JuspayEnv
                 { key = "SQLWL_STREAM_NAME"
                 , actionLeft = Env.mkDefaultEnvAction ("sql-write-logs-stream" :: Text)
                 , decryptFunc = pure
                 , logWhenThrowException = Nothing
                 }
  in Env.lookupEnv envType

redisCallsHardLimit :: Int
redisCallsHardLimit =
  let envType = Env.JuspayEnv
                 { key = "REDIS_CALLS_HARD_LIMIT"
                 , actionLeft = Env.mkDefaultEnvAction (5000 :: Int)
                 , decryptFunc = pure
                 , logWhenThrowException = Nothing
                 }
  in Env.lookupEnv envType

redisCallsSoftLimit :: Int
redisCallsSoftLimit =
  let envType = Env.JuspayEnv
                 { key = "REDIS_CALLS_SOFT_LIMIT"
                 , actionLeft = Env.mkDefaultEnvAction (200 :: Int)
                 , decryptFunc = pure
                 , logWhenThrowException = Nothing
                 }
  in Env.lookupEnv envType

streamId2Double :: L.KVDBStreamEntryID -> Double
streamId2Double (L.KVDBStreamEntryID key seqn) = fromIntegral key + (doubleStreamIdPrecision * fromIntegral seqn)

double2StreamId :: Double -> L.KVDBStreamEntryID
double2StreamId doubleId = do
  let intID = floor (doubleId * fromIntegral doubleTointegerStreamIdPrecision)
  L.KVDBStreamEntryID (intID `div` doubleTointegerStreamIdPrecision) (intID `mod` doubleTointegerStreamIdPrecision)

doubleTointegerStreamIdPrecision :: Integer
doubleTointegerStreamIdPrecision = 1000

doubleStreamIdPrecision :: Double
doubleStreamIdPrecision = 0.001

-- Convert from String to KVDBStreamEntryID
parseKVDBStreamEntryIDFromString :: Text -> Maybe L.KVDBStreamEntryID
parseKVDBStreamEntryIDFromString str = 
  case T.split (== '-') str of
    [timestampStr, sequenceStr] -> liftM2 L.KVDBStreamEntryID (readMaybe timestampStr) (readMaybe sequenceStr)
    _ -> Nothing

kvdbStreamEntryIDToString :: L.KVDBStreamEntryID -> Text
kvdbStreamEntryIDToString (L.KVDBStreamEntryID timestamp sequence') =
  T.pack $ show timestamp ++ "-" ++ show sequence'

tablesWithoutRedisLimit :: [Text]
tablesWithoutRedisLimit =
  let envType = Env.JuspayEnv
                 { key = "TABLES_WITHOUT_REDIS_LIMIT"
                 , actionLeft = Env.mkDefaultEnvAction ([] :: [Text])
                 , decryptFunc = pure
                 , logWhenThrowException = Nothing
                 }
  in Env.lookupEnv envType

inMemFindAllCacheLimit :: Int
inMemFindAllCacheLimit =
  let envType = Env.JuspayEnv
                 { key = "INMEM_FINDALL_LIMIT"
                 , actionLeft = Env.mkDefaultEnvAction (20 :: Int)
                 , decryptFunc = pure
                 , logWhenThrowException = Nothing
                 }
  in Env.lookupEnv envType

redisCounterEnabledTables :: [Text]
redisCounterEnabledTables =
  let envType = Env.JuspayEnv
                 { key = "REDIS_COUNTER_ENABLED_TABLES"
                 , actionLeft = Env.mkDefaultEnvAction ([] :: [Text])
                 , decryptFunc = pure
                 , logWhenThrowException = Nothing
                 }
  in Env.lookupEnv envType

redisCreateCounterTTLSec :: Integer -- Default 24 hours
redisCreateCounterTTLSec =
  let envType = Env.JuspayEnv
                 { key = "REDIS_CREATE_COUNTER_TTL_IN_SEC"
                 , actionLeft = Env.mkDefaultEnvAction (86400 :: Integer)
                 , decryptFunc = pure
                 , logWhenThrowException = Nothing
                 }
  in Env.lookupEnv envType


---------------- Redis migration helpers --------------------

getPrimaryIdFromRedis :: (L.MonadFlow m, HasArtRecOptions) => MeshConfig -> Text -> m (MeshResult AutoPrimaryId)
getPrimaryIdFromRedis meshCfg tName = do
  let key = (T.pack . quietSnake . T.unpack) tName <> "_auto_increment_id"
      finalKey = if meshCfg.prefixMigrationMode `elem` [L.NEW_READ_ENABLED, L.OLD_WRITE_DISABLED, L.OLD_WRITE_ENABLED]
          then withCacheKeyPrefix meshCfg key
          else key
  (L.rIncrWithART meshCfg.kvRedis finalKey) <&> either failure (success . Just)
  where
    success = Right . AutoPrimaryId
    failure = Left . MRedisError

createSecondaryKeyMapping :: (L.MonadFlow m, KVConnector (table Identity), HasArtRecOptions) => Bool -> MeshConfig -> PKvKey -> table Identity -> m [Either KVDBReply Bool]
createSecondaryKeyMapping isMySQL meshCfg pKey row = do
  mapM (addPkeyInSecondaryKey meshCfg pKey) $ getSecondaryLookupKeys isMySQL meshCfg row


withRedisMigrationWrite :: L.MonadFlow m => MeshConfig -> t -> t -> (t -> m (Either e a)) -> m (Either e a)
withRedisMigrationWrite meshCfg inputWithoutPrefix inputWithPrefix act = do
  case meshCfg.prefixMigrationMode of
    L.NEW_WRITE_DISABLED -> act inputWithoutPrefix
    L.OLD_WRITE_DISABLED -> act inputWithPrefix
    L.DUAL_WRITE -> runExceptT $ do
      _ <- ExceptT $ act inputWithPrefix
      ExceptT $ act inputWithoutPrefix
    _ -> runExceptT $ do -- NEW_READ_ENABLED || OLD_WRITE_ENABLED
      _ <- ExceptT $ act inputWithoutPrefix
      ExceptT $ act inputWithPrefix

addPkeyInSecondaryKey :: (L.MonadFlow m, HasArtRecOptions) => MeshConfig -> PKvKey -> SKvKey -> m (Either KVDBReply Bool)
addPkeyInSecondaryKey meshCfg pKey sKey = withRedisMigrationWrite meshCfg (pKey._key, sKey.sKey) (pKey.prefixedKey, sKey.prefixedSKey) (\(pKey', sKey') -> do
    res <- L.rMultiExec meshCfg.kvRedis $ do  
        void $ L.saddTx (fromString $ T.unpack sKey') [encodeUtf8 pKey']
        L.expireTx (fromString $ T.unpack sKey') (meshCfg.redisTtl + secondaryKeyExpiryBuffer)
    case res of
        Left err -> pure $ Left err
        Right  _ -> pure $ Right True)

removePKeyInSecondaryKey :: (L.MonadFlow m, HasArtRecOptions) => MeshConfig -> PKvKey -> SKvKey -> m (MeshResult Integer)
removePKeyInSecondaryKey meshCfg pKey sKey = withRedisMigrationWrite meshCfg (pKey._key, sKey.sKey) (pKey.prefixedKey, sKey.prefixedSKey) (\(pKey', sKey') -> do
  mapLeft MRedisError <$> L.sRemBWithART meshCfg.kvRedis (fromString $ T.unpack sKey') [encodeUtf8 pKey'])

resetTTLInUpdate :: (L.MonadFlow m, HasArtRecOptions) => MeshConfig -> PKvKey -> SKvKey -> Bool -> m (Either KVDBReply Bool)
resetTTLInUpdate meshCfg pKey sKey retainTtl = do
  if retainTtl then
    pure $ Right True
  else do
    res <- 
      if meshCfg.prefixMigrationMode `elem` [L.DUAL_WRITE, L.NEW_READ_ENABLED]
        then L.rSaddWithART meshCfg.kvRedis (fromString $ T.unpack sKey.prefixedSKey) [encodeUtf8 pKey.prefixedKey]
        else if meshCfg.prefixMigrationMode == L.OLD_WRITE_ENABLED
          then L.rSaddWithART meshCfg.kvRedis (fromString $ T.unpack sKey.sKey) [encodeUtf8 pKey._key]
          else pure $ Right 0
    case res of
      Left err -> pure $ Left err
      Right _ -> withRedisMigrationWrite meshCfg sKey.sKey sKey.prefixedSKey (\sKey' -> L.rExpireWithART meshCfg.kvRedis (fromString $ T.unpack sKey') (meshCfg.redisTtl + secondaryKeyExpiryBuffer))
isRecacheEnabledConfigTable :: forall table. ModelMeta table => Bool
isRecacheEnabledConfigTable = isConfigTable (modelTableType @table) && elem (modelTableName @table) recacheEnabledConfigTables

isKvReadBlackListedTable :: forall table. ModelMeta table => Bool
isKvReadBlackListedTable = isConfigTable (modelTableType @table) && elem (modelTableName @table) kvReadBlacklistedTables

isConfigTable :: Maybe TableType -> Bool
isConfigTable tableT = elem tableT [Just CONFIG, Just COMMON_CONFIG]
