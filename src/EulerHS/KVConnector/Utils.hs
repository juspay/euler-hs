{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}


module EulerHS.KVConnector.Utils where

import qualified Data.Aeson as A
-- import           Data.Aeson (Value (Object), object)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import           Data.List (findIndices, intersect)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Database.Beam as B
import qualified Database.Beam.Schema.Tables as B
import qualified EulerHS.KVConnector.Encoding as Encoding
import           EulerHS.KVConnector.Metrics (incrementMetric, KVMetric(..),incrementRedisCallMetric)
import           EulerHS.KVConnector.Types (AutoPrimaryId(..), MeshMeta(..), MeshResult, MeshConfig, KVConnector(..), PrimaryKey(..), SecondaryKey(..),
                    DBLogEntry(..), Source(..), MerchantID(..), ETLStreamKeys)
import qualified EulerHS.Language as L
import           EulerHS.Prelude
import           Text.Casing (quietSnake)
-- import           Servant (err500)
import           Data.Either.Extra (mapLeft, mapRight)
import           Data.Time.Clock.POSIX
import qualified EulerHS.Logger.Types as Log
import           EulerHS.Types (Operation(..), ApiTag(..), BeamRuntime, BeamRunner, DBConfig (MySQLPoolConf), KVDBAnswer, TxResult, KVDBStatus)
import           EulerHS.Extra.Snowflakes.Types (SnowflakeError(..))
import           Sequelize (fromColumnar', columnize, Model, Where, Clause(..), Term(..), Set(..), modelTableName)
import           System.Random (randomRIO)
import           Unsafe.Coerce (unsafeCoerce)
import           Juspay.Extra.Config (lookupEnvT)
import qualified Data.Fixed as Fixed
import qualified Data.Serialize as Serialize
import qualified Data.Serialize as Cereal
import           Safe (atMay)
import EulerHS.KVDB.Types (MeshError(..))

jsonKeyValueUpdates ::
  forall be table. (HasCallStack, Model be table, MeshMeta be table)
  => [Set be table] -> [(Text, A.Value)]
jsonKeyValueUpdates = fmap jsonSet

jsonSet ::
  forall be table.
  (HasCallStack, Model be table, MeshMeta be table) =>
  Set be table -> (Text, A.Value)
jsonSet (Set column value) = (key, modifiedValue)
  where
    key = B._fieldName . fromColumnar' . column . columnize $
      B.dbTableSettings (meshModelTableEntityDescriptor @table @be)
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
  let updVals' = map (\(key,v) -> (key, Map.findWithDefault id key (valueMapper @be @table) v)) updVals
  case A.toJSON model of
    A.Object o -> Right (A.Object $ foldr (uncurry HM.insert) o updVals')
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

getDataFromRedisForPKey ::forall table m. (
    KVConnector (table Identity),
    FromJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m) => MeshConfig -> Text -> m (MeshResult (Maybe (Text, Bool, table Identity)))
getDataFromRedisForPKey meshCfg pKey = do
  res <- L.rGetBEither meshCfg.kvRedis (fromString $ T.unpack $ pKey)
  case res of
    Right (Just r) ->
      let
        (decodeResult, isLive) = decodeToField $ BSL.fromChunks [r]
      in case decodeResult  of
        Right [decodeRes] -> return . Right . Just $ (pKey, isLive, decodeRes)
        Right _           -> return . Right $ Nothing   -- Something went wrong
        Left e            -> return $ Left e
    Right Nothing -> do
      let traceMsg = "redis_fetch_noexist: Could not find key: " <> show pKey
      L.logWarningT "getCacheWithHash" traceMsg
      return $ Right Nothing
    Left e -> return $ Left $ MRedisError e

getDataFromPKeysRedis :: forall table m. (
    KVConnector (table Identity),
    FromJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m) => MeshConfig -> [ByteString] -> m (MeshResult ([table Identity], [table Identity]))
getDataFromPKeysRedis _ [] = pure $ Right ([], [])
getDataFromPKeysRedis meshCfg (pKey : pKeys) = do
  res <- L.rGetBEither meshCfg.kvRedis (fromString $ T.unpack $ decodeUtf8 pKey)
  case res of
    Right (Just r) -> do
      let (decodeResult, isLive) = decodeToField $ BSL.fromChunks [r]
      case decodeResult of
        Right decodeRes -> do
          remainingPKeysResult <- getDataFromPKeysRedis meshCfg pKeys
          case remainingPKeysResult of
            Right remainingResult -> do
              if isLive
                then return $ Right (decodeRes ++ (fst remainingResult), snd remainingResult)
                else return $ Right (fst remainingResult, decodeRes ++ (snd remainingResult))
            Left err -> return $ Left err
        Left e -> return $ Left e
    Right Nothing -> do
      getDataFromPKeysRedis meshCfg pKeys
    Left e -> return $ Left $ MRedisError e

------------- KEY UTILS ------------------

keyDelim:: Text
keyDelim = "_"

getPKeyWithShard :: forall table beM. (KVConnector (table Identity)) => DBConfig beM -> table Identity -> Text
getPKeyWithShard dbConf table =
  let pKey = getLookupKeyByPKey (isMySQLConfig dbConf) table
  in pKey <> getShardedHashTag pKey

getLookupKeyByPKey :: forall table. (KVConnector (table Identity)) => Bool -> table Identity -> Text
getLookupKeyByPKey isMySQL table = do
  let tName = tableName @(table Identity)
  let (PKey k) = primaryKey isMySQL table
  let lookupKey = getSortedKey k
  tName <> keyDelim <> lookupKey

getLookupKeyByPKey' :: forall table. (KVConnector (table Identity)) => table Identity -> Text
getLookupKeyByPKey' table = do
  let tName = tableName @(table Identity)
  let (PKey k) = primaryKey' table
  let lookupKey = getSortedKey k
  tName <> keyDelim <> lookupKey

getSecondaryLookupKeys :: forall table. (KVConnector (table Identity)) => Bool -> table Identity -> [Text]
getSecondaryLookupKeys isMySQL table = do
  let tName = tableName @(table Identity)
  let skeys = secondaryKeysFiltered isMySQL table
  let tupList = map (\(SKey s) -> s) skeys
  let list = map (\x -> tName <> keyDelim <> getSortedKey x ) tupList
  list

secondaryKeysFiltered :: forall table. (KVConnector (table Identity)) => Bool -> table Identity -> [SecondaryKey]
secondaryKeysFiltered isMySQL table = filter filterEmptyValues (secondaryKeys isMySQL table)
  where
    filterEmptyValues :: SecondaryKey -> Bool
    filterEmptyValues (SKey sKeyPairs) = not $ any (\p -> snd p == "") sKeyPairs

applyFPair :: (t -> b) -> (t, t) -> (b, b)
applyFPair f (x, y) = (f x, f y)

getPKeyAndValueList :: forall table beM. (HasCallStack, KVConnector (table Identity), A.ToJSON (table Identity)) => DBConfig beM -> table Identity -> [(Text, A.Value)]
getPKeyAndValueList dbConf table = do
  let (PKey k) = primaryKey (isMySQLConfig dbConf) table
      keyValueList = sortBy (compare `on` fst) k
      rowObject = A.toJSON table
  case rowObject of
    A.Object hm -> foldl' (\ acc x -> (go hm x) : acc) [] keyValueList
    _           -> error "Cannot work on row that isn't an Object"

  where
    go hm x = case HM.lookup (fst x) hm of
      Just val -> (fst x, val)
      Nothing  -> error $ "Cannot find " <> (fst x) <> " field in the row"

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

addToETLStream :: forall table m. ((KVConnector (table Identity)), L.MonadFlow m) => MeshConfig -> Bool -> ETLStreamKeys -> table Identity -> m ()
addToETLStream meshCfg isMySQLConf key dbRes = when meshCfg.shouldPushToETLStream $ do
  let pKeyText = getLookupKeyByPKey isMySQLConf dbRes
      shard    = getShardedHashTag pKeyText
      pKey     = fromString . T.unpack $ pKeyText <> shard
  void $ L.runKVDB meshCfg.kvRedis $ L.xadd (encodeUtf8 (getETLStreamName <> shard)) L.AutoID [(show key, pKey)]

addToDBSyncStreamAndRedis :: forall table m.
  ( KVConnector (table Identity)
  , Serialize (table Identity)
  , ToJSON (table Identity)
  , L.MonadFlow m
  )
  => MeshConfig 
  -> Text 
  -> A.Value 
  -> ByteString 
  -> table Identity 
  -> m (KVDBAnswer (TxResult KVDBStatus))
addToDBSyncStreamAndRedis meshCfg shard qCmd pKey val = L.runKVDB meshCfg.kvRedis $ L.multiExecWithHash (encodeUtf8 shard) $ do
    void $ L.xaddTx
          (encodeUtf8 (meshCfg.ecRedisDBStream <> shard))
          L.AutoID
          [("command", BSL.toStrict $ A.encode qCmd)]
    L.setexTx pKey meshCfg.redisTtl (BSL.toStrict $ Encoding.encode_ meshCfg.cerealEnabled val)

addToDBSyncStreamETLStreamAndRedis :: forall table m. 
  ( KVConnector (table Identity)
  , Serialize (table Identity)
  , ToJSON (table Identity)
  , L.MonadFlow m
  )
  => MeshConfig
  -> Text
  -> A.Value
  -> ETLStreamKeys
  -> ByteString
  -> table Identity
  -> m (KVDBAnswer (TxResult KVDBStatus))
addToDBSyncStreamETLStreamAndRedis meshCfg shard qCmd key pKey val = L.runKVDB meshCfg.kvRedis $ L.multiExecWithHash (encodeUtf8 shard) $ do
    void $ L.xaddTx
          (encodeUtf8 (meshCfg.ecRedisDBStream <> shard))
          L.AutoID
          [("command", BSL.toStrict $ A.encode qCmd)]
    void $ L.xaddTx
          (encodeUtf8 (getETLStreamName <> shard))
          L.AutoID
          [(show key, pKey)]
    L.setexTx pKey meshCfg.redisTtl (BSL.toStrict $ Encoding.encode_ meshCfg.cerealEnabled val)

------------------------------------------

getTableRowWithPrimaryId :: forall (table :: (Type -> Type) -> Type) m beM.
  (ToJSON (table Identity), FromJSON (table Identity), KVConnector (table Identity), L.MonadFlow m) 
  => DBConfig beM -> MeshConfig -> Text -> table Identity -> m (MeshResult (table Identity))
getTableRowWithPrimaryId dbConf meshCfg tName value = do
  eitherPrimaryId <- mapRight toIntegerId <$> 
    if meshCfg.snowFlakeEnabled
      then getSnowflakeValue tName
      else 
        if meshCfg.meshEnabled && not meshCfg.kvHardKilled 
          then getPrimaryIdFromRedis meshCfg tName
          else return . Right . AutoPrimaryId $ Nothing
  return $ case eitherPrimaryId of
    Left err -> Left $ err
    Right (Just primaryId) -> unsafeSetPrimaryIdInTableRow dbConf value primaryId
    Right Nothing -> Right $ value

getPrimaryIdFromRedis :: (L.MonadFlow m) => MeshConfig -> Text -> m (MeshResult AutoPrimaryId)
getPrimaryIdFromRedis meshCfg tName =
      let key = (T.pack . quietSnake . T.unpack) tName <> "_auto_increment_id"
      in (L.runKVDB meshCfg.kvRedis (L.incr . encodeUtf8 $ key)) <&> either failure (success . Just)
  where
    success = Right . AutoPrimaryId
    failure = Left . MRedisError

getSnowflakeValue :: forall m .(L.MonadFlow m) => Text -> m (MeshResult AutoPrimaryId)
getSnowflakeValue tName = go snowflakeGenerationRetryLimit
  where
    snowflakeGenerationRetryLimit :: Int
    snowflakeGenerationRetryLimit = fromMaybe 3 $ readMaybe =<< lookupEnvT "SNOWFLAKE_GENERATION_RETRY_LIMIT"

    snowflakeGenerationRetryDelay :: Integer
    snowflakeGenerationRetryDelay = fromMaybe 500 $ readMaybe =<< lookupEnvT "SNOWFLAKE_GENERATION_RETRY_DELAY"

    go :: Int -> m (MeshResult AutoPrimaryId)
    go 0           = pure . Left . UnexpectedError $ "Could not generate snowflake value"
    go retriesLeft = do
      eitherSnowflakeId <- L.generateSnowflake $ show tName
      case eitherSnowflakeId of
        Left (Fatal err)           -> pure . Left . UnexpectedError $ err
        Left (NonFatal err) -> do
          L.logWarningT "getSnowflakeValue" err
          void $ L.runIO $ threadDelayMilisec snowflakeGenerationRetryDelay
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
  
unsafeSetPrimaryIdInTableRow :: forall table beM. (ToJSON (table Identity), FromJSON (table Identity), KVConnector (table Identity)) => 
  DBConfig beM -> table Identity -> Integer -> MeshResult (table Identity)
unsafeSetPrimaryIdInTableRow dbConf tableRow primaryId = do
  let (PKey p) = primaryKey (isMySQLConfig dbConf) tableRow
  case p of
    [(field, _)] ->
      case A.toJSON tableRow of
        A.Object jsonObject ->
          if HM.member field jsonObject
            then Right tableRow
            else 
              let 
                  setPrimaryIdInJson = \pId -> A.Object (HM.insert field (A.toJSON pId) jsonObject)
              in decodeJsonToTableRow . setPrimaryIdInJson $ primaryId
        _ ->  Left $ MDecodingError "Can't set AutoIncId value of JSON which isn't a object."
    _ ->  Right tableRow
  where
    decodeJsonToTableRow :: A.Value -> MeshResult (table Identity)
    decodeJsonToTableRow = either (Left . MDecodingError) Right . resultToEither . A.fromJSON  

foldEither :: [Either a b] -> Either a [b]
foldEither []               = Right []
foldEither ((Left a) : _)   = Left a
foldEither ((Right b) : xs) = mapRight ((:) b) (foldEither xs)

resultToEither :: A.Result a -> Either Text a
resultToEither (A.Success res) = Right res
resultToEither (A.Error e)     = Left $ T.pack e

getUniqueDBRes :: KVConnector (table Identity) => DBConfig beM -> [table Identity] -> [table Identity] -> [table Identity]
getUniqueDBRes dbConf dbRows kvRows = do
  let isMySQL = isMySQLConfig dbConf
      kvPkeys = map (getLookupKeyByPKey isMySQL) kvRows
  filter (\r -> getLookupKeyByPKey isMySQL r `notElem` kvPkeys) dbRows

mergeKVAndDBResults :: KVConnector (table Identity) => [table Identity] -> [table Identity] -> [table Identity]
mergeKVAndDBResults dbRows kvRows = do
  let kvPkeys = map getLookupKeyByPKey' kvRows
      uniqueDbRes = filter (\r -> getLookupKeyByPKey' r `notElem` kvPkeys) dbRows
  kvRows ++ uniqueDbRes
   
removeDeleteResults :: KVConnector (table Identity) => [table Identity] -> [table Identity] -> [table Identity]
removeDeleteResults delRows rows = do
  let delPKeys = map getLookupKeyByPKey' delRows
      nonDelRows = filter (\r -> getLookupKeyByPKey' r `notElem` delPKeys) rows
  nonDelRows 

getLatencyInMicroSeconds :: Integer -> Integer
getLatencyInMicroSeconds execTime = execTime `div` 1000000

isMySQLConfig :: DBConfig beM -> Bool
isMySQLConfig (MySQLPoolConf {}) = True
isMySQLConfig _ = False

---------------- Match where clauses -------------
findOneMatching :: (B.Beamable table, BeamRuntime be beM, BeamRunner beM) => Where be table -> DBConfig beM -> [table Identity] -> Maybe (table Identity)
findOneMatching whereClause dbConf = find (matchWhereClause (isMySQLConfig dbConf) whereClause)

findAllMatching :: (B.Beamable table, BeamRuntime be beM, BeamRunner beM) => Where be table -> DBConfig beM -> [table Identity] -> [table Identity]
findAllMatching whereClause dbConf = filter (matchWhereClause (isMySQLConfig dbConf) whereClause)

matchWhereClause :: B.Beamable table => Bool -> [Clause be table] -> table Identity -> Bool
matchWhereClause isMySQL whereClause row = all matchClauseQuery whereClause
  where
  matchClauseQuery = \case
    And queries     -> all matchClauseQuery queries
    Or queries      -> any matchClauseQuery queries
    Is column' term ->
      let column = fromColumnar' . column' . columnize
        in termQueryMatch isMySQL (column row) term

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

getRandomStream :: (L.MonadFlow m) => m Text
getRandomStream = do
  streamShard <- L.runIO' "random shard" $ randomRIO (1, getConfigStreamMaxShards)
  return $ getStreamName (show streamShard)

getConfigStreamNames :: [Text]
getConfigStreamNames = fmap (\shardNo -> getStreamName (show shardNo) ) [1..getConfigStreamMaxShards]

getConfigStreamBasename :: Text
getConfigStreamBasename = fromMaybe "ConfigStream" $ lookupEnvT "CONFIG_STREAM_BASE_NAME"

getConfigStreamMaxShards :: Int
getConfigStreamMaxShards = fromMaybe 20 $ readMaybe =<< lookupEnvT "CONFIG_STREAM_MAX_SHARDS"

getConfigStreamLooperDelayInSec :: Int
getConfigStreamLooperDelayInSec = fromMaybe 5 $ readMaybe =<< lookupEnvT "CONFIG_STREAM_LOOPER_DELAY_IN_SEC"

getConfigEntryTtlJitterInSeconds :: Int
getConfigEntryTtlJitterInSeconds = fromMaybe 5 $ readMaybe =<< lookupEnvT "CONFIG_STREAM_TTL_JITTER_IN_SEC"

getConfigEntryBaseTtlInSeconds :: Int
getConfigEntryBaseTtlInSeconds = fromMaybe 10 $ readMaybe =<< lookupEnvT "CONFIG_STREAM_BASE_TTL_IN_SEC"

getConfigEntryNullBaseTtlInSeconds :: Int
getConfigEntryNullBaseTtlInSeconds = fromMaybe 120 $ readMaybe =<< lookupEnvT "CONFIG_STREAM_NULL_BASE_TTL_IN_SEC"

getConfigEntryNewTtl :: (L.MonadFlow m) => m POSIXTime
getConfigEntryNewTtl = do
    currentTime <- L.runIO getPOSIXTime
    let
      jitterInSec = getConfigEntryTtlJitterInSeconds
      baseTtlInSec = getConfigEntryBaseTtlInSeconds
      t = round currentTime  :: Int
    noise <- L.runIO' "random seconds" $ randomRIO (1, jitterInSec)
    return $ fromIntegral (baseTtlInSec + noise +  t)

getConfigNullEntryNewTtl :: (L.MonadFlow m) => m POSIXTime
getConfigNullEntryNewTtl = do
    currentTime <- L.runIO getPOSIXTime
    let
      baseTtlInSec = getConfigEntryNullBaseTtlInSeconds
      t = round currentTime  :: Int
    return $ fromIntegral (baseTtlInSec +  t)

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

decodeToField :: forall a. (FromJSON a, Serialize.Serialize a) => BSL.ByteString -> (MeshResult [a], Bool)
decodeToField val =
  let decodeRes = Encoding.decodeLiveOrDead val
    in  case decodeRes of
          (isLive, byteString) ->
            let decodedMeshResult =
                        let (h, v) = BSL.splitAt 4 byteString
                          in case h of
                                "CBOR" -> case Cereal.decodeLazy v of
                                            Right r' -> Right [r']
                                            Left _ -> case Cereal.decodeLazy v of
                                                        Right r'' -> Right r''
                                                        Left _ -> case Cereal.decodeLazy v of
                                                                      Right r''' -> decodeField @a r'''
                                                                      Left err' -> Left $ MDecodingError $ T.pack err'
                                "JSON" ->
                                  case A.eitherDecode v of
                                    Right r' -> decodeField @a r'
                                    Left e   -> Left $ MDecodingError $ T.pack e
                                _      ->
                                  case A.eitherDecode val of
                                    Right r' -> decodeField @a r'
                                    Left e   -> Left $ MDecodingError $ T.pack e
              in (decodedMeshResult, isLive)

decodeField :: forall a. (FromJSON a, Serialize.Serialize a) => A.Value -> MeshResult [a]
decodeField o@(A.Object _) =
  case A.eitherDecode @a $ A.encode o of
    Right r -> return [r]
    Left e  -> Left $ MDecodingError $ T.pack e
decodeField o@(A.Array _) =
  mapLeft (MDecodingError . T.pack)
    $ A.eitherDecode @[a] $ A.encode o
decodeField o = Left $ MDecodingError
  ("Expected list or object but got '" <> T.pack (show o) <> "'.")

{-# INLINE getInMemCacheKeyFromWhereClause #-}
getInMemCacheKeyFromWhereClause :: forall table be beM. (Model be table, MeshMeta be table) =>
  DBConfig beM -> Text -> B.DatabaseEntityDescriptor be (B.TableEntity table) -> Clause be table -> Text
getInMemCacheKeyFromWhereClause dbConf acc dt = \case
    And cs -> acc <> "_AND_" <> (foldl (\curr ins-> getInMemCacheKeyFromWhereClause dbConf curr dt ins) "" cs)
    Or cs -> acc <> "_OR_" <> (foldl (\curr ins-> getInMemCacheKeyFromWhereClause dbConf curr dt ins) "" cs)
    Is column (Eq v1) -> do
      let key = B._fieldName . fromColumnar' . column . columnize $ B.dbTableSettings dt
      acc <> "_IS_" <> key <> "=" <> (showVal isMySQL $ A.toJSON v1)
    Is column (In v2) -> do
      let key = B._fieldName . fromColumnar' . column . columnize $ B.dbTableSettings dt
      acc <> "_IN_" <> key <> "=" <> (showVal isMySQL $ A.toJSON v2)
    Is column (GreaterThan v3) -> do
      let key = B._fieldName . fromColumnar' . column . columnize $ B.dbTableSettings dt
      acc <> "_GT_" <> key <> "=" <> (showVal isMySQL $ A.toJSON v3)
    Is column (GreaterThanOrEq v4) -> do
      let key = B._fieldName . fromColumnar' . column . columnize $ B.dbTableSettings dt
      acc <> "_GTE_" <> key <> "=" <> (showVal isMySQL $ A.toJSON v4)
    Is column (LessThan v5) -> do
      let key = B._fieldName . fromColumnar' . column . columnize $ B.dbTableSettings dt
      acc <> "_LT_" <> key <> "=" <> (showVal isMySQL $ A.toJSON v5)
    Is column (LessThanOrEq v6) -> do
      let key = B._fieldName . fromColumnar' . column . columnize $ B.dbTableSettings dt
      acc <> "_LTE_" <> key <> "=" <> (showVal isMySQL $ A.toJSON v6)
    Is column (Null) -> do
      let key = B._fieldName . fromColumnar' . column . columnize $ B.dbTableSettings dt
      acc <> key <> "_NULL"
    Is column (Not Null) -> do
      let key = B._fieldName . fromColumnar' . column . columnize $ B.dbTableSettings dt
      acc <> key <> "_NOT_NULL"
    Is column (Not term) -> getInMemCacheKeyFromWhereClause dbConf (acc <> "_NOT_") dt (Is column term)
    _ -> acc

  where
    isMySQL = isMySQLConfig dbConf

    showVal :: Bool -> A.Value -> Text
    showVal isItMySQL res = case res of
      A.String r -> if isItMySQL then T.toLower r else r
      A.Number n -> T.pack $ show n
      A.Array l  -> T.pack $ show l
      A.Object o -> T.pack $ show o
      A.Bool b -> T.pack $ show b
      A.Null -> T.pack ""

{-# INLINABLE mkTrackerKey #-}
mkTrackerKey :: Text -> Text -> Text
mkTrackerKey pkVal tName = "key_" <> tName <> "_id_" <> pkVal

getFieldsAndValuesFromClause :: forall table be beM. (Model be table, MeshMeta be table) =>
  DBConfig beM -> B.DatabaseEntityDescriptor be (B.TableEntity table) -> Clause be table -> [[(Text, Text)]]
getFieldsAndValuesFromClause dbConf dt = \case
  And cs -> foldl' processAnd [[]] $ map (getFieldsAndValuesFromClause dbConf dt) cs
  Or cs -> processOr cs
  Is column (Eq val) -> do
    let !key = B._fieldName . fromColumnar' . column . columnize $ B.dbTableSettings dt
    [[(key, showVal . snd $ (toPSJSON @be @table) (key, A.toJSON val))]]
  Is column (In vals) -> do
    let !key = B._fieldName . fromColumnar' . column . columnize $ B.dbTableSettings dt
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

getPrimaryKeyFromFieldsAndValues :: (L.MonadFlow m,HasCallStack) => Text -> MeshConfig -> HM.HashMap Text Bool -> [(Text, Text)] -> m (MeshResult [ByteString])
getPrimaryKeyFromFieldsAndValues _ _ _ [] = pure $ Right []
getPrimaryKeyFromFieldsAndValues modelName meshCfg keyHashMap fieldsAndValues = do
  res <- foldEither <$> mapM getPrimaryKeyFromFieldAndValueHelper fieldsAndValues
  pure $ mapRight (intersectList . catMaybes) res
  where

    getPrimaryKeyFromFieldAndValueHelper (k, v) = do
      let constructedKey = modelName <> "_" <> k <> "_" <> v
      case HM.lookup k keyHashMap of
        Just True -> pure $ Right $ Just [fromString $ T.unpack (constructedKey <> getShardedHashTag constructedKey)]
        Just False -> do
          res <- L.rSmembersB meshCfg.kvRedis (fromString $ T.unpack constructedKey)
          case res of
            Right [] -> pure $ Right Nothing
            Right r -> pure $ Right $ Just r
            Left e  -> pure $ Left $ MRedisError e
        _ -> pure $ Right Nothing

    intersectList (x : y : xs) = intersectList (intersect x y : xs)
    intersectList (x : [])     = x
    intersectList []           = []

filterPrimaryAndSecondaryKeys :: HM.HashMap Text Bool -> [(Text, Text)] -> [(Text, Text)]
filterPrimaryAndSecondaryKeys keyHashMap = filter (\(k, _) -> HM.member k keyHashMap)

getSecondaryKeyLength :: HM.HashMap Text Bool -> [(Text, Text)] -> Int
getSecondaryKeyLength keyHashMap = length . filter (\(k, _) -> HM.lookup k keyHashMap == Just False)


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
  DBConfig beM -> Where be table -> m (Maybe [[Text]])
whereClauseDiffCheck dbConf whereClause = 
  if isWhereClauseDiffCheckEnabled then do
    let keyAndValueCombinations = getFieldsAndValuesFromClause dbConf meshModelTableEntityDescriptor (And whereClause)
        andCombinations = map (uncurry zip . applyFPair (map (T.intercalate "_") . sortOn (Down . length) . nonEmptySubsequences) . unzip . sort) keyAndValueCombinations
        keyHashMap = keyMap @(table Identity)
        failedKeys = catMaybes $ map (atMay keyAndValueCombinations) $ findIndices (checkForPrimaryOrSecondary keyHashMap) andCombinations
    if (not $ null failedKeys)
      then do
        let diffRes = map (map fst) failedKeys
        if null $ concat diffRes
          then pure Nothing
          else L.logInfoT "WHERE_DIFF_CHECK" (tableName @(table Identity) <> ": " <> show diffRes) $> Just diffRes
      else pure Nothing
  else pure Nothing
  where
    checkForPrimaryOrSecondary _ [] = True
    checkForPrimaryOrSecondary keyHashMap ((k, _) : xs) =
      case HM.member k keyHashMap of
        True -> False
        _    -> checkForPrimaryOrSecondary keyHashMap xs

isWhereClauseDiffCheckEnabled :: Bool
isWhereClauseDiffCheckEnabled = fromMaybe True $ readMaybe =<< lookupEnvT "IS_WHERE_CLAUSE_DIFF_CHECK_ENABLED"

isRecachingEnabled :: Bool
isRecachingEnabled = fromMaybe False $ readMaybe =<< lookupEnvT "IS_RECACHING_ENABLED"

shouldLogFindDBCallLogs :: Bool
shouldLogFindDBCallLogs = fromMaybe False $ readMaybe =<< lookupEnvT "IS_FIND_DB_LOGS_ENABLED"

isLogsEnabledForModel :: Text -> Bool
isLogsEnabledForModel modelName = do
  let env :: Text = fromMaybe "development" $ lookupEnvT "NODE_ENV"
  if env == "production" then do
    let enableModelList = fromMaybe [] $ readMaybe =<< lookupEnvT "IS_LOGS_ENABLED_FOR_MODEL"
    modelName `elem` enableModelList
    else True

logAndIncrementKVMetric :: (L.MonadFlow m, ToJSON a) => Bool -> Text -> Operation -> MeshResult a -> Int -> Text -> Source -> Maybe [[Text]] -> m ()
logAndIncrementKVMetric shouldLogData action operation res latency model source mbDiffCheckRes = do
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
    }
  case res of
    Left err ->
      logDb Log.Error ("DB" :: Text) source action model latency dblog (Just err)
    Right _ -> 
      if action == "FIND" then
        when shouldLogFindDBCallLogs $ logDb Log.Debug ("DB" :: Text) source action model latency dblog Nothing
        else logDb Log.Info ("DB" :: Text) source action model latency dblog Nothing
  when (source == KV) $ L.setLoggerContext "PROCESSED_THROUGH_KV" "True"
  incrementMetric KVAction dblog (isLeft res)

logDb :: (L.MonadFlow m, ToJSON val) => Log.LogLevel -> Text -> Source -> Log.Action -> Log.Entity -> Int -> val -> Maybe MeshError -> m ()
logDb logLevel tag source action entity latency message maybeMeshError =
  L.evalLogger' $ L.masterLogger logLevel tag category (Just action) (Just entity) (getErrorLog <$> maybeMeshError) (Just $ toInteger latency) Nothing $ Log.Message Nothing (Just $ A.toJSON message)
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

withRedisLimit :: (L.MonadFlow m) => Text -> Text -> Int -> m (MeshResult a) -> m (MeshResult a)
withRedisLimit tag modelName expectedRedisCalls redisFunc =
  if expectedRedisCalls > redisCallsHardLimit
    then do
        incrementRedisCallMetric tag modelName expectedRedisCalls  (expectedRedisCalls > redisCallsSoftLimit) (expectedRedisCalls > redisCallsHardLimit)
        pure $ Left $ UnexpectedError ("Redis Calls Limit Exceeded with length " <> show expectedRedisCalls)
  else
    redisFunc

getETLStreamName :: Text 
getETLStreamName = fromMaybe "etl-stream" $ lookupEnvT "ETL_STREAM_NAME"

redisCallsHardLimit :: Int
redisCallsHardLimit = fromMaybe 5000 $ readMaybe =<< lookupEnvT "REDIS_CALLS_HARD_LIMIT"

redisCallsSoftLimit :: Int
redisCallsSoftLimit = fromMaybe 100 $ readMaybe =<< lookupEnvT "REDIS_CALLS_SOFT_LIMIT"

tablesWithoutRedisLimit :: [Text]
tablesWithoutRedisLimit = fromMaybe [] $ readMaybe =<< lookupEnvT "TABLES_WITHOUT_REDIS_LIMIT"
