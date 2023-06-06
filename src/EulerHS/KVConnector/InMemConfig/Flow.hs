{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module EulerHS.KVConnector.InMemConfig.Flow

    where

import           Control.Monad.Catch (bracket)
import qualified Data.HashMap.Strict as HM
import           EulerHS.Prelude hiding (bracket)
import           EulerHS.SqlDB.Types (BeamRunner, BeamRuntime, DBConfig)
import qualified EulerHS.SqlDB.Language as DB
import qualified Data.Aeson as A
import qualified Database.Beam as B
-- import           Control.Monad.Extra (notM)
import qualified Data.Set as Set
import qualified Data.List as DL
import qualified Data.ByteString.Lazy as BSL
import           Data.Time (LocalTime)
import qualified Data.Text as T
import qualified EulerHS.Language as L
import           EulerHS.KVConnector.InMemConfig.Types
import           EulerHS.KVConnector.Types (KVConnector(..), MeshConfig, tableName, MeshResult, MeshMeta(..), MeshError(MDBError, UnexpectedError), Source(..))
import           Unsafe.Coerce (unsafeCoerce)
import           Data.Either.Extra (mapLeft, mapRight)
import           EulerHS.CachedSqlDBQuery (runQuery)
import           EulerHS.Runtime (mkConfigEntry, ConfigEntry(..))
import           EulerHS.KVConnector.Utils
import           Sequelize (Model, Where, Clause(..), sqlSelect)
import           Named (defaults, (!))
import qualified Data.Serialize as Serialize
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.LocalTime (addLocalTime)

checkAndStartLooper :: forall table m.
    (
    HasCallStack,
    KVConnector (table Identity),
    L.MonadFlow m) => MeshConfig -> (ByteString -> Maybe (ImcStreamValue (table Identity))) -> m ()
checkAndStartLooper meshCfg decodeTable = do
    hasLooperStarted <- L.getOption $ (LooperStarted (tableName @(table Identity)))
    case hasLooperStarted of
        _hasLooperStarted
            | _hasLooperStarted == Just True ->  pure ()
            | otherwise ->  do
                streamName <- getRandomStream
                when shouldLogFindDBCallLogs $ L.logDebug @Text "checkAndStartLooper" $ "Connecting with Stream <" <> streamName <> "> for table " <> tableName @(table Identity)
                L.setOption (LooperStarted (tableName @(table Identity))) True
                L.fork $ looperForRedisStream  decodeTable meshCfg.kvRedis streamName

looperForRedisStream :: forall table m.(
    HasCallStack,
    KVConnector (table Identity),
    L.MonadFlow m
    ) =>
    (ByteString -> Maybe (ImcStreamValue (table Identity))) -> Text -> Text -> m ()
looperForRedisStream decodeTable redisName streamName = bracket
    (pure ())
    (\ _ -> do
      L.logInfoT "looperForRedisStream failed" ("Setting LooperStarted option as False for table " <> tableName @(table Identity))
      L.setOption (LooperStarted (tableName @(table Identity))) False)
    (\ _ -> forever $ do
      let tName = tableName @(table Identity)
      maybeRId <- L.getOption (RecordId tName)
      case maybeRId of
          Nothing -> do
              rId <- T.pack . show <$> L.getCurrentDateInMillis
              initRecords <- getRecordsFromStream redisName streamName rId tName
              case initRecords of
                  Nothing -> do
                      L.setOption (RecordId tName) rId
                      return ()
                  Just (latestId, rs) -> do
                      L.setOption (RecordId tName) latestId
                      mapM_ (setInMemCache tName decodeTable) rs
          Just rId -> do
              newRecords <- getRecordsFromStream redisName streamName rId tName
              case newRecords of
                  Nothing -> return ()
                  Just (latestId, rs) -> do
                      L.setOption (RecordId tName) latestId
                      mapM_ (setInMemCache tName decodeTable) rs
      void $ looperDelayInSec)


looperDelayInSec :: (L.MonadFlow m) => m ()
looperDelayInSec = L.runIO $ threadDelay $ getConfigStreamLooperDelayInSec * 1000000

setInMemCache :: forall table m.(
    HasCallStack,
    KVConnector(table Identity),
    L.MonadFlow m
    ) =>
    Text ->
    (ByteString -> Maybe (ImcStreamValue (table Identity))) ->
    RecordKeyValues -> m ()
setInMemCache tName decodeTable (key,value) = do
  when (tName == key) $                             -- decode only when entry is for the looper's table
    case decodeTable value of
        Nothing -> do
          L.logErrorT "setInMemCache" $ "Unable to decode ImcStreamValue for the table <" <> key
          return ()
        Just strmVal ->
          case strmVal.command of
            ImcInsert -> strmVal & \x -> do
              let
                pKeyText = getLookupKeyByPKey x.tableRow
                pKey = pKeyText  <> getShardedHashTag pKeyText
              updateAllKeysInIMC pKey x.tableRow

            ImcDelete -> strmVal.tableRow & \x -> do
              let
                pKeyText = getLookupKeyByPKey x
                pKey = pKeyText  <> getShardedHashTag pKeyText
              deletePrimaryAndSecondaryKeysFromIMC pKey x

extractRecordsFromStreamResponse :: [L.KVDBStreamReadResponseRecord] -> [RecordKeyValues]
extractRecordsFromStreamResponse  = foldMap (fmap (bimap decodeUtf8 id) . L.records)

getRecordsFromStream :: Text -> Text -> LatestRecordId -> Text -> (L.MonadFlow m) => m (Maybe (LatestRecordId, [RecordKeyValues]))
getRecordsFromStream redisName streamName lastRecordId tName = do
    eitherReadResponse <- L.rXreadT redisName streamName lastRecordId
    case eitherReadResponse of
        Left err -> do
            L.delOption (RecordId tName)    -- TODO Necessary?
            L.logErrorT "getRecordsFromStream" $ "Error getting initial records from stream <" <> streamName <> ">" <> show err
            return Nothing
        Right maybeRs -> case maybeRs of
            Nothing -> do
            -- L.delOption (RecordId tName)    -- Maybe stream doesn't exist or Maybe no new records
                return Nothing
            Just [] -> do             -- Never seems to occur
                return Nothing
            Just rs -> case filter (\rec-> (decodeUtf8 rec.streamName) == streamName) rs of
                [] -> do
                    return Nothing
                (rss : _) -> do
                    case uncons . reverse . L.response $ rss of
                        Nothing -> return Nothing
                        Just (latestRecord, _) -> do
                                L.logInfoT ("getRecordsFromStream for " <> tName) $ (show . length . L.response $ rss) <> " new records in stream <" <> streamName <> ">"
                                return . Just . bimap (decodeUtf8 . L.recordId) (extractRecordsFromStreamResponse . L.response ) $ (latestRecord, rss)

getDataFromPKeysIMC :: forall table m. (
    KVConnector (table Identity),
    FromJSON (table Identity),
    Show (table Identity),
    L.MonadFlow m) => MeshConfig -> [ByteString] -> m (MeshResult [InMemCacheResult (table Identity)])
getDataFromPKeysIMC _ [] = pure $ Right []
getDataFromPKeysIMC meshCfg (pKey : pKeys) = do
  let k = decodeUtf8 pKey
  mbVal <- L.getConfig k
  record <- case mbVal of
    Nothing -> pure . EntryNotFound $ k
    Just val -> do
      currentTime <- L.getCurrentTimeUTC
      if val.ttl > currentTime
        then pure $ EntryValid (unsafeCoerce @_ @(table Identity) val.entry)
        else pure $ EntryExpired (unsafeCoerce @_ @(table Identity) val.entry) k
  mapRight (record :) <$> getDataFromPKeysIMC meshCfg pKeys

searchInMemoryCache :: forall be beM table m.
  (
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    HasCallStack,
    KVConnector (table Identity),
    ToJSON (table Identity),
    Show (table Identity),
    Serialize.Serialize (table Identity),
    FromJSON (table Identity),
    Model be table,
    MeshMeta be table,
    L.MonadFlow m
  ) =>  MeshConfig ->
        DBConfig beM ->
        Where be table ->
        m (Source, MeshResult [table Identity])
searchInMemoryCache meshCfg dbConf whereClause = do
  eitherPKeys <- getPrimaryKeys False
  let
    decodeTable :: ByteString -> Maybe (ImcStreamValue (table Identity))
    decodeTable = A.decode . BSL.fromStrict
  checkAndStartLooper meshCfg decodeTable
  case eitherPKeys of
    Right pKeys -> do
        allRowsRes <- mapM (getDataFromPKeysIMC meshCfg) pKeys
        case mapRight concat (foldEither allRowsRes) of
          (Left e) -> return . (IN_MEM,) . Left $ e
          Right results -> do
              let
                validResult = catMaybes $ results <&> getValidResult
                keysRequiringRedisFetch = catMaybes $ results <&> getKeysRequiringRedisFetch
              when shouldLogFindDBCallLogs $ L.logDebugV ("searchInMemoryCache: <" <> tname <> "> got validResult") validResult
              if length validResult == 0
                then if (length keysRequiringRedisFetch) /= 0
                  then do
                    kvFetch keysRequiringRedisFetch
                  else do
                    getPrimaryKeys True >>= \case
                      Left err -> return . (KV,) . Left $ err
                      Right pKeysFromRedis -> kvFetch $ decodeUtf8 <$> concat pKeysFromRedis

                else if length keysRequiringRedisFetch == 0
                  then return . (IN_MEM,) . Right $ validResult
                  else do
                    let
                      lockKey :: Text
                      lockKey = T.pack . DL.intercalate "_" $ T.unpack <$> keysRequiringRedisFetch
                    whenM (L.acquireConfigLock lockKey) (forkKvFetchAndSave keysRequiringRedisFetch lockKey)
                    return . (IN_MEM,) . Right $ validResult

    Left e -> return . (IN_MEM,) . Left $ e
  where

    tname :: Text = (tableName @(table Identity))

    getValidResult :: InMemCacheResult (table Identity) -> Maybe (table Identity)
    getValidResult r = case r of
      EntryValid v -> Just $ v
      EntryExpired v _ -> Just $ v
      _ -> Nothing

    getKeysRequiringRedisFetch :: InMemCacheResult (table Identity) -> Maybe KeysRequiringRedisFetch
    getKeysRequiringRedisFetch r = case r of
      EntryExpired _ k -> Just $ k
      EntryNotFound k -> Just $ k
      _ -> Nothing

    forkKvFetchAndSave :: [Text] -> Text -> m ()
    forkKvFetchAndSave pKeys lockKey = do
      when shouldLogFindDBCallLogs $ L.logDebugT "forkKvFetchAndSave" $ "Starting Timeout for redis-fetch for in-mem-config"
      L.fork $
        do
          void $ L.runIO $ threadDelayMilisec 5
          void $ L.releaseConfigLock lockKey      -- TODO  Check if release fails
      when shouldLogFindDBCallLogs $ L.logDebugT "forkKvFetchAndSave" $ "Initiating updation for " <> (show $ length pKeys) <> " pKeys in-mem-config"
      L.fork $ void $ kvFetch pKeys

    kvFetch :: [Text] -> m (Source, MeshResult [table Identity])
    kvFetch pKeys =
      if meshCfg.kvHardKilled
        then (SQL,) <$> doDbFetchAndUpdateIMC
        else useKvcAndUpdateIMC pKeys

    useKvcAndUpdateIMC :: [Text] -> m (Source, MeshResult [table Identity])
    useKvcAndUpdateIMC pKeys = do
      (eTuples :: MeshResult [Maybe (Text, Bool, table Identity)]) <- foldEither <$> mapM (getDataFromRedisForPKey meshCfg)  pKeys
      case eTuples of
        Left err -> do
          L.logErrorT "kvFetch: " (show err)
          return . (KV,) . Left $ err
        Right mtups -> do
          let
            tups = catMaybes mtups
          if length tups == 0
            then (SQL,) <$> doDbFetchAndUpdateIMC
            else do
              let liveTups = filter (\(_, isLive, _) -> isLive) tups
              mapM_ (\(k, _, row) ->  updateAllKeysInIMC k row) liveTups
              return . (KV,) . Right $ (\(_, _, row) -> row) <$> liveTups

    doDbFetchAndUpdateIMC :: m (MeshResult [(table Identity)])
    doDbFetchAndUpdateIMC = do
      when shouldLogFindDBCallLogs $ L.logDebugT "searchInMemoryCache" "Fetching from DB and updating IMC"
      eDbTups <- dbFetch
      case eDbTups of
        (Left e) -> pure .Left $ e
        Right dbTups -> do
          mapM_ (uncurry updateAllKeysInIMC) dbTups
          return . Right $ snd <$> dbTups

    dbFetch :: m (MeshResult [(Text, table Identity)])
    dbFetch = do
      let findAllQuery = DB.findRows (sqlSelect ! #where_ whereClause ! defaults)
      res <- mapLeft MDBError <$> runQuery dbConf findAllQuery
      case res of
        Left err -> pure $ Left err
        Right rows -> pure . Right $ (\row -> (getPKeyFromPKeyText row, row)) <$> rows

    getPKeyFromPKeyText :: table Identity -> Text
    getPKeyFromPKeyText row =
      let
        pKeyText = getLookupKeyByPKey row
      in pKeyText  <> getShardedHashTag pKeyText
    getPrimaryKeys :: Bool -> m (MeshResult [[ByteString]])
    getPrimaryKeys fetchFromRedis = do
      let
        keyAndValueCombinations = getFieldsAndValuesFromClause meshModelTableEntityDescriptor (And whereClause)
        andCombinations = map (uncurry zip . applyFPair (map (T.intercalate "_") . sortOn (Down . length) . nonEmptySubsequences) . unzip . sort) keyAndValueCombinations
        modelName = tableName @(table Identity)
        keyHashMap = keyMap @(table Identity)
      eitherKeyRes <- if fetchFromRedis
          then mapM (getPrimaryKeyFromFieldsAndValues modelName meshCfg keyHashMap) andCombinations
          else mapM (getPrimaryKeyInIMCFromFieldsAndValues modelName keyHashMap) andCombinations
      pure $ foldEither eitherKeyRes

    getPrimaryKeyInIMCFromFieldsAndValues :: (L.MonadFlow m) => Text -> HM.HashMap Text Bool -> [(Text, Text)] -> m (MeshResult [ByteString])
    getPrimaryKeyInIMCFromFieldsAndValues modelName keyHashMap fieldsAndValues = do
      res <- foldEither <$> mapM getPrimaryKeyFromFieldAndValueHelper fieldsAndValues
      pure $ mapRight (intersectList . catMaybes) res
      where

        getPrimaryKeyFromFieldAndValueHelper (k, v) = do
          let constructedKey = modelName <> "_" <> k <> "_" <> v
          case HM.lookup k keyHashMap of
            Just True -> pure $ Right $ Just [fromString $ T.unpack (constructedKey <> getShardedHashTag constructedKey)]
            Just False -> do
              L.getConfig constructedKey >>= \case
                Nothing -> pure $ Right Nothing
                Just pKeyEntries -> pure . Right . Just $ encodeUtf8 <$> unsafeCoerce @_ @[Text] pKeyEntries.entry
            _ -> pure $ Right Nothing

        intersectList (x : y : xs) = intersectList (DL.intersect x y : xs)
        intersectList (x : [])     = x
        intersectList []           = []

updateAllKeysInIMC :: forall table m. (KVConnector (table Identity), L.MonadFlow m) => Text -> table Identity -> m ()
updateAllKeysInIMC pKey val = do
  newTtl <- getConfigEntryNewTtl
  L.setConfig pKey $ mkConfigEntry newTtl val
  let
    sKeys = getSecondaryLookupKeys val
  mapM_ (updateSecondaryKeyInIMC pKey newTtl) sKeys

updateSecondaryKeyInIMC :: L.MonadFlow m => Text -> LocalTime -> Text -> m ()
updateSecondaryKeyInIMC pKey newttl sKey = do
  pkeyList <- L.getConfig sKey >>= \case
    Nothing -> return []
    Just ls -> return $ unsafeCoerce ls.entry
  L.setConfig sKey $ mkConfigEntry newttl (Set.toList . Set.fromList $ pKey:pkeyList)

deletePrimaryAndSecondaryKeysFromIMC :: forall table m. (KVConnector (table Identity), L.MonadFlow m) => Text -> table Identity -> m ()
deletePrimaryAndSecondaryKeysFromIMC pKey val = do
  -- expiredTime <- getExpiredTime
  -- L.modifyConfig pKey (\cEntry -> cEntry {ttl = expiredTime})
  L.delConfig pKey
  let
    sKeys = getSecondaryLookupKeys val
  mapM_ ((flip L.modifyConfig) (modifySecondaryKeysFromIMC)) sKeys

  where
    _getExpiredTime :: m LocalTime
    _getExpiredTime = do
      currentTime <- L.getCurrentTimeUTC
      return $ addLocalTime (-1 * (secondsToNominalDiffTime $ toPico 1)) currentTime
    modifySecondaryKeysFromIMC :: L.MonadFlow m => ConfigEntry -> ConfigEntry
    modifySecondaryKeysFromIMC configEntry =
      let
        pKeys :: [Text]
        pKeys = unsafeCoerce configEntry.entry
      in mkConfigEntry configEntry.ttl $ DL.delete pKey pKeys

pushToConfigStream :: (L.MonadFlow m ) => Text -> Text -> Text -> Text -> m ()
pushToConfigStream redisName k v streamName =
  void $ L.runKVDB redisName $ L.xadd (encodeUtf8 streamName) L.AutoID [applyFPair encodeUtf8 (k,v)]

pushToInMemConfigStream :: forall table m.
  ( HasCallStack,
    KVConnector (table Identity),
    ToJSON (table Identity),
    L.MonadFlow m
  ) => MeshConfig -> ImcStreamCommand -> table Identity -> m ()
pushToInMemConfigStream meshCfg imcCommand alteredModel = do
  let
    -- pKeyText = getLookupKeyByPKey alteredModel
    -- shard = getShardedHashTag pKeyText
    -- pKey =  pKeyText <> shard
    strmValue = ImcStreamValue {
      command = imcCommand,
      tableRow = Just alteredModel
    }
    strmValueT = decodeUtf8 . A.encode $ strmValue
  mapM_ (pushToConfigStream meshCfg.kvRedis (tableName @(table Identity)) strmValueT) getConfigStreamNames
  pure ()

fetchRowFromDBAndAlterImc :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    ToJSON (table Identity),
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  MeshConfig ->
  Where be table ->
  ImcStreamCommand ->
  m (MeshResult ())
fetchRowFromDBAndAlterImc dbConf meshCfg whereClause imcCommand = do
  let findQuery = DB.findRows (sqlSelect ! #where_ whereClause ! defaults)
  dbRes <- runQuery dbConf findQuery
  case dbRes of
    Right [x] -> do
      when meshCfg.memcacheEnabled $ pushToInMemConfigStream meshCfg imcCommand x
      return $ Right ()
    Right [] -> return $ Right ()
    Right xs -> do
      let message = "DB returned \"" <> show (length xs) <> "\" rows after update for table: " <> show (tableName @(table Identity))
      L.logError @Text "updateWoReturningWithKVConnector" message
      return $ Left $ UnexpectedError message
    Left e -> return $ Left (MDBError e)
