{-# LANGUAGE AllowAmbiguousTypes, CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module EulerHS.KVConnector.InMemConfig.Flow

    where

import Streamly.Data.Serialize.Instances ()
import           Control.Monad.Catch (bracket)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Database.Beam as B
import           EulerHS.Prelude hiding (bracket)
import           EulerHS.SqlDB.Types (BeamRunner, BeamRuntime, ModelDBConfig)
-- import           Control.Monad.Extra (notM)
import qualified Data.HashSet as HS
import qualified Data.Text as T
import           Data.Time (getCurrentTime, utc, utcToZonedTime, zonedTimeToLocalTime)
import           EulerHS.CachedSqlDBQuery (findAll)
import qualified EulerHS.Language as L
#if defined(REDIS_CORE_EXPORT)
#else
import qualified EulerHS.KVDB.Language as L
import qualified EulerHS.Extra.Redis as L
#endif
import           EulerHS.KVConnector.InMemConfig.Types
import           EulerHS.KVConnector.Types (KVConnector(..), MeshConfig, tableName, MeshResult, MeshMeta(..), Source(..), KVEntry(..),prefixedKey,prefixedSKey)
import           Unsafe.Coerce (unsafeCoerce)
import           EulerHS.Runtime (mkConfigEntry)
import           EulerHS.KVConnector.Utils
import           Sequelize (Model, modelTableName, Where, Clause(..))
import qualified Data.Serialize as Serialize
import           EulerHS.KVConnector.DBSync (whereClauseToJson)
import EulerHS.ART.Types 
import           EulerHS.ART.FlowUtils (addRecToState)
import           EulerHS.ART.V2.FlowUtils (producePayload)
import           EulerHS.Extra.KafkaClient.Utils (ValueType(..))
import           EulerHS.Logger.Types (ErrorL(..))
import qualified EulerHS.ART.ReplayFunctions as ER
import qualified EulerHS.ART.EnvVars as Env
import qualified EulerHS.ART.Utils as ARTUtils
import qualified EulerHS.ART.V2.Utils as ARTV2Utils
import qualified Data.ByteString.Lazy as BS
import           EulerHS.PIIEncryption hiding (PIIEncryptionKey(..))
import           EulerHS.KVDB.Types (MeshError(MDBError, UnexpectedError))
import qualified GHC.Stack as GHC

import qualified Streamly.Internal.Data.Array as Array (serialize)
import           EulerHS.ART.V2.Types (HasArtRecOptions)
import qualified EulerHS.Framework.Runtime as R
import EulerHS.ART.IOReplay (runIOWithART)

-- Kepping looperForRedisStream and updateInMemCache for backward compatibility
looperForRedisStream :: forall table m beM.(
    HasCallStack,
    HasArtRecOptions,
    KVConnector (table Identity),
    L.MonadFlow m
    ) =>
    ModelDBConfig beM ->
    (ByteString -> Either String (ImcStreamValue (table Identity))) -> Text -> MeshConfig -> m ()
looperForRedisStream dbConf decodeTable strmName meshConfig = bracket 
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
              initRecords <- getRecordsFromStream meshConfig.kvRedis strmName rId tName
              case initRecords of 
                  Nothing -> do
                      L.setOption (RecordId tName) rId
                      return ()
                  Just (latestId, rs) -> do
                      L.setOption (RecordId tName) latestId
                      mapM_ (updateInMemCache dbConf tName decodeTable meshConfig) rs
          Just rId -> do
              newRecords <- getRecordsFromStream meshConfig.kvRedis strmName rId tName
              case newRecords of
                  Nothing ->
                      return ()
                  Just (latestId, rs) -> do
                      L.setOption (RecordId tName) latestId
                      mapM_ (updateInMemCache dbConf tName decodeTable meshConfig) rs
      void $ looperDelayInSec)

updateInMemCache :: forall table m beM.(
    HasCallStack,
    KVConnector(table Identity),
    L.MonadFlow m
    ) =>
    ModelDBConfig beM ->
    Text ->
    (ByteString -> Either String (ImcStreamValue (table Identity))) ->
    MeshConfig -> 
    RecordKeyValues -> m ()
updateInMemCache dbConf tName decodeTable meshConfig (k,val) = do
  when (tName == k) $                             -- decode only when entry is for the looper's table
    case decodeTable val of
        Left e-> do
          L.logErrorWithCategory ("setInMemCache" :: Text) ("Unable to decode ImcStreamValue for the table <" <> k) $ ErrorL Nothing "MEM_CACHE_ERROR" $ T.pack e
          return ()
        Right strmVal -> do
          when (meshConfig.memcacheEnabled) $ invalidatePrimaryKeys strmVal
          when (meshConfig.memcacheFindAllEnabled) $ invalidateSecondaryKeys strmVal 

  where 
    invalidatePrimaryKeys strmVal = do
       let prefixesList = maybe [Nothing] (map (\x -> if x == "" then Nothing else Just x)) strmVal.cacheKeyPrefixes
       mapM_ (\prefix -> do
        let meshCfg = meshConfig{cachePrefix = prefix, prefixMigrationMode = L.OLD_WRITE_DISABLED}
        let pKeyText = prefixedKey $ getLookupKeyByPKey (isMySQLConfig dbConf) meshCfg strmVal.tableRow
        invalidateDataCache (Array.serialize pKeyText)) prefixesList

    invalidateSecondaryKeys strmVal = do
      let prefixesList = maybe [Nothing] (map (\x -> if x == "" then Nothing else Just x)) strmVal.cacheKeyPrefixes
      mapM_ (\prefix -> do
        let meshCfg = meshConfig{cachePrefix = prefix, prefixMigrationMode = L.OLD_WRITE_DISABLED}
            lookupKeys = getSecondaryLookupKeys (isMySQLConfig dbConf) meshCfg strmVal.tableRow
            sKeyList = map prefixedSKey lookupKeys
        invalidateDataCacheFindAll (fmap Array.serialize sKeyList)) prefixesList

looperForRedisStreamU :: forall m.(
    HasCallStack,
    HasArtRecOptions,
    L.MonadFlow m
    ) => 
    Text -> (RecordKeyValues -> m ()) -> Text -> MeshConfig -> m ()
looperForRedisStreamU looperId looperHandler strmName meshConfig = bracket 
    (pure ())
    (\ _ -> do
      L.logInfoT "looperForRedisStreamU failed" ("Setting LooperStarted option as False for Looper " <> looperId)
      L.setOption (LooperStarted looperId) False)
    (\ _ -> forever $ do
      maybeRId <- L.getOption (RecordId looperId)
      case maybeRId  of
          Nothing -> do
              rId <- getLooperCurrIdWithOffset
              initRecords <- getRecordsFromStream meshConfig.kvRedis strmName rId looperId
              case initRecords of 
                  Nothing -> do
                      L.setOption (RecordId looperId) rId
                      return ()
                  Just (latestId, rs) -> do
                      L.setOption (RecordId looperId) latestId
                      mapM_ looperHandler rs
          Just rId -> do
              newRecords <- getRecordsFromStream meshConfig.kvRedis strmName rId looperId
              case newRecords of
                  Nothing ->
                      return ()
                  Just (latestId, rs) -> do
                      L.setOption (RecordId looperId) latestId
                      mapM_ looperHandler rs
      void $ looperDelayInSec)


looperDelayInSec :: (L.MonadFlow m) => m ()
looperDelayInSec = (runIOWithART "EulerHS.KVConnector.InMemConfig.Flow::looperDelayInSec::threadDelay" threadDelay) $ getConfigStreamLooperDelayInSec * 1000000

updateInMemCacheU :: forall table m.(
    HasCallStack,
    KVConnector(table Identity),
    L.MonadFlow m
    ) =>
    Bool ->
    Text ->
    (ByteString -> Either String (ImcStreamValue (table Identity))) ->
    MeshConfig -> 
    RecordKeyValues -> m ()
updateInMemCacheU isMySQL tName decodeTable meshConfig (k,val) = do
  when (tName == k) $                             -- decode only when entry is for the looper's table
    case decodeTable val of
        Left e-> do
          L.logErrorWithCategory ("setInMemCache" :: Text) ("Unable to decode ImcStreamValue for the table <" <> k) $ ErrorL Nothing "MEM_CACHE_ERROR" $ T.pack e
          return ()
        Right strmVal -> do
          when (meshConfig.memcacheEnabled) $ invalidatePrimaryKeys strmVal
          when (meshConfig.memcacheFindAllEnabled) $ invalidateSecondaryKeys strmVal 

  where 
    invalidatePrimaryKeys strmVal = do
       let prefixesList = maybe [Nothing] (map (\x -> if x == "" then Nothing else Just x)) strmVal.cacheKeyPrefixes
       mapM_ (\prefix -> do
        let meshCfg = meshConfig{cachePrefix = prefix, prefixMigrationMode = L.OLD_WRITE_DISABLED}
        let pKeyText = prefixedKey $ getLookupKeyByPKey isMySQL meshCfg strmVal.tableRow
        invalidateDataCache (Array.serialize pKeyText)) prefixesList

    invalidateSecondaryKeys strmVal = do
      let prefixesList = maybe [Nothing] (map (\x -> if x == "" then Nothing else Just x)) strmVal.cacheKeyPrefixes
      mapM_ (\prefix -> do
        let meshCfg = meshConfig{cachePrefix = prefix, prefixMigrationMode = L.OLD_WRITE_DISABLED}
            lookupKeys = getSecondaryLookupKeys isMySQL meshCfg strmVal.tableRow
            sKeyList = map prefixedSKey lookupKeys
        invalidateDataCacheFindAll (fmap Array.serialize sKeyList)) prefixesList


extractRecordsFromStreamResponse :: [L.KVDBStreamReadResponseRecord] -> [RecordKeyValues]
extractRecordsFromStreamResponse  = foldMap (fmap (bimap decodeUtf8 id) . L.records)

getRecordsFromStream :: HasArtRecOptions => Text -> Text -> LatestRecordId -> Text -> (L.MonadFlow m) => m (Maybe (LatestRecordId, [RecordKeyValues]))
getRecordsFromStream redisName' strmName lastRecordId looperId = do
    end <- getLooperCurrIdWithOffset
    eitherReadResponse <- L.rXrangeT redisName' strmName lastRecordId end (Just getConfigStreamFetchLimit)
    case eitherReadResponse of
        Left err -> do
            L.delOption (RecordId looperId)    -- TODO Necessary?
            L.logErrorWithCategoryV @Text ("getRecordsFromStream recorded 1" :: Text) ("Error getting initial records from stream <" <> strmName <> ">", err) $ ErrorL Nothing "STREAM_ERROR" (show err)
            return Nothing
        Right maybeRs -> case maybeRs of
            [] -> return Nothing
            rss -> do
              case uncons . reverse $ rss of
                  Nothing -> return Nothing
                  Just (latestRecord, _) -> do
                          updRecordId <- maybe
                            getLooperCurrIdWithOffset
                            (\(L.KVDBStreamEntryID t s) -> pure . kvdbStreamEntryIDToString $ L.KVDBStreamEntryID t (s+1))
                            (parseKVDBStreamEntryIDFromString (decodeUtf8 $ L.recordId latestRecord))
                          L.logDebugT ("getRecordsFromStream for " <> looperId) $ (show . length $ rss) <> " new records in stream <" <> strmName <> ">"
                          return $ Just (updRecordId ,extractRecordsFromStreamResponse rss)

-- IMC Replay function for ART
imcReplay :: forall be beM table m.
  (
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    HasCallStack,
    KVConnector (table Identity),
    ToJSON (table Identity),
    Show (table Identity),
    Serialize.Serialize (table Identity),
    PII table,
    FromJSON (table Identity),
    Model be table,
    MeshMeta be table,
    L.MonadFlow m
  ) =>  Text -> ModelDBConfig beM ->
        Where be table ->
        m (Source, MeshResult (Maybe (KVEntry table)))
imcReplay method _ whereClause = do
  recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.KVConnector.InMemConfig.Flow::imcReplay::getCurrentTime" getCurrentTime)
  let recInmem = RunInMemEntryT (RunInMemEntry method A.Null (whereClauseToJson whereClause) (toJSON $ modelTableName @table) (A.Null) recTimestamp)
  msessionId <- L.getLoggerContext "x-request-id"
  resp <- ER.callBrahmaReplayDB recInmem msessionId
  meshRes <- parseDataReplay resp
  pure (IN_MEM, meshRes)

-- IMC Replay function for ART_V2
imcReplayV2 :: forall be beM table m.
  (
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    HasCallStack,
    KVConnector (table Identity),
    ToJSON (table Identity),
    Show (table Identity),
    Serialize.Serialize (table Identity),
    PII table,
    FromJSON (table Identity),
    Model be table,
    MeshMeta be table,
    L.MonadFlow m
  ) =>  Text -> ModelDBConfig beM ->
        Where be table ->
        m (Maybe R.ConfigEntry)
imcReplayV2 method _ whereClause = do
  recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.KVConnector.InMemConfig.Flow::imcReplayV2::getCurrentTime" getCurrentTime)
  let recInmem = RunInMemEntryT (RunInMemEntry method A.Null (whereClauseToJson whereClause) (toJSON $ modelTableName @table) (A.Null) recTimestamp)
  msessionId <- L.getLoggerContext "x-request-id"
  resp <- ER.callBrahmaReplayDB recInmem msessionId
  let eReply = A.eitherDecode resp :: Either String InMemEntryValue
  case eReply of
    Left err -> do 
      let errorMsg = "Failed to decode response: " <> (T.pack err)
      when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR" $ GHC.prettyCallStack $ GHC.callStack
      L.logErrorV @Text "IMC_REPLAY_RESPONSE_PARSE_ERROR" $ (errorMsg :: Text)
      pure Nothing
    Right reply -> do
      let mbKVEntry = A.parseEither A.parseJSON reply.jsonVal :: Either String (Maybe (table Identity))
      case mbKVEntry of
        Left err -> do 
          let errorMsg = "Failed to decode response: " <> (T.pack err)
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_IMC" $ GHC.prettyCallStack $ GHC.callStack
          L.logErrorV @Text "IMC_REPLAY_TABLE_PARSE_ERROR" $ (errorMsg :: Text)
          pure . Just $ mkConfigEntry reply.ttl Nothing
        Right kvEntry -> pure . Just $ mkConfigEntry reply.ttl kvEntry

-- IMC Replay function for ART
imcReplayList :: forall be beM table m.
  (
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    HasCallStack,
    KVConnector (table Identity),
    ToJSON (table Identity),
    Show (table Identity),
    Serialize.Serialize (table Identity),
    PII table,
    FromJSON (table Identity),
    Model be table,
    MeshMeta be table,
    L.MonadFlow m
  ) =>  Text -> ModelDBConfig beM ->
        Where be table ->
        m (Source, MeshResult [KVEntry table])
imcReplayList method _ whereClause = do
  recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.KVConnector.InMemConfig.Flow::imcReplayList::getCurrentTime" getCurrentTime)
  let recInmem = RunInMemEntryT (RunInMemEntry method A.Null (whereClauseToJson whereClause) (toJSON $ modelTableName @table) (A.Null) recTimestamp)
  msessionId <- L.getLoggerContext "x-request-id"
  resp <- ER.callBrahmaReplayDB recInmem msessionId
  meshRes <- parseDataReplayList resp
  pure (IN_MEM, meshRes)

-- IMC Replay function for ART_V2
imcReplayListV2 :: forall be beM table m.
  (
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    HasCallStack,
    KVConnector (table Identity),
    ToJSON (table Identity),
    Show (table Identity),
    Serialize.Serialize (table Identity),
    PII table,
    FromJSON (table Identity),
    Model be table,
    MeshMeta be table,
    L.MonadFlow m
  ) =>  Text -> ModelDBConfig beM ->
        Where be table ->
        m (Maybe R.ConfigEntry)
imcReplayListV2 method _ whereClause = do
  recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.KVConnector.InMemConfig.Flow::imcReplayListV2::getCurrentTime" getCurrentTime)
  let recInmem = RunInMemEntryT (RunInMemEntry method A.Null (whereClauseToJson whereClause) (toJSON $ modelTableName @table) (A.Null) recTimestamp)
  msessionId <- L.getLoggerContext "x-request-id"
  resp <- ER.callBrahmaReplayDB recInmem msessionId
  let eReply = A.eitherDecode resp :: Either String InMemEntryValue
  case eReply of
    Left err -> do 
      let errorMsg = "Failed to decode response: " <> (T.pack err)
      when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR" $ GHC.prettyCallStack $ GHC.callStack
      L.logErrorV @Text "IMC_REPLAY_RESPONSE_PARSE_ERROR" $ (errorMsg :: Text)
      pure Nothing
    Right reply -> do
      let mbKVEntry = A.parseEither A.parseJSON reply.jsonVal :: Either String (Maybe [table Identity])
      case mbKVEntry of
        Left err -> do 
          let errorMsg = "Failed to decode response: " <> (T.pack err)
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_IMC" $ GHC.prettyCallStack $ GHC.callStack
          L.logErrorV @Text "IMC_REPLAY_TABLE_PARSE_ERROR" $ (errorMsg :: Text)
          pure . Just $ mkConfigEntry reply.ttl Nothing
        Right kvEntry -> pure . Just $ mkConfigEntry reply.ttl kvEntry

-- NOTE: If there are any new cases for IMC, We need to add recording and replay wrapper.

{-# INLINE searchInMemoryCacheHelper #-}
searchInMemoryCacheHelper :: forall be beM table m.
  (
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    HasCallStack,
    KVConnector (table Identity),
    ToJSON (table Identity),
    Show (table Identity),
    Serialize.Serialize (table Identity),
    PII table,
    FromJSON (table Identity),
    Model be table,
    L.MonadFlow m ,
    MeshMeta be table
  ) => Text -> ModelDBConfig beM ->
        Where be table ->
        MeshConfig ->
        (ModelDBConfig beM -> 
          MeshConfig ->
          Where be table ->
          m (Source, MeshResult (Maybe (KVEntry table)))) ->
        m (Source, MeshResult (Maybe (KVEntry table)))
searchInMemoryCacheHelper method dbConf whereClause meshCfg kvFetch = do
  isArtV2ReplayEnabledWithSessId <- ARTV2Utils.shouldARTV2Replay
  mbVal <- if isArtV2ReplayEnabledWithSessId 
    then imcReplayV2 method dbConf whereClause 
    else L.getConfig inMemCacheKey FIND_ONE
  shouldRecord <- ARTUtils.isArtRecEnabled
  shouldV2Record <- ARTV2Utils.isArtV2RecEnabled
  case mbVal of
    Just val -> do
      currentTime <- L.getPOSIXTime
      if (val.ttl < currentTime)
        then getFromDBAndCache
        else
          case val.entry of
            Nothing -> do
              when shouldLogFindDBCallLogs $ L.logDebugT "IMC_EMPTY_RESULT" $ (tableName @(table Identity))
              if Env.isArtReplayEnabled
                then imcReplay method dbConf whereClause
                else do
                  when shouldRecord $ do
                    recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.KVConnector.InMemConfig.Flow::searchInMemoryCacheHelper::getCurrentTime" getCurrentTime )
                    addRecToState $ RunInMemEntryT (RunInMemEntry method A.Null (whereClauseToJson whereClause) (toJSON $ modelTableName @table) (toJSON $ A.Null) recTimestamp)
                  when shouldV2Record $ do
                    recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.KVConnector.InMemConfig.Flow::searchInMemoryCacheHelper::getCurrentTime" getCurrentTime )
                    producePayload IMC . BS.toStrict . A.encode $ RunInMemEntryT (RunInMemEntry method A.Null (whereClauseToJson whereClause) (toJSON $ modelTableName @table) (toJSON $ InMemEntryValue val.ttl A.Null) recTimestamp)
                  return (IN_MEM, Right Nothing)         
            Just item -> do
              let parsedVal = (unsafeCoerce @_ @(table Identity) item)
              when shouldLogFindDBCallLogs $ L.logDebugV ("IMC_RESULT_FOUND for " <> tableName @(table Identity) ::Text) parsedVal
              if Env.isArtReplayEnabled
                then imcReplay method dbConf whereClause
                else do
                  when shouldRecord $ do
                    recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.KVConnector.InMemConfig.Flow::searchInMemoryCacheHelper::getCurrentTime" getCurrentTime )
                    addRecToState $ RunInMemEntryT (RunInMemEntry method A.Null (whereClauseToJson whereClause) (toJSON $ modelTableName @table) (toJSON $ getKVEntryWithDefaultDB parsedVal) recTimestamp)
                  when shouldV2Record $ do
                    recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.KVConnector.InMemConfig.Flow::searchInMemoryCacheHelper::getCurrentTime" getCurrentTime )
                    producePayload IMC . BS.toStrict . A.encode $ RunInMemEntryT (RunInMemEntry method A.Null (whereClauseToJson whereClause) (toJSON $ modelTableName @table) (toJSON $ InMemEntryValue val.ttl $ toJSON parsedVal) recTimestamp)
                  return . (IN_MEM,) . Right . Just $ (getKVEntryWithDefaultDB parsedVal)
    Nothing -> getFromDBAndCache
  where
    inMemCacheKey = getInMemCacheKeyFromWhereClause dbConf (withCacheKeyPrefix meshCfg $ inMemPrefix @(table Identity) <> (tableName @(table Identity)) <>"_key_") meshModelTableEntityDescriptor (And whereClause)
    {-# INLINE getFromDBAndCache #-}
    getFromDBAndCache = do
      (source, eiVal) <- (kvFetch dbConf meshCfg whereClause)
      when (isRight eiVal) $ do
        let mbVal = fromRight Nothing eiVal
        case mbVal of
          Just val -> do
            newTtl <- runIOWithART "EulerHS.KVConnector.InMemConfig.Flow::searchInMemoryCacheHelper::getFromDBAndCache::getConfigEntryNewTtlIO" getConfigEntryNewTtlIO
            setInMemCache newTtl val.row
          Nothing -> do
            when shouldLogFindDBCallLogs  $ L.logDebugT ("IMC_KEY_NOT_FOUND_IN_DB - ") ((tableName @(table Identity)))
            newTtl <- runIOWithART "EulerHS.KVConnector.InMemConfig.Flow::searchInMemoryCacheHelper::getFromDBAndCache::getConfigNullEntryNewTtlIO" getConfigNullEntryNewTtlIO
            L.setConfig inMemCacheKey (mkConfigEntry newTtl Nothing) FIND_ONE
      pure (source, eiVal)

    setInMemCache newTtl val = do
      L.setConfig inMemCacheKey (mkConfigEntry newTtl $ Just val) FIND_ONE
      let pKeyText = prefixedKey $ getLookupKeyByPKey (isMySQLConfig dbConf) meshCfg val
      addToInMemTrackerIO inMemCacheKey (Array.serialize pKeyText) FIND_ONE

{-# INLINE searchInMemoryCache #-}
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
    PII table,
    FromJSON (table Identity),
    
    Model be table,
    MeshMeta be table,
    L.MonadFlow m
  ) =>  Text -> ModelDBConfig beM ->
        Where be table ->
        MeshConfig ->
        (ModelDBConfig beM -> 
        MeshConfig ->
        Where be table ->
        m (Source, MeshResult (Maybe (KVEntry table)))) ->
        m (Source, MeshResult (Maybe (KVEntry table)))
searchInMemoryCache method dbConf whereClause meshCfg kvFetch = do
   searchInMemoryCacheHelper method dbConf whereClause meshCfg kvFetch

{-# NOINLINE searchInMemoryCacheFindAll #-}
searchInMemoryCacheFindAll :: forall be beM table m.
  (
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    HasCallStack,
    KVConnector (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    PII table,
    FromJSON (table Identity),
    Show (table Identity),
    Model be table,
    MeshMeta be table,
    L.MonadFlow m
  ) =>  Text -> ModelDBConfig beM ->
        Where be table ->
        MeshConfig ->
        (ModelDBConfig beM -> 
          MeshConfig ->
          Where be table ->
          m (MeshResult [KVEntry table])) ->
        m (Source, MeshResult [KVEntry table])
searchInMemoryCacheFindAll method dbConf whereClause meshCfg kvFetchFindAll = do
  isArtV2ReplayEnabledWithSessId <- ARTV2Utils.shouldARTV2Replay
  mbVal <- if isArtV2ReplayEnabledWithSessId 
    then imcReplayListV2 method dbConf whereClause 
    else L.getConfig inMemCacheKey FIND_ALL
  case mbVal of
    Just val -> do
      currentTime <- L.getPOSIXTime
      if (val.ttl < currentTime)
        then getFromDBAndCache
        else 
          case val.entry of
            Nothing -> do
             when shouldLogFindDBCallLogs $ L.logDebugT "IMC_EMPTY_RESULT" $ (tableName @(table Identity))
             if Env.isArtReplayEnabled
               then imcReplayList method dbConf whereClause
               else do
                 whenM ARTUtils.isArtRecEnabled $ do
                   recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.KVConnector.InMemConfig.Flow::searchInMemoryCacheFindAll::getCurrentTime" getCurrentTime )
                   addRecToState $ RunInMemEntryT (RunInMemEntry method A.Null (whereClauseToJson whereClause) (toJSON $ modelTableName @table) (A.Null) recTimestamp)
                 whenM ARTV2Utils.isArtV2RecEnabled $ do
                   recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.KVConnector.InMemConfig.Flow::searchInMemoryCacheFindAll::getCurrentTime" getCurrentTime )
                   producePayload IMC . BS.toStrict . A.encode $ RunInMemEntryT (RunInMemEntry method A.Null (whereClauseToJson whereClause) (toJSON $ modelTableName @table) (toJSON $ InMemEntryValue val.ttl A.Null) recTimestamp)
                 return (IN_MEM, Right [])         
            Just item -> do
              let parsedVal = (unsafeCoerce @_ @[table Identity] item)
              when shouldLogFindDBCallLogs $ L.logDebugV ("IMC_RESULT_FOUND for " <> tableName @(table Identity) ::Text) parsedVal
              if Env.isArtReplayEnabled
                then imcReplayList method dbConf whereClause
                else do
                  whenM ARTUtils.isArtRecEnabled $ do
                    recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.KVConnector.InMemConfig.Flow::searchInMemoryCacheFindAll::getCurrentTime" getCurrentTime )
                    addRecToState $ RunInMemEntryT (RunInMemEntry method A.Null (whereClauseToJson whereClause) (toJSON $ modelTableName @table) (toJSON parsedVal) recTimestamp)
                  whenM ARTV2Utils.isArtV2RecEnabled $ do
                    recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.KVConnector.InMemConfig.Flow::searchInMemoryCacheFindAll::getCurrentTime" getCurrentTime )
                    producePayload IMC . BS.toStrict . A.encode $ RunInMemEntryT (RunInMemEntry method A.Null (whereClauseToJson whereClause) (toJSON $ modelTableName @table) (toJSON $ InMemEntryValue val.ttl $ toJSON parsedVal) recTimestamp)
                  return (IN_MEM, Right $ (map getKVEntryWithDefaultDB) parsedVal)
    Nothing -> getFromDBAndCache
  where
    inMemCacheKey = getInMemCacheKeyFromWhereClause dbConf (withCacheKeyPrefix meshCfg $ inMemPrefix @(table Identity) <> (tableName @(table Identity)) <>"findAll_key_") meshModelTableEntityDescriptor (And whereClause)
    {-# INLINE getFromDBAndCache #-}
    getFromDBAndCache = do 
      eiVal <- kvFetchFindAll dbConf meshCfg whereClause
      void $ case eiVal of
        Left _err ->  when shouldLogFindDBCallLogs $ L.logDebugT ("DB_ERROR_OCCURRED_IN_FINDALL- ") ((tableName @(table Identity)))
        Right val -> do
            newTtl <- if null val then getConfigNullEntryNewTtl else getConfigEntryNewTtl
            when (length val <= inMemFindAllCacheLimit) (void $ setInMemCache newTtl (map row val))
      pure (KV_AND_SQL, eiVal)

    setInMemCache newTtl val = do
      L.setConfig inMemCacheKey (mkConfigEntry newTtl $ Just val) FIND_ALL
      let sKeyList = fmap Array.serialize $ mkUniq $ concatMap (\v -> prefixedSKey <$> (getSecondaryLookupKeys (isMySQLConfig dbConf) meshCfg v)) val
      when shouldLogFindDBCallLogs $ L.logDebugT ("IMC_KEY_NOT_FOUND_IN_DB - ") ((tableName @(table Identity)))
      mapM (\sKey' -> addToInMemTracker inMemCacheKey sKey' FIND_ALL) sKeyList

{-# INLINE addToInMemTracker #-}
addToInMemTracker :: L.MonadFlow m => W8Arr -> W8Arr -> QueryType -> m ()
addToInMemTracker inMemCacheKey kVal queryType = do
  addToInMemTrackerIO inMemCacheKey kVal queryType

{-# INLINE addToInMemTrackerIO #-}
addToInMemTrackerIO
    :: L.MonadFlow m
    => W8Arr
    -> W8Arr
    -> QueryType
    -> m ()
addToInMemTrackerIO inMemCacheKey kVal queryType = do
  newTtl <- getConfigEntryNewTtl
  let f = (\cfgEntry -> do
                      let newEntry = case cfgEntry of
                            Just val -> do
                              case val.entry of
                                Just item -> HS.insert inMemCacheKey (unsafeCoerce @_ @(HashSet W8Arr) item)
                                Nothing -> HS.singleton inMemCacheKey
                            Nothing -> HS.singleton inMemCacheKey
                      mkConfigEntry newTtl $ Just newEntry)
  let trackerKey = kVal
  L.modifyConfig trackerKey f queryType

{-# INLINE invalidateDataCache #-}
invalidateDataCache :: L.MonadFlow m => W8Arr -> m ()
invalidateDataCache kVal = do
 let trackerKey = kVal
 cfgEntry <- L.getConfig trackerKey FIND_ONE
 case cfgEntry of
  Just val -> do
    case val.entry of
      Just item -> do
        let trackerValues = (unsafeCoerce @_ @(HashSet W8Arr) item)
        mapM_ (\tVal -> L.delConfig tVal FIND_ONE) trackerValues
      Nothing -> pure ()
  Nothing -> pure ()

{-# INLINE invalidateDataCacheFindAll #-}
invalidateDataCacheFindAll :: L.MonadFlow m => [W8Arr] -> m ()
invalidateDataCacheFindAll sKeyList = do
  cfgEntryList <- mapM (\sKeyVal -> do
    let trackerKey = sKeyVal
    cfgEntry <- L.getConfig trackerKey FIND_ALL
    pure $ case cfgEntry of
      Just val -> do
        case val.entry of
          Just item -> (unsafeCoerce @_ @(HashSet W8Arr) item)
          Nothing -> mempty
      Nothing -> mempty) sKeyList
  mapM_ (\tVal -> L.delConfig tVal FIND_ALL) (HS.unions cfgEntryList)

pushToConfigStream :: (L.MonadFlow m, HasArtRecOptions) => Text -> Text -> Text -> Text -> m ()
pushToConfigStream redisName' k v strmName = 
  void $ L.rXaddBWithART redisName' (encodeUtf8 strmName) [(encodeUtf8 k,encodeUtf8 v)] L.AutoID

pushToInMemConfigStream :: forall table m.
  ( KVConnector (table Identity),
    HasArtRecOptions,
    ToJSON (table Identity),
    L.MonadFlow m
  ) => MeshConfig -> ImcStreamCommand -> table Identity -> m ()
pushToInMemConfigStream meshCfg imcCommand alteredModel = do
  let
    strmValue = ImcStreamValue {
      command = imcCommand,
      tableRow = Just alteredModel,
      cacheKeyPrefixes = getCacheKeyPrefixes
    }
    strmValueT = decodeUtf8 . A.encode $ strmValue
  mapM_ (pushToConfigStream meshCfg.kvRedis (tableName @(table Identity)) strmValueT) getConfigStreamNames
  pure ()

  where
    getCacheKeyPrefixes = case (meshCfg.cachePrefix, meshCfg.prefixMigrationMode) of
      (Nothing, _) -> Nothing
      (_, L.NEW_WRITE_DISABLED) -> Nothing
      (Just prefix, L.OLD_WRITE_DISABLED) -> Just [prefix]
      (Just prefix, _) -> Just [prefix, ""]

parseDataReplay ::(FromJSON b,L.MonadFlow m,HasCallStack) => BS.ByteString -> m (Either MeshError (Maybe b))
parseDataReplay res = do
  let eReply = A.eitherDecode res :: (FromJSON b) => Either String (Either MeshError (Maybe b))
  case eReply of
    Left err -> do 
      let errorMsg = "Failed to decode response: " <> (T.pack err)
      when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR" $ GHC.prettyCallStack $ GHC.callStack
      L.logErrorV @Text "IMC_REPLAY_PARSE_ERROR" $ (errorMsg :: Text)
      pure $ Right Nothing
    Right reply -> pure $ reply

parseDataReplayList ::(FromJSON b,L.MonadFlow m,HasCallStack) => BS.ByteString -> m (Either MeshError [b])
parseDataReplayList res = do
  let eReply = A.eitherDecode res :: (FromJSON b) => Either String (Either MeshError [b])
  case eReply of
    Left err -> do 
      let errorMsg = "Failed to decode response: " <> (T.pack err)
      when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR" $ GHC.prettyCallStack $ GHC.callStack
      L.logErrorV @Text "IMC_REPLAY_PARSE_ERROR" $ (errorMsg :: Text)
      pure $ Right []
    Right reply -> pure $ reply


fetchRowFromDBAndAlterImc :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    MeshMeta be table,
    ToJSON (table Identity),
    FromJSON (table Identity),
    PII table,
    L.MonadFlow m
  ) =>
  ModelDBConfig beM ->
  MeshConfig ->
  Where be table ->
  ImcStreamCommand ->
  m (MeshResult ())
fetchRowFromDBAndAlterImc dbConf meshCfg whereClause imcCommand = L.withArtRecOptionsForKVDB @table $ do
  -- let findQuery = DB.findRows (sqlSelect ! #where_ whereClause ! defaults)
  if Env.isArtReplayEnabled
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.KVConnector.InMemConfig.Flow::fetchRowFromDBAndAlterImc::getCurrentTime" getCurrentTime)
      msessionId <- L.getLoggerContext "x-request-id"
      let recDBFind = RunDBEntryT (RunDBEntry "fetchRowFromDBAndAlterImc" A.Null (whereClauseToJson whereClause) (modelTableName @table) A.Null recTimestamp)
      _ <- ER.callBrahmaReplayDB recDBFind msessionId
      return $ Right ()
    else do
      dbRes <- findAll dbConf Nothing whereClause meshCfg.cleanDBHardKilled
      whenM ARTUtils.isArtRecEnabled $ do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.KVConnector.InMemConfig.Flow::fetchRowFromDBAndAlterImc::getCurrentTime" getCurrentTime)
        addRecToState $ RunDBEntryT (RunDBEntry "fetchRowFromDBAndAlterImc" A.Null (whereClauseToJson whereClause) (modelTableName @table) (toJSON dbRes) recTimestamp)
      whenM (ARTV2Utils.isArtV2RecEnabledForTable @table) $ do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.KVConnector.InMemConfig.Flow::fetchRowFromDBAndAlterImc::getCurrentTime" getCurrentTime)
        producePayload DB . BS.toStrict . A.encode $ RunDBEntryT (RunDBEntry "fetchRowFromDBAndAlterImc" A.Null (whereClauseToJson whereClause) (modelTableName @table) (toJSON dbRes) recTimestamp)
      case dbRes of
        Right [x] -> do
          when meshCfg.memcacheEnabled $ pushToInMemConfigStream meshCfg imcCommand x.row
          return $ Right ()
        Right [] -> return $ Right ()
        Right xs -> do
          let message = "DB returned \"" <> show (length xs) <> "\" rows after update for table: " <> (tableName @(table Identity))
          L.logErrorWithCategory @Text "updateWoReturningWithKVConnector" message $ ErrorL Nothing "DB_ERROR" message
          return $ Left $ UnexpectedError message
        Left e -> return $ Left (MDBError e)
