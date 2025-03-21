{-# LANGUAGE AllowAmbiguousTypes, CPP #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE NamedFieldPuns #-}

module EulerHS.KVConnector.Flow
  (
    createWoReturingKVConnector,
    createWithKVConnector,
    findWithKVConnector,
    updateWoReturningWithKVConnector,
    updateWithKVConnector,
    findAllWithKVConnector,
    updateAllWithKVConnector,
    getFieldsAndValuesFromClause,
    updateAllReturningWithKVConnector,
    findAllWithOptionsKVConnector,
    deleteWithKVConnector,
    deleteReturningWithKVConnector,
    deleteAllReturningWithKVConnector,
    reCacheDBRows,
    markDBRowAsDeletedKV
  )
 where


import           EulerHS.PIIEncryption
import           EulerHS.Extra.Time (getCurrentDateInMillis)
import           EulerHS.Prelude hiding (maximum)
import EulerHS.CachedSqlDBQuery
    ( createSqlWoReturing,
      updateOneSqlWoReturning,
      SqlReturning(..),
      findOne,
      findAllSql,
      findAll,
      updateOnePG,
      deleteSql,
      updateAllSql,
      updateAllSqlReturningList,
      findAllWithOptions)
import           EulerHS.KVConnector.Types (KVConnector(..), MeshConfig, MeshResult, MeshMeta(..), SecondaryKey(..),  tableName, keyMap, Source(..), ETLStreamKeys (ETLCreate, ETLUpdate), KVEntry(..), DBName(..), SKvKey)
import           EulerHS.KVConnector.DBSync (getCreateQuery, getUpdateQuery, getDeleteQuery, getDbDeleteCommandJson, getDbUpdateCommandJson, getDbUpdateCommandJsonWithPrimaryKey, getDbDeleteCommandJsonWithPrimaryKey, makeStreamMetadata, DBCommandVersion(..))
import           EulerHS.KVConnector.InMemConfig.Flow (searchInMemoryCache, pushToInMemConfigStream, fetchRowFromDBAndAlterImc, searchInMemoryCacheFindAll)
import           EulerHS.KVConnector.InMemConfig.Types (ImcStreamCommand(..))
import           EulerHS.KVConnector.Utils
import           EulerHS.KVDB.Types (KVDBReply, TxResult(..), MeshError(..))
import           EulerHS.Types (Operation(..),ModelDBConfig(..))
import           Control.Arrow ((>>>))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import           Data.Maybe (listToMaybe)
import qualified EulerHS.Language as L
import qualified Data.Aeson.KeyMap as KM
#if defined(REDIS_CORE_EXPORT)
#else
import qualified EulerHS.KVDB.Language as L
-- import qualified EulerHS.Extra.Redis as L
#endif
import           Data.Either.Extra (mapLeft, mapRight)
import qualified Data.Serialize as Serialize
import           EulerHS.SqlDB.Types (BeamRunner, BeamRuntime, DBError(..))
import           Sequelize (fromColumnar', columnize, modelTableName, modelTableType, Model, Where, Clause(..), Set(..), OrderBy(..))
import qualified Database.Beam as B
import qualified Database.Beam.Postgres as BP
import qualified EulerHS.KVConnector.Encoding as Encoding
import           EulerHS.Logger.Types (ErrorL (..))
import qualified EulerHS.ART.DBReplay as DBReplay
import           EulerHS.ART.V2.Types (HasArtRecOptions)
import qualified EulerHS.Framework.Interpreter as In
import qualified Data.Aeson.Key as AK
import qualified EulerHS.Extra.Redis as R
import qualified EulerHS.EnvVars as Env

createWoReturingKVConnector :: forall (table :: (Type -> Type) -> Type) be m beM.
  ( HasCallStack,
    SqlReturning beM be,
    Model be table,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    FromJSON (table Identity),
    ToJSON (table Identity),
    (PII table),
    Serialize.Serialize (table Identity),
    Show (table Identity),
    KVConnector (table Identity),
    L.MonadFlow m) =>
  ModelDBConfig beM -> 
  MeshConfig ->
  table Identity ->
  m (MeshResult ())
createWoReturingKVConnector dbConf meshCfg value = R.withArtRecOptionsForKVDB @table $ do
  let 
    isEnabled = meshCfg.meshEnabled && not meshCfg.kvHardKilled
    tName     = (modelTableName @table)
  t1                          <- getCurrentDateInMillis
  eitherTableRowWithPrimaryId <- getTableRowWithPrimaryId dbConf meshCfg tName value
  handleError  (eitherTableRowWithPrimaryId) $ \(tableRowWithPrimaryId, idSource) -> do
    res <- create tName isEnabled tableRowWithPrimaryId
    t2  <- getCurrentDateInMillis
    let 
      source = if isEnabled then KV else SQL
      res'   = mapRight (const value) res
    logAndIncrementKVMetric (Just $ In.getConnTagFromDbName dbConf $ getDBName meshCfg) True "CREATE" CREATE res' (Just (t2-t1)) (modelTableName @table) source (Just idSource) Nothing $> res
  where
    handleError :: MeshResult a -> (a -> m (MeshResult b)) -> m (MeshResult b)
    handleError = flip (either (pure . Left))

    create :: HasArtRecOptions => Text -> Bool -> table Identity -> m (MeshResult ()) 
    create tName isEnabled tableRowWithPrimaryId = getEncryptionKey tName >>= \case
      Right mbKeyConfig -> if isEnabled
        then mapRight (const ()) <$> createKV dbConf meshCfg tableRowWithPrimaryId mbKeyConfig 
        else DBReplay.runWithArtCreatemSQl dbConf tableRowWithPrimaryId "createSqlWoReturing" $ createSqlWoReturing dbConf tableRowWithPrimaryId mbKeyConfig (meshCfg.cleanDBEnabled && not meshCfg.cleanDBHardKilled)
      Left e -> return $ Left $ MDBError e

  
createWithKVConnector ::
  forall (table :: (Type -> Type) -> Type) be m beM.
  ( HasCallStack,
    SqlReturning beM be,
    Model be table,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    FromJSON (table Identity),
    PII table,
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    Show (table Identity),
    KVConnector (table Identity),
    L.MonadFlow m) =>
  ModelDBConfig beM -> 
  MeshConfig ->
  table Identity ->
  m (MeshResult (table Identity))
createWithKVConnector dbConf meshCfg value = R.withArtRecOptionsForKVDB @table $ do
  let 
    isEnabled = meshCfg.meshEnabled && not meshCfg.kvHardKilled
    tName     = (modelTableName @table)
  t1                          <- getCurrentDateInMillis
  eitherTableRowWithPrimaryId <- getTableRowWithPrimaryId dbConf meshCfg tName value
  handleError eitherTableRowWithPrimaryId $ \(tableRowWithPrimaryId, idSource) -> do
    res <- imcPush =<< create tName isEnabled tableRowWithPrimaryId 
    t2  <- getCurrentDateInMillis
    let source = if isEnabled then KV else SQL
    logAndIncrementKVMetric (Just $ In.getConnTagFromDbName dbConf $ getDBName meshCfg) True "CREATE" CREATE_RETURNING res (Just (t2-t1)) (modelTableName @table) source (Just idSource) Nothing $> res

  where
    handleError :: MeshResult a -> (a -> m (MeshResult b)) -> m (MeshResult b)
    handleError = flip (either (pure . Left))

    imcPush :: HasArtRecOptions => MeshResult (table Identity) -> m (MeshResult (table Identity))
    imcPush res = res <$ (when meshCfg.memcacheEnabled $ do
      case res of
        Right obj -> pushToInMemConfigStream meshCfg ImcInsert obj
        Left _    -> pure ())

    create :: HasArtRecOptions => Text -> Bool -> table Identity -> m (MeshResult (table Identity)) 
    create tName isKVEnabled tableRowWithPrimaryId = getEncryptionKey tName >>= \case
      Right mbKeyConfig -> if isKVEnabled
        then createKV dbConf meshCfg tableRowWithPrimaryId mbKeyConfig
        else do
          createRes <- DBReplay.runWithArtCreatemSQl dbConf tableRowWithPrimaryId "createReturning" $ createReturning dbConf tableRowWithPrimaryId Nothing mbKeyConfig (meshCfg.cleanDBEnabled && not meshCfg.cleanDBHardKilled)
          whenRight createRes $ \row -> 
              addToETLSAndSQLLogStream meshCfg (isMySQLConfig dbConf) ETLCreate (KVEntry (getDBName meshCfg) row Nothing)
          pure createRes
      Left e -> pure $ Left $ MDBError e

createKV :: forall (table :: (Type -> Type) -> Type) m beM.
  ( FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    Show (table Identity),
    KVConnector (table Identity),
    HasArtRecOptions,
    L.MonadFlow m) =>
  ModelDBConfig beM -> 
  MeshConfig ->
  table Identity ->
  Maybe PIIKeyConfig ->
  m (MeshResult (table Identity))
createKV dbConf meshCfg val mbKeyConfig = do
  let isMySQL = isMySQLConfig dbConf
      pKvKey = getLookupKeyByPKey isMySQL meshCfg val
      shard = pKvKey._shard
      dbName = getDBName meshCfg
  ntag <- makeStreamMetadata dbConf (mbKeyConfig >>= (\x -> Just x.keyId)) pKvKey meshCfg dbName
  time <- fromIntegral <$> L.getCurrentDateInMillis
  let qCmd = (\metadata -> getCreateQuery (tableName @(table Identity)) V1 metadata time dbName val) <$> ntag
  revMappingRes <- createSecondaryKeyMapping isMySQL meshCfg pKvKey val
  case foldEither revMappingRes of
    Left err -> pure $ Left $ MRedisError err
    Right _ -> do
      let qCmdEncoded = (BSL.toStrict . A.encode) <$> qCmd
      t <- fromIntegral <$> L.getCurrentDateInSeconds
      kvRes <- addToDBSyncStreamETLStreamAndRedis meshCfg shard qCmdEncoded ETLCreate pKvKey (KVEntry dbName val (Just t))
      case kvRes of
        Right (TxSuccess res) -> do
              let success = zip res qCmdEncoded
              when meshCfg.shouldPushToSQLWriteLogsStream $ do
                void $ R.rXaddBWithART meshCfg.reconRedis  (encodeUtf8 (getSQLWriteLogsStreamName <> shard)) [(show ETLCreate, encodeUtf8 pKvKey.prefixedKey)] L.AutoID
              incRedisCounterForCreates val meshCfg
              when meshCfg.shouldPushToReconRedis $ 
                forM_ success $ \(L.KVDBStreamEntryID key seqn, qCmd') -> 
                  L.fork $ 
                    R.rZAddWithART 
                      meshCfg.reconRedis 
                      (encodeUtf8 (meshCfg.ecRedisDBStream <> shard <> "_recon")) 
                      [(streamId2Double (L.KVDBStreamEntryID key seqn), qCmd')]
              pure $ Right val

        Right _ -> pure $ Right val
        Left err -> pure $ Left (MRedisError err)

---------------- Update -----------------

updateWoReturningWithKVConnector :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    SqlReturning beM be,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    PII table,
    PIIUpdate be table,
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    B.FromBackendRow be Int,
    Show (table Identity), --debugging purpose
    L.MonadFlow m
  ) =>
  ModelDBConfig beM -> 
  MeshConfig ->
  [Set be table] ->
  Where be table ->
  m (MeshResult ())
updateWoReturningWithKVConnector dbConf meshCfg setClause whereClause = do
  let isDisabled = meshCfg.kvHardKilled
  t1        <- getCurrentDateInMillis
  let tName = (modelTableName @table)
  eitherPiiKeys <- getEncryptionKey tName
  case eitherPiiKeys of 
    Left e -> return $ Left $ MDBError e 
    Right mbval -> do 
      (source, dbName, res) <- if not isDisabled
        then do
          -- Discarding object
          res <-  modifyOneKV dbConf meshCfg whereClause (Just setClause) True True mbval
          let dbName = either (const Nothing) (\x -> db <$> x) (snd res)
          pure $ (fst res, dbName, mapRight (const ()) (snd res))
        else do
          res <- DBReplay.runWithArtUpdate dbConf setClause whereClause "updateOneSqlWoReturning" $ updateOneSqlWoReturning dbConf setClause whereClause mbval meshCfg
          (SQL,Nothing,) <$> case res of
            Right val -> do         
                {-
                  Since beam-mysql doesn't implement updateRowsReturning, we fetch the row from imc (or lower layers)
                  and then update the json so fetched and finally setting it in the imc.
                -}
                if meshCfg.memcacheEnabled
                  then fetchRowFromDBAndAlterImc dbConf meshCfg whereClause ImcInsert
                  else return $ Right val

            Left e -> return $ Left e
      t2        <- getCurrentDateInMillis
      diffRes   <- whereClauseDiffCheck dbConf whereClause
      logAndIncrementKVMetric (In.getConnTagFromDbName dbConf <$> dbName) True "UPDATE" UPDATE res (Just (t2-t1)) (modelTableName @table) source Nothing diffRes
      pure res

updateWithKVConnector :: forall table be beM m.
  ( HasCallStack,
    SqlReturning beM be,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    FromJSON (table Identity),
    PII table,
    PIIUpdate be table,
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    Show (table Identity), --debugging purpose
    B.FromBackendRow be Int,
    L.MonadFlow m
  ) =>
  ModelDBConfig beM -> 
  MeshConfig ->
  [Set be table] ->
  Where be table ->
  m (MeshResult (Maybe (table Identity)))
updateWithKVConnector dbConf meshCfg setClause whereClause = R.withArtRecOptionsForKVDB @table $ do
  let isDisabled = meshCfg.kvHardKilled
  t1        <- getCurrentDateInMillis
  eitherPiiKeys <- getEncryptionKey (modelTableName @table)
  case eitherPiiKeys of 
    Left e -> return $ Left $ MDBError e
    Right mbval -> do
      (source, res) <- if not isDisabled
        then do
          modifyOneKV dbConf meshCfg whereClause (Just setClause) False True mbval
        else do
          res <- genericUpdateReturning dbConf meshCfg setClause whereClause mbval
          (SQL, ) <$> case res of
            Right (Just x) -> do
              when meshCfg.memcacheEnabled $ pushToInMemConfigStream meshCfg ImcInsert x.row
              return $ Right (Just x)
            Right Nothing -> return $ Right Nothing
            Left e -> return $ Left e
      t2        <- getCurrentDateInMillis
      diffRes   <- whereClauseDiffCheck dbConf whereClause
      let res' =  mapRight (row <$>) res
      let dbName = either (const Nothing) (\x -> db <$> x) res
      logAndIncrementKVMetric (In.getConnTagFromDbName dbConf <$> dbName) True "UPDATE" UPDATE_RETURNING res' (Just (t2-t1)) (modelTableName @table) source Nothing diffRes
      pure res'

genericUpdateReturning :: forall table be beM m.
  ( HasCallStack,
    SqlReturning beM be,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    FromJSON (table Identity),
    PII table,
    PIIUpdate be table,
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    Show (table Identity),
    B.FromBackendRow be Int,
    L.MonadFlow m
  ) =>
  ModelDBConfig beM -> 
  MeshConfig ->
  [Set be table] ->
  Where be table ->
  Maybe PIIKeyConfig ->
  m (MeshResult (Maybe (KVEntry table)))
genericUpdateReturning dbConf meshCfg setClause whereClause mbval = R.withArtRecOptionsForKVDB @table $ do
  res <- if isMySQLConfig dbConf
    then do
      findResp <- findOneFromDB dbConf whereClause meshCfg
      result <- DBReplay.runWithArtUpdate dbConf setClause whereClause "updateOneSqlWoReturning" $ updateOneSqlWoReturning dbConf setClause whereClause mbval meshCfg
      case result of 
        Left err -> pure $ Left err
        Right _  -> do
          case findResp of 
              Right (Just respVal) -> case updateModel' setClause respVal.row of
                Right val  -> pure $ Right (Just (respVal {row = val}))
                Left  errU -> L.logErrorV ("UPDATE_MODEL_LOG_FAILURE" :: Text) (A.object [("model", A.String (modelTableName @table)),("error", A.toJSON errU)]) *> 
                              findOneFromDB dbConf whereClause meshCfg-- Doesn't seem to occur but just to avoid decode issue in SQL flow because of KV instances
              _           -> pure findResp
    else do
      res <- mapRight (getKVEntryWithDefaultDB <$> ) <$> (DBReplay.runWithArtUpdate dbConf setClause whereClause "updateWithKVConnector" $ updateOnePG dbConf Nothing setClause whereClause meshCfg)
      case res of
        Right val -> return $ Right val
        Left e -> return $ Left e
  case res of
    Right (Just row) -> addToETLSAndSQLLogStream meshCfg (isMySQLConfig dbConf) ETLUpdate row $> res
    _ -> pure res
  

modifyOneKV :: forall be table beM m.
  ( HasCallStack,
    SqlReturning beM be,
    BeamRuntime be beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    PII table,
    PIIUpdate be table,
    ToJSON (table Identity),
    FromJSON (table Identity),  
    Show (table Identity),
    Serialize.Serialize (table Identity),
    B.FromBackendRow be Int,
    L.MonadFlow m, B.HasQBuilder be, BeamRunner beM) =>
  ModelDBConfig beM -> 
  MeshConfig ->
  Where be table ->
  Maybe [Set be table] ->
  Bool ->
  Bool ->
  Maybe PIIKeyConfig ->
  m (Source, MeshResult (Maybe (KVEntry table)))
modifyOneKV dbConf meshCfg whereClause mbSetClause updateWoReturning isLive mbKeyConfig = do
  let setClause = fromMaybe [] mbSetClause
      updVals = jsonKeyValueUpdates setClause
  kvResult <- findOneFromRedis dbConf meshCfg whereClause
  case kvResult of
    Right ([], []) -> 
      if meshCfg.shouldSkipDBForRecency 
        then pure (KV, Right Nothing)
        else updateInKVOrSQL Nothing updVals setClause
    Right ([], _) -> do
      L.logDebugT "modifyOneKV" ("Modifying nothing - Row is deleted already for " <> tableName @(table Identity))
      pure (KV, Right Nothing)
    Right (kvLiveRows, _) -> do
      findFromDBIfMatchingFailsRes <- findFromDBIfMatchingFails dbConf whereClause kvLiveRows meshCfg
      case findFromDBIfMatchingFailsRes of
        (_, Right [])        -> pure (KV, Right Nothing)
        (SQL, Right [dbRow]) -> updateInKVOrSQL (Just dbRow) updVals setClause
        (KV, Right [obj])   -> (KV,) . mapRight Just <$> (if isLive
           then updateObjectRedis dbConf meshCfg updVals False whereClause mbKeyConfig obj
           else deleteObjectRedis dbConf meshCfg False whereClause obj)
        (source, Right _)   -> do
          L.logErrorWithCategory ("modifyOneKV" :: Text) "Found more than one record in redis - Modification failed" $ ErrorL Nothing "KV_ERROR" ""
          pure (source, Left $ MUpdateFailed "Found more than one record in redis")
        (source, Left err) -> pure (source, Left err)
    Left err -> pure (KV, Left err)

    where
      alterImc :: HasArtRecOptions => Maybe (KVEntry table) -> m (MeshResult ())
      alterImc mbRow = do
        case (isLive, mbRow) of
          (True, Nothing) -> fetchRowFromDBAndAlterImc dbConf meshCfg whereClause ImcInsert
          (True, Just x) -> Right <$> pushToInMemConfigStream meshCfg ImcInsert x.row
          (False, Nothing) ->
              searchInMemoryCache "alterImc" dbConf whereClause meshCfg kvFetch >>= (snd >>> \case
                Left e -> return $ Left e
                Right (Just a) -> Right <$> (pushToInMemConfigStream meshCfg ImcDelete) a.row
                Right (Nothing) -> fetchRowFromDBAndAlterImc dbConf meshCfg whereClause ImcDelete)
          (False, Just x) -> Right <$> pushToInMemConfigStream meshCfg ImcDelete x.row

      runUpdateOrDelete setClause = R.withArtRecOptionsForKVDB @table $ do
        case (isLive, updateWoReturning) of
          (True, True) -> do
            oldRes <- fromRight Nothing <$> findOneFromDB dbConf whereClause meshCfg
            res <- DBReplay.runWithArtUpdate dbConf setClause whereClause "updateOneSqlWoReturning" $ updateOneSqlWoReturning dbConf setClause whereClause mbKeyConfig meshCfg
            case res of
              Right _ -> do
                void $ maybe (return ()) (addToETLSAndSQLLogStream meshCfg (isMySQLConfig dbConf) ETLUpdate) oldRes
                pure $ Right Nothing
              Left e -> return $ Left e
          (True, False) -> genericUpdateReturning dbConf meshCfg setClause whereClause mbKeyConfig
          (False, True) -> do
            -- let deleteQuery = DB.deleteRows $ sqlDelete ! #where_ whereClause
            res <- DBReplay.runWithArtDelete dbConf whereClause "deleteRows" $ deleteSql dbConf whereClause meshCfg
            case res of
                Right _ -> return $ Right Nothing
                Left e  -> return $ Left e
          (False, False) -> do
            res <- DBReplay.runWithArtDelete dbConf whereClause "runUpdateOrDelete" $ deleteAllReturning dbConf whereClause meshCfg
            case res of
                Right [x] -> return $ Right (Just x)
                Right [] -> return $ Right Nothing
                Right xs -> do
                  let message = "DB returned " <> show (length xs) <> " rows after delete for table: " <> (tableName @(table Identity))
                  L.logErrorWithCategory ("deleteReturningWithKVConnector" :: Text) message $ ErrorL Nothing "KV_ERROR" ""
                  return $ Left $ UnexpectedError message
                Left e -> return $ Left e

      updateInKVOrSQL maybeRow updVals setClause = R.withArtRecOptionsForKVDB @table $ do
        if isRecachingEnabled && meshCfg.meshEnabled
          then do
            dbRes <- case maybeRow of
              Nothing    -> findOneFromDB dbConf whereClause meshCfg
              Just dbrow -> pure $ Right $ Just dbrow
            (KV,) <$> case dbRes of
              Right (Just obj) -> do
                reCacheDBRowsRes <- reCacheDBRows dbConf meshCfg [obj] False True False
                case reCacheDBRowsRes of
                  Left err -> return $ Left $ MRedisError err
                  Right _  -> mapRight Just <$> if isLive
                    then updateObjectRedis dbConf meshCfg updVals False whereClause mbKeyConfig obj 
                    else deleteObjectRedis dbConf meshCfg False whereClause obj
              Right Nothing -> pure $ Right Nothing
              Left err -> pure $ Left err
          else (SQL,) <$> (do
            runUpdateOrDelete setClause >>= \case
              Left e -> return $ Left e
              Right mbRow -> if meshCfg.memcacheEnabled
                  then
                    alterImc mbRow <&> ($> mbRow)
                  else
                    return . Right $ mbRow
            )

updateObjectInMemConfig :: forall be table m.
  ( HasCallStack,
    HasArtRecOptions,
    MeshMeta be table ,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    L.MonadFlow m
  ) => MeshConfig -> Where be table -> [(Text, A.Value)] -> table Identity ->  m (MeshResult ())
updateObjectInMemConfig meshCfg _ updVals obj = do
  let shouldUpdateIMC = meshCfg.memcacheEnabled
  if not shouldUpdateIMC
    then pure . Right $ ()
    else
      case (updateModel @be @table) obj updVals of
        Left err -> return $ Left err
        Right updatedModelJson ->
          case A.fromJSON updatedModelJson of
            A.Error decodeErr -> return . Left . MDecodingError . T.pack $ decodeErr
            A.Success (updatedModel' :: table Identity)  -> do
              pushToInMemConfigStream meshCfg ImcInsert updatedModel'
              pure . Right $ ()



updateObjectRedis :: forall beM be table m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    -- Show (table Identity), --debugging purpose
    L.MonadFlow m
  ) =>
  ModelDBConfig beM ->  MeshConfig -> [(Text, A.Value)] -> Bool -> Where be table -> Maybe PIIKeyConfig -> KVEntry table  -> m (MeshResult (KVEntry table))
updateObjectRedis dbConf meshCfg updVals addPrimaryKeyToWhereClause whereClause mbKeyConfig (KVEntry dbName obj updatedAt) = R.withArtRecOptionsForKVDB @table $ do
  configUpdateResult <- updateObjectInMemConfig meshCfg whereClause updVals obj
  when (isLeft configUpdateResult) $ L.logErrorWithCategoryV @Text ("MEMCONFIG_UPDATE_ERROR" :: Text) (configUpdateResult) $ ErrorL Nothing "MEM_CONFIG_ERROR" (show configUpdateResult)
  case (updateModel @be @table) obj updVals of
    Left err -> return $ Left err
    Right updatedModel -> do
      time <- fromIntegral <$> L.getCurrentDateInMillis
      let isMySQL = isMySQLConfig dbConf
          pKvKey  = getLookupKeyByPKey isMySQL meshCfg obj
          shard     = pKvKey._shard
      ntag <- makeStreamMetadata dbConf (mbKeyConfig >>= (\x -> Just x.keyId)) pKvKey meshCfg dbName
      let updateCmd = if addPrimaryKeyToWhereClause 
                        then getDbUpdateCommandJsonWithPrimaryKey dbConf (tableName @(table Identity)) updVals obj whereClause
                        else getDbUpdateCommandJson (tableName @(table Identity)) updVals whereClause
          qCmd      = (\metadata -> getUpdateQuery V1 metadata time dbName updateCmd) <$> ntag
      case resultToEither $ A.fromJSON updatedModel of
        Right value -> do
          let olderSkeys = map (\(SKey s) -> s) (secondaryKeys isMySQL obj)
          skeysUpdationRes <- modifySKeysRedis olderSkeys (KVEntry dbName value updatedAt)
          case skeysUpdationRes of
            Right newKVEntry -> do
              let qCmdEncoded = (BSL.toStrict . A.encode) <$> qCmd
              kvdbRes <- addToDBSyncStreamETLStreamAndRedis meshCfg shard qCmdEncoded ETLUpdate pKvKey newKVEntry
              case kvdbRes of
                Right (TxSuccess res) -> do
                  let success = zip res qCmdEncoded
                  when meshCfg.shouldPushToSQLWriteLogsStream $ do
                    void $ R.rXaddBWithART meshCfg.reconRedis (encodeUtf8 (getSQLWriteLogsStreamName <> shard)) [(show ETLUpdate, encodeUtf8 pKvKey.prefixedKey)] L.AutoID
                  when meshCfg.shouldPushToReconRedis $ 
                    forM_ success $ \(L.KVDBStreamEntryID key seqn, qCmd') -> 
                      L.fork $ 
                        R.rZAddWithART 
                          meshCfg.reconRedis 
                          (encodeUtf8 (meshCfg.ecRedisDBStream <> shard <> "_recon")) 
                          [(streamId2Double (L.KVDBStreamEntryID key seqn), qCmd')]
                  pure $ Right newKVEntry
                Right _ -> pure $ Right newKVEntry
                Left err -> pure $ Left (MRedisError err)
            Left err -> pure $ Left err
        Left err -> pure $ Left $ MDecodingError err

  where
    modifySKeysRedis :: [[(Text, Text)]] -> KVEntry table -> m (MeshResult (KVEntry table)) -- TODO: Optimise this logic
    modifySKeysRedis olderSkeys (KVEntry db table updatedAtTime)  = do
      let isMySQL = isMySQLConfig dbConf
          pKvKey = getLookupKeyByPKey isMySQL meshCfg table
      currTime <- fromIntegral <$> L.getCurrentDateInSeconds
      let (newUpdatedAt, skipTtlReset) = case updatedAtTime of 
                      Just oldTtl -> if oldTtl + secondaryKeyExpiryBuffer > currTime 
                                        then (oldTtl, True)
                                      else (currTime, False)
                      Nothing -> (currTime, False)

      let newKVEntry = KVEntry db table (Just newUpdatedAt)
      let tName = tableName @(table Identity)
          newKeysMap = KM.fromList $ concatMap (map (\(k, v) -> (AK.fromText k, v))) newSkeys
          oldKeysMap = KM.fromList $ concatMap (map (\(k, v) -> (AK.fromText k, v))) olderSkeys
          (modifiedSkeysOld, unModifiedSkeys) = applyFPair (map (getSortedKeyAndValue tName)) $
                                                segregateList (`isKeyModified` newKeysMap) olderSkeys
          newSkeys =  map (\(SKey s) -> s) (secondaryKeys isMySQL table)
          (modifiedSkeysNew, _) = applyFPair (map (getSortedKeyAndValue tName)) $
                                                segregateList (`isKeyModified` oldKeysMap) newSkeys
      mapRight (const newKVEntry) <$> runExceptT (do
                                    mapM_ (ExceptT . resetTTL pKvKey skipTtlReset) unModifiedSkeys
                                    mapM_ (ExceptT . deletePkeyFromSkey pKvKey) modifiedSkeysOld
                                    mapM_ (ExceptT . addPkeyToSkey pKvKey) modifiedSkeysNew)
    resetTTL _  _ Nothing = pure $ Right False
    resetTTL pKey skipTtlReset (Just sKey)  = R.withArtRecOptionsForKVDB @table $ mapLeft MRedisError <$> resetTTLInUpdate meshCfg pKey sKey skipTtlReset
    
    deletePkeyFromSkey _ Nothing = pure $ Right 0
    deletePkeyFromSkey pKey (Just sKey) = R.withArtRecOptionsForKVDB @table $ removePKeyInSecondaryKey meshCfg pKey sKey

    -- addPkeyToSkey :: PKvKey -> Maybe Text -> m (MeshResult Bool)
    addPkeyToSkey _ Nothing = pure $ Right False
    addPkeyToSkey pKey (Just sKey) = R.withArtRecOptionsForKVDB @table $ mapLeft MRedisError <$> addPkeyInSecondaryKey meshCfg pKey sKey

    getSortedKeyAndValue :: Text -> [(Text,Text)] -> Maybe SKvKey
    getSortedKeyAndValue tName kvTup = do
      let sortArr = sortBy (compare `on` fst) kvTup
      let (appendedKeys, appendedValues) = applyFPair (T.intercalate "_") $ unzip sortArr
      if any (\(_, v) -> v=="") kvTup
        then Nothing
        else Just $ getKvSKey meshCfg $ tName <> "_" <> appendedKeys <> "_" <> appendedValues

    isKeyModified :: [(Text, Text)] -> KM.KeyMap Text -> Bool
    isKeyModified sKey updValsMap = foldl' (\r (k, v) -> KM.lookup (AK.fromText k) updValsMap /= Just v || r) False sKey

    segregateList :: (a -> Bool) -> [a] -> ([a], [a])
    segregateList func list = go list ([], [])
      where
        go [] res     = res 
        go (x : xs) (trueList, falseList)
          | func x    = go xs (x : trueList, falseList)
          | otherwise = go xs (trueList, x : falseList)

updateAllReturningWithKVConnector :: forall table m.
  ( HasCallStack,
    Model BP.Postgres table,
    MeshMeta BP.Postgres table,  
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    PIIUpdate BP.Postgres table,
    Serialize.Serialize (table Identity),
    Show (table Identity), --debugging purpose
    L.MonadFlow m
  ) =>
  ModelDBConfig BP.Pg ->
  MeshConfig ->
  [Set BP.Postgres table] ->
  Where BP.Postgres table ->
  m (MeshResult [table Identity])
updateAllReturningWithKVConnector dbConf meshCfg setClause whereClause = R.withArtRecOptionsForKVDB @table $ do
  let isDisabled = meshCfg.kvHardKilled
  t1        <- getCurrentDateInMillis
  res <- if not isDisabled
    then do
      let updVals = jsonKeyValueUpdates setClause
      kvRows <- redisFindAll dbConf meshCfg whereClause
      dbRows <- if meshCfg.shouldSkipDBForRecency then (pure $ Right []) else DBReplay.runWithArtFindALL dbConf whereClause "updateAllReturningWithKVConnector" (findAllSql dbConf whereClause meshCfg.cleanDBHardKilled)
      updateKVAndDBResults meshCfg whereClause dbRows kvRows (Just updVals) False dbConf (Just setClause) True Nothing
    else do
      let tName = (modelTableName @table)
      eitherPiiKeys <- getEncryptionKey tName
      case eitherPiiKeys of 
        Left e -> return $ Left $ MDBError e 
        Right mbval -> do
          res <- (DBReplay.runWithArtUpdate dbConf setClause whereClause "updateAllReturningWithKVConnector" $ updateAllSqlReturningList  dbConf setClause mbval whereClause meshCfg)
          case res of
            Right x -> do
              when meshCfg.memcacheEnabled $
                mapM_ (\entry -> pushToInMemConfigStream meshCfg ImcInsert entry.row) x
              return $ Right x
            Left e -> return $ Left e
  t2        <- getCurrentDateInMillis
  diffRes <- whereClauseDiffCheck dbConf whereClause
  let source = if isDisabled then SQL else if (isRecachingEnabled && meshCfg.meshEnabled) then KV else (if meshCfg.shouldSkipDBForRecency then KV else KV_AND_SQL)
      res' = mapRight (map row) res
  logAndIncrementKVMetric (Just $ getDbConnTags dbConf) True "UPDATE" UPDATE_ALL_RETURNING res' (Just (t2-t1)) (modelTableName @table) source Nothing diffRes
  pure res'

updateAllWithKVConnector :: forall be table beM m.
  ( HasCallStack,
    SqlReturning beM be,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    PII table,
    PIIUpdate be table,
    FromJSON (table Identity),
    ToJSON (table Identity),  
    Serialize.Serialize (table Identity),
    Show (table Identity), --debugging purpose
    B.FromBackendRow be Int,
    L.MonadFlow m
  ) =>
  ModelDBConfig beM -> 
  MeshConfig ->
  [Set be table] ->
  Where be table ->
  m (MeshResult ())
updateAllWithKVConnector dbConf meshCfg setClause whereClause = R.withArtRecOptionsForKVDB @table $ do
  let isDisabled = meshCfg.kvHardKilled
  t1        <- getCurrentDateInMillis
  let tname = (modelTableName @table)
  eitherPiiKeys <- getEncryptionKey tname
  case eitherPiiKeys of 
    Left e -> return $ Left $ MDBError e 
    Right mbval -> do 
      res <- if not isDisabled
        then do
          let updVals = jsonKeyValueUpdates setClause
          kvRows <- redisFindAll dbConf meshCfg whereClause
          dbRows <- if meshCfg.shouldSkipDBForRecency then (pure $ Right []) else DBReplay.runWithArtFindALL dbConf whereClause "updateAllWithKVConnector" $ findAll dbConf Nothing whereClause meshCfg.cleanDBHardKilled
          mapRight (const ()) <$> updateKVAndDBResults meshCfg whereClause dbRows kvRows (Just updVals) True dbConf (Just setClause) True mbval
        else do
          res <- DBReplay.runWithArtUpdate dbConf setClause whereClause "updateAllWithKVConnector" $ updateAllSql dbConf setClause whereClause mbval meshCfg
          case res of
            Right _ -> do
              dbRes <- DBReplay.runWithArtFindALL dbConf whereClause "updateAllWithKVConnector" $ findAll dbConf Nothing whereClause meshCfg.cleanDBHardKilled
              case dbRes of
                Right dbRows -> do
                  when meshCfg.memcacheEnabled $
                    mapM_ (pushToInMemConfigStream meshCfg ImcInsert) (map row dbRows)
                  return . Right $ ()
                Left e -> return . Left . MDBError $ e
            Left e -> return $ Left e
      t2        <- getCurrentDateInMillis
  
      diffRes <- whereClauseDiffCheck dbConf whereClause
      let source = if isDisabled then SQL else if (isRecachingEnabled && meshCfg.meshEnabled) then KV else (if meshCfg.shouldSkipDBForRecency then KV else KV_AND_SQL)
      logAndIncrementKVMetric (Just $ getDbConnTags dbConf) True "UPDATE" UPDATE_ALL res (Just (t2-t1)) (modelTableName @table) source Nothing diffRes
      pure res

updateKVAndDBResults :: forall be table beM m.
  ( HasCallStack,
    SqlReturning beM be,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    MeshMeta be table,
    PIIUpdate be table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    B.FromBackendRow be Int,
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    Show (table Identity), --debugging purpose
    L.MonadFlow m
  ) => MeshConfig -> Where be table -> Either DBError [KVEntry table] ->  MeshResult ([KVEntry table], [KVEntry table]) -> Maybe [(Text, A.Value)] -> Bool -> ModelDBConfig beM ->  Maybe [Set be table] -> Bool -> Maybe PIIKeyConfig -> m (MeshResult [KVEntry table])
updateKVAndDBResults meshCfg whereClause eitherDbRows eitherKvRows mbUpdateVals updateWoReturning dbConf mbSetClause isLive mbKeyConfig = R.withArtRecOptionsForKVDB @table $ do
  let setClause = fromMaybe [] mbSetClause --Change this logic
      updVals = fromMaybe [] mbUpdateVals
  case (eitherDbRows, eitherKvRows) of
    (Right allDBRows, Right allKVRows) -> do
      let kvLiveRows = fst allKVRows
          kvDeadRows = snd allKVRows
          kvLiveAndDeadRows = kvLiveRows ++ kvDeadRows
          matchedKVLiveRows = findAllMatching whereClause dbConf kvLiveRows
          uniqueDbRows =  getUniqueDBRes dbConf meshCfg allDBRows kvLiveAndDeadRows
      if isRecachingEnabled && meshCfg.meshEnabled
        then do
          reCacheDBRowsRes <- reCacheDBRows dbConf meshCfg uniqueDbRows False True False
          case reCacheDBRowsRes of
            Left err -> return $ Left $ MRedisError err
            Right _  -> do
              let allRows = matchedKVLiveRows ++ uniqueDbRows
              sequence <$> if isLive
                  then mapM (updateObjectRedis dbConf meshCfg updVals True whereClause mbKeyConfig) allRows
                  else mapM (deleteObjectRedis dbConf meshCfg True whereClause) allRows
        else do
          sequence_ $ addToETLSAndSQLLogStream meshCfg (isMySQLConfig dbConf) ETLUpdate <$> uniqueDbRows
          updateOrDelKVRowRes <- if isLive
            then mapM (updateObjectRedis dbConf meshCfg updVals True whereClause mbKeyConfig) kvLiveRows
            else mapM (deleteObjectRedis dbConf meshCfg True whereClause) kvLiveRows
          kvres <- pure $ foldEither updateOrDelKVRowRes
          case kvres of
            Left err -> return $ Left err
            Right kvRes -> runUpdateOrDelete setClause kvRes kvLiveAndDeadRows

    (Left err, _) -> pure $ Left $ MDBError err
    (_, Left err) -> pure $ Left err


    where
      runUpdateOrDelete setClause kvres kvLiveAndDeadRows = do
        case (isLive, updateWoReturning) of
          (True, True) -> do
              res <- DBReplay.runWithArtUpdate dbConf setClause whereClause "updateKVAndDBResults" $ updateAllSql dbConf setClause whereClause mbKeyConfig meshCfg
              case res of
                  Right _ -> return $ Right []
                  Left e -> return $ Left e
          (True, False) -> do
              res <- DBReplay.runWithArtUpdate dbConf setClause whereClause "updateKVAndDBResults" $ updateAllSqlReturningList  dbConf setClause mbKeyConfig whereClause meshCfg
              case res of
                  Right x -> return $ Right $ (getUniqueDBRes dbConf meshCfg x kvLiveAndDeadRows) ++ kvres
                  Left e  -> return $ Left e
          (False, _) -> do
            res <- DBReplay.runWithArtDelete dbConf whereClause "updateKVAndDBResults" $ deleteAllReturning dbConf whereClause meshCfg
            case res of
                Right x -> return $ Right $ (getUniqueDBRes dbConf meshCfg x kvLiveAndDeadRows) ++ kvres
                Left e  -> return $ Left e


---------------- Find -----------------------
findWithKVConnector :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    Model be table,
    MeshMeta be table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    PII table,
    Serialize.Serialize (table Identity),  
    L.MonadFlow m,
    Show (table Identity)
  ) =>
  ModelDBConfig beM -> 
  MeshConfig ->
  Where be table ->
  m (MeshResult (Maybe (table Identity)))
findWithKVConnector dbConf meshCfg whereClause = R.withArtRecOptionsForKVDB @table $ do --This function fetches all possible rows and apply where clause on it.
  let shouldSearchInMemoryCache = meshCfg.memcacheEnabled
  (source, result :: MeshResult (Maybe (KVEntry table))) <- if shouldSearchInMemoryCache
    then searchInMemoryCache "findWithKVConnector" dbConf whereClause meshCfg kvFetch
    else do
      fetchRes <- kvFetch dbConf meshCfg whereClause
      pure fetchRes
  let res = mapRight (row <$>) result
      dbName = either (const Nothing) (\x -> db <$> x) result
  when (shouldLogAndIncrementKVMetric source) $ do
    diffRes <- whereClauseDiffCheck dbConf whereClause
    logAndIncrementKVMetric (In.getConnTagFromDbName dbConf <$> dbName) False "FIND" FIND res Nothing (modelTableName @table) source Nothing diffRes   -- We have removed latency parameter as we don't need it for performance reason.
  pure res

kvFetch :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    Model be table,
    MeshMeta be table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    PII table,
    Serialize.Serialize (table Identity),  
    L.MonadFlow m,
    Show (table Identity)
  ) =>
  ModelDBConfig beM -> 
  MeshConfig ->
  Where be table ->
  m (Source, MeshResult (Maybe (KVEntry table)))
kvFetch dbConf meshCfg whereClause = R.withArtRecOptionsForKVDB @table $ do
  let isDisabled = meshCfg.kvHardKilled || isKvReadBlackListedTable @table
  fetchRes <- if not isDisabled
    then do
      eitherKvRows <- findOneFromRedis dbConf meshCfg whereClause
      case eitherKvRows of
        Right ([], []) -> do
              if (meshCfg.shouldSkipDBForRecency || Env.disableDBFallBack) then (pure (KV, Right Nothing)) else (SQL,) <$> findOneFromDB dbConf whereClause meshCfg
        Right ([], _) -> do
          L.logInfoT "findWithKVConnector" ("Returning nothing - Row is deleted already for " <> tableName @(table Identity))
          pure $ (KV, Right Nothing)
        Right (kvLiveRows, _) -> do
          second (mapRight listToMaybe) <$> findFromDBIfMatchingFails dbConf whereClause kvLiveRows meshCfg
        Left err -> pure $ (KV, Left err)
    else do
      (SQL,) <$> findOneFromDB dbConf whereClause meshCfg
  when (meshCfg.shouldRecacheFind && meshCfg.meshEnabled) $ do
    case fetchRes of
      (SQL, Right (Just r)) -> void $ reCacheDBRows dbConf meshCfg [r] True True False
      (KV,  Right (Just r)) -> do
        let isMySQL = isMySQLConfig dbConf
            pKvKey = getLookupKeyByPKey isMySQL meshCfg r.row
        when meshCfg.shouldPushToETLStream $ void $ R.rXaddBWithART meshCfg.kvRedis (encodeUtf8 (getETLStreamName <> pKvKey._shard)) (getETLStreamVal meshCfg ETLUpdate pKvKey) L.AutoID
      _                     -> pure ()
  when (isRecacheEnabledConfigTable @table) $ do
    case fetchRes of
      (SQL, Right (Just r)) -> do
        void $ reCacheDBRows dbConf meshCfg [r] True True True
      _                   -> pure ()
  pure fetchRes
        
      

findFromDBIfMatchingFails :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    Model be table,
    MeshMeta be table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity), 
    PII table,
    L.MonadFlow m) =>
  ModelDBConfig beM -> 
  Where be table ->
  [KVEntry table] ->
  MeshConfig ->
  m (Source, MeshResult [KVEntry table])
findFromDBIfMatchingFails dbConf whereClause kvRows meshCfg = do
  case (findAllMatching whereClause dbConf kvRows, meshCfg.shouldSkipDBForRecency || Env.disableDBFallBack) of -- For solving partial data case - One row in SQL and one in DB
    ([], False) -> do
      dbRes <- findOneFromDB dbConf whereClause meshCfg
      case dbRes of
        Right (Just dbRow) -> do
          let isMySQL = isMySQLConfig dbConf
              kvPkeys = map (\e -> getLookupKeyByPKey isMySQL meshCfg e.row) kvRows
          if getLookupKeyByPKey isMySQL meshCfg dbRow.row `notElem` kvPkeys
            then pure (SQL, Right [dbRow])
            else pure (KV, Right [])
        Left err           -> pure (SQL, Left err)
        {- Below source cannot be determined as there can be 2 possiblities
           1. Row is in KV but matching failed because of some column like status
           2. Row is in KV for other clause (Eg. merchantId) but not for required clause and also not in SQL
           Source as KV can be more misleading, therefore returning source as SQL -}
        _                  -> pure (SQL, Right [])
    (xs, _) -> pure (KV, Right xs)

-- TODO: Once record matched in redis stop and return it
findOneFromRedis :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    Model be table,
    MeshMeta be table,
    KVConnector (table Identity),
    Serialize.Serialize (table Identity),
    FromJSON (table Identity),
    L.MonadFlow m
  ) =>
  ModelDBConfig beM ->  MeshConfig -> Where be table -> m (MeshResult ([KVEntry table], [KVEntry table]))
findOneFromRedis dbConf meshCfg whereClause = R.withArtRecOptionsForKVDB @table $ do
  let keyAndValueCombinations = getFieldsAndValuesFromClause dbConf meshModelTableEntityDescriptor (And whereClause)
      andCombinations = map (uncurry zip . applyFPair (map (T.intercalate "_") . sortOn (Down . length) . nonEmptySubsequences) . unzip . sort) keyAndValueCombinations
      modelName = tableName @(table Identity)
      tableType = modelTableType @table
      keyHashMap = keyMap @(table Identity)
      andCombinationsFiltered = mkUniq $ filterPrimaryAndSecondaryKeys keyHashMap <$> andCombinations
      sKeys = getSecondaryKeys keyHashMap <$> andCombinationsFiltered
  res <- withRedisLimit "REDIS_FIND_ONE_LIMIT_EXCEEDED" (modelTableName @table) sKeys tableType $ do
    eitherKeyRes <- mapM (getPrimaryKeyFromFieldsAndValues modelName meshCfg keyHashMap) andCombinationsFiltered
    case foldEither eitherKeyRes of
      Right keyRes -> do
        let uniqueKeyRes = mkUniq $ concat keyRes
        withRedisLimit "REDIS_FIND_ONE_LIMIT_EXCEEDED" (modelTableName @table) [uniqueKeyRes] tableType $ getDataFromPKeysRedis @table meshCfg uniqueKeyRes
      Left err -> pure $ Left err
  case res of
    Left err -> if isConfigTable tableType then do
        L.logErrorV ("kv config fetch failed for model " <> modelName) (toJSON err)
        pure $ Right ([],[]) 
      else pure $ res
    Right _ -> pure $ res

findOneFromDB :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    Model be table,
    MeshMeta be table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),  
    PII table,
    L.MonadFlow m
  ) =>
  ModelDBConfig beM ->  Where be table -> MeshConfig -> m (MeshResult (Maybe (KVEntry table)))
findOneFromDB dbConf whereClause meshCfg = DBReplay.runWithArtFind dbConf whereClause "findOneFromDB" (findOne dbConf Nothing whereClause meshCfg)

findAllWithOptionsKVConnector :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    Model be table,
    MeshMeta be table,
    KVConnector (table Identity),
    Serialize.Serialize (table Identity),
    Show (table Identity),
    ToJSON (table Identity),
    FromJSON (table Identity),
    PII table,
    L.MonadFlow m, B.HasQBuilder be, BeamRunner beM) =>
  ModelDBConfig beM -> 
  MeshConfig ->
  Where be table ->
  OrderBy table ->
  Maybe Int ->
  m (MeshResult [table Identity])
findAllWithOptionsKVConnector dbConf meshCfg whereClause orderBy mbLimit = do
  let isDisabled = meshCfg.kvHardKilled
  result <- if not isDisabled
    then do
      kvRes <- redisFindAll dbConf meshCfg whereClause
      case kvRes of
        Right kvRows -> do
          let matchedKVLiveRows = findAllMatching whereClause dbConf (fst kvRows)
          if meshCfg.shouldSkipDBForRecency || Env.disableDBFallBack
            then pure $ Right $ applyOptions matchedKVLiveRows
            else do
              findAllres <- DBReplay.runWithArtFindAllExtended dbConf whereClause "findAllWithOptionsKVConnector" (findAllWithOptions dbConf meshCfg.cleanDBHardKilled Nothing whereClause orderBy mbLimit)
              case findAllres of
                Left err -> pure $ Left $ MDBError err
                Right dbRows -> do
                  let mergedRows = matchedKVLiveRows ++ getUniqueDBRes dbConf meshCfg dbRows (snd kvRows ++ fst kvRows)
                  pure $ Right $ applyOptions mergedRows
        Left err -> pure $ Left err
    else do
      mapLeft MDBError <$> (DBReplay.runWithArtFindAllExtended dbConf whereClause "findAllWithOptionsKVConnector" (findAllWithOptions dbConf meshCfg.cleanDBHardKilled Nothing whereClause orderBy mbLimit))
  let res = mapRight (map row) result 
  diffRes <- whereClauseDiffCheck dbConf whereClause
  let source = if not isDisabled then (if meshCfg.shouldSkipDBForRecency || Env.disableDBFallBack then KV else KV_AND_SQL) else SQL
  logAndIncrementKVMetric (Just $ getDbConnTags dbConf) False "FIND" FIND_ALL_WITH_OPTIONS res Nothing (modelTableName @table) source Nothing diffRes  -- We have removed latency parameter as we don't need it for performance reason.
  pure res

    where
      applyOptions :: [KVEntry table] -> [KVEntry table]
      applyOptions rows = do
        let cmp = case orderBy of
              (Asc col)  -> compareCols (fromColumnar' . col . columnize) True
              (Desc col) -> compareCols (fromColumnar' . col . columnize) False
        let resWithoutLimit = sortBy cmp rows
        maybe resWithoutLimit (`take` resWithoutLimit) mbLimit

      compareCols :: (Ord value) => (table Identity -> value) -> Bool -> KVEntry table -> KVEntry table -> Ordering
      compareCols col isAsc r1 r2 = if isAsc then compare (col r1.row) (col r2.row) else compare (col r2.row) (col r1.row)

findAllWithKVConnector :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    Model be table,
    MeshMeta be table,
    KVConnector (table Identity),
    ToJSON (table Identity),
    FromJSON (table Identity),  
    (Show (table Identity)),
    PII table,
    Serialize.Serialize (table Identity),
    L.MonadFlow m, B.HasQBuilder be, BeamRunner beM) =>
  ModelDBConfig beM -> 
  MeshConfig ->
  Where be table ->
  m (MeshResult [table Identity])
findAllWithKVConnector dbConf meshCfg whereClause = R.withArtRecOptionsForKVDB @table $ do
  let isDisabled = meshCfg.kvHardKilled 
      kvSource = if not isDisabled then (if meshCfg.shouldSkipDBForRecency || Env.disableDBFallBack then KV else KV_AND_SQL) else SQL
      memcacheFindAllEnabled = meshCfg.memcacheFindAllEnabled
  (src, res) <- if memcacheFindAllEnabled
    then do
      second (mapRight (map row)) <$> searchInMemoryCacheFindAll "findAllWithKVConnector" dbConf whereClause meshCfg kvFetchFindAll
    else (kvSource,) <$> (mapRight (map row) <$> kvFetchFindAll dbConf meshCfg whereClause)
  diffRes <- whereClauseDiffCheck dbConf whereClause
  logAndIncrementKVMetric (Just $ getDbConnTags dbConf) False "FIND" FIND_ALL res Nothing (modelTableName @table) src Nothing diffRes  -- We have removed latency parameter as we don't need it for performance reason.
  pure res

kvFetchFindAll :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    Model be table,
    MeshMeta be table,
    KVConnector (table Identity),
    ToJSON (table Identity),
    FromJSON (table Identity),  
    (Show (table Identity)),
    PII table,
    Serialize.Serialize (table Identity),
    L.MonadFlow m, B.HasQBuilder be, BeamRunner beM) =>
  ModelDBConfig beM -> 
  MeshConfig ->
  Where be table ->
  m (MeshResult [KVEntry table])
kvFetchFindAll dbConf meshCfg whereClause = R.withArtRecOptionsForKVDB @table $ do
  let isDisabled = meshCfg.kvHardKilled || isKvReadBlackListedTable @table
  if not isDisabled then do
    kvRes <- redisFindAll dbConf meshCfg whereClause
    case kvRes of
      Right kvRows -> do
            let matchedKVLiveRows = findAllMatching whereClause dbConf (fst kvRows)
            if meshCfg.shouldSkipDBForRecency || Env.disableDBFallBack
              then pure $ Right matchedKVLiveRows
              else do
                dbRes <- DBReplay.runWithArtFindALL dbConf whereClause "findAllWithKVConnector" $ findAll dbConf Nothing whereClause meshCfg.cleanDBHardKilled
                case dbRes of
                  Right dbRows -> do
                    let uniqueDbRows = getUniqueDBRes dbConf meshCfg dbRows (fst kvRows ++ snd kvRows)
                    when (meshCfg.shouldRecacheFind && meshCfg.meshEnabled) $ void $ reCacheDBRows dbConf meshCfg uniqueDbRows True True False
                    when (isRecacheEnabledConfigTable @table) $ void $ reCacheDBRows dbConf meshCfg uniqueDbRows True True True
                    pure $ Right $ matchedKVLiveRows ++ uniqueDbRows
                  Left err     -> return $ Left $ MDBError err
      Left err -> return $ Left err
  else mapLeft MDBError <$> ((DBReplay.runWithArtFindALL dbConf whereClause "findAllWithKVConnector" $ findAll dbConf Nothing whereClause meshCfg.cleanDBHardKilled))

redisFindAll :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    Model be table,
    MeshMeta be table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m, B.HasQBuilder be, BeamRunner beM) =>
  ModelDBConfig beM -> 
  MeshConfig ->
  Where be table ->
  m (MeshResult ([KVEntry table], [KVEntry table]))
redisFindAll dbConf meshCfg whereClause = R.withArtRecOptionsForKVDB @table $ do
  let keyAndValueCombinations = getFieldsAndValuesFromClause dbConf meshModelTableEntityDescriptor (And whereClause)
      andCombinations = map (uncurry zip . applyFPair (map (T.intercalate "_") . sortOn (Down . length) . nonEmptySubsequences) . unzip . sort) keyAndValueCombinations
      modelName = tableName @(table Identity)
      tableType = modelTableType @table
      keyHashMap = keyMap @(table Identity)
      andCombinationsFiltered = mkUniq $ filterPrimaryAndSecondaryKeys keyHashMap <$> andCombinations
      sKeys = getSecondaryKeys keyHashMap <$> andCombinationsFiltered
  res <- withRedisLimit "REDIS_FIND_ALL_LIMIT_EXCEEDED" (modelTableName @table) sKeys tableType $ do
    eitherKeyRes <- mapM (getPrimaryKeyFromFieldsAndValues modelName meshCfg keyHashMap) andCombinationsFiltered
    case foldEither eitherKeyRes of
      Right keyRes -> do
        let uniqueKeyRes = mkUniq $ concat keyRes
        withRedisLimit "REDIS_FIND_ALL_LIMIT_EXCEEDED" (modelTableName @table) [uniqueKeyRes] tableType $ getDataFromPKeysRedis @table meshCfg uniqueKeyRes
      Left err -> pure $ Left err
  case res of
    Left err -> if isConfigTable tableType then do
        L.logErrorV ("kv config fetch failed for model " <> modelName) (toJSON err)
        pure $ Right ([],[]) 
      else pure $ res
    Right _ -> pure $ res

deleteObjectRedis :: forall table be beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    -- Show (table Identity), --debugging purpose
    L.MonadFlow m
  ) =>
  ModelDBConfig beM ->  MeshConfig -> Bool -> Where be table -> KVEntry table -> m (MeshResult (KVEntry table))
deleteObjectRedis dbConf meshCfg addPrimaryKeyToWhereClause whereClause entry@(KVEntry dbName obj _) = R.withArtRecOptionsForKVDB @table $ do
  time <- fromIntegral <$> L.getCurrentDateInMillis
  let pKvKey  = getLookupKeyByPKey (isMySQLConfig dbConf) meshCfg obj
      shard     =  pKvKey._shard
      deleteCmd = if addPrimaryKeyToWhereClause
                    then getDbDeleteCommandJsonWithPrimaryKey dbConf (tableName @(table Identity)) obj whereClause
                    else getDbDeleteCommandJson (tableName @(table Identity)) whereClause
  strmMetadata <- makeStreamMetadata dbConf Nothing pKvKey meshCfg dbName
  let qCmd      = (\metadata -> getDeleteQuery V1 metadata time dbName deleteCmd) <$> strmMetadata
      qCmdEncoded =  (BSL.toStrict . A.encode) <$> qCmd
  kvDbRes <- R.rMultiExecWithHashWithART meshCfg.kvRedis (encodeUtf8 shard) $ do
    when (meshCfg.prefixMigrationMode /= L.NEW_WRITE_DISABLED) $ void $
      L.setexTx (encodeUtf8 pKvKey.prefixedKey) meshCfg.redisTtl (BSL.toStrict $ Encoding.encodeDead $ Encoding.encode_ meshCfg.cerealEnabled entry)
    when (meshCfg.prefixMigrationMode /= L.OLD_WRITE_DISABLED) $ void $
      L.setexTx (encodeUtf8 pKvKey._key) meshCfg.redisTtl (BSL.toStrict $ Encoding.encodeDead $ Encoding.encode_ meshCfg.cerealEnabled entry)
    sequence <$> mapM (\qCmds -> L.xaddTx
      (encodeUtf8 (meshCfg.ecRedisDBStream <> shard))
      L.AutoID
      [("command", qCmds)]) qCmdEncoded
  case kvDbRes of
    Left err -> return . Left $ MRedisError err
    Right (TxSuccess res) -> do
              let success = zip res qCmdEncoded
              when meshCfg.shouldPushToReconRedis $ 
                forM_ success $ \(L.KVDBStreamEntryID key seqn, qCmd') ->
                  L.fork $ R.rZAddWithART meshCfg.reconRedis (encodeUtf8 (meshCfg.ecRedisDBStream <> shard <> "_recon")) [(streamId2Double (L.KVDBStreamEntryID key seqn), qCmd')]
              when meshCfg.memcacheEnabled $ pushToInMemConfigStream meshCfg ImcDelete obj
              pure $ Right entry
    Right _  -> do
      when meshCfg.memcacheEnabled $ pushToInMemConfigStream meshCfg ImcDelete obj
      return $ Right entry


markDBRowAsDeletedKV :: forall table m beM.
  ( HasCallStack,
    HasArtRecOptions,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m
  ) =>
  ModelDBConfig beM -> 
  MeshConfig ->
  KVEntry table ->
  Bool -> 
  m (MeshResult (KVEntry table))
markDBRowAsDeletedKV dbConf meshCfg entry@(KVEntry _ obj _) isConfigRecaching = do
  let redisTtl = if isConfigRecaching then configKvTtl else meshCfg.redisTtl
      pKvKey  = getLookupKeyByPKey (isMySQLConfig dbConf) meshCfg obj
  when (meshCfg.prefixMigrationMode /= L.NEW_WRITE_DISABLED) $ void $ R.rSetexWithART meshCfg.kvRedis pKvKey.prefixedKey (BSL.toStrict $ Encoding.encodeDead $ Encoding.encode_ meshCfg.cerealEnabled entry) redisTtl 
  when (meshCfg.prefixMigrationMode /= L.OLD_WRITE_DISABLED) $ void $ R.rSetexWithART meshCfg.kvRedis pKvKey._key (BSL.toStrict $ Encoding.encodeDead $ Encoding.encode_ meshCfg.cerealEnabled entry) redisTtl
  pure $ Right entry

reCacheDBRows :: forall table m beM.
  ( HasCallStack,
    HasArtRecOptions,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    -- Show (table Identity), --debugging purpose
    L.MonadFlow m
  ) =>
  ModelDBConfig beM -> 
  MeshConfig ->
  [KVEntry table] ->
  Bool ->
  Bool ->
  Bool ->
  m (Either KVDBReply [()])
reCacheDBRows dbConf meshCfg dbRows shouldSetPrimaryKey shouldRecacheIfNotExist isConfigRecaching = do
  let redisTtl = if isConfigRecaching then configKvTtl else meshCfg.redisTtl
  reCacheRes <- mapM (\entry -> do
      let isMySQL = isMySQLConfig dbConf
          pKvKey = getLookupKeyByPKey isMySQL meshCfg entry.row
          shard = pKvKey._shard
      L.logDebug @Text "reCacheDBRows" (pKvKey._key )
      res <- createSecondaryKeyMapping isMySQL meshCfg pKvKey entry.row
      case (shouldSetPrimaryKey, sequence res) of
        (True, Right _) -> 
          mapRight (const ()) <$> (R.rMultiExecWithHashWithART meshCfg.kvRedis (encodeUtf8 shard) $ do
            when (meshCfg.shouldPushToETLStream && not isConfigRecaching ) $ void $ L.xaddTx (encodeUtf8 (getETLStreamName <> shard)) L.AutoID (getETLStreamVal meshCfg ETLUpdate pKvKey)
            if meshCfg.prefixMigrationMode == L.NEW_WRITE_DISABLED
              then setOptsHelper redisTtl pKvKey._key entry
            else if meshCfg.prefixMigrationMode == L.OLD_WRITE_DISABLED
              then setOptsHelper redisTtl pKvKey.prefixedKey entry
            else do
              void $ setOptsHelper redisTtl pKvKey._key entry
              setOptsHelper redisTtl pKvKey.prefixedKey entry)
        (_, Right _)    -> pure $ Right ()
        (_, Left err)   -> pure $ Left err
    ) dbRows
  return $ sequence reCacheRes
  where
    setOptsHelper redisTtl pKeyWithShard entry = do
      if not shouldRecacheIfNotExist 
        then L.setOptsTx (encodeUtf8 $ pKeyWithShard) (BSL.toStrict $ Encoding.encode_ meshCfg.cerealEnabled entry) (L.Seconds redisTtl) L.SetAlways
        else L.setOptsTx (encodeUtf8 $ pKeyWithShard) (BSL.toStrict $ Encoding.encode_ meshCfg.cerealEnabled entry) (L.Seconds redisTtl) L.SetIfNotExist

deleteWithKVConnector :: forall be table beM m.
  ( HasCallStack,
    SqlReturning beM be,
    BeamRuntime be beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    PII table,
    PIIUpdate be table,
    ToJSON (table Identity),
    FromJSON (table Identity),  
    Show (table Identity),
    Serialize.Serialize (table Identity),
    B.FromBackendRow be Int,
    L.MonadFlow m, B.HasQBuilder be, BeamRunner beM) =>
  ModelDBConfig beM -> 
  MeshConfig ->
  Where be table ->
  m (MeshResult ())
deleteWithKVConnector dbConf meshCfg whereClause = R.withArtRecOptionsForKVDB @table $ do
  let isDisabled = meshCfg.kvHardKilled
  t1        <- getCurrentDateInMillis
  (source, res) <- if not isDisabled
    then do
      (\delRes -> (fst delRes, mapRight (const ()) (snd delRes))) <$> modifyOneKV dbConf meshCfg whereClause Nothing True False Nothing
    else do
      res <- DBReplay.runWithArtDelete dbConf whereClause "deleteWithKVConnector" $ deleteSql dbConf whereClause meshCfg
      (SQL,) <$> case res of
        Left err -> return $ Left err
        Right re -> do
          if meshCfg.memcacheEnabled
            then
              searchInMemoryCache "deleteWithKVConnector" dbConf whereClause meshCfg kvFetch >>= (snd >>> \case
                Left e -> return $ Left e
                Right (Just a) -> do
                  (pushToInMemConfigStream meshCfg ImcDelete) a.row
                  return $ Right re
                Right (Nothing)-> return $ Right re)
            else do
                return $ Right re

  t2        <- getCurrentDateInMillis
  diffRes <- whereClauseDiffCheck dbConf whereClause
  logAndIncrementKVMetric (Just $ getDbConnTags dbConf) False "DELETE" DELETE_ONE res (Just (t2-t1)) (modelTableName @table) source Nothing diffRes
  pure res

deleteReturningWithKVConnector :: forall be table beM m.
  ( HasCallStack,
    SqlReturning beM be,
    BeamRuntime be beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    PII table,
    PIIUpdate be table,
    ToJSON (table Identity),
    B.FromBackendRow be Int,
    FromJSON (table Identity),
    Show (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m, B.HasQBuilder be, BeamRunner beM) =>
  ModelDBConfig beM -> 
  MeshConfig ->
  Where be table ->
  m (MeshResult (Maybe (table Identity)))
deleteReturningWithKVConnector dbConf meshCfg whereClause = R.withArtRecOptionsForKVDB @table $ do
  let isDisabled = meshCfg.kvHardKilled
  t1        <- getCurrentDateInMillis
  (source, res) <- if not isDisabled
    then do
      modifyOneKV dbConf meshCfg whereClause Nothing False False Nothing
    else do
      res <- DBReplay.runWithArtDelete dbConf whereClause "deleteReturningWithKVConnector" $ deleteAllReturning dbConf whereClause meshCfg
      (SQL,) <$> case res of
        Left err  -> return $ Left err
        Right []  -> return $ Right Nothing
        Right [r] -> do
          when meshCfg.memcacheEnabled $ pushToInMemConfigStream meshCfg ImcDelete r.row
          return $ Right (Just r)
        Right rs   -> do
          when meshCfg.memcacheEnabled $ mapM_ (\entry -> pushToInMemConfigStream meshCfg ImcDelete entry.row) rs
          return $ Left $ MUpdateFailed "SQL delete returned more than one record"
  t2        <- getCurrentDateInMillis
  diffRes <- whereClauseDiffCheck dbConf whereClause
  let res' = mapRight (row <$>) res
  logAndIncrementKVMetric (Just $ getDbConnTags dbConf) False "DELETE" DELETE_ONE_RETURNING res' (Just (t2-t1)) (modelTableName @table) source Nothing diffRes
  pure res'

deleteAllReturningWithKVConnector :: forall be table beM m.
  ( HasCallStack,
    SqlReturning beM be,
    BeamRuntime be beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    ToJSON (table Identity),
    PIIUpdate be table,
    FromJSON (table Identity),
    B.FromBackendRow be Int,
    Show (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m, B.HasQBuilder be, BeamRunner beM) =>
  ModelDBConfig beM -> 
  MeshConfig ->
  Where be table ->
  m (MeshResult [table Identity])
deleteAllReturningWithKVConnector dbConf meshCfg whereClause = R.withArtRecOptionsForKVDB @table $ do
  let isDisabled = meshCfg.kvHardKilled
  t1        <- getCurrentDateInMillis
  res <- if not isDisabled
    then do
      kvResult <- redisFindAll dbConf meshCfg whereClause
      dbRows   <- if meshCfg.shouldSkipDBForRecency then (pure $ Right []) else DBReplay.runWithArtFindALL dbConf whereClause "deleteAllReturningWithKVConnector" $ findAllSql dbConf whereClause meshCfg.cleanDBHardKilled
      updateKVAndDBResults meshCfg whereClause dbRows kvResult Nothing False dbConf Nothing False Nothing
    else do
      res <- DBReplay.runWithArtDelete dbConf whereClause "deleteAllReturningWithKVConnector" $ deleteAllReturning dbConf whereClause meshCfg
      case res of
        Left err -> return $ Left err
        Right re -> do
          when meshCfg.memcacheEnabled $ mapM_ (\entry -> pushToInMemConfigStream meshCfg ImcDelete entry.row) re
          return $ Right re
  t2        <- getCurrentDateInMillis
  diffRes <- whereClauseDiffCheck dbConf whereClause
  let source = if isDisabled then SQL else if isRecachingEnabled then KV else (if meshCfg.shouldSkipDBForRecency then KV else KV_AND_SQL)
      res' = mapRight (row <$>) res
  logAndIncrementKVMetric (Just $ getDbConnTags dbConf) False "DELETE" DELETE_ALL_RETURNING res' (Just (t2-t1)) (modelTableName @table) source Nothing diffRes
  pure res'

getDbConnTags :: ModelDBConfig beM -> Text
getDbConnTags beM =
  "(" <> In.getConnTagFromDbName beM ECRDB <> "," <> In.getConnTagFromDbName beM TRACKERDB <> ")"