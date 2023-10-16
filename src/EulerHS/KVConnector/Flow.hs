{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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
    findAllWithOptionsKVConnector',
    deleteWithKVConnector,
    deleteReturningWithKVConnector,
    deleteAllReturningWithKVConnector,
    findAllWithKVAndConditionalDBInternal
  )
 where

import           EulerHS.Extra.Time (getCurrentDateInMillis)
import           EulerHS.Prelude hiding (maximum)
import           EulerHS.CachedSqlDBQuery (runQuery)
import           EulerHS.KVConnector.Types (KVConnector(..), MeshConfig, MeshResult, MeshError(..), MeshMeta(..), SecondaryKey(..), tableName, keyMap, Source(..), Operation(..))
import           EulerHS.KVConnector.DBSync (getCreateQuery, getUpdateQuery, getDeleteQuery, getDbDeleteCommandJson, getDbUpdateCommandJson, getDbUpdateCommandJsonWithPrimaryKey, getDbDeleteCommandJsonWithPrimaryKey, DBCommandVersion(..))
import           EulerHS.KVConnector.InMemConfig.Flow (searchInMemoryCache, pushToInMemConfigStream, fetchRowFromDBAndAlterImc)
import           EulerHS.KVConnector.InMemConfig.Types (ImcStreamCommand(..))
import           EulerHS.KVConnector.Utils
import           EulerHS.KVDB.Types (KVDBReply)
import           Control.Arrow ((>>>))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import           Data.List (span, maximum)
import qualified Data.Text as T
import qualified EulerHS.Language as L
import qualified Data.HashMap.Strict as HM
import           EulerHS.SqlDB.Types (BeamRunner, BeamRuntime, DBConfig, DBError)
import qualified EulerHS.SqlDB.Language as DB
import           Sequelize (fromColumnar', columnize, sqlSelect, sqlSelect', sqlUpdate, sqlDelete, modelTableName, Model, Where, Clause(..), Set(..), OrderBy(..))
import           EulerHS.CachedSqlDBQuery (findAllSql, createReturning, createSqlWoReturing, updateOneSqlWoReturning, SqlReturning(..))
import qualified Database.Beam as B
import qualified Database.Beam.Postgres as BP
import           Data.Either.Extra (mapRight, mapLeft)
import           Named (defaults, (!))
import qualified Data.Serialize as Serialize
import qualified EulerHS.KVConnector.Encoding as Encoding
import           System.CPUTime (getCPUTime)
import qualified Data.Maybe as DMaybe

createWoReturingKVConnector :: forall (table :: (Type -> Type) -> Type) be m beM.
  ( HasCallStack,
    SqlReturning beM be,
    Model be table,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    Show (table Identity),
    KVConnector (table Identity),
    L.MonadFlow m) =>
  DBConfig beM ->
  MeshConfig ->
  table Identity ->
  m (MeshResult ())
createWoReturingKVConnector dbConf meshCfg value = do
  let isEnabled = meshCfg.meshEnabled && not meshCfg.kvHardKilled
  res <- if isEnabled
    then do
      mapRight (const ()) <$> createKV meshCfg value
    else do
      res <- createSqlWoReturing dbConf value
      case res of
        Right _ -> return $ Right ()
        Left e -> return $ Left $ MDBError e
  let source = if isEnabled then KV else SQL
      res' = mapRight (const value) res
  pure res


createWithKVConnector ::
  forall (table :: (Type -> Type) -> Type) be m beM.
  ( HasCallStack,
    SqlReturning beM be,
    Model be table,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    Show (table Identity),
    KVConnector (table Identity),
    L.MonadFlow m) =>
  DBConfig beM ->
  MeshConfig ->
  table Identity ->
  m (MeshResult (table Identity))
createWithKVConnector dbConf meshCfg value = do
  let isEnabled = meshCfg.meshEnabled && not meshCfg.kvHardKilled
  res <- if isEnabled
    then do
      createKV meshCfg value
    else do
      res <- createReturning dbConf value Nothing
      case res of
        Right val -> return $ Right val
        Left e -> return $ Left $ MDBError e
  when meshCfg.memcacheEnabled $ do
    case res of
      Right obj -> pushToInMemConfigStream meshCfg ImcInsert obj
      Left _    -> pure ()
  let source = if isEnabled then KV else SQL
  pure res

createKV :: forall (table :: (Type -> Type) -> Type) m.
  ( FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    Show (table Identity),
    KVConnector (table Identity),
    L.MonadFlow m) =>
  MeshConfig ->
  table Identity ->
  m (MeshResult (table Identity))
createKV meshCfg value = do
  autoIncIdRes <- unsafeJSONSetAutoIncId meshCfg value
  case autoIncIdRes of
    Right val -> do
      let pKeyText = getLookupKeyByPKey val
          shard = getShardedHashTag pKeyText
          pKey = fromString . T.unpack $ pKeyText <> shard
      time <- fromIntegral <$> L.getCurrentDateInMillis
      let qCmd = getCreateQuery (tableName @(table Identity)) V1 (pKeyText <> shard) time meshCfg.meshDBName val
      revMappingRes <- mapM (\secIdx -> do
        let sKey = fromString . T.unpack $ secIdx
        _ <- L.runKVDB meshCfg.kvRedis $ L.sadd sKey [pKey]
        L.runKVDB meshCfg.kvRedis $ L.expire sKey meshCfg.redisTtl
        ) $ getSecondaryLookupKeys val
      case foldEither revMappingRes of
        Left err -> pure $ Left $ MRedisError err
        Right _ -> do
          kvRes <- L.runKVDB meshCfg.kvRedis $ L.multiExecWithHash (encodeUtf8 shard) $ do
            _ <- L.xaddTx
                  (encodeUtf8 (meshCfg.ecRedisDBStream <> shard))
                  L.AutoID
                  [("command", BSL.toStrict $ A.encode qCmd)]
            L.setexTx pKey meshCfg.redisTtl (BSL.toStrict $ Encoding.encode_ meshCfg.cerealEnabled val)
          case kvRes of
            Right _ -> pure $ Right val
            Left err -> pure $ Left (MRedisError err)
    Left err -> pure $ Left err

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
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    Show (table Identity), --debugging purpose
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  MeshConfig ->
  [Set be table] ->
  Where be table ->
  m (MeshResult ())
updateWoReturningWithKVConnector dbConf meshCfg setClause whereClause = do
  let isDisabled = meshCfg.kvHardKilled
  (source, res) <- if not isDisabled
    then do
      -- Discarding object
      (\updRes -> (fst updRes, mapRight (const ()) (snd updRes))) <$> modifyOneKV dbConf meshCfg whereClause (Just setClause) True True
    else do
      res <- updateOneSqlWoReturning dbConf setClause whereClause
      (SQL,) <$> case res of
        Right val -> do         
           {-
              Since beam-mysql doesn't implement updateRowsReturning, we fetch the row from imc (or lower layers)
              and then update the json so fetched and finally setting it in the imc.
            -}
            if meshCfg.memcacheEnabled
              then fetchRowFromDBAndAlterImc dbConf meshCfg whereClause ImcInsert
              else return $ Right val

        Left e -> return $ Left $ MDBError e
  diffRes   <- whereClauseDiffCheck whereClause
  pure res
  

updateWithKVConnector :: forall table m.
  ( HasCallStack,
    Model BP.Postgres table,
    MeshMeta BP.Postgres table,
    B.HasQBuilder BP.Postgres,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    Show (table Identity), --debugging purpose
    L.MonadFlow m
  ) =>
  DBConfig BP.Pg ->
  MeshConfig ->
  [Set BP.Postgres table] ->
  Where BP.Postgres table ->
  m (MeshResult (Maybe (table Identity)))
updateWithKVConnector dbConf meshCfg setClause whereClause = do
  let isDisabled = meshCfg.kvHardKilled  
  (source, res) <- if not isDisabled
    then do
      modifyOneKV dbConf meshCfg whereClause (Just setClause) False True
    else do
      let updateQuery = DB.updateRowsReturningList $ sqlUpdate ! #set setClause ! #where_ whereClause
      res <- runQuery dbConf updateQuery
      (SQL, ) <$> case res of
        Right [x] -> do
          when meshCfg.memcacheEnabled $ pushToInMemConfigStream meshCfg ImcInsert x
          return $ Right (Just x)
        Right [] -> return $ Right Nothing
        Right xs -> do
          let message = "DB returned \"" <> show (length xs) <> "\" rows after update for table: " <> show (tableName @(table Identity))
          L.logError @Text "updateWithKVConnector" message
          return $ Left $ UnexpectedError message
        Left e -> return $ Left $ MDBError e
  diffRes   <- whereClauseDiffCheck whereClause
  pure res

modifyOneKV :: forall be table beM m.
  ( HasCallStack,
    SqlReturning beM be,
    BeamRuntime be beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    ToJSON (table Identity),
    FromJSON (table Identity),
    Show (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m, B.HasQBuilder be, BeamRunner beM) =>
  DBConfig beM ->
  MeshConfig ->
  Where be table ->
  Maybe [Set be table] ->
  Bool ->
  Bool ->
  m (Source, MeshResult (Maybe (table Identity)))
modifyOneKV dbConf meshCfg whereClause mbSetClause updateWoReturning isLive = do
  let setClause = fromMaybe [] mbSetClause
      updVals = jsonKeyValueUpdates setClause
  kvResult <- findOneFromRedis meshCfg whereClause
  case kvResult of
    Right ([], []) -> updateInKVOrSQL Nothing updVals setClause
    Right ([], _) -> do
      L.logDebugT "modifyOneKV" ("Modifying nothing - Row is deleted already for " <> tableName @(table Identity))
      pure (KV, Right Nothing)
    Right (kvLiveRows, _) -> do
      findFromDBIfMatchingFailsRes <- findFromDBIfMatchingFails dbConf whereClause kvLiveRows
      case findFromDBIfMatchingFailsRes of
        (_, Right [])        -> pure (KV, Right Nothing)
        (SQL, Right [dbRow]) -> updateInKVOrSQL (Just dbRow) updVals setClause
        (KV, Right [obj])   -> (KV,) . mapRight Just <$> (if isLive
           then updateObjectRedis meshCfg updVals False whereClause obj
           else deleteObjectRedis meshCfg False whereClause obj)
        (source, Right _)   -> do
          L.logErrorT "modifyOneKV" "Found more than one record in redis - Modification failed"
          pure (source, Left $ MUpdateFailed "Found more than one record in redis")
        (source, Left err) -> pure (source, Left err)
    Left err -> pure (KV, Left err)

    where
      alterImc :: Maybe (table Identity) -> m (MeshResult ())
      alterImc mbRow = do
        case (isLive, mbRow) of
          (True, Nothing) -> fetchRowFromDBAndAlterImc dbConf meshCfg whereClause ImcInsert
          (True, Just x) -> Right <$> pushToInMemConfigStream meshCfg ImcInsert x
          (False, Nothing) -> 
              searchInMemoryCache meshCfg dbConf whereClause >>= (snd >>> \case
                Left e -> return $ Left e
                Right rs -> Right <$> mapM_ (pushToInMemConfigStream meshCfg ImcDelete) rs)
          (False, Just x) -> Right <$> pushToInMemConfigStream meshCfg ImcDelete x

      runUpdateOrDelete setClause = do
        case (isLive, updateWoReturning) of
          (True, True) -> do
            res <- updateOneSqlWoReturning dbConf setClause whereClause
            case res of
              Right _ -> pure $ Right Nothing
              Left e -> return $ Left $ MDBError e
          (True, False) -> do
            let updateQuery = DB.updateRowsReturningList $ sqlUpdate ! #set setClause ! #where_ whereClause
            res <- runQuery dbConf updateQuery
            case res of
              Right [x] -> return $ Right (Just x)
              Right [] -> return $ Right Nothing
              Right xs -> do
                let message = "DB returned " <> show (length xs) <> " rows after update for table: " <> show (tableName @(table Identity))
                L.logErrorT "updateWithKVConnector" message
                return $ Left $ UnexpectedError message
              Left e -> return $ Left $ MDBError e
          (False, True) -> do
            let deleteQuery = DB.deleteRows $ sqlDelete ! #where_ whereClause
            res <- runQuery dbConf deleteQuery
            case res of
                Right _ -> return $ Right Nothing
                Left e  -> return $ Left $ MDBError e
          (False, False) -> do
            res <- deleteAllReturning dbConf whereClause
            case res of
                Right [x] -> return $ Right (Just x)
                Right [] -> return $ Right Nothing
                Right xs -> do
                  let message = "DB returned " <> show (length xs) <> " rows after delete for table: " <> show (tableName @(table Identity))
                  L.logErrorT "deleteReturningWithKVConnector" message
                  return $ Left $ UnexpectedError message
                Left e -> return $ Left $ MDBError e

      updateInKVOrSQL maybeRow updVals setClause = do
        if isRecachingEnabled && meshCfg.meshEnabled
          then do
            dbRes <- case maybeRow of
              Nothing -> findOneFromDB dbConf whereClause
              Just dbrow -> pure $ Right $ Just dbrow
            (KV,) <$> case dbRes of
              Right (Just obj) -> do
                reCacheDBRowsRes <- reCacheDBRows meshCfg [obj]
                case reCacheDBRowsRes of
                  Left err -> return $ Left $ MRedisError err
                  Right _  -> mapRight Just <$> if isLive
                    then updateObjectRedis meshCfg updVals False whereClause obj
                    else deleteObjectRedis meshCfg False whereClause obj
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
  MeshConfig -> [(Text, A.Value)] -> Bool -> Where be table -> table Identity -> m (MeshResult (table Identity))
updateObjectRedis meshCfg updVals addPrimaryKeyToWhereClause whereClause obj = do
  configUpdateResult <- updateObjectInMemConfig meshCfg whereClause updVals obj
  when (isLeft configUpdateResult) $ L.logErrorT "MEMCONFIG_UPDATE_ERROR" (show configUpdateResult)
  case (updateModel @be @table) obj updVals of
    Left err -> return $ Left err
    Right updatedModel -> do
      time <- fromIntegral <$> L.getCurrentDateInMillis
      let pKeyText  = getLookupKeyByPKey obj
          shard     = getShardedHashTag pKeyText
          pKey      = fromString . T.unpack $ pKeyText <> shard
          updateCmd = if addPrimaryKeyToWhereClause 
                        then getDbUpdateCommandJsonWithPrimaryKey (tableName @(table Identity)) updVals obj whereClause
                        else getDbUpdateCommandJson (tableName @(table Identity)) updVals whereClause
          qCmd      = getUpdateQuery V1 (pKeyText <> shard) time meshCfg.meshDBName updateCmd
      case resultToEither $ A.fromJSON updatedModel of
        Right value -> do
          let olderSkeys = map (\(SKey s) -> s) (secondaryKeysFiltered obj)
          skeysUpdationRes <- modifySKeysRedis olderSkeys value
          case skeysUpdationRes of
            Right _ -> do
              kvdbRes <- L.runKVDB meshCfg.kvRedis $ L.multiExecWithHash (encodeUtf8 shard) $ do
                _ <- L.xaddTx
                      (encodeUtf8 (meshCfg.ecRedisDBStream <> shard))
                      L.AutoID
                      [("command", BSL.toStrict $ A.encode qCmd)]
                L.setexTx pKey meshCfg.redisTtl (BSL.toStrict $ Encoding.encode_ meshCfg.cerealEnabled value)
              case kvdbRes of
                Right _ -> pure $ Right value
                Left err -> pure $ Left $ MRedisError err
            Left err -> pure $ Left err
        Left err -> pure $ Left $ MDecodingError err

  where
    modifySKeysRedis :: [[(Text, Text)]] -> table Identity -> m (MeshResult (table Identity)) -- TODO: Optimise this logic
    modifySKeysRedis olderSkeys table = do
      let pKeyText = getLookupKeyByPKey table
          shard = getShardedHashTag pKeyText
          pKey = fromString . T.unpack $ pKeyText <> shard
      let tName = tableName @(table Identity)
          updValsMap = HM.fromList (map (\p -> (fst p, True)) updVals)
          (modifiedSkeysOld, unModifiedSkeys) = applyFPair (map (getSortedKeyAndValue tName)) $
                                                segregateList (`isKeyModified` updValsMap) olderSkeys
          newSkeys =  map (\(SKey s) -> s) (secondaryKeys table)
          (modifiedSkeysNew, _) = applyFPair (map (getSortedKeyAndValue tName)) $
                                                segregateList (`isKeyModified` updValsMap) newSkeys
      mapRight (const table) <$> runExceptT (do
                                    mapM_ (ExceptT . resetTTL) unModifiedSkeys
                                    mapM_ (ExceptT . deletePkeyFromSkey pKey) modifiedSkeysOld
                                    mapM_ (ExceptT . addPkeyToSkey pKey) modifiedSkeysNew)

    resetTTL Nothing = pure $ Right False
    resetTTL (Just key) = do
      x <- L.rExpire meshCfg.kvRedis (fromString $ T.unpack key) meshCfg.redisTtl
      pure $ mapLeft MRedisError x
    
    deletePkeyFromSkey _ Nothing = pure $ Right 0
    deletePkeyFromSkey pKey (Just key) = mapLeft MRedisError <$> L.sRemB meshCfg.kvRedis (fromString $ T.unpack key) [pKey]

    addPkeyToSkey :: ByteString -> Maybe Text -> m (MeshResult Bool)
    addPkeyToSkey _ Nothing = pure $ Right False
    addPkeyToSkey pKey (Just key) = do
      _ <- L.rSadd meshCfg.kvRedis (fromString $ T.unpack key) [pKey]
      mapLeft MRedisError <$> L.rExpireB meshCfg.kvRedis (fromString $ T.unpack key) meshCfg.redisTtl

    getSortedKeyAndValue :: Text -> [(Text,Text)] -> Maybe Text
    getSortedKeyAndValue tName kvTup = do
      let sortArr = sortBy (compare `on` fst) kvTup
      let (appendedKeys, appendedValues) = applyFPair (T.intercalate "_") $ unzip sortArr
      if not $ null (filter (\(_, v) -> v=="") kvTup)
        then Nothing
        else Just $ tName <> "_" <> appendedKeys <> "_" <> appendedValues

    isKeyModified :: [(Text, Text)] -> HM.HashMap Text Bool -> Bool
    isKeyModified sKey updValsMap = foldl' (\r k -> HM.member (fst k) updValsMap || r) False sKey

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
    Serialize.Serialize (table Identity),
    Show (table Identity), --debugging purpose
    L.MonadFlow m
  ) =>
  DBConfig BP.Pg ->
  MeshConfig ->
  [Set BP.Postgres table] ->
  Where BP.Postgres table ->
  m (MeshResult [table Identity])
updateAllReturningWithKVConnector dbConf meshCfg setClause whereClause = do
  let isDisabled = meshCfg.kvHardKilled 
  res <- if not isDisabled
    then do
      let updVals = jsonKeyValueUpdates setClause
      kvRows <- redisFindAll meshCfg whereClause
      dbRows <- findAllSql dbConf whereClause
      updateKVAndDBResults meshCfg whereClause dbRows kvRows (Just updVals) False dbConf (Just setClause) True
    else do
      let updateQuery = DB.updateRowsReturningList $ sqlUpdate ! #set setClause ! #where_ whereClause
      res <- runQuery dbConf updateQuery
      case res of
        Right x -> do
          when meshCfg.memcacheEnabled $
            mapM_ (pushToInMemConfigStream meshCfg ImcInsert ) x
          return $ Right x
        Left e -> return $ Left $ MDBError e
  diffRes <- whereClauseDiffCheck whereClause
  let source = if isDisabled then SQL else if (isRecachingEnabled && meshCfg.meshEnabled) then KV else KV_AND_SQL
  pure res

updateAllWithKVConnector :: forall be table beM m.
  ( HasCallStack,
    SqlReturning beM be,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    Show (table Identity), --debugging purpose
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  MeshConfig ->
  [Set be table] ->
  Where be table ->
  m (MeshResult ())
updateAllWithKVConnector dbConf meshCfg setClause whereClause = do
  let isDisabled = meshCfg.kvHardKilled  
  res <- if not isDisabled
    then do
      let updVals = jsonKeyValueUpdates setClause
      kvRows <- redisFindAll meshCfg whereClause
      dbRows <- findAllSql dbConf whereClause
      mapRight (const ()) <$> updateKVAndDBResults meshCfg whereClause dbRows kvRows (Just updVals) True dbConf (Just setClause) True
    else do
      let updateQuery = DB.updateRows $ sqlUpdate ! #set setClause ! #where_ whereClause
      res <- runQuery dbConf updateQuery
      case res of
        Right _ -> do
          let findAllQuery = DB.findRows (sqlSelect ! #where_ whereClause ! defaults)
          dbRes <- runQuery dbConf findAllQuery
          case dbRes of
            Right dbRows -> do
              when meshCfg.memcacheEnabled $
                mapM_ (pushToInMemConfigStream meshCfg ImcInsert) dbRows
              return . Right $ ()
            Left e -> return . Left . MDBError $ e
        Left e -> return $ Left $ MDBError e
  diffRes <- whereClauseDiffCheck whereClause
  let source = if isDisabled then SQL else if (isRecachingEnabled && meshCfg.meshEnabled) then KV else KV_AND_SQL
  pure res

updateKVAndDBResults :: forall be table beM m.
  ( HasCallStack,
    SqlReturning beM be,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    Show (table Identity), --debugging purpose
    L.MonadFlow m
  ) => MeshConfig -> Where be table -> Either DBError [table Identity] -> MeshResult ([table Identity], [table Identity]) -> Maybe [(Text, A.Value)] -> Bool -> DBConfig beM -> Maybe [Set be table] -> Bool -> m (MeshResult [table Identity])
updateKVAndDBResults meshCfg whereClause eitherDbRows eitherKvRows mbUpdateVals updateWoReturning dbConf mbSetClause isLive = do
  let setClause = fromMaybe [] mbSetClause --Change this logic
      updVals = fromMaybe [] mbUpdateVals
  case (eitherDbRows, eitherKvRows) of
    (Right allDBRows, Right allKVRows) -> do
      let kvLiveRows = fst allKVRows
          kvDeadRows = snd allKVRows
          kvLiveAndDeadRows = kvLiveRows ++ kvDeadRows
          matchedKVLiveRows = findAllMatching whereClause kvLiveRows
      if isRecachingEnabled && meshCfg.meshEnabled
        then do
          let uniqueDbRows =  getUniqueDBRes allDBRows kvLiveAndDeadRows
          reCacheDBRowsRes <- reCacheDBRows meshCfg uniqueDbRows
          case reCacheDBRowsRes of
            Left err -> return $ Left $ MRedisError err
            Right _  -> do
              let allRows = matchedKVLiveRows ++ uniqueDbRows
              sequence <$> if isLive
                  then mapM (updateObjectRedis meshCfg updVals True whereClause) allRows
                  else mapM (deleteObjectRedis meshCfg True whereClause) allRows
        else do
          updateOrDelKVRowRes <- if isLive
            then mapM (updateObjectRedis meshCfg updVals True whereClause) matchedKVLiveRows
            else mapM (deleteObjectRedis meshCfg True whereClause) matchedKVLiveRows
          kvres <- pure $ foldEither updateOrDelKVRowRes
          case kvres of 
            Left err -> return $ Left err
            Right kvRes -> runUpdateOrDelete setClause kvRes kvLiveAndDeadRows
          
    (Left err, _) -> pure $ Left (MDBError err)
    (_, Left err) -> pure $ Left err
          

    where
      runUpdateOrDelete setClause kvres kvLiveAndDeadRows = do
        case (isLive, updateWoReturning) of
          (True, True) -> do
            let updateQuery = DB.updateRows $ sqlUpdate ! #set setClause ! #where_ whereClause
            res <- runQuery dbConf updateQuery
            case res of
                Right _ -> return $ Right []
                Left e -> return $ Left $ MDBError e
          (True, False) -> do
            let updateQuery = DB.updateRowsReturningList $ sqlUpdate ! #set setClause ! #where_ whereClause
            res <- runQuery dbConf updateQuery
            case res of
                Right x -> return $ Right $ (getUniqueDBRes x kvLiveAndDeadRows) ++ kvres
                Left e  -> return $ Left $ MDBError e
          (False, _) -> do
            res <- deleteAllReturning dbConf whereClause
            case res of
                Right x -> return $ Right $ (getUniqueDBRes x kvLiveAndDeadRows) ++ kvres
                Left e  -> return $ Left $ MDBError e


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
    Serialize.Serialize (table Identity),
    L.MonadFlow m,
    Show (table Identity)
  ) =>
  DBConfig beM ->
  MeshConfig ->
  Where be table ->
  m (MeshResult (Maybe (table Identity)))
findWithKVConnector dbConf meshCfg whereClause = do --This function fetches all possible rows and apply where clause on it.
  let shouldSearchInMemoryCache = meshCfg.memcacheEnabled
  (source, res) <- if shouldSearchInMemoryCache
    then do
      inMemResult <- searchInMemoryCache meshCfg dbConf whereClause
      findOneFromDBIfNotFound inMemResult
    else
      kvFetch
  diffRes <- whereClauseDiffCheck whereClause
  pure res
  where

    findOneFromDBIfNotFound :: (Source, MeshResult [table Identity]) -> m (Source, MeshResult (Maybe (table Identity)))
    findOneFromDBIfNotFound res = case res of
      (source, Right []) -> pure (source, Right Nothing)
      (source, Right rows) -> do
        let matchingRes = findOneMatching whereClause rows
        if isJust matchingRes
          then pure (source, Right matchingRes)
          else (SQL,) <$> findOneFromDB dbConf whereClause
      (source, Left err) -> pure (source, Left err)

    kvFetch :: m ((Source, MeshResult (Maybe (table Identity))))
    kvFetch = do
      let isDisabled = meshCfg.kvHardKilled
      if not isDisabled
        then do
          eitherKvRows <- findOneFromRedis meshCfg whereClause
          case eitherKvRows of
            Right ([], []) -> do
              (SQL,) <$> findOneFromDB dbConf whereClause
            Right ([], _) -> do
              L.logInfoT "findWithKVConnector" ("Returning nothing - Row is deleted already for " <> tableName @(table Identity))
              pure $ (KV, Right Nothing)
            Right (kvLiveRows, _) -> do
              second (mapRight listToMaybe) <$> findFromDBIfMatchingFails dbConf whereClause kvLiveRows
            Left err -> pure $ (KV, Left err)
        else do
          (SQL,) <$> findOneFromDB dbConf whereClause

findFromDBIfMatchingFails :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    Model be table,
    MeshMeta be table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    L.MonadFlow m) =>
  DBConfig beM ->
  Where be table ->
  [table Identity] ->
  m (Source, MeshResult [table Identity])
findFromDBIfMatchingFails dbConf whereClause kvRows = do
  case findAllMatching whereClause kvRows of -- For solving partial data case - One row in SQL and one in DB
    [] -> do
      dbRes <- findOneFromDB dbConf whereClause
      case dbRes of
        Right (Just dbRow) -> do
          let kvPkeys = map getLookupKeyByPKey kvRows
          if getLookupKeyByPKey dbRow `notElem` kvPkeys
            then pure (SQL, Right [dbRow])
            else pure (KV, Right [])
        Left err           -> pure (SQL, Left err)
        {- Below source cannot be determined as there can be 2 possiblities
           1. Row is in KV but matching failed because of some column like status
           2. Row is in KV for other clause (Eg. merchantId) but not for required clause and also not in SQL
           Source as KV can be more misleading, therefore returning source as SQL -}
        _                  -> pure (SQL, Right [])
    xs -> pure (KV, Right xs)

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
  MeshConfig -> Where be table -> m (MeshResult ([table Identity], [table Identity]))
findOneFromRedis meshCfg whereClause = do
  let keyAndValueCombinations = getFieldsAndValuesFromClause meshModelTableEntityDescriptor (And whereClause)
      andCombinations = map (uncurry zip . applyFPair (map (T.intercalate "_") . sortOn (Down . length) . nonEmptySubsequences) . unzip . sort) keyAndValueCombinations
      modelName = tableName @(table Identity)
      keyHashMap = keyMap @(table Identity)
      andCombinationsFiltered = mkUniq $ filterPrimaryAndSecondaryKeys keyHashMap <$> andCombinations
  eitherKeyRes <- mapM (getPrimaryKeyFromFieldsAndValues modelName meshCfg keyHashMap) andCombinationsFiltered
  case foldEither eitherKeyRes of
    Right keyRes -> do
      allRowsRes <- foldEither <$> mapM (getDataFromPKeysRedis meshCfg) (mkUniq keyRes)
      case allRowsRes of
        Right allRowsResPairList -> do
          let (allRowsResLiveListOfList, allRowsResDeadListOfList) = unzip allRowsResPairList
          return $ Right (concat allRowsResLiveListOfList, concat allRowsResDeadListOfList)
        Left err -> return $ Left err
    Left err -> pure $ Left err

findOneFromDB :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    Model be table,
    MeshMeta be table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    L.MonadFlow m
  ) =>
  DBConfig beM -> Where be table -> m (MeshResult (Maybe (table Identity)))
findOneFromDB dbConf whereClause = do
  let findQuery = DB.findRow (sqlSelect ! #where_ whereClause ! defaults)
  mapLeft MDBError <$> runQuery dbConf findQuery



findAllWithOptionsHelper :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    Model be table,
    MeshMeta be table,
    KVConnector (table Identity),
    Serialize.Serialize (table Identity),
    Show (table Identity),
    ToJSON (table Identity),
    FromJSON (table Identity),
    L.MonadFlow m, B.HasQBuilder be, BeamRunner beM) =>
  DBConfig beM ->
  MeshConfig ->
  Where be table ->
  Maybe (OrderBy table) ->
  Maybe Int ->
  Maybe Int ->
  m (MeshResult [table Identity])
findAllWithOptionsHelper dbConf meshCfg whereClause orderBy mbLimit mbOffset = do
  let isDisabled = meshCfg.kvHardKilled  
  res <- if not isDisabled
    then do
      kvRes <- redisFindAll meshCfg whereClause
      case kvRes of
        Right kvRows -> do
          let matchedKVLiveRows = findAllMatching whereClause (fst kvRows)
              matchedKVDeadRows = snd kvRows
              offset = fromMaybe 0 mbOffset
              shift = length matchedKVLiveRows + length matchedKVDeadRows
              updatedOffset = if offset - shift >= 0 then offset - shift else 0
              findAllQueryUpdated = DB.findRows (sqlSelect'
                ! #where_ whereClause
                ! #orderBy (if isJust orderBy then Just [DMaybe.fromJust orderBy] else Nothing)
                ! #limit ((shift +) <$> mbLimit)
                ! #offset (Just updatedOffset) -- Offset is 0 in case mbOffset Nothing
                ! defaults)
          dbRes <- runQuery dbConf findAllQueryUpdated
          case dbRes of
            Left err -> pure $ Left $ MDBError err
            Right [] -> pure $ Right $ applyOptions offset matchedKVLiveRows
            Right dbRows -> do
              let mergedRows = matchedKVLiveRows ++ getUniqueDBRes dbRows (snd kvRows ++ fst kvRows)
              if isJust mbOffset
                then do
                  let noOfRowsFelledLeftSide = calculateLeftFelledRedisEntries matchedKVLiveRows dbRows
                  pure $ Right $ applyOptions ((if updatedOffset == 0 then offset else shift) - noOfRowsFelledLeftSide) mergedRows
                else pure $ Right $ applyOptions 0 mergedRows
        Left err -> pure $ Left err
    else do
      let findAllQuery = DB.findRows (sqlSelect'
            ! #where_ whereClause
            ! #orderBy (if isJust orderBy then Just [DMaybe.fromJust orderBy] else Nothing)
            ! #limit mbLimit
            ! #offset mbOffset
            ! defaults)
      mapLeft MDBError <$> runQuery dbConf findAllQuery
  diffRes <- whereClauseDiffCheck whereClause
  let source = if not isDisabled then KV_AND_SQL else SQL
  pure res
    where
      applyOptions :: Int -> [table Identity] -> [table Identity]
      applyOptions shift rows = do
        let resWithoutLimit = case orderBy of 
                          Nothing -> drop shift rows
                          Just res -> do
                            let cmp = case res of 
                                  Asc col -> compareCols (fromColumnar' . col . columnize) True
                                  Desc col -> compareCols (fromColumnar' . col . columnize) False
                            (drop shift . sortBy cmp) rows
        maybe resWithoutLimit (`take` resWithoutLimit) mbLimit
      compareCols :: (Ord value) => (table Identity -> value) -> Bool -> table Identity -> table Identity -> Ordering
      compareCols col isAsc r1 r2 = if isAsc then compare (col r1) (col r2) else compare (col r2) (col r1)

      calculateLeftFelledRedisEntries :: [table Identity] -> [table Identity] -> Int
      calculateLeftFelledRedisEntries kvRows dbRows = do
        case orderBy of
          Just (Asc col) -> do
            let dbMn = maximum $ map (fromColumnar' . col . columnize) dbRows
            length $ filter (\r -> dbMn > fromColumnar' (col $ columnize r)) kvRows
          Just (Desc col) -> do
            let dbMx = maximum $ map (fromColumnar' . col . columnize) dbRows
            length $ filter (\r -> dbMx < fromColumnar' (col $ columnize r)) kvRows
          Nothing -> 0

-- Need to recheck offset implementation
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
    L.MonadFlow m, B.HasQBuilder be, BeamRunner beM) =>
  DBConfig beM ->
  MeshConfig ->
  Where be table ->
  OrderBy table ->
  Maybe Int ->
  Maybe Int ->
  m (MeshResult [table Identity])
findAllWithOptionsKVConnector dbConf meshCfg whereClause orderBy mbLimit mbOffset = do
  findAllWithOptionsHelper dbConf meshCfg whereClause (Just orderBy) mbLimit mbOffset

-- Need to recheck offset implementation
findAllWithOptionsKVConnector' :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    Model be table,
    MeshMeta be table,
    KVConnector (table Identity),
    Serialize.Serialize (table Identity),
    Show (table Identity),
    ToJSON (table Identity),
    FromJSON (table Identity),
    L.MonadFlow m, B.HasQBuilder be, BeamRunner beM) =>
  DBConfig beM ->
  MeshConfig ->
  Where be table ->
  Maybe Int ->
  Maybe Int ->
  m (MeshResult [table Identity])
findAllWithOptionsKVConnector' dbConf meshCfg whereClause  = 
  findAllWithOptionsHelper dbConf meshCfg whereClause Nothing 

findAllWithKVConnector :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    Model be table,
    MeshMeta be table,
    KVConnector (table Identity),
    ToJSON (table Identity),
    FromJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m, B.HasQBuilder be, BeamRunner beM) =>
  DBConfig beM ->
  MeshConfig ->
  Where be table ->
  m (MeshResult [table Identity])
findAllWithKVConnector dbConf meshCfg whereClause = do
  let findAllQuery = DB.findRows (sqlSelect ! #where_ whereClause ! defaults)
  let isDisabled = meshCfg.kvHardKilled  
  res <- if not isDisabled
    then do
      kvRes <- redisFindAll meshCfg whereClause
      case kvRes of
        Right kvRows -> do
          let matchedKVLiveRows = findAllMatching whereClause (fst kvRows)
          dbRes <- runQuery dbConf findAllQuery
          case dbRes of
            Right dbRows -> pure $ Right $ matchedKVLiveRows ++ getUniqueDBRes dbRows (fst kvRows ++ snd kvRows)
            Left err     -> return $ Left $ MDBError err
        Left err -> return $ Left err  
    else do
      mapLeft MDBError <$> runQuery dbConf findAllQuery
  diffRes <- whereClauseDiffCheck whereClause
  let source = if not isDisabled then KV_AND_SQL else SQL
  pure res

findAllWithKVAndConditionalDBInternal :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    Model be table,
    MeshMeta be table,
    KVConnector (table Identity),
    ToJSON (table Identity),
    FromJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m, B.HasQBuilder be, BeamRunner beM) =>
  DBConfig beM ->
  MeshConfig ->
  Where be table ->
  m (MeshResult [table Identity])
findAllWithKVAndConditionalDBInternal dbConf meshCfg whereClause  = do
  let isDisabled = meshCfg.kvHardKilled
  if not isDisabled 
    then do
      kvRes <- redisFindAll meshCfg whereClause
      case kvRes of
        Right kvRows -> do
          let matchedKVLiveRows = findAllMatching whereClause (fst kvRows)
          if not (null matchedKVLiveRows)
            then pure $ Right matchedKVLiveRows
            else do
              let findAllQueryUpdated = DB.findRows (sqlSelect ! #where_ whereClause ! defaults)
              dbRes <- runQuery dbConf findAllQueryUpdated
              case dbRes of
                Right dbRows -> pure $ Right $ getUniqueDBRes dbRows (snd kvRows)
                Left err     -> return $ Left $ MDBError err
        Left err -> return $ Left err
    else do
      let findAllQuery = DB.findRows (sqlSelect ! #where_ whereClause ! defaults)
      mapLeft MDBError <$> runQuery dbConf findAllQuery

redisFindAll :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    Model be table,
    MeshMeta be table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m, B.HasQBuilder be, BeamRunner beM) =>
  MeshConfig ->
  Where be table ->
  m (MeshResult ([table Identity], [table Identity]))
redisFindAll meshCfg whereClause = do
  let keyAndValueCombinations = getFieldsAndValuesFromClause meshModelTableEntityDescriptor (And whereClause)
      andCombinations = map (uncurry zip . applyFPair (map (T.intercalate "_") . sortOn (Down . length) . nonEmptySubsequences) . unzip . sort) keyAndValueCombinations
      modelName = tableName @(table Identity)
      keyHashMap = keyMap @(table Identity)
      andCombinationsFiltered = mkUniq $ filterPrimaryAndSecondaryKeys keyHashMap <$> andCombinations
  eitherKeyRes <- mapM (getPrimaryKeyFromFieldsAndValues modelName meshCfg keyHashMap) andCombinationsFiltered
  case foldEither eitherKeyRes of
    Right keyRes -> do
      allRowsRes <- foldEither <$> mapM (getDataFromPKeysRedis meshCfg) (mkUniq keyRes)
      case allRowsRes of
        Right allRowsResPairList -> do
          let (allRowsResLiveListOfList, allRowsResDeadListOfList) = unzip allRowsResPairList
          return $ Right (concat allRowsResLiveListOfList, concat allRowsResDeadListOfList)
        Left err -> return $ Left err
    Left err -> pure $ Left err

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
  MeshConfig -> Bool -> Where be table -> table Identity -> m (MeshResult (table Identity))
deleteObjectRedis meshCfg addPrimaryKeyToWhereClause whereClause obj = do
  time <- fromIntegral <$> L.getCurrentDateInMillis
  let pKeyText  = getLookupKeyByPKey obj
      shard     = getShardedHashTag pKeyText
      pKey      = fromString . T.unpack $ pKeyText <> shard
      deleteCmd = if addPrimaryKeyToWhereClause
                    then getDbDeleteCommandJsonWithPrimaryKey (tableName @(table Identity)) obj whereClause
                    else getDbDeleteCommandJson (tableName @(table Identity)) whereClause
      qCmd      = getDeleteQuery V1 (pKeyText <> shard) time meshCfg.meshDBName deleteCmd
  kvDbRes <- L.runKVDB meshCfg.kvRedis $ L.multiExecWithHash (encodeUtf8 shard) $ do
    _ <- L.xaddTx
          (encodeUtf8 (meshCfg.ecRedisDBStream <> shard))
          L.AutoID
          [("command", BSL.toStrict $ A.encode qCmd)]
    L.setexTx pKey meshCfg.redisTtl (BSL.toStrict $ Encoding.encodeDead $ Encoding.encode_ meshCfg.cerealEnabled obj)
  case kvDbRes of
    Left err -> return . Left $ MRedisError err
    Right _  -> do
      pushToInMemConfigStream meshCfg ImcDelete obj 
      return $ Right obj

reCacheDBRows :: forall table m.
  ( HasCallStack,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    -- Show (table Identity), --debugging purpose
    L.MonadFlow m
  ) =>
  MeshConfig ->
  [table Identity] ->
  m (Either KVDBReply [[Bool]])
reCacheDBRows meshCfg dbRows = do
  reCacheRes <- mapM (\obj -> do
      let pKeyText = getLookupKeyByPKey obj
          shard = getShardedHashTag pKeyText
          pKey = fromString . T.unpack $ pKeyText <> shard
      res <- mapM (\secIdx -> do -- Recaching Skeys in redis
          let sKey = fromString . T.unpack $ secIdx
          res1 <- L.runKVDB meshCfg.kvRedis $ L.sadd sKey [pKey]
          case res1 of
            Left err -> return $ Left err
            Right _  -> 
              L.runKVDB meshCfg.kvRedis $  L.expire sKey meshCfg.redisTtl
        ) $ getSecondaryLookupKeys obj
      return $ sequence res
    ) dbRows
  return $ sequence reCacheRes

deleteWithKVConnector :: forall be table beM m.
  ( HasCallStack,
    SqlReturning beM be,
    BeamRuntime be beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    ToJSON (table Identity),
    FromJSON (table Identity),
    Show (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m, B.HasQBuilder be, BeamRunner beM) =>
  DBConfig beM ->
  MeshConfig ->
  Where be table ->
  m (MeshResult ())
deleteWithKVConnector dbConf meshCfg whereClause = do
  let isDisabled = meshCfg.kvHardKilled
  (source, res) <- if not isDisabled
    then do
      (\delRes -> (fst delRes, mapRight (const ()) (snd delRes))) <$> modifyOneKV dbConf meshCfg whereClause Nothing True False
    else do
      let deleteQuery = DB.deleteRows $ sqlDelete ! #where_ whereClause
      res <- runQuery dbConf deleteQuery
      (SQL,) <$> case res of
        Left err -> return $ Left $ MDBError err
        Right re -> do
          if meshCfg.memcacheEnabled 
            then 
              searchInMemoryCache meshCfg dbConf whereClause >>= (snd >>> \case
                Left e -> return $ Left e
                Right rs -> do
                  mapM_ (pushToInMemConfigStream meshCfg ImcDelete) rs
                  return $ Right re)
            else do
                return $ Right re

  diffRes <- whereClauseDiffCheck whereClause
  pure res

deleteReturningWithKVConnector :: forall be table beM m.
  ( HasCallStack,
    SqlReturning beM be,
    BeamRuntime be beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    ToJSON (table Identity),
    FromJSON (table Identity),
    Show (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m, B.HasQBuilder be, BeamRunner beM) =>
  DBConfig beM ->
  MeshConfig ->
  Where be table ->
  m (MeshResult (Maybe (table Identity)))
deleteReturningWithKVConnector dbConf meshCfg whereClause = do
  let isDisabled = meshCfg.kvHardKilled
  (source, res) <- if not isDisabled
    then do
      modifyOneKV dbConf meshCfg whereClause Nothing False False
    else do
      res <- deleteAllReturning dbConf whereClause
      (SQL,) <$> case res of
        Left err  -> return $ Left $ MDBError err
        Right []  -> return $ Right Nothing
        Right [r] -> do
          when meshCfg.memcacheEnabled $ pushToInMemConfigStream meshCfg ImcDelete r
          return $ Right (Just r)
        Right rs   -> do
          when meshCfg.memcacheEnabled $ mapM_ (pushToInMemConfigStream meshCfg ImcDelete) rs
          return $ Left $ MUpdateFailed "SQL delete returned more than one record"
  diffRes <- whereClauseDiffCheck whereClause
  pure res

deleteAllReturningWithKVConnector :: forall be table beM m.
  ( HasCallStack,
    SqlReturning beM be,
    BeamRuntime be beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    ToJSON (table Identity),
    FromJSON (table Identity),
    Show (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m, B.HasQBuilder be, BeamRunner beM) =>
  DBConfig beM ->
  MeshConfig ->
  Where be table ->
  m (MeshResult [table Identity])
deleteAllReturningWithKVConnector dbConf meshCfg whereClause = do
  let isDisabled = meshCfg.kvHardKilled
  res <- if not isDisabled
    then do
      kvResult <- redisFindAll meshCfg whereClause
      dbRows   <- findAllSql dbConf whereClause
      updateKVAndDBResults meshCfg whereClause dbRows kvResult Nothing False dbConf Nothing False
    else do
      res <- deleteAllReturning dbConf whereClause
      case res of
        Left err -> return $ Left $ MDBError err
        Right re -> do
          when meshCfg.memcacheEnabled $ mapM_ (pushToInMemConfigStream meshCfg ImcDelete) re
          return $ Right re
  diffRes <- whereClauseDiffCheck whereClause
  let source = if isDisabled then SQL else if isRecachingEnabled then KV else KV_AND_SQL
  pure res
