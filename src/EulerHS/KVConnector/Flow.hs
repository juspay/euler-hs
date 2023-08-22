{- |
Module      :  EulerHS.KVConnector.Flow
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}


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
    deleteAllReturningWithKVConnector
  )
 where

import           EulerHS.PIIEncryption
import           EulerHS.Extra.Time (getCurrentDateInMillis)
import           EulerHS.Prelude hiding (maximum)
import EulerHS.CachedSqlDBQuery
    ( findAllSql,
      createReturning,
      createSqlWoReturing,
      updateOneSqlWoReturning,
      SqlReturning(..),
      findOne, 
      findAll, 
      findAllExtended')
import           EulerHS.KVConnector.Types (KVConnector(..), MeshConfig, MeshResult, MeshMeta(..), SecondaryKey(..), tableName, keyMap, Source(..), ETLStreamKeys (ETLCreate, ETLUpdate))
import           EulerHS.KVConnector.DBSync (getCreateQuery, getUpdateQuery, getDeleteQuery, getDbDeleteCommandJson, getDbUpdateCommandJson, getDbUpdateCommandJsonWithPrimaryKey, getDbDeleteCommandJsonWithPrimaryKey, DBCommandVersion(..))
import           EulerHS.KVConnector.InMemConfig.Flow (searchInMemoryCache, pushToInMemConfigStream, fetchRowFromDBAndAlterImc)
import           EulerHS.KVConnector.InMemConfig.Types (ImcStreamCommand(..))
import           EulerHS.KVConnector.Utils
import           EulerHS.KVDB.Types (KVDBReply,MeshError(..))
import           EulerHS.Types (Operation(..))
import           Control.Arrow ((>>>))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import           Data.List (span,maximum)
import qualified Data.Text as T
import qualified EulerHS.Language as L
import qualified Data.HashMap.Strict as HM
import           Data.Either.Extra (mapLeft, mapRight)
import qualified Data.Serialize as Serialize
import           EulerHS.SqlDB.Types (BeamRunner, BeamRuntime, DBConfig, DBError(DBError), DBErrorType (PIIError))
import qualified EulerHS.SqlDB.Language as DB
import           Sequelize (fromColumnar', columnize, sqlSelect, sqlSelect', sqlUpdate, sqlDelete, modelTableName, Model, Where, Clause(..), Set(..), OrderBy(..))
import qualified Database.Beam as B
import qualified Database.Beam.Postgres as BP
import qualified EulerHS.KVConnector.Encoding as Encoding
import           EulerHS.Logger.Types (ErrorL (..))
import           Named (defaults, (!))
import qualified EulerHS.ART.DBReplay as DBReplay
import qualified EulerHS.ART.EnvVars as Env (isArtReplayEnabled)
import           EulerHS.KVConnector.Metrics( incrementRedisCallMetric) 
import qualified Juspay.Extra.Config as Conf
import           EulerHS.CachedSqlDBQuery (runQuery)

isArtRecEnabled :: Bool
isArtRecEnabled = fromMaybe False $ readMaybe =<< Conf.lookupEnvT "RUNNING_MODE"

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
  DBConfig beM ->
  MeshConfig ->
  table Identity ->
  m (MeshResult ())
createWoReturingKVConnector dbConf meshCfg value = do
  let 
    isEnabled = meshCfg.meshEnabled && not meshCfg.kvHardKilled
    tName     = (modelTableName @table)
  t1                          <- getCurrentDateInMillis
  eitherTableRowWithPrimaryId <- getTableRowWithPrimaryId dbConf meshCfg tName value
  handleError eitherTableRowWithPrimaryId $ \tableRowWithPrimaryId -> do
    res <- create tName isEnabled tableRowWithPrimaryId
    t2  <- getCurrentDateInMillis
    let 
      source = if isEnabled then KV else SQL
      res'   = mapRight (const value) res
    logAndIncrementKVMetric True "CREATE" CREATE res' (t2 - t1) (modelTableName @table) source Nothing $> res
  where
    handleError :: MeshResult a -> (a -> m (MeshResult b)) -> m (MeshResult b)
    handleError = flip (either (pure . Left))

    create :: Text -> Bool -> table Identity -> m (MeshResult ()) 
    create tName isEnabled tableRowWithPrimaryId = getEncryptionKey tName >>= \case
      Right mbKeyConfig -> if isEnabled
        then mapRight (const ()) <$> createKV dbConf meshCfg tableRowWithPrimaryId mbKeyConfig 
        else DBReplay.runWithArtCreatemSQl dbConf tableRowWithPrimaryId "createSqlWoReturing" $ createSqlWoReturing dbConf tableRowWithPrimaryId mbKeyConfig
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
  DBConfig beM ->
  MeshConfig ->
  table Identity ->
  m (MeshResult (table Identity))
createWithKVConnector dbConf meshCfg value = do
  let 
    isEnabled = meshCfg.meshEnabled && not meshCfg.kvHardKilled
    tName     = (modelTableName @table)
  t1                          <- getCurrentDateInMillis
  eitherTableRowWithPrimaryId <- getTableRowWithPrimaryId dbConf meshCfg tName value
  handleError eitherTableRowWithPrimaryId $ \tableRowWithPrimaryId -> do
    res <- imcPush =<< create tName isEnabled tableRowWithPrimaryId 
    t2  <- getCurrentDateInMillis
    let source = if isEnabled then KV else SQL
    logAndIncrementKVMetric True "CREATE" CREATE_RETURNING res (t2 - t1) (modelTableName @table) source Nothing $> res

  where
    handleError :: MeshResult a -> (a -> m (MeshResult b)) -> m (MeshResult b)
    handleError = flip (either (pure . Left))

    imcPush :: MeshResult (table Identity) -> m (MeshResult (table Identity))
    imcPush res = res <$ (when meshCfg.memcacheEnabled $ do
      case res of
        Right obj -> pushToInMemConfigStream meshCfg ImcInsert obj
        Left _    -> pure ())

    create :: Text -> Bool -> table Identity -> m (MeshResult (table Identity)) 
    create tName isKVEnabled tableRowWithPrimaryId = getEncryptionKey tName >>= \case
      Right mbKeyConfig -> if isKVEnabled
        then createKV dbConf meshCfg tableRowWithPrimaryId mbKeyConfig
        else DBReplay.runWithArtCreatemSQl dbConf tableRowWithPrimaryId "createReturning" $ createReturning dbConf tableRowWithPrimaryId Nothing mbKeyConfig
      Left e -> pure $ Left $ MDBError e

createKV :: forall (table :: (Type -> Type) -> Type) m beM.
  ( FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    Show (table Identity),
    KVConnector (table Identity),
    L.MonadFlow m) =>
  DBConfig beM ->
  MeshConfig ->
  table Identity ->
  Maybe (PIIEncryptionKeyId, PIIEncryptionKey) ->
  m (MeshResult (table Identity))
createKV dbConf meshCfg val mbKeyConfig = do
  let isMySQL = isMySQLConfig dbConf
      pKeyText = getLookupKeyByPKey isMySQL val
      shard = getShardedHashTag pKeyText
      pKey = fromString . T.unpack $ pKeyText <> shard
      ntag = makePIIMetadata (mbKeyConfig >>= (\(PIIEncryptionKeyId{encKeyId}, _) -> Just encKeyId)) (pKeyText <> shard)
  time <- fromIntegral <$> L.getCurrentDateInMillis
  let qCmd = getCreateQuery (tableName @(table Identity)) V1 ntag time meshCfg.meshDBName val
  revMappingRes <- mapM (\secIdx -> do
    let sKey = fromString . T.unpack $ secIdx
    _ <- L.rSadd meshCfg.kvRedis sKey [pKey]
    L.rExpireB meshCfg.kvRedis sKey meshCfg.redisTtl
    ) $ getSecondaryLookupKeys isMySQL val
  case foldEither revMappingRes of
    Left err -> pure $ Left $ MRedisError err
    Right _ -> do
      kvRes <- if meshCfg.shouldPushToETLStream 
                  then addToDBSyncStreamETLStreamAndRedis meshCfg shard qCmd ETLCreate pKey val
                  else addToDBSyncStreamAndRedis meshCfg shard qCmd pKey val
      case kvRes of
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
  t1        <- getCurrentDateInMillis
  let tName = (modelTableName @table)
  eitherPiiKeys <- getEncryptionKey tName
  case eitherPiiKeys of 
    Left e -> return $ Left $ MDBError e 
    Right mbval -> do 
      (source, res) <- if not isDisabled
        then do
          -- Discarding object
          (\updRes -> (fst updRes, mapRight (const ()) (snd updRes))) <$> modifyOneKV dbConf meshCfg whereClause (Just setClause) True True mbval
        else do
          res <- DBReplay.runWithArtUpdate dbConf setClause whereClause "updateOneSqlWoReturning" $ updateOneSqlWoReturning dbConf setClause whereClause mbval
          (SQL,) <$> case res of
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
      logAndIncrementKVMetric True "UPDATE" UPDATE res (t2 - t1) (modelTableName @table) source diffRes
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
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  MeshConfig ->
  [Set be table] ->
  Where be table ->
  m (MeshResult (Maybe (table Identity)))
updateWithKVConnector dbConf meshCfg setClause whereClause = do
  let isDisabled = meshCfg.kvHardKilled
  t1        <- getCurrentDateInMillis
  (source, res) <- if not isDisabled
    then do
      modifyOneKV dbConf meshCfg whereClause (Just setClause) False True Nothing
    else do
      eitherPiiKeys <- getEncryptionKey (modelTableName @table)
      case eitherPiiKeys of 
        Left e -> return (SQL, Left $ MDBError e) 
        Right mbval -> do 
          res <- genericUpdateReturning dbConf meshCfg setClause whereClause mbval
          (SQL, ) <$> case res of
            Right (Just x) -> do
              when meshCfg.memcacheEnabled $ pushToInMemConfigStream meshCfg ImcInsert x
              return $ Right (Just x)
            Right Nothing -> return $ Right Nothing
            Left e -> return $ Left e
  t2        <- getCurrentDateInMillis
  diffRes   <- whereClauseDiffCheck dbConf whereClause
  logAndIncrementKVMetric True "UPDATE" UPDATE_RETURNING res (t2 - t1) (modelTableName @table) source diffRes
  pure res

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
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  MeshConfig ->
  [Set be table] ->
  Where be table ->
  Maybe (PIIEncryptionKeyId, PIIEncryptionKey) ->
  m (MeshResult (Maybe (table Identity)))
genericUpdateReturning dbConf meshCfg setClause whereClause mbval = do
  res <- if isMySQLConfig dbConf
    then do
      findResp <- findOneFromDB dbConf whereClause
      result <- DBReplay.runWithArtUpdate dbConf setClause whereClause "updateOneSqlWoReturning" $ updateOneSqlWoReturning dbConf setClause whereClause mbval
      case result of 
        Left err -> pure $ Left err
        Right _  -> do
          case findResp of 
              Right (Just respVal) -> case updateModel' setClause respVal of
                Right val  -> pure $ Right (Just val)
                Left  errU -> L.logErrorV ("UPDATE_MODEL_LOG_FAILURE" :: Text) (A.object [("model", A.String (modelTableName @table)),("error", A.toJSON errU)]) *> 
                              findOneFromDB dbConf whereClause -- Doesn't seem to occur but just to avoid decode issue in SQL flow because of KV instances
              _           -> pure $ findResp
    else do
      let updateQuery = DB.updateRowsReturningList $ sqlUpdate ! #set setClause ! #where_ whereClause
      res <- DBReplay.runWithArtUpdate dbConf setClause whereClause "updateWithKVConnector" $runQuery dbConf updateQuery
      case res of
        Right [x] -> return $ Right (Just x)
        Right [] -> return $ Right Nothing
        Right xs -> do
          let message = "DB returned \"" <> show (length xs) <> "\" rows after update for table: " <> show (tableName @(table Identity))
          L.logErrorWithCategory @Text "updateWithKVConnector" message $ ErrorL Nothing "KV_ERROR" message
          return $ Left $ UnexpectedError message
        Left e -> return $ Left e
  case res of
    Right (Just row) -> addToETLStream meshCfg (isMySQLConfig dbConf) ETLUpdate row $> res
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
    L.MonadFlow m, B.HasQBuilder be, BeamRunner beM) =>
  DBConfig beM ->
  MeshConfig ->
  Where be table ->
  Maybe [Set be table] ->
  Bool ->
  Bool ->
  Maybe (PIIEncryptionKeyId , PIIEncryptionKey) ->
  m (Source, MeshResult (Maybe (table Identity)))
modifyOneKV dbConf meshCfg whereClause mbSetClause updateWoReturning isLive mbKeyConfig = do
  let setClause = fromMaybe [] mbSetClause
      updVals = jsonKeyValueUpdates setClause
  kvResult <- findOneFromRedis dbConf meshCfg whereClause
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
           then updateObjectRedis dbConf meshCfg updVals False whereClause mbKeyConfig obj
           else deleteObjectRedis dbConf meshCfg False whereClause obj)
        (source, Right _)   -> do
          L.logErrorWithCategory ("modifyOneKV" :: Text) "Found more than one record in redis - Modification failed" $ ErrorL Nothing "KV_ERROR" ""
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
              searchInMemoryCache dbConf whereClause >>= (snd >>> \case
                Left e -> return $ Left e
                Right (Just a) -> Right <$> (pushToInMemConfigStream meshCfg ImcDelete) a
                Right (Nothing) -> fetchRowFromDBAndAlterImc dbConf meshCfg whereClause ImcDelete)
          (False, Just x) -> Right <$> pushToInMemConfigStream meshCfg ImcDelete x

      runUpdateOrDelete setClause = do
        case (isLive, updateWoReturning) of
          (True, True) -> do
            oldRes <- fromRight Nothing <$> findOneFromDB dbConf whereClause
            res <- DBReplay.runWithArtUpdate dbConf setClause whereClause "updateOneSqlWoReturning" $ updateOneSqlWoReturning dbConf setClause whereClause mbKeyConfig
            case res of
              Right _ -> do
                void $ maybe (return ()) (addToETLStream meshCfg (isMySQLConfig dbConf) ETLUpdate) oldRes
                pure $ Right Nothing
              Left e -> return $ Left e
          (True, False) -> do
            setCl <- maybe (pure $ Right $ setClause) (\(encKeyId, encKey) -> transformSetClause setClause encKeyId encKey) mbKeyConfig
            case setCl of
              Left err -> return $ Left $ MDBError $ DBError PIIError err
              Right setClause' -> genericUpdateReturning dbConf meshCfg setClause' whereClause mbKeyConfig
          (False, True) -> do
            let deleteQuery = DB.deleteRows $ sqlDelete ! #where_ whereClause
            res <- DBReplay.runWithArtDelete dbConf whereClause "deleteRows" $ runQuery dbConf deleteQuery
            case res of
                Right _ -> return $ Right Nothing
                Left e  -> return $ Left e
          (False, False) -> do
            res <- DBReplay.runWithArtDelete dbConf whereClause "runUpdateOrDelete" $ deleteAllReturning dbConf whereClause
            case res of
                Right [x] -> return $ Right (Just x)
                Right [] -> return $ Right Nothing
                Right xs -> do
                  let message = "DB returned " <> show (length xs) <> " rows after delete for table: " <> show (tableName @(table Identity))
                  L.logErrorWithCategory ("deleteReturningWithKVConnector" :: Text) message $ ErrorL Nothing "KV_ERROR" ""
                  return $ Left $ UnexpectedError message
                Left e -> return $ Left e

      updateInKVOrSQL maybeRow updVals setClause = do
        if isRecachingEnabled && meshCfg.meshEnabled
          then do
            dbRes <- case maybeRow of
              Nothing    -> findOneFromDB dbConf whereClause
              Just dbrow -> pure $ Right $ Just dbrow
            (KV,) <$> case dbRes of
              Right (Just obj) -> do
                reCacheDBRowsRes <- reCacheDBRows dbConf meshCfg [obj]
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
  DBConfig beM -> MeshConfig -> [(Text, A.Value)] -> Bool -> Where be table -> Maybe (PIIEncryptionKeyId, PIIEncryptionKey) -> table Identity -> m (MeshResult (table Identity))
updateObjectRedis dbConf meshCfg updVals addPrimaryKeyToWhereClause whereClause mbKeyConfig obj = do
  configUpdateResult <- updateObjectInMemConfig meshCfg whereClause updVals obj
  when (isLeft configUpdateResult) $ L.logErrorWithCategory ("MEMCONFIG_UPDATE_ERROR" :: Text) (show configUpdateResult) $ ErrorL Nothing "MEM_CONFIG_ERROR" (show configUpdateResult)
  case (updateModel @be @table) obj updVals of
    Left err -> return $ Left err
    Right updatedModel -> do
      time <- fromIntegral <$> L.getCurrentDateInMillis
      let isMySQL = isMySQLConfig dbConf
          pKeyText  = getLookupKeyByPKey isMySQL obj
          shard     = getShardedHashTag pKeyText
          pKey      = fromString . T.unpack $ pKeyText <> shard
          ntag      = makePIIMetadata (mbKeyConfig >>= (\(PIIEncryptionKeyId{encKeyId}, _) -> Just encKeyId)) (pKeyText <> shard)
          updateCmd = if addPrimaryKeyToWhereClause 
                        then getDbUpdateCommandJsonWithPrimaryKey dbConf (tableName @(table Identity)) updVals obj whereClause
                        else getDbUpdateCommandJson (tableName @(table Identity)) updVals whereClause
          qCmd      = getUpdateQuery V1 ntag time meshCfg.meshDBName updateCmd
      case resultToEither $ A.fromJSON updatedModel of
        Right value -> do
          let olderSkeys = map (\(SKey s) -> s) (secondaryKeysFiltered isMySQL obj)
          skeysUpdationRes <- modifySKeysRedis olderSkeys value
          case skeysUpdationRes of
            Right _ -> do
              kvdbRes <- if meshCfg.shouldPushToETLStream 
                            then addToDBSyncStreamETLStreamAndRedis meshCfg shard qCmd ETLUpdate pKey value
                            else addToDBSyncStreamAndRedis meshCfg shard qCmd pKey value
              case kvdbRes of
                Right _  -> pure $ Right value
                Left err -> pure $ Left $ MRedisError err
            Left err -> pure $ Left err
        Left err -> pure $ Left $ MDecodingError err

  where
    modifySKeysRedis :: [[(Text, Text)]] -> table Identity -> m (MeshResult (table Identity))
    modifySKeysRedis olderSkeys table = do
      let isMySQL = isMySQLConfig dbConf
          pKeyText = getLookupKeyByPKey isMySQL table
          shard = getShardedHashTag pKeyText
          pKey = fromString . T.unpack $ pKeyText <> shard
      let tName = tableName @(table Identity)
          updValsMap = HM.fromList (map (\p -> (fst p, True)) updVals)
          (modifiedSkeysValues, unModifiedSkeysValues) = applyFPair (map getSortedKeyAndValue) $
                                                span (`isKeyModified` updValsMap) olderSkeys
          newSkeysValues = map (\(SKey s) -> getSortedKeyAndValue s) (secondaryKeysFiltered isMySQL table)
      let unModifiedSkeys = map (\x -> tName <> "_" <> fst x <> "_" <> snd x) unModifiedSkeysValues
      let modifiedSkeysValuesMap = HM.fromList modifiedSkeysValues
      mapRight (const table) <$> runExceptT (do
                                    mapM_ ((ExceptT . resetTTL) . (fromString . T.unpack)) unModifiedSkeys
                                    mapM_ (ExceptT . addNewSkey pKey tName) (foldSkeysFunc modifiedSkeysValuesMap newSkeysValues))

    resetTTL key= do
      x <- L.rExpire meshCfg.kvRedis key meshCfg.redisTtl
      pure $ mapLeft MRedisError x

    foldSkeysFunc :: HashMap Text Text -> [(Text, Text)] -> [(Text, Text, Text)]
    foldSkeysFunc _ [] = []
    foldSkeysFunc hm (x : xs) = do
      case HM.lookup (fst x) hm of
        Just val -> (fst x, snd x, val) : foldSkeysFunc hm xs
        Nothing  -> foldSkeysFunc hm xs


    addNewSkey :: ByteString -> Text -> (Text, Text, Text) -> m (MeshResult ())
    addNewSkey pKey tName (k, v1, v2) = do
      let newSKey = fromString . T.unpack $ tName <> "_" <> k <> "_" <> v1
          oldSKey = fromString . T.unpack $ tName <> "_" <> k <> "_" <> v2
      res <- runExceptT $ do
        _ <- ExceptT $ L.sRemB meshCfg.kvRedis oldSKey [pKey]
        _ <- ExceptT $ L.rSadd meshCfg.kvRedis newSKey [pKey]
        ExceptT $ L.rExpireB meshCfg.kvRedis newSKey meshCfg.redisTtl
      case res of
        Right _  -> pure $ Right ()
        Left err -> pure $ Left (MRedisError err)

    getSortedKeyAndValue :: [(Text,Text)] -> (Text, Text)
    getSortedKeyAndValue kvTup = do
      let sortArr = sortBy (compare `on` fst) kvTup
      let (appendedKeys, appendedValues) = applyFPair (T.intercalate "_") $ unzip sortArr
      (appendedKeys, appendedValues)

    isKeyModified :: [(Text, Text)] -> HM.HashMap Text Bool -> Bool
    isKeyModified sKey updValsMap = foldl' (\r k -> HM.member (fst k) updValsMap || r) False sKey

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
  DBConfig BP.Pg ->
  MeshConfig ->
  [Set BP.Postgres table] ->
  Where BP.Postgres table ->
  m (MeshResult [table Identity])
updateAllReturningWithKVConnector dbConf meshCfg setClause whereClause = do
  let isDisabled = meshCfg.kvHardKilled
  t1        <- getCurrentDateInMillis
  res <- if not isDisabled
    then do
      let updVals = jsonKeyValueUpdates setClause
      kvRows <- redisFindAll dbConf meshCfg whereClause
      dbRows <- DBReplay.runWithArtFindALL dbConf whereClause "updateAllReturningWithKVConnector" (findAllSql dbConf whereClause)
      updateKVAndDBResults meshCfg whereClause dbRows kvRows (Just updVals) False dbConf (Just setClause) True Nothing
    else do
      let updateQuery = DB.updateRowsReturningList $ sqlUpdate ! #set setClause ! #where_ whereClause
      res <- DBReplay.runWithArtUpdate dbConf setClause whereClause "updateAllReturningWithKVConnector" $ runQuery dbConf updateQuery
      case res of
        Right x -> do
          when meshCfg.memcacheEnabled $
            mapM_ (pushToInMemConfigStream meshCfg ImcInsert ) x
          return $ Right x
        Left e -> return $ Left e
  t2        <- getCurrentDateInMillis
  diffRes <- whereClauseDiffCheck dbConf whereClause
  let source = if isDisabled then SQL else if (isRecachingEnabled && meshCfg.meshEnabled) then KV else KV_AND_SQL
  logAndIncrementKVMetric True "UPDATE" UPDATE_ALL_RETURNING res (t2 - t1) (modelTableName @table) source diffRes
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
    PII table,
    PIIUpdate be table,
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
          dbRows <- DBReplay.runWithArtFindALL dbConf whereClause "updateAllWithKVConnector" $ findAll dbConf Nothing whereClause
          mapRight (const ()) <$> updateKVAndDBResults meshCfg whereClause dbRows kvRows (Just updVals) True dbConf (Just setClause) True mbval
        else do
          setCl <- maybe (pure $ Right $ setClause) (\(x, y) -> transformSetClause setClause x y) mbval
          case setCl of
            Left err -> return $ Left $ MDBError $ DBError PIIError err
            Right setClause' -> do
              let updateQuery = DB.updateRows $ sqlUpdate ! #set setClause' ! #where_ whereClause
              res <- DBReplay.runWithArtUpdate dbConf setClause whereClause "updateAllWithKVConnector" $ runQuery dbConf updateQuery
              case res of
                Right _ -> do
                  let findAllQuery = DB.findRows (sqlSelect ! #where_ whereClause ! defaults)
                  dbRes <- findAllExtended' dbConf findAllQuery
                  case dbRes of
                    Right dbRows -> do
                      when meshCfg.memcacheEnabled $
                        mapM_ (pushToInMemConfigStream meshCfg ImcInsert) dbRows
                      return . Right $ ()
                    Left e -> return . Left . MDBError $ e
                Left e -> return $ Left e
      t2        <- getCurrentDateInMillis
  
      diffRes <- whereClauseDiffCheck dbConf whereClause
      let source = if isDisabled then SQL else if (isRecachingEnabled && meshCfg.meshEnabled) then KV else KV_AND_SQL
      logAndIncrementKVMetric True "UPDATE" UPDATE_ALL res (t2 - t1) (modelTableName @table) source diffRes
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
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    Show (table Identity), --debugging purpose
    L.MonadFlow m
  ) => MeshConfig -> Where be table -> Either DBError [table Identity] -> MeshResult ([table Identity], [table Identity]) -> Maybe [(Text, A.Value)] -> Bool -> DBConfig beM -> Maybe [Set be table] -> Bool -> Maybe (PIIEncryptionKeyId, PIIEncryptionKey) -> m (MeshResult [table Identity])
updateKVAndDBResults meshCfg whereClause eitherDbRows eitherKvRows mbUpdateVals updateWoReturning dbConf mbSetClause isLive mbKeyConfig = do
  let setClause = fromMaybe [] mbSetClause --Change this logic
      updVals = fromMaybe [] mbUpdateVals
  case (eitherDbRows, eitherKvRows) of
    (Right allDBRows, Right allKVRows) -> do
      let kvLiveRows = fst allKVRows
          kvDeadRows = snd allKVRows
          kvLiveAndDeadRows = kvLiveRows ++ kvDeadRows
          matchedKVLiveRows = findAllMatching whereClause dbConf kvLiveRows
          uniqueDbRows =  getUniqueDBRes dbConf allDBRows kvLiveAndDeadRows
      if isRecachingEnabled && meshCfg.meshEnabled
        then do
          reCacheDBRowsRes <- reCacheDBRows dbConf meshCfg uniqueDbRows
          case reCacheDBRowsRes of
            Left err -> return $ Left $ MRedisError err
            Right _  -> do
              let allRows = matchedKVLiveRows ++ uniqueDbRows
              sequence <$> if isLive
                  then mapM (updateObjectRedis dbConf meshCfg updVals True whereClause mbKeyConfig) allRows 
                  else mapM (deleteObjectRedis dbConf meshCfg True whereClause) allRows
        else do
          sequence_ $ addToETLStream meshCfg (isMySQLConfig dbConf) ETLUpdate <$> uniqueDbRows
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
            setCl <- maybe (pure $ Right $ setClause) (\(encKeyId, encKey) -> transformSetClause setClause encKeyId encKey) mbKeyConfig
            case setCl of
              Left err -> return $ Left $ MDBError $ DBError PIIError err
              Right setClause' -> do
                let updateQuery = DB.updateRows $ sqlUpdate ! #set setClause' ! #where_ whereClause
                res <- DBReplay.runWithArtUpdate dbConf setClause whereClause "updateKVAndDBResults" $ runQuery dbConf updateQuery
                case res of
                    Right _ -> return $ Right []
                    Left e -> return $ Left e
          (True, False) -> do
            setCl <- maybe (pure $ Right setClause) (\(encKeyId, encKey) -> transformSetClause setClause encKeyId encKey) mbKeyConfig
            case setCl of
              Left err -> return $ Left $ MDBError $ DBError PIIError err
              Right setClause' -> do
                let updateQuery = DB.updateRowsReturningList $ sqlUpdate ! #set setClause' ! #where_ whereClause
                res <- DBReplay.runWithArtUpdate dbConf setClause whereClause "updateKVAndDBResults" $ runQuery dbConf updateQuery
                case res of
                    Right x -> return $ Right $ (getUniqueDBRes dbConf x kvLiveAndDeadRows) ++ kvres
                    Left e  -> return $ Left e
          (False, _) -> do
            res <- deleteAllReturning dbConf whereClause
            case res of
                Right x -> return $ Right $ (getUniqueDBRes dbConf x kvLiveAndDeadRows) ++ kvres
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
    PII table,
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
  t1        <- getCurrentDateInMillis
  (source, res) <- if shouldSearchInMemoryCache
    then do
      if (Env.isArtReplayEnabled || isArtRecEnabled)
        then
          DBReplay.searchInMemoryCacheRecRepWrapper "findWithKVConnector" dbConf whereClause
        else 
          searchInMemoryCache dbConf whereClause
    else 
      kvFetch
  t2        <- getCurrentDateInMillis
  diffRes <- whereClauseDiffCheck dbConf whereClause
  logAndIncrementKVMetric False "FIND" FIND res (t2 - t1) (modelTableName @table) source diffRes
  pure res
  where

    kvFetch :: m ((Source, MeshResult (Maybe (table Identity))))
    kvFetch = do
      let isDisabled = meshCfg.kvHardKilled
      if not isDisabled
        then do
          eitherKvRows <- findOneFromRedis dbConf meshCfg whereClause
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
    ToJSON (table Identity),
    PII table,
    L.MonadFlow m) =>
  DBConfig beM ->
  Where be table ->
  [table Identity] ->
  m (Source, MeshResult [table Identity])
findFromDBIfMatchingFails dbConf whereClause kvRows = do
  case findAllMatching whereClause dbConf kvRows of -- For solving partial data case - One row in SQL and one in DB
    [] -> do
      dbRes <- findOneFromDB dbConf whereClause
      case dbRes of
        Right (Just dbRow) -> do
          let isMySQL = isMySQLConfig dbConf
              kvPkeys = map (getLookupKeyByPKey isMySQL) kvRows
          if (getLookupKeyByPKey isMySQL) dbRow `notElem` kvPkeys
            then pure (SQL, Right [dbRow])
            else pure (KV, Right [])
        Left err           -> pure (SQL, Left err)
        _                  -> pure (SQL, Right [])
    xs -> pure (KV, Right xs)

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
  DBConfig beM -> MeshConfig -> Where be table -> m (MeshResult ([table Identity], [table Identity]))
findOneFromRedis dbConf meshCfg whereClause = do
  let keyAndValueCombinations = getFieldsAndValuesFromClause dbConf meshModelTableEntityDescriptor (And whereClause)
      andCombinations = map (uncurry zip . applyFPair (map (T.intercalate "_") . sortOn (Down . length) . nonEmptySubsequences) . unzip . sort) keyAndValueCombinations
      modelName = tableName @(table Identity)
      keyHashMap = keyMap @(table Identity)
      andCombinationsFiltered = mkUniq $ filterPrimaryAndSecondaryKeys keyHashMap <$> andCombinations
      modelWithoutRedisLimit = modelName `elem` tablesWithoutRedisLimit
      secondaryKeysLength = if modelWithoutRedisLimit then 0 else sum $ getSecondaryKeyLength keyHashMap <$> andCombinationsFiltered
  withRedisLimit "REDIS_FIND_ONE_LIMIT_EXCEEDED" (modelTableName @table) secondaryKeysLength $ do
    eitherKeyRes <- mapM (getPrimaryKeyFromFieldsAndValues modelName meshCfg keyHashMap) andCombinationsFiltered
    case foldEither eitherKeyRes of
      Right keyRes -> do
        let lenKeyRes = if modelWithoutRedisLimit then 0 else lengthOfLists keyRes
        withRedisLimit "REDIS_FIND_ONE_LIMIT_EXCEEDED" (modelTableName @table) lenKeyRes $ do
          allRowsRes <- foldEither <$> mapM (getDataFromPKeysRedis meshCfg) (mkUniq keyRes)
          case allRowsRes of
            Right allRowsResPairList -> do
              let (allRowsResLiveListOfList, allRowsResDeadListOfList) = unzip allRowsResPairList
              let total_len = secondaryKeysLength + lenKeyRes
              unless modelWithoutRedisLimit $ incrementRedisCallMetric "REDIS_FIND_ONE" (modelTableName @table) total_len (total_len > redisCallsSoftLimit ) (total_len > redisCallsHardLimit )
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
    ToJSON (table Identity),
    PII table,
    L.MonadFlow m
  ) =>
  DBConfig beM -> Where be table -> m (MeshResult (Maybe (table Identity)))
findOneFromDB dbConf whereClause = DBReplay.runWithArtFind dbConf whereClause "findOneFromDB" (findOne dbConf Nothing whereClause)

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
  let isDisabled = meshCfg.kvHardKilled  
  res <- if not isDisabled
    then do
      kvRes <- redisFindAll dbConf meshCfg whereClause
      case kvRes of
        Right kvRows -> do
          let matchedKVLiveRows = findAllMatching whereClause dbConf (fst kvRows)
              matchedKVDeadRows = snd kvRows
              offset = fromMaybe 0 mbOffset
              shift = length matchedKVLiveRows + length matchedKVDeadRows
              updatedOffset = if offset - shift >= 0 then offset - shift else 0
              findAllQueryUpdated = DB.findRows (sqlSelect'
                ! #where_ whereClause
                ! #orderBy (Just [orderBy])
                ! #limit ((shift +) <$> mbLimit)
                ! #offset (Just updatedOffset) -- Offset is 0 in case mbOffset Nothing
                ! defaults)
          dbRes <- runQuery dbConf findAllQueryUpdated
          case dbRes of
            Left err -> pure $ Left $ MDBError err
            Right [] -> pure $ Right $ applyOptions offset matchedKVLiveRows
            Right dbRows -> do
              let mergedRows = matchedKVLiveRows ++ getUniqueDBRes dbConf dbRows (snd kvRows ++ fst kvRows)
              if isJust mbOffset
                then do
                  let noOfRowsFelledLeftSide = calculateLeftFelledRedisEntries matchedKVLiveRows dbRows
                  pure $ Right $ applyOptions ((if updatedOffset == 0 then offset else shift) - noOfRowsFelledLeftSide) mergedRows
                else pure $ Right $ applyOptions 0 mergedRows
        Left err -> pure $ Left err
    else do
      let findAllQuery = DB.findRows (sqlSelect'
            ! #where_ whereClause
            ! #orderBy (Just [orderBy])
            ! #limit mbLimit
            ! #offset mbOffset
            ! defaults)
      mapLeft MDBError <$> runQuery dbConf findAllQuery
  let source = if not isDisabled then KV_AND_SQL else SQL
  pure res

    where
      applyOptions :: Int -> [table Identity] -> [table Identity]
      applyOptions shift rows = do
        let cmp = case orderBy of
              (Asc col) -> compareCols (fromColumnar' . col . columnize) True
              (Desc col) -> compareCols (fromColumnar' . col . columnize) False
        let resWithoutLimit = (drop shift . sortBy cmp) rows
        maybe resWithoutLimit (`take` resWithoutLimit) mbLimit

      compareCols :: (Ord value) => (table Identity -> value) -> Bool -> table Identity -> table Identity -> Ordering
      compareCols col isAsc r1 r2 = if isAsc then compare (col r1) (col r2) else compare (col r2) (col r1)

      calculateLeftFelledRedisEntries :: [table Identity] -> [table Identity] -> Int
      calculateLeftFelledRedisEntries kvRows dbRows = do
        case orderBy of
          (Asc col) -> do
            let dbMn = maximum $ map (fromColumnar' . col . columnize) dbRows
            length $ filter (\r -> dbMn > fromColumnar' (col $ columnize r)) kvRows
          (Desc col) -> do
            let dbMx = maximum $ map (fromColumnar' . col . columnize) dbRows
            length $ filter (\r -> dbMx < fromColumnar' (col $ columnize r)) kvRows

findAllWithKVConnector :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    Model be table,
    MeshMeta be table,
    KVConnector (table Identity),
    ToJSON (table Identity),
    FromJSON (table Identity),
    PII table,
    Serialize.Serialize (table Identity),
    L.MonadFlow m, B.HasQBuilder be, BeamRunner beM) =>
  DBConfig beM ->
  MeshConfig ->
  Where be table ->
  m (MeshResult [table Identity])
findAllWithKVConnector dbConf meshCfg whereClause = do
  let findAllQuery = DB.findRows (sqlSelect ! #where_ whereClause ! defaults)
  let isDisabled = meshCfg.kvHardKilled  
  t1        <- getCurrentDateInMillis
  res <- if not isDisabled
    then do
      kvRes <- redisFindAll dbConf meshCfg whereClause
      case kvRes of
        Right kvRows -> do
          let matchedKVLiveRows = findAllMatching whereClause dbConf (fst kvRows)
          dbRes <- findAllExtended' dbConf findAllQuery
          case dbRes of
            Right dbRows -> pure $ Right $ matchedKVLiveRows ++ (getUniqueDBRes dbConf dbRows (fst kvRows ++ snd kvRows))
            Left err     -> return $ Left $ MDBError err
        Left err -> return $ Left err 
    else mapLeft MDBError <$> (DBReplay.runWithArtFindALL dbConf whereClause "findAllWithKVConnector" $ findAll dbConf Nothing whereClause)
  t2        <- getCurrentDateInMillis
  diffRes <- whereClauseDiffCheck dbConf whereClause
  let source = if not isDisabled then KV_AND_SQL else SQL
  logAndIncrementKVMetric False "FIND" FIND_ALL res (t2 - t1) (modelTableName @table) source diffRes
  pure res
redisFindAll :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    Model be table,
    MeshMeta be table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m, B.HasQBuilder be, BeamRunner beM) =>
  DBConfig beM ->
  MeshConfig ->
  Where be table ->
  m (MeshResult ([table Identity], [table Identity]))
redisFindAll dbConf meshCfg whereClause = do
  let keyAndValueCombinations = getFieldsAndValuesFromClause dbConf meshModelTableEntityDescriptor (And whereClause)
      andCombinations = map (uncurry zip . applyFPair (map (T.intercalate "_") . sortOn (Down . length) . nonEmptySubsequences) . unzip . sort) keyAndValueCombinations
      modelName = tableName @(table Identity)
      keyHashMap = keyMap @(table Identity)
      andCombinationsFiltered = mkUniq $ filterPrimaryAndSecondaryKeys keyHashMap <$> andCombinations
      modelWithoutRedisLimit = modelName `elem` tablesWithoutRedisLimit
      secondaryKeysLength = if modelWithoutRedisLimit then 0 else sum $ getSecondaryKeyLength keyHashMap <$> andCombinationsFiltered
  withRedisLimit "REDIS_FIND_ALL_LIMIT_EXCEEDED" (modelTableName @table) secondaryKeysLength $ do
    eitherKeyRes <- mapM (getPrimaryKeyFromFieldsAndValues modelName meshCfg keyHashMap) andCombinationsFiltered
    case foldEither eitherKeyRes of
      Right keyRes -> do
        let lenKeyRes = if modelWithoutRedisLimit then 0 else lengthOfLists keyRes
        withRedisLimit "REDIS_FIND_ALL_LIMIT_EXCEEDED" (modelTableName @table) lenKeyRes $ do
          allRowsRes <- foldEither <$> mapM (getDataFromPKeysRedis meshCfg) (mkUniq keyRes)
          case allRowsRes of
              Right allRowsResPairList -> do
                let (allRowsResLiveListOfList, allRowsResDeadListOfList) = unzip allRowsResPairList
                let total_len = secondaryKeysLength + lenKeyRes
                unless modelWithoutRedisLimit $ incrementRedisCallMetric "REDIS_FIND_ALL" modelName total_len (total_len > redisCallsSoftLimit) (total_len>redisCallsHardLimit)
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
    L.MonadFlow m
  ) =>
  DBConfig beM -> MeshConfig -> Bool -> Where be table -> (table Identity) -> m (MeshResult (table Identity))
deleteObjectRedis dbConf meshCfg addPrimaryKeyToWhereClause whereClause obj = do
  time <- fromIntegral <$> L.getCurrentDateInMillis
  let pKeyText  = getLookupKeyByPKey (isMySQLConfig dbConf) obj
      shard     = getShardedHashTag pKeyText
      pKey      = fromString . T.unpack $ pKeyText <> shard
      deleteCmd = if addPrimaryKeyToWhereClause
                    then getDbDeleteCommandJsonWithPrimaryKey dbConf (tableName @(table Identity)) obj whereClause
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

reCacheDBRows :: forall table m beM.
  ( HasCallStack,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    -- Show (table Identity), --debugging purpose
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  MeshConfig ->
  [table Identity] ->
  m (Either KVDBReply [[Bool]])
reCacheDBRows dbConf meshCfg dbRows = do
  reCacheRes <- mapM (\obj -> do
      let isMySQL = isMySQLConfig dbConf
          pKeyText = getLookupKeyByPKey isMySQL obj
          shard = getShardedHashTag pKeyText
          pKey = fromString . T.unpack $ pKeyText <> shard
      res <- mapM (\secIdx -> do -- Recaching Skeys in redis
          let sKey = fromString . T.unpack $ secIdx
          res1 <- L.rSadd meshCfg.kvRedis sKey [pKey]
          case res1 of
            Left err -> return $ Left err
            Right _  -> 
              L.rExpireB meshCfg.kvRedis sKey meshCfg.redisTtl
        ) $ getSecondaryLookupKeys isMySQL obj
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
    PII table,
    PIIUpdate be table,
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
  t1        <- getCurrentDateInMillis
  (source, res) <- if not isDisabled
    then do
      (\delRes -> (fst delRes, mapRight (const ()) (snd delRes))) <$> modifyOneKV dbConf meshCfg whereClause Nothing True False Nothing
    else do
      let deleteQuery = DB.deleteRows $ sqlDelete ! #where_ whereClause
      res <- DBReplay.runWithArtDelete dbConf whereClause "deleteWithKVConnector" $ runQuery dbConf deleteQuery
      (SQL,) <$> case res of
        Left err -> return $ Left err
        Right re -> do
          if meshCfg.memcacheEnabled
            then
              searchInMemoryCache dbConf whereClause >>= (snd >>> \case
                Left e -> return $ Left e
                Right (Just a) -> do
                  (pushToInMemConfigStream meshCfg ImcDelete) a
                  return $ Right re
                Right (Nothing)-> return $ Right re)
            else do
                return $ Right re

  t2        <- getCurrentDateInMillis
  diffRes <- whereClauseDiffCheck dbConf whereClause
  logAndIncrementKVMetric False "DELETE" DELETE_ONE res (t2 - t1) (modelTableName @table) source diffRes
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
  t1        <- getCurrentDateInMillis
  (source, res) <- if not isDisabled
    then do
      modifyOneKV dbConf meshCfg whereClause Nothing False False Nothing
    else do
      res <- DBReplay.runWithArtDelete dbConf whereClause "deleteReturningWithKVConnector" $ deleteAllReturning dbConf whereClause
      (SQL,) <$> case res of
        Left err  -> return $ Left err
        Right []  -> return $ Right Nothing
        Right [r] -> do
          when meshCfg.memcacheEnabled $ pushToInMemConfigStream meshCfg ImcDelete r
          return $ Right (Just r)
        Right rs   -> do
          when meshCfg.memcacheEnabled $ mapM_ (pushToInMemConfigStream meshCfg ImcDelete) rs
          return $ Left $ MUpdateFailed "SQL delete returned more than one record"
  t2        <- getCurrentDateInMillis
  diffRes <- whereClauseDiffCheck dbConf whereClause
  logAndIncrementKVMetric False "DELETE" DELETE_ONE_RETURNING res (t2 - t1) (modelTableName @table) source diffRes
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
    PIIUpdate be table,
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
  t1        <- getCurrentDateInMillis
  res <- if not isDisabled
    then do
      kvResult <- redisFindAll dbConf meshCfg whereClause
      dbRows   <- DBReplay.runWithArtFindALL dbConf whereClause "deleteAllReturningWithKVConnector" $ findAllSql dbConf whereClause
      updateKVAndDBResults meshCfg whereClause dbRows kvResult Nothing False dbConf Nothing False Nothing
    else do
      res <- DBReplay.runWithArtDelete dbConf whereClause "deleteAllReturningWithKVConnector" $ deleteAllReturning dbConf whereClause
      case res of
        Left err -> return $ Left err
        Right re -> do
          when meshCfg.memcacheEnabled $ mapM_ (pushToInMemConfigStream meshCfg ImcDelete) re
          return $ Right re
  t2        <- getCurrentDateInMillis
  diffRes <- whereClauseDiffCheck dbConf whereClause
  let source = if isDisabled then SQL else if isRecachingEnabled then KV else KV_AND_SQL
  logAndIncrementKVMetric False "DELETE" DELETE_ALL_RETURNING res (t2 - t1) (modelTableName @table) source diffRes
  pure res