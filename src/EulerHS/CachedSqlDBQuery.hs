{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-error=unused-top-binds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveAnyClass #-}

module EulerHS.CachedSqlDBQuery
  ( create
  , updateAllSql
  , createSqlWoReturing
  , updateOnePG
  , updateOneWoReturning
  , deleteSql
  , deleteAllMySQL
  , updateOneSql
  , findAllSql
  , countSql
  , updateAllSqlReturningList
  , updateOneSqlWoReturning
  , findOne
  , findOneSql -- doesn't have PII Support avoid using directly
  , findAll
  , findAllExtended
  , findAllExtended'
  , findAllWithSqlSelect
  , findAllWithOptions
  , deleteExtended
  , deleteWithReturningPG
  , createMultiSql
  , createMultiSqlWoReturning
  , countRows
  , findDecryptUtility
  , createSqlWithConn
  , createMultiSqlWithConn
  , updateSqlWithConn
  , deleteSqlWithConn
  , findAllWithConn
  , findOneWithConn
  , sqlMultiCreate
  , sqlMultiCreateIgnoringDuplicates
  , SqlReturning(..)
  , findAllWithSqlSelectWithART
  , countRowsWithART
  )
where

import           EulerHS.PIIEncryption
import qualified Data.Text as T
import qualified Database.Beam as B
import qualified Database.Beam.MySQL as BM
import qualified Database.Beam.Postgres as BP
import qualified Database.Beam.Sqlite as BS
import qualified Database.Beam.Backend.SQL.BeamExtensions as BExt
import           EulerHS.Extra.Language (getOrInitSqlConn)
import           EulerHS.Extra.Redis (rGetWithART, rDelWithART, withArtRecOptionsForKVDB)
import qualified EulerHS.Framework.Language as L
import           EulerHS.Prelude
import qualified EulerHS.SqlDB.Language as DB
import           EulerHS.SqlDB.Types (BeamRunner, BeamRuntime, DBConfig(..),ModelDBConfig(..),
                                      DBError (DBError), DBErrorType (UnrecognizedError),
                                      DBErrorType (UnexpectedResult, PIIError), DBResult, NativeSqlConn)
import           Named (defaults, (!))
import           Sequelize (Model, Set, Where, mkExprWithDefault,  mkMultiExprWithDefault,
                            modelTableEntity, sqlSelect, sqlSelect', sqlUpdate, sqlDelete, sqlCount, modelTableName, OrderBy(..),fromColumnar', columnize, TableType (TRACKER), modelTableType)
import           EulerHS.Logger.Types (ErrorL(..))
import           EulerHS.KVConnector.Types (KVEntry(..), MeshMeta, MeshConfig)
import EulerHS.KVConnector.Utils ( getKVEntryWithDefaultDB, getKVEntryWithTrackerDB)
import           EulerHS.EnvVars
import           EulerHS.SqlDB.Helper
import           Data.Either.Extra (mapRight)
import           EulerHS.KVConnector.PIIUtils
import qualified EulerHS.ART.DBReplay as DBReplay
import           EulerHS.ART.V2.Types
--------------- Core API ---------------

-- | Create a new database entry with the given value.
--   Cache the value if the DB insert succeeds.

class SqlReturning (beM :: Type -> Type) (be :: Type) where
  createReturning ::
    forall (table :: (Type -> Type) -> Type)
           m.
    ( HasCallStack,
      BeamRuntime be beM,
      BeamRunner beM,
      PII table,
      B.HasQBuilder be,
      Model be table,
      ToJSON (table Identity),
      FromJSON (table Identity),
      Show (table Identity),
      L.MonadFlow m
    ) =>
    ModelDBConfig beM ->
    table Identity ->
    Maybe Text ->
    Maybe PIIKeyConfig ->
    Bool ->
    m (Either DBError (table Identity))

  deleteAllReturning ::
    forall (table :: (Type -> Type) -> Type)
          m.
    ( HasCallStack,
      BeamRuntime be beM,
      BeamRunner beM,
      B.HasQBuilder be,
      Model be table,
      PII table,
      ToJSON (table Identity),
      FromJSON (table Identity),
      Show (table Identity),
      L.MonadFlow m
    ) =>
    ModelDBConfig beM ->
    Where be table ->
    MeshConfig  ->
    m (Either DBError [KVEntry table])


instance SqlReturning BM.MySQLM BM.MySQL where
  createReturning = createMySQL
  deleteAllReturning = deleteAllMySQL

instance SqlReturning BP.Pg BP.Postgres where
  createReturning = create
  deleteAllReturning = deleteAll

instance SqlReturning BS.SqliteM BS.Sqlite where
  createReturning = create
  deleteAllReturning = deleteAll

create ::
  forall (be :: Type)
          (beM :: Type -> Type)
          (table :: (Type -> Type) -> Type)
          m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    Model be table,
    PII table,
    ToJSON (table Identity),
    Show (table Identity),
    L.MonadFlow m
  ) =>
  ModelDBConfig beM ->
  table Identity ->
  Maybe Text ->
  Maybe PIIKeyConfig -> 
  Bool ->
  m (Either DBError (table Identity))
create dbConf value mCacheKey mbKeyConfig cleanDBEnabled = do
  updatedValue <- maybe (pure $ Right value) (encryptRow value) mbKeyConfig
  case updatedValue of 
    Left err -> return $ Left $ DBError PIIError err
    Right encResult -> do
      res <- case dbConf of
                WithFallbackDB oldDbCfg newDbCfg ->
                  if cleanDBEnabled then createSql newDbCfg encResult else createSql oldDbCfg encResult
                WithoutFallbackDB oldDbCfg -> createSql oldDbCfg encResult
      withArtRecOptionsForKVDB @table $ setPrimaryKeyUtility value mCacheKey res

createMySQL ::
  forall (table :: (Type -> Type) -> Type)
          m.
  ( HasCallStack,
    Model BM.MySQL table,
    ToJSON (table Identity),
    PII table,
    Show (table Identity),
    L.MonadFlow m
  ) =>
  ModelDBConfig BM.MySQLM ->
  table Identity ->
  Maybe Text ->
  Maybe PIIKeyConfig ->
  Bool ->
  m (Either DBError (table Identity))
createMySQL dbConf value mCacheKey mbKeyConfig cleanDBEnabled = do
  updatedValue <- maybe (pure $ Right value) (encryptRow value) mbKeyConfig
  case updatedValue of 
    Left err -> return $ Left $ DBError PIIError err
    Right mbEncValue -> do
      res <- case dbConf of
                WithFallbackDB oldDbCfg newDbCfg -> if cleanDBEnabled then createSqlMySQL newDbCfg mbEncValue else createSqlMySQL oldDbCfg mbEncValue
                WithoutFallbackDB oldDbCfg -> createSqlMySQL oldDbCfg mbEncValue
      withArtRecOptionsForKVDB @table $ setPrimaryKeyUtility value mCacheKey res      

setPrimaryKeyUtility ::   --- to do can we optimize this?
  ( ToJSON (table Identity),
    PII table,
    HasArtRecOptions,
    L.MonadFlow m
  ) =>
  table Identity ->
  Maybe Text ->
  (Either DBError (table Identity)) ->
  m (Either DBError (table Identity))
setPrimaryKeyUtility value mbCacheKey eitherDbRes = do 
  case eitherDbRes of 
    Right mbEncResult -> do
      let updatedDbRes = setPrimaryKey value mbEncResult
      whenJust mbCacheKey (`cacheWithKey` updatedDbRes)
      return $ Right $ updatedDbRes
    Left dbError -> return $ Left $ dbError

updateOneWoReturning ::
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    PIIUpdate be table,
    B.HasQBuilder be,
    B.FromBackendRow be Int,
    L.MonadFlow m
  ) =>
  ModelDBConfig beM ->
  [Set be table] ->
  Where be table ->
  Maybe PIIKeyConfig ->
  MeshConfig  ->
  m (Either DBError ())
updateOneWoReturning dbConf newVals whereClause maybeKeyConfig meshConfig =
  updateOneSqlWoReturning dbConf newVals whereClause maybeKeyConfig meshConfig

updateOneSqlWoReturning ::
  forall m be beM table.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    PIIUpdate be table,
    B.FromBackendRow be Int,
    B.HasQBuilder be,
    L.MonadFlow m
  ) =>
  ModelDBConfig beM ->
  [Set be table] ->
  Where be table ->
  Maybe PIIKeyConfig ->
  MeshConfig  ->
  m (DBResult ())
updateOneSqlWoReturning dbConf newVals whereClause mbKeyConfig meshConfig = do
  setClause' <- maybe (pure $ Right newVals) (transformSetClause newVals) mbKeyConfig
  case setClause' of
      Left err -> return $ Left $ DBError PIIError err
      Right setClause -> do
        let updateQuery = DB.updateRows . (sqlUpdate
              ! #set setClause
              ! #where_ whereClause)
        let countFindQuery = DB.countRows . (sqlCount ! #where_ whereClause ! defaults)
        res <- updateHelper dbConf updateQuery countFindQuery meshConfig.cleanDBHardKilled
        case res of
          Right x -> do
            L.logDebug @Text "updateOneSqlWoReturning" "query executed"
            return $ Right x
          Left e -> return $ Left e
  where
    updateHelper dbConfig updateQuery countFindQuery isCleanDBHardKilled = do
      case dbConfig of
        WithFallbackDB oldDbCfg newDbCfg -> do
          result <- if isCleanDBHardKilled then pure $ Right 0 else runQuery newDbCfg countFindQuery
          case result of
            Left err -> L.incrementDbMetric err newDbCfg *> getFallBackResponse @table @be meshConfig () (runQuery oldDbCfg updateQuery)
            Right 0 -> getFallBackResponse @table @be meshConfig () $ runQuery oldDbCfg updateQuery
            Right _ -> runQuery newDbCfg updateQuery
        WithoutFallbackDB oldDbCfg -> runQuery oldDbCfg updateQuery

updateOneSql ::
  forall m be beM table.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    Show (table Identity),
    PIIUpdate be table,
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  [Set be table] ->
  Where be table ->
  m (DBResult (table Identity))
updateOneSql dbConf newVals whereClause = do
  let tName = (modelTableName @table)
  eitherPiiKeys <- getEncryptionKey tName
  case eitherPiiKeys of
    Left err -> pure $ Left err
    Right mbKeyConfig -> do
      setClause' <- maybe (pure $ Right newVals) (transformSetClause newVals) mbKeyConfig
      case setClause' of
        Left e -> pure $ Left $ DBError PIIError e
        Right setClause -> do
          let updateQuery = DB.updateRowsReturningList . (sqlUpdate
                ! #set setClause
                ! #where_ whereClause)
          res <- runQuery dbConf updateQuery
          case res of
            Right [x] -> return $ Right x
            Right xs -> do
              let message = "DB returned \"" <> show xs <> "\" after update"
              L.logErrorWithCategory @Text "create" message $ ErrorL Nothing "SQL_ERROR" message
              return $ Left $ DBError UnexpectedResult message
            Left e -> return $ Left e

updateAllSql ::
  forall m be beM table.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    PIIUpdate be table,
    L.MonadFlow m
  ) =>
  ModelDBConfig beM ->
  [Set be table] ->
  Where be table ->
  Maybe PIIKeyConfig ->
  MeshConfig  ->
  m (DBResult ())
updateAllSql dbConf newVals whereClause mbVal meshConfig = do 
  updatedClause <- maybe (pure $ Right newVals) (transformSetClause newVals) mbVal
  case updatedClause of 
    Left e -> pure $ Left $ DBError PIIError e 
    Right val -> do 
      let updateQuery = DB.updateRows . (sqlUpdate
            ! #set val
            ! #where_ whereClause)
      updateAllSql' updateQuery
  where
    updateAllSql' updateQuery = 
      case dbConf of
        WithFallbackDB oldDbCfg newDbCfg -> do
          runExceptT (do
            result1 <-  ExceptT $ if meshConfig.cleanDBHardKilled then pure $ Right () else runQuery newDbCfg updateQuery
            result2 <- ExceptT . getFallBackResponse @table @be meshConfig () $ runQuery oldDbCfg updateQuery
            pure $ result1 <> result2)
        WithoutFallbackDB oldDbCfg -> runQuery oldDbCfg updateQuery
      

updateOnePG :: forall table be beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    ToJSON (table Identity),
    Show (table Identity),
    PIIUpdate be table,
    L.MonadFlow m
  ) =>
  ModelDBConfig beM ->
  Maybe Text ->
  [Set be table] ->
  Where be table ->
  MeshConfig  ->
  m (Either DBError (Maybe (table Identity)))
updateOnePG dbConf mCacheKey newVals whereClause meshConfig = withArtRecOptionsForKVDB @table $ do
  val <- case dbConf of
    WithFallbackDB oldDbCfg newDbCfg -> do
      runExceptT ( do
        result1 <- ExceptT $ if meshConfig.cleanDBHardKilled then pure $ Right Nothing else mapRight Just <$> updateOneSql newDbCfg newVals whereClause
        ExceptT $ maybe (getFallBackResponse @table @be meshConfig Nothing $ mapRight Just <$> updateOneSql oldDbCfg newVals whereClause) (pure . Right . Just) result1
       )
    WithoutFallbackDB oldDbCfg -> mapRight Just <$> updateOneSql oldDbCfg newVals whereClause
  case mCacheKey of
    Just cacheKey -> whenRight val (\_ -> cacheWithKey cacheKey val)
    Nothing -> pure ()
  pure val

updateAllSqlReturningList ::
  forall m be beM table.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    PIIUpdate be table,
    B.HasQBuilder be,
    L.MonadFlow m
  ) =>
  ModelDBConfig beM ->
  [Set be table] ->
  Maybe PIIKeyConfig ->
  Where be table ->
  MeshConfig  ->
  m (Either DBError [KVEntry table])
updateAllSqlReturningList dbConf newVals mbKeyConfig whereClause meshConfig = do 
  setClause <- maybe (pure $ Right newVals) (transformSetClause newVals) mbKeyConfig
  case setClause of
      Left err -> return $ Left $ DBError PIIError err
      Right newVals' -> do
        let updateQuery = DB.updateRowsReturningList . (sqlUpdate
                  ! #set newVals'
                  ! #where_ whereClause)
        updateAllSql' updateQuery
  where
    updateAllSql' updateQuery = 
      case dbConf of
        WithFallbackDB oldDbCfg newDbCfg -> do  
          runExceptT (do
            result1 <-  ExceptT $ if meshConfig.cleanDBHardKilled then pure $ Right [] else mapRight (map getKVEntryWithTrackerDB) <$> runQuery newDbCfg updateQuery
            result2 <- ExceptT . getFallBackResponse @table @be meshConfig [] $ mapRight (map getKVEntryWithDefaultDB) <$> runQuery oldDbCfg updateQuery
            pure $ result1 <> result2)
        WithoutFallbackDB oldDbCfg -> mapRight (map getKVEntryWithDefaultDB) <$> runQuery oldDbCfg updateQuery

-- | Find No of Rows
countRows :: forall table be beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    L.MonadFlow m,
    B.FromBackendRow be Int
  ) =>
  ModelDBConfig beM ->
  Where be table ->
  MeshConfig ->
  m (Either DBError Int)
countRows dbConf whClause meshConfig = 
  case dbConf of
    WithFallbackDB oldDbCfg newDbCfg -> do
      runExceptT (do
              result1 <-  ExceptT $ if meshConfig.cleanDBHardKilled  then pure $ Right 0 else countSqlImpl newDbCfg whClause
              result2 <- ExceptT . getFallBackResponse @table @be meshConfig 0 $ countSqlImpl oldDbCfg whClause
              pure $ result1 + result2)
    WithoutFallbackDB oldDbCfg -> countSqlImpl oldDbCfg whClause

-- | Find No of Rows
countRowsWithART ::
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    L.MonadFlow m,
    MeshMeta be table,
    B.FromBackendRow be Int
  ) =>
  ModelDBConfig beM ->
  Where be table ->
  MeshConfig ->
  m (Either DBError Int)
countRowsWithART dbConf whClause meshConfig = DBReplay.runWithArtCount dbConf whClause "countAll" $ countRows dbConf whClause meshConfig

-- | Find an element matching the query. Only uses the DB if the cache is empty.
--   Caches the result using the given key.

findOne :: forall table be beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    ToJSON (table Identity),
    FromJSON (table Identity),
    PII table,
    L.MonadFlow m
  ) =>
  ModelDBConfig beM ->
  Maybe Text ->
  Where be table ->
  MeshConfig  ->
  m (Either DBError (Maybe (KVEntry table)))
findOne dbConf mCacheKey whereClause meshConfig = withArtRecOptionsForKVDB @table $ do
  case mCacheKey of
    Just cacheKey -> do
      mRes <- rGetWithART (T.pack cacheName) cacheKey
      case join mRes of
        (Just res) -> return $ Right $ Just res
        Nothing -> lookupInDB 
    Nothing -> lookupInDB
  where 
    lookupInDB = do
      case dbConf of
        WithFallbackDB oldDbCfg newDbCfg -> do
          res <- if meshConfig.cleanDBHardKilled then pure $ Right Nothing else mapRight (fmap getKVEntryWithTrackerDB) <$> getDecryptedRow newDbCfg 
          case res of
            Right Nothing -> getFallBackResponse @table @be meshConfig Nothing (mapRight (fmap getKVEntryWithDefaultDB) <$> getDecryptedRow oldDbCfg)
            _             -> pure res
        WithoutFallbackDB oldDbCfg -> mapRight (fmap getKVEntryWithDefaultDB) <$> getDecryptedRow oldDbCfg 

    getDecryptedRow dbConf' = withArtRecOptionsForKVDB @table $ do
      mDBRes <- findOneSql dbConf' whereClause
      findDecryptUtility mDBRes mCacheKey

-- | Find all elements matching the query. Only uses the DB if the cache is empty.
--   Caches the result using the given key.
--   NOTE: Can't use the same key as findOne, updateOne or create since it's result is a list.

findAll :: forall table be beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    ToJSON (table Identity),
    PII table,
    FromJSON (table Identity),
    L.MonadFlow m
  ) =>
  ModelDBConfig beM ->
  Maybe Text ->
  Where be table -> 
  Bool ->
  m (Either DBError [KVEntry table])
findAll dbConf mCacheKey whereClause isCleanDBHardKilled = withArtRecOptionsForKVDB @table $ do
  case mCacheKey of
    Just cacheKey -> do
      mRes <- rGetWithART (T.pack cacheName) cacheKey
      case join mRes of
        (Just res) -> return $ Right res
        Nothing -> findAllSql dbConf whereClause isCleanDBHardKilled
    Nothing -> findAllSql dbConf whereClause isCleanDBHardKilled
      

findAllWithOptions :: forall beM be table m .
  (HasCallStack,
   L.MonadFlow m,
   BeamRunner beM,
   BeamRuntime be beM,
   Model be table,
   PII table,
   FromJSON (table Identity),
   B.HasQBuilder be,
   ToJSON (table Identity)) =>
  ModelDBConfig beM ->
  Bool -> 
  Maybe Text ->
  Where be table ->
  OrderBy table ->
  Maybe Int ->
  m (Either DBError [KVEntry table])
findAllWithOptions dbConf cleanDBHardKilled mKey whereClause orderBy mbLimit = withArtRecOptionsForKVDB @table $ do
  let findAllQuerySel = sqlSelect'
            ! #where_ whereClause
            ! #orderBy (Just [orderBy])
            ! #limit mbLimit
            ! defaults
  case dbConf of
    (WithFallbackDB _ _) -> do
      mapRight applyOptions <$> findAllWithSqlSelect dbConf findAllQuerySel cleanDBHardKilled mKey
    (WithoutFallbackDB oldDbCfg) -> 
      mapRight (map getKVEntryWithDefaultDB) <$> findAllExtended oldDbCfg mKey findAllQuerySel
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

findAllWithSqlSelect :: forall beM be table m .
  (HasCallStack,
  L.MonadFlow m,
  BeamRunner beM,
  BeamRuntime be beM,
  Model be table,
  PII table,
  FromJSON (table Identity),
  ToJSON (table Identity)) =>
  ModelDBConfig beM ->
  (Text -> B.SqlSelect be (table Identity)) ->
  Bool -> 
  Maybe Text ->
  m (Either DBError [KVEntry table])
findAllWithSqlSelect dbConf findSqlSelect cleanDBHardKilled mKey = withArtRecOptionsForKVDB @table $ do
  case dbConf of 
    WithFallbackDB oldDbCfg newDbCfg -> do
      if forkFLowEnabled then do
          mDB1Awaitable <- L.forkAwaitable "" $ if cleanDBHardKilled 
            then pure $ Right []
            else mapRight (map getKVEntryWithTrackerDB) <$> findAllExtended newDbCfg mKey findSqlSelect
          mDB2Awaitable <- L.forkAwaitable "" $ 
            mapRight (map getKVEntryWithDefaultDB) <$> findAllExtended oldDbCfg mKey findSqlSelect
          runExceptT ( do
            result1 <- ExceptT $ either (Left . DBError UnrecognizedError . show) id <$> L.await Nothing mDB1Awaitable
            result2 <- ExceptT $ either (Left . DBError UnrecognizedError . show) id <$> L.await Nothing mDB2Awaitable
            pure (result1 <> result2))
        else do
          runExceptT ( do
            result1 <- ExceptT $ if cleanDBHardKilled then pure $ Right [] else mapRight (map getKVEntryWithTrackerDB) <$> findAllExtended newDbCfg mKey findSqlSelect
            result2 <- ExceptT $ mapRight (map getKVEntryWithDefaultDB) <$> findAllExtended oldDbCfg mKey findSqlSelect
            pure (result1 <> result2))
    WithoutFallbackDB oldDbCfg -> mapRight (map getKVEntryWithDefaultDB) <$> findAllExtended oldDbCfg mKey findSqlSelect


findAllWithSqlSelectWithART :: forall beM be table m .
  (HasCallStack,
  L.MonadFlow m,
  BeamRunner beM,
  BeamRuntime be beM,
  Model be table,
  PII table,
  FromJSON (table Identity),
  ToJSON (table Identity)) =>
  ModelDBConfig beM ->
  (Text -> B.SqlSelect be (table Identity)) ->
  Bool -> 
  Maybe Text ->
  ShouldRecordJoin ->
  m (Either DBError [KVEntry table])
findAllWithSqlSelectWithART dbConf findSqlSelect cleanDBHardKilled mKey recordJoin = DBReplay.runWithArtFindSqlSelectWithJoin dbConf findSqlSelect recordJoin "findAllWithSqlSelect" $ findAllWithSqlSelect dbConf findSqlSelect cleanDBHardKilled mKey


-- | Like 'findAll', but takes an explicit 'SqlSelect'.
findAllExtended :: forall beM be table m .
  (HasCallStack,
   HasArtRecOptions,
   L.MonadFlow m,
   B.FromBackendRow be (table Identity),
   BeamRunner beM,
   BeamRuntime be beM,
   PII table,
   FromJSON (table Identity),
   ToJSON (table Identity)) =>
  DBConfig beM ->
  Maybe Text ->
  (Text -> B.SqlSelect be (table Identity))->
  m (Either DBError [table Identity])
findAllExtended dbConf mKey sel = case mKey of
  Nothing -> do 
    res <- go
    findAllDecryptUtility res Nothing
  Just k -> do
    mCached <- rGetWithART (T.pack cacheName) k
    case mCached of
      Just res -> pure . Right $ res
      Nothing -> do
        dbRes <- go
        findAllDecryptUtility dbRes (Just k)
  where
    go :: m (Either DBError [table Identity])
    go = runQuery dbConf $ DB.findRows . sel

findAllExtended' :: forall beM be table m .
  (HasCallStack,
   HasArtRecOptions,
   L.MonadFlow m,
   BeamRunner beM,
   BeamRuntime be beM,
   PII table,
   ToJSON (table Identity)) =>
  DBConfig beM ->
  DB.SqlDB beM [(table Identity)] -> 
  m (Either DBError [table Identity])
findAllExtended' dbConf sel = do
    dbRes <- go
    findAllDecryptUtility dbRes Nothing
  where
    go :: m (Either DBError [table Identity])
    go = do
      eConn <- getOrInitSqlConn dbConf
      rows <- join <$> traverse (\conn -> L.runDB conn $ sel) eConn
      case rows of
        Left err -> L.incrementDbMetric err dbConf *> pure rows
        Right _ -> pure rows
 
---- Note -  The below function does not support limit -------------

deleteExtended :: forall beM be table m .
  (HasCallStack,
   HasArtRecOptions,
    L.MonadFlow m,
    BeamRunner beM,
    BeamRuntime be beM) =>
  ModelDBConfig beM ->
  Maybe Text ->
  (Text -> B.SqlDelete be table) ->
  Bool ->
  m (Either DBError ())
deleteExtended dbConf mKey delQuery isCleanDBHardKilled = do
  case dbConf of
    WithFallbackDB oldDbCfg newDbCfg ->  do
      runExceptT (do
        _ <- ExceptT $ if isCleanDBHardKilled then pure $ Right () else deleteExtendedImpl newDbCfg mKey delQuery
        _ <- ExceptT $ deleteExtendedImpl oldDbCfg mKey delQuery
        pure ()
        )
    WithoutFallbackDB oldDbCfg -> deleteExtendedImpl oldDbCfg mKey delQuery
  
deleteExtendedImpl :: forall beM be table m .
  (HasCallStack,
   HasArtRecOptions,
   L.MonadFlow m,
   BeamRunner beM,
   BeamRuntime be beM) =>
  DBConfig beM ->
  Maybe Text ->
  (Text -> B.SqlDelete be table) ->
  m (Either DBError ())
deleteExtendedImpl dbConf mKey delQuery = case mKey of
  Nothing -> go
  Just k -> do
    rDelWithART (T.pack cacheName) [k] *> go
  where
    go = runQuery dbConf (DB.deleteRows . delQuery)


deleteWithReturningPG :: forall table m .
  (HasCallStack,
   HasArtRecOptions,
   B.Beamable table,
   B.FromBackendRow BP.Postgres (table Identity),
   L.MonadFlow m) =>
  DBConfig BP.Pg ->
  Maybe Text ->
  (Text -> B.SqlDelete BP.Postgres table) ->
  m (Either DBError [table Identity])
deleteWithReturningPG dbConf mKey delQuery = case mKey of
  Nothing -> go
  Just k -> do
    rDelWithART (T.pack cacheName) [k] *> go
  where
    go = runQuery dbConf (DB.deleteRowsReturningListPG . delQuery)

sqlCreate ::
  forall be table.
  (B.HasQBuilder be, Model be table) =>
  table Identity ->
  Text ->
  B.SqlInsert be table
sqlCreate value schemaName = B.insert (modelTableEntity schemaName) (mkExprWithDefault value)

createSql ::
  forall m be beM table.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    Model be table,
    Show (table Identity),
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  table Identity ->
  m (Either DBError (table Identity))
createSql dbConf value = do
  res <- runQuery dbConf $ DB.insertRowsReturningList . sqlCreate value
  case res of
    Right [val] -> return $ Right val
    Right xs -> do
      let message = "DB returned \"" <> show xs <> "\" after inserting \"" <> show value <> "\""
      L.logErrorWithCategory @Text "create" message $ ErrorL Nothing "SQL_ERROR" message
      return $ Left $ DBError UnexpectedResult message
    Left e -> return $ Left e

createSqlMySQL ::
  forall m  table.
  ( HasCallStack,
    Model BM.MySQL table,
    Show (table Identity),
    L.MonadFlow m
  ) =>
  DBConfig BM.MySQLM ->
  table Identity ->
  m (Either DBError (table Identity))
createSqlMySQL dbConf value = do
  res <- runQueryMySQL dbConf $ DB.insertRowReturningMySQL . (sqlCreate value)
  case res of
    Right (Just val) -> return $ Right val
    Right Nothing -> do
      let message = "DB returned \"" <> "Nothing" <> "\" after inserting \"" <> show value <> "\""
      L.logErrorWithCategory @Text "createSqlMySQL" message $ ErrorL Nothing "SQL_ERROR" message
      return $ Left $ DBError UnexpectedResult message -- do we add metric here ?
    Left e -> return $ Left e

createSqlWoReturing ::
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    PII table,
    L.MonadFlow m
  ) =>
  ModelDBConfig beM ->
  table Identity ->
  Maybe PIIKeyConfig -> 
  Bool ->
  m (Either DBError ())
createSqlWoReturing dbConf value mkeyConfig isCleanDbEnabled = do 
  updatedValue <- case mkeyConfig of
    Nothing -> pure $ Right value
    Just keyConfig -> encryptRow value keyConfig
  case updatedValue of 
    Left err -> return $ Left $ DBError PIIError err
    Right encResult -> runQuery dbConfDecider $ DB.insertRows . sqlCreate encResult
  where
    dbConfDecider = case dbConf of
      WithFallbackDB oldDbCfg newDbCfg -> if isCleanDbEnabled then newDbCfg else oldDbCfg
      WithoutFallbackDB oldDbCfg -> oldDbCfg

countSql :: forall table be beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    L.MonadFlow m,
    B.FromBackendRow be Int
  ) =>
  ModelDBConfig beM ->
  Where be table ->
  MeshConfig  -> 
  m (Either DBError Int)
countSql dbConf whereClause meshConfig = do
  case dbConf of
    WithFallbackDB oldDbCfg newDbCfg -> do
      runExceptT (do
              result1 <-  ExceptT $ if meshConfig.cleanDBHardKilled then pure $ Right 0 else runQuery newDbCfg findQuery
              result2 <- ExceptT . getFallBackResponse @table @be meshConfig 0 $ runQuery oldDbCfg findQuery
              pure $ result1 + result2)
    WithoutFallbackDB oldDbCfg -> runQuery oldDbCfg findQuery
  where findQuery = DB.countRows . (sqlCount ! #where_ whereClause ! defaults)

countSqlImpl ::
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    L.MonadFlow m,
    B.FromBackendRow be Int
  ) =>
  DBConfig beM ->
  Where be table ->
  m (Either DBError Int)
countSqlImpl dbConf whereClause = runQuery dbConf findQuery
  where findQuery = DB.countRows . (sqlCount ! #where_ whereClause ! defaults)


findOneSql ::
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  Where be table ->
  m (Either DBError (Maybe (table Identity)))
findOneSql dbConf whereClause = runQuery dbConf findQuery
  where findQuery = DB.findRow . (sqlSelect ! #where_ whereClause ! defaults)      

findAllSql ::
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    PII table,
    FromJSON (table Identity),
    ToJSON (table Identity),
    L.MonadFlow m
  ) =>
  ModelDBConfig beM ->
  Where be table ->
  Bool ->
  m (Either DBError [KVEntry table])
findAllSql dbConf whereClause isCleanDBHardKilled = do
  let findAllQuerySel = sqlSelect'
            ! #where_ whereClause
            ! defaults
  findAllWithSqlSelect dbConf findAllQuerySel isCleanDBHardKilled Nothing

sqlMultiCreate ::
  forall be table.
  (BExt.BeamHasInsertOnConflict be, Model be table) =>
  [table Identity] ->
  Text ->
  B.SqlInsert be table
sqlMultiCreate value schemaName = B.insert (modelTableEntity schemaName) (mkMultiExprWithDefault value)

sqlMultiCreateIgnoringDuplicates ::
  forall be table.
  (BExt.BeamHasInsertOnConflict be, Model be table) =>
  [table Identity] ->
  Text ->
  B.SqlInsert be table
sqlMultiCreateIgnoringDuplicates value schemaName = BExt.insertOnConflict (modelTableEntity schemaName) (mkMultiExprWithDefault value) BExt.anyConflict BExt.onConflictDoNothing

createMultiSql ::
  forall m be beM table.
  ( HasCallStack,
    BExt.BeamHasInsertOnConflict be,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    PII table,
    L.MonadFlow m
  ) =>
  ModelDBConfig beM ->
  [table Identity] ->
  Bool ->
  [Maybe PIIKeyConfig] ->
  Bool -> 
  m (Either DBError [table Identity])
createMultiSql dbConf value ignoreDuplicates arrKeys isCleanDbEnabled = do 
  maybeUpdatedValue <- decryptOrEncryptAllUtility value arrKeys True
  case maybeUpdatedValue of 
    Left err -> return $ Left $ err 
    Right updatedValue -> do 
      res <- runQuery dbConfDecider $ DB.insertRowsReturningList . bool sqlMultiCreate sqlMultiCreateIgnoringDuplicates ignoreDuplicates updatedValue
      setPrimaryKeyAllUtility res value
  where
    dbConfDecider = do
      case dbConf of
        WithFallbackDB oldDbCfg newDbCfg -> if isCleanDbEnabled then newDbCfg else oldDbCfg
        WithoutFallbackDB oldDbCfg -> oldDbCfg

createMultiSqlWoReturning ::
  ( HasCallStack,
    BExt.BeamHasInsertOnConflict be,
    BeamRuntime be beM,
    BeamRunner beM,
    PII table,
    Model be table,
    L.MonadFlow m
  ) =>
  ModelDBConfig beM ->
  [table Identity] ->
  Bool ->
  [Maybe PIIKeyConfig] ->
  Bool ->
  m (Either DBError ())
createMultiSqlWoReturning dbConf value ignoreDuplicates arrKeys isCleanDbEnabled = do
  maybeUpdatedValue <- decryptOrEncryptAllUtility value arrKeys True
  case maybeUpdatedValue of 
    Left err -> return $ Left $ err 
    Right updatedValue -> runQuery dbConfDecider $ DB.insertRows . bool sqlMultiCreate sqlMultiCreateIgnoringDuplicates ignoreDuplicates updatedValue
  where
    dbConfDecider =
      case dbConf of
        WithFallbackDB oldDbCfg newDbCfg -> if isCleanDbEnabled then newDbCfg else oldDbCfg
        WithoutFallbackDB oldDbCfg -> oldDbCfg

setPrimaryKeyAllUtility ::  
  ( PII table,
    L.MonadFlow m
  ) =>
  (Either DBError [table Identity]) ->
  [table Identity] ->
  m (Either DBError [table Identity])
setPrimaryKeyAllUtility eitherDbRes unencrytpedRows = do 
  case eitherDbRes of 
    Right encResult ->return $ Right $ map (\(plainTextRow, dbRow) -> setPrimaryKey plainTextRow dbRow) $ zip unencrytpedRows encResult
    Left dbError -> return $ Left $ dbError

deleteSql ::
  forall m be beM table.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    Model be table,
    L.MonadFlow m
  ) =>
  ModelDBConfig beM ->
  Where be table ->
  MeshConfig  ->
  m (Either DBError ())
deleteSql dbConf value meshConfig = do
  case dbConf of
    WithFallbackDB oldDbCfg newDbCfg -> do
      if meshConfig.cleanDBHardKilled then pure () else void $ runQuery newDbCfg $ DB.deleteRows . (sqlDelete ! #where_ value ! defaults)
      getFallBackResponse @table @be meshConfig () $ runQuery oldDbCfg $ DB.deleteRows . (sqlDelete ! #where_ value ! defaults)
    WithoutFallbackDB oldDbCfg -> runQuery oldDbCfg $ DB.deleteRows . (sqlDelete ! #where_ value ! defaults)

deleteAllSql ::
  forall m be beM table.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    Model be table,
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  Where be table ->
  m (Either DBError [table Identity])
deleteAllSql dbConf value = do
  res <- runQuery dbConf $ DB.deleteRowsReturningList . (sqlDelete ! #where_ value ! defaults)
  return res

deleteAllSqlMySQL ::
  forall m be beM table.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    Model be table,
    PII table,
    FromJSON (table Identity),
    ToJSON (table Identity),
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  Where be table ->
  m (Either DBError [table Identity])
deleteAllSqlMySQL dbConf value = withArtRecOptionsForKVDB @table $ do
  let findAllQuerySel = sqlSelect'
            ! #where_ value
            ! defaults
  findRes <- findAllExtended dbConf Nothing findAllQuerySel --findAllSql dbConf value
  case findRes of
    Left err  -> return $ Left err
    Right res -> do
      delRes  <- runQuery dbConf $ DB.deleteRows . (sqlDelete ! #where_ value ! defaults)
      case delRes of
        Left err -> return $ Left err
        Right _  -> return $ Right res

deleteAllMySQL ::
  forall m be beM table.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    PII table,
    Model be table,
    FromJSON (table Identity),
    ToJSON (table Identity),
    L.MonadFlow m
  ) =>
  ModelDBConfig beM ->
  Where be table ->
  MeshConfig  ->
  m (Either DBError [KVEntry table])
deleteAllMySQL dbConf whClause meshConfig = 
  case dbConf of
    WithFallbackDB oldDbCfg newDbCfg -> do
      runExceptT (do
        result1 <-  ExceptT $ if meshConfig.cleanDBHardKilled then pure $ Right [] else mapRight (map getKVEntryWithTrackerDB) <$> deleteAllSqlMySQL newDbCfg whClause
        result2 <- ExceptT . getFallBackResponse @table @be meshConfig [] $ mapRight (map getKVEntryWithDefaultDB) <$> deleteAllSqlMySQL oldDbCfg whClause
        pure $ result1 <> result2)
    WithoutFallbackDB oldDbCfg ->  mapRight (map getKVEntryWithDefaultDB) <$> deleteAllSqlMySQL oldDbCfg whClause

deleteAll ::
  forall m be beM table.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    Model be table,
    L.MonadFlow m
  ) =>
  ModelDBConfig beM ->
  Where be table ->
  MeshConfig ->
  m (Either DBError [KVEntry table])
deleteAll dbConf whClause meshConfig =
  case dbConf of
    WithFallbackDB oldDbCfg newDbCfg ->
      runExceptT (do
        result1 <-  ExceptT $ if meshConfig.cleanDBHardKilled then pure $ Right [] else mapRight (map getKVEntryWithTrackerDB) <$> deleteAllSql newDbCfg whClause
        result2 <- ExceptT . getFallBackResponse @table @be meshConfig [] $ mapRight (map getKVEntryWithDefaultDB) <$> deleteAllSql oldDbCfg whClause
        pure $ result1 <> result2)
    WithoutFallbackDB oldDbCfg -> mapRight (map getKVEntryWithDefaultDB) <$> deleteAllSql oldDbCfg whClause 

createSqlWithConn ::
  forall m be beM table.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    Model be table,
    Show (table Identity),
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  NativeSqlConn ->
  table Identity ->
  m (Either DBError (table Identity))
createSqlWithConn dbConf conn value = do
  res <- runQueryWithConn dbConf conn $ DB.insertRowsReturningList . sqlCreate value
  case res of
    Right [val] -> return $ Right val
    Right xs -> do
      let message = "DB returned \"" <> show xs <> "\" after inserting \"" <> show value <> "\""
      L.logErrorWithCategory @Text "create" message $ ErrorL Nothing "SQL_ERROR" message
      return $ Left $ DBError UnexpectedResult message
    Left e -> return $ Left e

createMultiSqlWithConn ::
  forall m be beM table.
  ( HasCallStack,
    BExt.BeamHasInsertOnConflict be,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  NativeSqlConn ->
  [table Identity] ->
  Bool ->
  m (Either DBError [table Identity])
createMultiSqlWithConn dbConf conn value ignoreDuplicates = runQueryWithConn dbConf conn $ DB.insertRowsReturningList . bool sqlMultiCreate sqlMultiCreateIgnoringDuplicates ignoreDuplicates value

updateSqlWithConn ::
  forall m be beM table.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    Model be table,
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  NativeSqlConn ->
  [Set be table] ->
  Where be table ->
  m (Either DBError ())
updateSqlWithConn dbConf conn newVals whereClause = runQueryWithConn dbConf conn $ DB.updateRows . (sqlUpdate ! #set newVals ! #where_ whereClause)

deleteSqlWithConn ::
  forall m be beM table.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    Model be table,
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  NativeSqlConn ->
  Where be table ->
  m (Either DBError [table Identity])
deleteSqlWithConn dbConf conn value = runQueryWithConn dbConf conn $ DB.deleteRowsReturningList . (sqlDelete ! #where_ value ! defaults)

findOneWithConn ::
  forall m be beM table.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    Model be table,
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  NativeSqlConn ->
  Where be table ->
  m (Either DBError (Maybe (table Identity)))
findOneWithConn dbConf conn whereClause = runQueryWithConn dbConf conn $ DB.findRow . (sqlSelect ! #where_ whereClause ! defaults)
-- TO-DO

findAllWithConn ::
  forall m be beM table.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    Model be table,
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  NativeSqlConn ->
  Where be table ->
  m (Either DBError [table Identity])
findAllWithConn dbConf conn whereClause = runQueryWithConn dbConf conn $ DB.findRows . (sqlSelect ! #where_ whereClause ! defaults)


getFallBackResponse ::
  forall table be m a.
  ( Model be table,
    L.MonadFlow m
  ) =>
  MeshConfig ->
  a ->
  m (Either DBError a) ->
  m (Either DBError a)
getFallBackResponse meshConfig emptyFallBackResponse fallBack = if meshConfig.disableFallBackToCommonDb && (modelTableType @table) == (Just TRACKER) then pure $ Right emptyFallBackResponse else fallBack