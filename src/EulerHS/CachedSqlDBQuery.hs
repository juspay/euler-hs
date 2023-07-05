{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-error=unused-top-binds #-}

module EulerHS.CachedSqlDBQuery
  ( create
  , createSql
  , createSqlWoReturing
  , updateOne
  , updateOneWoReturning
  , updateOneSql
  , updateOneSqlWoReturning
  , updateAllSql
  , updateExtended
  , findOne
  , findOneSql
  , findAll
  , findAllSql
  , findAllExtended
  , deleteSql
  , deleteExtended
  , deleteWithReturningPG
  , createMultiSql
  , createMultiSqlWoReturning
  , runQuery
  , countRows
  , SqlReturning(..)
  )
where

import           Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Database.Beam as B
import qualified Database.Beam.MySQL as BM
import qualified Database.Beam.Postgres as BP
import qualified Database.Beam.Sqlite as BS
import qualified Database.Beam.Backend.SQL.BeamExtensions as BExt
import           EulerHS.Extra.Language (getOrInitSqlConn, rGet, rSetB, rDel)
import qualified EulerHS.Framework.Language as L
import           EulerHS.Prelude
import qualified EulerHS.SqlDB.Language as DB
import           EulerHS.SqlDB.Types (BeamRunner, BeamRuntime, DBConfig(..),
                                      DBError (DBError),
                                      DBErrorType (UnexpectedResult), DBResult)
import           Named (defaults, (!))
import           Sequelize (Model, Set, Where, mkExprWithDefault,  mkMultiExprWithDefault,
                            modelTableEntity, sqlSelect, sqlUpdate, sqlDelete, sqlCount)

-- TODO: What KVDB should be used
cacheName :: String
cacheName = "eulerKVDB"

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
      B.HasQBuilder be,
      Model be table,
      ToJSON (table Identity),
      FromJSON (table Identity),
      Show (table Identity),
      L.MonadFlow m
    ) =>
    DBConfig beM ->
    table Identity ->
    Maybe Text ->
    m (Either DBError (table Identity))

  deleteAllReturning ::
    forall (table :: (Type -> Type) -> Type)
          m.
    ( HasCallStack,
      BeamRuntime be beM,
      BeamRunner beM,
      B.HasQBuilder be,
      Model be table,
      ToJSON (table Identity),
      FromJSON (table Identity),
      Show (table Identity),
      L.MonadFlow m
    ) =>
    DBConfig beM ->
    Where be table ->
    m (Either DBError [table Identity])


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
    ToJSON (table Identity),
    Show (table Identity),
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  table Identity ->
  Maybe Text ->
  m (Either DBError (table Identity))
create dbConf value mCacheKey = do
  res <- createSql dbConf value
  case res of
    Right val -> do
      whenJust mCacheKey (`cacheWithKey` val)
      return $ Right val
    Left e -> return $ Left e

createMySQL ::
  forall (table :: (Type -> Type) -> Type)
         m.
  ( HasCallStack,
    Model BM.MySQL table,
    ToJSON (table Identity),
    L.MonadFlow m,
    Show (table Identity)
  ) =>
  DBConfig BM.MySQLM ->
  table Identity ->
  Maybe Text ->
  m (Either DBError (table Identity))
createMySQL dbConf value groupingKey = do
  res <- createSqlMySQL dbConf value
  case res of
    Right val -> do
      whenJust groupingKey (`cacheWithKey` val)
      return $ Right val
    Left err -> return $ Left err

-- | Update an element matching the query to the new value.
--   Cache the value at the given key if the DB update succeeds.
updateOne ::
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    ToJSON (table Identity),
    Show (table Identity),
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  Maybe Text ->
  [Set be table] ->
  Where be table ->
  m (Either DBError (table Identity))
updateOne dbConf (Just cacheKey) newVals whereClause = do
  val <- updateOneSql dbConf newVals whereClause
  whenRight val (\_ -> cacheWithKey cacheKey val)
  return val
updateOne dbConf Nothing value whereClause = updateOneSql dbConf value whereClause

updateOneWoReturning ::
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  Maybe Text ->
  [Set be table] ->
  Where be table ->
  m (Either DBError ())
updateOneWoReturning dbConf (Just _) newVals whereClause = do
  updateOneSqlWoReturning dbConf newVals whereClause
updateOneWoReturning dbConf Nothing value whereClause = updateOneSqlWoReturning dbConf value whereClause

updateOneSqlWoReturning ::
  forall m be beM table.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  [Set be table] ->
  Where be table ->
  m (DBResult ())
updateOneSqlWoReturning dbConf newVals whereClause = do
  let updateQuery = DB.updateRows $ sqlUpdate
        ! #set newVals
        ! #where_ whereClause
  res <- runQuery dbConf updateQuery
  case res of
    Right x -> do
      L.logDebug @Text "updateOneSqlWoReturning" "query executed"
      return $ Right x
   -- Right xs -> do
   --   let message = "DB returned \"" <> show xs <> "\" after update"
   --   L.logError @Text "create" message
   --   return $ Left $ DBError UnexpectedResult message
    Left e -> return $ Left e

updateOneSql ::
  forall m be beM table.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    Show (table Identity),
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  [Set be table] ->
  Where be table ->
  m (DBResult (table Identity))
updateOneSql dbConf newVals whereClause = do
  let updateQuery = DB.updateRowsReturningList $ sqlUpdate
        ! #set newVals
        ! #where_ whereClause
  res <- runQuery dbConf updateQuery
  case res of
    Right [x] -> return $ Right x
    Right xs -> do
      let message = "DB returned \"" <> show xs <> "\" after update"
      L.logError @Text "create" message
      return $ Left $ DBError UnexpectedResult message
    Left e -> return $ Left e

updateAllSql ::
  forall m be beM table.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  [Set be table] ->
  Where be table ->
  m (DBResult ())
updateAllSql dbConf newVals whereClause = do
  let updateQuery = DB.updateRows $ sqlUpdate
        ! #set newVals
        ! #where_ whereClause
  runQuery dbConf updateQuery

-- | Perform an arbitrary 'SqlUpdate'. This will cache if successful.
updateExtended :: (HasCallStack, L.MonadFlow m, BeamRunner beM, BeamRuntime be beM) =>
  DBConfig beM -> Maybe Text -> B.SqlUpdate be table -> m (Either DBError ())
updateExtended dbConf mKey upd = do
  res <- runQuery dbConf . DB.updateRows $ upd
  maybe (pure ()) (`cacheWithKey` res) mKey
  pure res

-- | Find No of Rows
countRows ::
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
countRows = countSql

-- | Find an element matching the query. Only uses the DB if the cache is empty.
--   Caches the result using the given key.
findOne ::
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    ToJSON (table Identity),
    FromJSON (table Identity),
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  Maybe Text ->
  Where be table ->
  m (Either DBError (Maybe (table Identity)))
findOne dbConf (Just cacheKey) whereClause = do
  mRes <- rGet (T.pack cacheName) cacheKey
  case join mRes of
    (Just res) -> return $ Right $ Just res
    Nothing -> do
      mDBRes <- findOneSql dbConf whereClause
      whenRight mDBRes (cacheWithKey cacheKey)
      return mDBRes
findOne dbConf Nothing whereClause = findOneSql dbConf whereClause

-- | Find all elements matching the query. Only uses the DB if the cache is empty.
--   Caches the result using the given key.
--   NOTE: Can't use the same key as findOne, updateOne or create since it's result is a list.
findAll ::
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    ToJSON (table Identity),
    FromJSON (table Identity),
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  Maybe Text ->
  Where be table ->
  m (Either DBError [table Identity])
findAll dbConf (Just cacheKey) whereClause = do
  mRes <- rGet (T.pack cacheName) cacheKey
  case mRes of
    (Just res) -> return $ Right res
    Nothing -> do
      mDBRes <- findAllSql dbConf whereClause
      whenRight mDBRes (cacheWithKey cacheKey)
      return mDBRes
findAll dbConf Nothing whereClause = findAllSql dbConf whereClause

-- | Like 'findAll', but takes an explicit 'SqlSelect'.
findAllExtended :: forall beM be table m .
  (HasCallStack,
   L.MonadFlow m,
   B.FromBackendRow be (table Identity),
   BeamRunner beM,
   BeamRuntime be beM,
   FromJSON (table Identity),
   ToJSON (table Identity)) =>
  DBConfig beM ->
  Maybe Text ->
  B.SqlSelect be (table Identity) ->
  m (Either DBError [table Identity])
findAllExtended dbConf mKey sel = case mKey of
  Nothing -> go
  Just k -> do
    mCached <- rGet (T.pack cacheName) k
    case mCached of
      Just res -> pure . Right $ res
      Nothing -> do
        dbRes <- go
        either (\_ -> pure ()) (cacheWithKey k) dbRes
        pure dbRes
  where
    go :: m (Either DBError [table Identity])
    go = do
      eConn <- getOrInitSqlConn dbConf
      rows <- join <$> traverse (\conn -> L.runDB conn . DB.findRows $ sel) eConn
      case rows of
        Left err -> L.incrementDbMetric err dbConf *> pure rows
        Right _ -> pure rows

deleteExtended :: forall beM be table m .
  (HasCallStack,
   L.MonadFlow m,
   BeamRunner beM,
   BeamRuntime be beM) =>
  DBConfig beM ->
  Maybe Text ->
  B.SqlDelete be table ->
  m (Either DBError ())
deleteExtended dbConf mKey delQuery = case mKey of
  Nothing -> go
  Just k -> do
    rDel (T.pack cacheName) [k] *> go
  where
    go = runQuery dbConf (DB.deleteRows delQuery)

deleteWithReturningPG :: forall table m .
  (HasCallStack,
   B.Beamable table,
   B.FromBackendRow BP.Postgres (table Identity),
   L.MonadFlow m) =>
  DBConfig BP.Pg ->
  Maybe Text ->
  B.SqlDelete BP.Postgres table ->
  m (Either DBError [table Identity])
deleteWithReturningPG dbConf mKey delQuery = case mKey of
  Nothing -> go
  Just k -> do
    rDel (T.pack cacheName) [k] *> go
  where
    go = runQuery dbConf (DB.deleteRowsReturningListPG delQuery)

------------ Helper functions ------------
runQuery ::
  ( HasCallStack,
    BeamRuntime be beM, BeamRunner beM,
    L.MonadFlow m
  ) =>
  DBConfig beM -> DB.SqlDB beM a -> m (Either DBError a)
runQuery dbConf query = do
  conn <- getOrInitSqlConn dbConf
  case conn of
    Right c -> do
      result <- L.runDB c query
      case result of
        Right _ -> pure result
        Left err -> do
          L.incrementDbMetric err dbConf
          pure result
    Left  e -> return $ Left e

runQueryMySQL ::
  ( HasCallStack,
    L.MonadFlow m
  ) =>
  DBConfig BM.MySQLM -> DB.SqlDB BM.MySQLM a -> m (Either DBError a)
runQueryMySQL dbConf query = do
  conn <- getOrInitSqlConn dbConf
  case conn of
    Right c -> do
      rows <- L.runTransaction c query
      case rows of
        Left err -> L.incrementDbMetric err dbConf *> pure rows
        Right _ -> pure rows
    Left  e -> return $ Left e

sqlCreate ::
  forall be table.
  (B.HasQBuilder be, Model be table) =>
  table Identity ->
  B.SqlInsert be table
sqlCreate value = B.insert modelTableEntity (mkExprWithDefault value)

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
  res <- runQuery dbConf $ DB.insertRowsReturningList $ sqlCreate value
  case res of
    Right [val] -> return $ Right val
    Right xs -> do
      let message = "DB returned \"" <> show xs <> "\" after inserting \"" <> show value <> "\""
      L.logError @Text "create" message
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
  res <- runQueryMySQL dbConf $ DB.insertRowReturningMySQL $ sqlCreate value
  case res of
    Right (Just val) -> return $ Right val
    Right Nothing -> do
      let message = "DB returned \"" <> "Nothing" <> "\" after inserting \"" <> show value <> "\""
      L.logError @Text "createSqlMySQL" message
      return $ Left $ DBError UnexpectedResult message -- do we add metric here ?
    Left e -> return $ Left e

createSqlWoReturing ::
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  table Identity ->
  m (Either DBError ())
createSqlWoReturing dbConf value = runQuery dbConf $ DB.insertRows $ sqlCreate value

countSql ::
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
countSql dbConf whereClause = runQuery dbConf findQuery
  where findQuery = DB.countRows (sqlCount ! #where_ whereClause ! defaults)

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
  where findQuery = DB.findRow (sqlSelect ! #where_ whereClause ! defaults)

findAllSql ::
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  Where be table ->
  m (Either DBError [table Identity])
findAllSql dbConf whereClause = do
  let findQuery = DB.findRows (sqlSelect ! #where_ whereClause ! defaults)
  sqlConn <- getOrInitSqlConn dbConf
  rows <- join <$> mapM (`L.runDB` findQuery) sqlConn
  case rows of
    Right _ -> pure rows
    Left err -> L.incrementDbMetric err dbConf *> pure rows

cacheWithKey :: (HasCallStack, ToJSON table, L.MonadFlow m) => Text -> table -> m ()
cacheWithKey key row = do
  -- TODO: Should we log errors here?
  void $ rSetB (T.pack cacheName) (encodeUtf8 key) (BSL.toStrict $ encode row)


sqlMultiCreate ::
  forall be table.
  (BExt.BeamHasInsertOnConflict be, Model be table) =>
  [table Identity] ->
  B.SqlInsert be table
sqlMultiCreate value = B.insert modelTableEntity (mkMultiExprWithDefault value)

sqlMultiCreateIgnoringDuplicates ::
  forall be table.
  (BExt.BeamHasInsertOnConflict be, Model be table) =>
  [table Identity] ->
  B.SqlInsert be table
sqlMultiCreateIgnoringDuplicates value = BExt.insertOnConflict modelTableEntity (mkMultiExprWithDefault value) BExt.anyConflict BExt.onConflictDoNothing

createMultiSql ::
  forall m be beM table.
  ( HasCallStack,
    BExt.BeamHasInsertOnConflict be,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  [table Identity] ->
  Bool ->
  m (Either DBError [table Identity])
createMultiSql dbConf value ignoreDuplicates = runQuery dbConf $ DB.insertRowsReturningList $ bool sqlMultiCreate sqlMultiCreateIgnoringDuplicates ignoreDuplicates value

createMultiSqlWoReturning ::
  ( HasCallStack,
    BExt.BeamHasInsertOnConflict be,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  [table Identity] ->
  Bool ->
  m (Either DBError ())
createMultiSqlWoReturning dbConf value ignoreDuplicates = runQuery dbConf $ DB.insertRows $ bool sqlMultiCreate sqlMultiCreateIgnoringDuplicates ignoreDuplicates value

deleteSql ::
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
  m (Either DBError ())
deleteSql dbConf value = do
  runQuery dbConf $ DB.deleteRows $ (sqlDelete ! #where_ value ! defaults)

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
  res <- runQuery dbConf $ DB.deleteRowsReturningList (sqlDelete ! #where_ value ! defaults)
  return res

deleteAllSqlMySQL ::
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
deleteAllSqlMySQL dbConf value = do
  findRes <- findAllSql dbConf value
  case findRes of
    Left err  -> return $ Left err
    Right res -> do
      delRes  <- runQuery dbConf $ DB.deleteRows $ (sqlDelete ! #where_ value ! defaults)
      case delRes of
        Left err -> return $ Left err
        Right _  -> return $ Right res

deleteAllMySQL ::
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
deleteAllMySQL = deleteAllSqlMySQL

deleteAll ::
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
deleteAll = deleteAllSql
