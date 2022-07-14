{-# LANGUAGE OverloadedStrings #-}

module EulerHS.CachedSqlDBQuery
  ( create
  , createSql
  , updateOne
  , updateOneWoReturning
  , updateOneSql
  , updateOneSqlWoReturning
  , updateExtended
  , findOne
  , findOneSql
  , findAll
  , findAllSql
  , findAllExtended
  , SqlReturning(..)
  )
where

import           Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Database.Beam as B
import qualified Database.Beam.MySQL as BM
import qualified Database.Beam.Postgres as BP
import qualified Database.Beam.Sqlite as BS
import qualified Data.Text as T
import qualified EulerHS.Core.SqlDB.Language as DB
import           EulerHS.Core.Types.DB
import           EulerHS.Core.Types.Serializable
import           EulerHS.Extra.Language (getOrInitSqlConn, rGet, rSetB)
import qualified EulerHS.Framework.Language as L
import           EulerHS.Prelude
import           Named (defaults, (!))
import           Sequelize

-- TODO: What KVDB should be used
cacheName :: String
cacheName = "eulerKVDB"

--------------- Core API ---------------

-- | Create a new database entry with the given value.
--   Cache the value if the DB insert succeeds.

class SqlReturning (beM :: Type -> Type) (be :: Type) where
  createReturning ::
    forall (table :: (Type -> Type) -> Type)
           (m :: Type -> Type) .
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

instance SqlReturning BM.MySQLM BM.MySQL where
  createReturning = createMySQL

instance SqlReturning BP.Pg BP.Postgres where
  createReturning = create

instance SqlReturning BS.SqliteM BS.Sqlite where
  createReturning = create


create ::
  forall (be :: Type)
         (beM :: Type -> Type)
         (table :: (Type -> Type) -> Type)
         (m :: Type -> Type) .
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
create dbConf value mCacheKey = do
  res <- createSql dbConf value
  case res of
    Right val -> do
      whenJust mCacheKey (`cacheWithKey` val)
      return $ Right val
    Left e -> return $ Left e

createMySQL ::
  forall (table :: (Type -> Type) -> Type)
         (m :: Type -> Type) .
  ( HasCallStack,
    Model BM.MySQL table,
    ToJSON (table Identity),
    FromJSON (table Identity),
    Show (table Identity),
    L.MonadFlow m
  ) =>
  DBConfig BM.MySQLM ->
  table Identity ->
  Maybe Text ->
  m (Either DBError (table Identity))
createMySQL dbConf value mCacheKey = do
  res <- createSqlMySQL dbConf value
  case res of
    Right val -> do
      whenJust mCacheKey (`cacheWithKey` val)
      return $ Right val
    Left e -> return $ Left e

-- | Update an element matching the query to the new value.
--   Cache the value at the given key if the DB update succeeds.
updateOne ::
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    ToJSON (table Identity),
    FromJSON (table Identity),
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
    ToJSON (table Identity),
    FromJSON (table Identity),
    Show (table Identity),
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  Maybe Text ->
  [Set be table] ->
  Where be table ->
  m (Either DBError ())
updateOneWoReturning dbConf (Just _) newVals whereClause = do
  val <- updateOneSqlWoReturning dbConf newVals whereClause
 -- whenRight val (\_ -> cacheWithKey cacheKey val)
  return val
updateOneWoReturning dbConf Nothing value whereClause = updateOneSqlWoReturning dbConf value whereClause

updateOneSqlWoReturning ::
  forall m be beM table.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    FromJSON (table Identity),
    ToJSON (table Identity),
    Show (table Identity),
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
    FromJSON (table Identity),
    ToJSON (table Identity),
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

-- | Perform an arbitrary 'SqlUpdate'. This will cache if successful.
updateExtended :: (HasCallStack, L.MonadFlow m, BeamRunner beM, BeamRuntime be beM) =>
  DBConfig beM -> Maybe Text -> B.SqlUpdate be table -> m (Either DBError ())
updateExtended dbConf mKey upd = do
  res <- runQuery dbConf . DB.updateRows $ upd
  maybe (pure ()) (`cacheWithKey` res) mKey
  pure res

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
      join <$> traverse (\conn -> L.runDB conn . DB.findRows $ sel) eConn

------------ Helper functions ------------
runQuery ::
  ( HasCallStack,
    BeamRuntime be beM, BeamRunner beM,
    JSONEx a,
    L.MonadFlow m
  ) =>
  DBConfig beM -> DB.SqlDB beM a -> m (Either DBError a)
runQuery dbConf query = do
  conn <- getOrInitSqlConn dbConf
  case conn of
    Right c -> L.runDB c query
    Left  e -> return $ Left e

runQueryMySQL ::
  ( HasCallStack,
    JSONEx a,
    L.MonadFlow m
  ) =>
  DBConfig BM.MySQLM -> DB.SqlDB BM.MySQLM a -> m (Either DBError a)
runQueryMySQL dbConf query = do
  conn <- getOrInitSqlConn dbConf
  case conn of
    Right c -> L.runTransaction c query
    Left  e -> return $ Left e

sqlCreate ::
  forall be table.
  (HasCallStack, B.HasQBuilder be, Model be table) =>
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
    ToJSON (table Identity),
    FromJSON (table Identity),
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
    ToJSON (table Identity),
    FromJSON (table Identity),
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
      return $ Left $ DBError UnexpectedResult message
    Left e -> return $ Left e

findOneSql ::
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
    JSONEx (table Identity),
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  Where be table ->
  m (Either DBError [table Identity])
findAllSql dbConf whereClause = do
  let findQuery = DB.findRows (sqlSelect ! #where_ whereClause ! defaults)
  sqlConn <- getOrInitSqlConn dbConf
  join <$> mapM (`L.runDB` findQuery) sqlConn

cacheWithKey :: (HasCallStack, ToJSON table, L.MonadFlow m) => Text -> table -> m ()
cacheWithKey key row = do
  -- TODO: Should we log errors here?
  void $ rSetB (T.pack cacheName) (encodeUtf8 key) (BSL.toStrict $ encode row)
