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

{-
Typeclass for executing SQL queries with returning results.

This typeclass defines a common interface for executing SQL queries that return results using different database backends.

Methods:
  - 'createReturning': Executes an SQL query to create a record and returns the created record.

Instances:
  - 'SqlReturning BM.MySQLM BM.MySQL': Instance for MySQL database backend.
  - 'SqlReturning BP.Pg BP.Postgres': Instance for PostgreSQL database backend.
  - 'SqlReturning BS.SqliteM BS.Sqlite': Instance for SQLite database backend.
-}
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

{-
Executes an SQL query to create a record and handles caching.

This function is a generic implementation of creating a record in the database. It utilizes the 'createSql' function
to perform the SQL query and handles caching of the created record based on the provided cache key.

Parameters:
  - 'dbConf': The 'DBConfig' specifying the database connection details.
  - 'value': The record to be created in the database.
  - 'mCacheKey': Optional cache key for caching the created record.

Returns:
  - An action in the 'L.MonadFlow' monad, producing a 'Either DBError (table Identity)' result.
-}
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
  DBConfig beM ->                     -- Database configuration for the connection.
  table Identity ->                   -- Record to be created in the database.
  Maybe Text ->                       -- Optional cache key for caching the created record
  m (Either DBError (table Identity)) -- Result in the 'L.MonadFlow' monad, containing either an error or the created record.
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

{-|
Update one or more elements in the database without returning any specific field.

Parameters:
  - 'dbConf': The 'DBConfig' representing the database connection.
  - 'newVals': A list of 'Set' operations to modify the values in the target table. Each 'Set' operation represents a field update.
  - 'whereClause': The 'Where' clause specifying the conditions for the update.

Returns:
  - 'Right ()' if the update is successful.
  - 'Left' with 'DBError' if there's an error.
-}
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

{-|
Update one or more records in the database.

Parameters:
  - 'dbConf': The 'DBConfig' representing the database connection.
  - 'newVals': A list of 'Set' operations to modify the values in the target table. Each 'Set' operation represents a field update.
  - 'whereClause': The 'Where' clause specifying the conditions for the update.

Returns:
  - 'Right val' if the update is successful, where 'val' is the value of the field specified in the 'RETURNING' clause of the SQL query.
  - 'Left' with 'DBError' if there's an error.
-}
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

{-|
Finds an element matching the query. Only uses the DB if the cache is empty.
Caches the result using the given key.

Parameters:
  - 'dbConf': The 'DBConfig' representing the database connection.
  - 'cacheKey': Optional key to cache the result. If 'Just', attempts to retrieve the result from the cache.
  - 'whereClause': The 'Where' clause specifying the conditions.

Returns:
  - 'Right (Just val)' if a record is found, where 'val' is the found record.
  - 'Right Nothing' if no record is found.
  - 'Left' with 'DBError' if there's an error.

Example:
> myFlow = do
>   let dbConfig = -- your DBConfig here --
>   let cacheKey = -- your cache key here --
>   let conditions = -- your Where clause here --
>   result <- findOne dbConfig cacheKey conditions
>   case result of
>     Right (Just foundRecord) -> logInfoT "Found record" $ show foundRecord
>     Right Nothing -> logInfoT "No record found"
>     Left error -> logErrorT "Query error" $ show error
-}
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

{-|
Finds all elements matching the query. Only uses the DB if the cache is empty.
Caches the result using the given key.

NOTE: Can't use the same key as 'findOne', 'updateOne', or 'create' since its result is a list.

Parameters:
  - 'dbConf': The 'DBConfig' representing the database connection.
  - 'cacheKey': Optional key to cache the result. If 'Just', attempts to retrieve the result from the cache.
  - 'whereClause': The 'Where' clause specifying the conditions.

Returns:
  - 'Right [val]' if records are found, where 'val' is a list of found records.
  - 'Left' with 'DBError' if there's an error.
-}
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
{-
Executes an SQL query using a database connection.
-}
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

{-
Generates a SQL insertion query for creating a record in the database.

This function takes a record of type 'table Identity' and generates a SQL insertion query using the 'B.insert' function.
It utilizes the 'modelTableEntity' to define the target table for insertion.

Parameters:
  - 'value': The record to be inserted into the database.

Returns:
  - A 'B.SqlInsert be table' representing the SQL insertion query.
-}
sqlCreate ::
  forall be table.
  (HasCallStack, B.HasQBuilder be, Model be table) =>
  table Identity ->
  B.SqlInsert be table
sqlCreate value = B.insert modelTableEntity (mkExprWithDefault value)

{-
Runs a SQL insertion query using the provided 'DBConfig' and returns the inserted record.

This function takes a 'DBConfig' for database configuration and a record of type 'table Identity' to be inserted.
It executes the insertion query using 'DB.insertRowsReturningList' and 'sqlCreate', and returns the inserted record.

Parameters:
  - 'dbConf': The 'DBConfig' representing the database connection.
  - 'value': The record to be inserted into the database.

Returns:
  - 'Right val' if the insertion is successful, where 'val' is the inserted record.
  - 'Left (DBError UnexpectedResult message)' if the database returns an unexpected result.

Example:
> myFlow = do
>   let dbConfig = -- your DBConfig here --
>   let myRecord = -- your record here --
>   result <- createSql dbConfig myRecord
>   case result of
>     Right insertedRecord -> logInfoT "Inserted record" $ show insertedRecord
>     Left error -> logErrorT "Insert error" $ show error
-}
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

{-|
Runs a SQL query to find a single record in the database based on the provided 'DBConfig' and 'Where' clause.

This function takes a 'DBConfig' for database configuration and a 'Where' clause specifying the conditions.
It executes the query using 'DB.findRow' and 'sqlSelect', returning 'Right (Just val)' if a record is found,
'Right Nothing' if no records match the conditions, and 'Left' if there's an error.

Parameters:
  - 'dbConf': The 'DBConfig' representing the database connection.
  - 'whereClause': The 'Where' clause specifying the conditions.

Returns:
  - 'Right (Just val)' if a record is found, where 'val' is the found record.
  - 'Right Nothing' if no records match the conditions.
  - 'Left' with 'DBError' if there's an error.

Example:
> myFlow = do
>   let dbConfig = -- your DBConfig here --
>   let conditions = -- your Where clause here --
>   result <- findOneSql dbConfig conditions
>   case result of
>     Right (Just foundRecord) -> logInfoT "Found record" $ show foundRecord
>     Right Nothing -> logInfoT "No matching records found" ""
>     Left error -> logErrorT "Query error" $ show error
-}
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


{-|
Runs a SQL query to find all records in the database based on the provided 'Where' clause.

This function takes a 'DBConfig' for database configuration and a 'Where' clause specifying the conditions.
It executes the query using 'DB.findRows' and 'sqlSelect', returning 'Right [val]' if records are found,
and 'Left' if there's an error.

Parameters:
  - 'dbConf': The 'DBConfig' representing the database connection.
  - 'whereClause': The 'Where' clause specifying the conditions.

Returns:
  - 'Right [val]' if records are found, where 'val' is a list of found records.
  - 'Left' with 'DBError' if there's an error.

Example:
> myFlow = do
>   let dbConfig = -- your DBConfig here --
>   let conditions = -- your Where clause here --
>   result <- findAllSql dbConfig conditions
>   case result of
>     Right foundRecords -> logInfoT "Found records" $ show foundRecords
>     Left error -> logErrorT "Query error" $ show error
-}
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
