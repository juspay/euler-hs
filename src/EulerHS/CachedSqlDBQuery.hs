{- |
Module      :  EulerHS.CachedSqlDBQuery
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable

This module contains the function to create, update, delete and find rows in the database.
This module has functions which are being used in Flows for KVDB operations.

-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-error=unused-top-binds #-}
{-# LANGUAGE NamedFieldPuns #-}

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
  , findAllExtended'
  , deleteSql
  , deleteExtended
  , deleteWithReturningPG
  , createMultiSql
  , createMultiSqlWoReturning
  , runQuery
  , countRows
  , findDecryptUtility
  , createSqlWithConn
  , createMultiSqlWithConn
  , updateSqlWithConn
  , deleteSqlWithConn
  , findAllWithConn
  , findOneWithConn
  , SqlReturning(..)
  )
where

import           EulerHS.PIIEncryption
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
                                      DBErrorType (UnexpectedResult, PIIError), DBResult, NativeSqlConn)
import           Named (defaults, (!))
import           Sequelize (Model, Set, Where, mkExprWithDefault,  mkMultiExprWithDefault,
                            modelTableEntity, sqlSelect, sqlUpdate, sqlDelete, sqlCount, modelTableName)
import           EulerHS.Logger.Types (ErrorL(..))
import qualified Data.HashMap.Strict as HM
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
      PII table,
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
    Maybe (PIIEncryptionKeyId, PIIEncryptionKey) ->
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
    PII table,
    ToJSON (table Identity),
    Show (table Identity),
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  table Identity ->
  Maybe Text ->
  Maybe (PIIEncryptionKeyId, PIIEncryptionKey) -> 
  m (Either DBError (table Identity))
create dbConf value mCacheKey mbKeyConfig = do
  updatedValue <- maybe (pure $ Right value) (\(kid, key) -> encryptRow value kid key) mbKeyConfig
  case updatedValue of 
    Left err -> return $ Left $ DBError PIIError err
    Right encResult -> do 
      res <- createSql dbConf encResult
      setPrimaryKeyUtility value mCacheKey res
    
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
  DBConfig BM.MySQLM ->
  table Identity ->
  Maybe Text ->
  Maybe (PIIEncryptionKeyId, PIIEncryptionKey) ->
  m (Either DBError (table Identity))
createMySQL dbConf value mCacheKey mbKeyConfig = do
  updatedValue <- maybe (pure $ Right value) (\(kid, key) -> encryptRow value kid key) mbKeyConfig
  case updatedValue of 
    Left err -> return $ Left $ DBError PIIError err
    Right mbEncValue -> do 
      res <- createSqlMySQL dbConf mbEncValue
      setPrimaryKeyUtility value mCacheKey res

setPrimaryKeyUtility ::   --- to do can we optimize this?
  ( ToJSON (table Identity),
    PII table,
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
    PIIUpdate be table,
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
    PIIUpdate be table,
    B.HasQBuilder be,
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  Maybe Text ->
  [Set be table] ->
  Where be table ->
  (Maybe (PIIEncryptionKeyId, PIIEncryptionKey)) ->
  m (Either DBError ())
updateOneWoReturning dbConf (Just _) newVals whereClause maybeKeyConfig = do
  updateOneSqlWoReturning dbConf newVals whereClause maybeKeyConfig 
updateOneWoReturning dbConf Nothing value whereClause maybeKeyConfig = updateOneSqlWoReturning dbConf value whereClause maybeKeyConfig

updateOneSqlWoReturning ::
  forall m be beM table.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    PIIUpdate be table,
    B.HasQBuilder be,
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  [Set be table] ->
  Where be table ->
  Maybe (PIIEncryptionKeyId, PIIEncryptionKey) ->
  m (DBResult ())
updateOneSqlWoReturning dbConf newVals whereClause mbKeyConfig = do
  setClause' <- maybe (pure $ Right newVals) (\(x, y)-> transformSetClause newVals x y) mbKeyConfig
  case setClause' of
      Left err -> return $ Left $ DBError PIIError err
      Right setClause -> do
        let updateQuery = DB.updateRows $ sqlUpdate
              ! #set setClause
              ! #where_ whereClause
        res <- runQuery dbConf updateQuery
        case res of
          Right x -> do
            L.logDebug @Text "updateOneSqlWoReturning" "query executed"
            return $ Right x
        -- Right xs -> do
        --   let message = "DB returned \"" <> show xs <> "\" after update"
        --   L.logErrorWithCategory @Text "create" message
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
      setClause' <- maybe (pure $ Right newVals) (\(x, y)-> transformSetClause newVals x y) mbKeyConfig
      case setClause' of
        Left e -> pure $ Left $ DBError PIIError e
        Right setClause -> do
          let updateQuery = DB.updateRowsReturningList $ sqlUpdate
                ! #set setClause
                ! #where_ whereClause
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
  DBConfig beM ->
  [Set be table] ->
  Where be table ->
  Maybe (PIIEncryptionKeyId, PIIEncryptionKey) ->
  m (DBResult ())
updateAllSql dbConf newVals whereClause mbVal = do 
  updatedClause <- maybe (pure $ Right newVals) (\(x, y)-> transformSetClause newVals x y) mbVal
  case updatedClause of 
    Left e -> pure $ Left $ DBError PIIError e 
    Right val -> do 
      let updateQuery = DB.updateRows $ sqlUpdate
            ! #set val
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
    PII table,
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
      findDecryptUtility mDBRes (Just cacheKey)
findOne dbConf Nothing whereClause = do
  mDBRes <- findOneSql dbConf whereClause
  findDecryptUtility mDBRes Nothing

findDecryptUtility ::
  ( ToJSON (table Identity),
    PII table,
    L.MonadFlow m
  ) =>
  (Either DBError (Maybe (table Identity))) ->
  Maybe Text ->
  m (Either DBError (Maybe (table Identity)))
findDecryptUtility mDBRes mbCacheKey = do 
  case mDBRes of 
    Right (Just encResult) -> do 
      decryptResult <- decryptRow encResult Nothing
      case decryptResult of 
        Left err -> return $ Left $ DBError PIIError err
        Right decryptedRes -> do
          whenJust mbCacheKey (`cacheWithKey` decryptedRes)
          return $ Right $ Just $ decryptedRes
    Right Nothing -> return $ Right $ Nothing
    Left dbError -> return $ Left $ dbError


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
    PII table,
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
      findAllDecryptUtility mDBRes (Just cacheKey)
findAll dbConf Nothing whereClause = do 
  mDBRes <- findAllSql dbConf whereClause
  findAllDecryptUtility mDBRes Nothing

findAllDecryptUtility ::  
  ( PII table,
    ToJSON (table Identity),
    L.MonadFlow m
  ) =>
  (Either DBError [table Identity]) ->
  Maybe Text ->
  m (Either DBError [table Identity])
findAllDecryptUtility mDBRes mbCacheKey = do 
  case mDBRes of 
    Right encResult -> do
      let emptyKeyConfig = replicate (length encResult) (Nothing, Nothing)
      eitherDecryptedRes <- decryptOrEncryptAllUtility encResult emptyKeyConfig False
      case eitherDecryptedRes of 
        Right res -> do
          whenJust mbCacheKey (`cacheWithKey` res)
          return $ Right $ res
        Left decErr -> return $ Left $ decErr
    Left dbError -> return $ Left $ dbError

-- | Like 'findAll', but takes an explicit 'SqlSelect'.
findAllExtended :: forall beM be table m .
  (HasCallStack,
   L.MonadFlow m,
   B.FromBackendRow be (table Identity),
   BeamRunner beM,
   BeamRuntime be beM,
   PII table,
   FromJSON (table Identity),
   ToJSON (table Identity)) =>
  DBConfig beM ->
  Maybe Text ->
  B.SqlSelect be (table Identity) ->
  m (Either DBError [table Identity])
findAllExtended dbConf mKey sel = case mKey of
  Nothing -> do 
    res <- go
    findAllDecryptUtility res Nothing
  Just k -> do
    mCached <- rGet (T.pack cacheName) k
    case mCached of
      Just res -> pure . Right $ res
      Nothing -> do
        dbRes <- go
        findAllDecryptUtility dbRes (Just k)
  where
    go :: m (Either DBError [table Identity])
    go = do
      eConn <- getOrInitSqlConn dbConf
      rows <- join <$> traverse (\conn -> L.runDB conn . DB.findRows $ sel) eConn
      case rows of
        Left err -> L.incrementDbMetric err dbConf *> pure rows
        Right _ -> pure rows

findAllExtended' :: forall beM be table m .
  (HasCallStack,
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

runQueryWithConn ::
  ( HasCallStack,
    BeamRuntime be beM, BeamRunner beM,
    L.MonadFlow m
  ) =>
  DBConfig beM -> NativeSqlConn -> DB.SqlDB beM a -> m (Either DBError a)
runQueryWithConn dbConf c query = do
  result <- L.runDBWithConn c query
  case result of
    Right _ -> pure result
    Left err -> do
      L.incrementDbMetric err dbConf
      pure result

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
  res <- runQueryMySQL dbConf $ DB.insertRowReturningMySQL $ sqlCreate value
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
  DBConfig beM ->
  table Identity ->
  Maybe (PIIEncryptionKeyId, PIIEncryptionKey) -> 
  m (Either DBError ())
createSqlWoReturing dbConf value (Just (kid , key)) = do 
  updatedValue <- encryptRow value kid key
  case updatedValue of 
    Left err -> return $ Left $ DBError PIIError err
    Right encResult -> runQuery dbConf $ DB.insertRows $ sqlCreate encResult
createSqlWoReturing dbConf value Nothing = runQuery dbConf $ DB.insertRows $ sqlCreate value

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
    PII table,
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  [table Identity] ->
  Bool ->
  [(Maybe PIIEncryptionKeyId, Maybe PIIEncryptionKey)] ->
  m (Either DBError [table Identity])
createMultiSql dbConf value ignoreDuplicates arrKeys = do 
  maybeUpdatedValue <- decryptOrEncryptAllUtility value arrKeys True
  case maybeUpdatedValue of 
    Left err -> return $ Left $ err 
    Right updatedValue -> do 
      res <- runQuery dbConf $ DB.insertRowsReturningList $ bool sqlMultiCreate sqlMultiCreateIgnoringDuplicates ignoreDuplicates updatedValue
      setPrimaryKeyAllUtility res value

createMultiSqlWoReturning ::
  ( HasCallStack,
    BExt.BeamHasInsertOnConflict be,
    BeamRuntime be beM,
    BeamRunner beM,
    PII table,
    Model be table,
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  [table Identity] ->
  Bool ->
  [(Maybe PIIEncryptionKeyId, Maybe PIIEncryptionKey)] ->
  m (Either DBError ())
createMultiSqlWoReturning dbConf value ignoreDuplicates arrKeys = do
  maybeUpdatedValue <- decryptOrEncryptAllUtility value arrKeys True
  case maybeUpdatedValue of 
    Left err -> return $ Left $ err 
    Right updatedValue -> runQuery dbConf $ DB.insertRows $ bool sqlMultiCreate sqlMultiCreateIgnoringDuplicates ignoreDuplicates updatedValue

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

decryptOrEncryptAllUtility :: (L.MonadFlow m, PII table) => [table Identity] -> [(Maybe PIIEncryptionKeyId, Maybe PIIEncryptionKey)] -> Bool ->  m (Either DBError [table Identity])
decryptOrEncryptAllUtility [] _ _ = pure $ Right []
decryptOrEncryptAllUtility listRow mayKeyConfiglist shouldEncrypt = do
  eithResList <- foldM doEncryptDecrypt [] $ zip listRow mayKeyConfiglist
  case eithResList of
    [Left err] -> do
      L.logError @Text "PII error : decryptOrEncryptAllUtility" $ if shouldEncrypt then "encryption failed" else "decryption failed"
      pure $ Left err
    _ -> pure $ Right $ foldl (\acc row -> either (const $ acc) (\rw -> rw : acc) row) [] eithResList

  where
    -- doEncryptDecrypt : for any Left case, directly return [Left err]
    doEncryptDecrypt :: (L.MonadFlow m, PII table) => [Either DBError (table Identity)] -> ((table Identity), (Maybe PIIEncryptionKeyId, Maybe PIIEncryptionKey)) -> m [Either DBError (table Identity)]
    doEncryptDecrypt [Left e] _ = pure [Left e]
    doEncryptDecrypt _ (_, (Just _, Nothing)) = pure [Left $ DBError PIIError "tableT does not have a correspoding key config"]
    doEncryptDecrypt _ (_, (Nothing, Just _)) = pure [Left $ DBError PIIError "tableT does not have a correspoding keyId config"]
    doEncryptDecrypt acc (row, (mayEncKeyId, mayEncKey)) = do
      eithRes <- case (shouldEncrypt, mayEncKeyId, mayEncKey) of
          (True, Just encKeyId, Just encKey) -> encryptRow row encKeyId encKey
          (False, Just PIIEncryptionKeyId{encKeyId}, Just PIIEncryptionKey{encKey}) -> decryptRow row $ Just $ HM.singleton encKeyId encKey
          (False, _, _) -> decryptRow row Nothing
          (_, _, _) -> return $ Right row
      case eithRes of
        Left e -> pure [Left $ DBError PIIError e]
        Right rowRes -> pure ((Right rowRes) : acc)

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
  res <- runQueryWithConn dbConf conn $ DB.insertRowsReturningList $ sqlCreate value
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
createMultiSqlWithConn dbConf conn value ignoreDuplicates = runQueryWithConn dbConf conn $ DB.insertRowsReturningList $ bool sqlMultiCreate sqlMultiCreateIgnoringDuplicates ignoreDuplicates value

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
updateSqlWithConn dbConf conn newVals whereClause = runQueryWithConn dbConf conn $ DB.updateRows (sqlUpdate ! #set newVals ! #where_ whereClause)

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
deleteSqlWithConn dbConf conn value = runQueryWithConn dbConf conn $ DB.deleteRowsReturningList (sqlDelete ! #where_ value ! defaults)

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
findOneWithConn dbConf conn whereClause = runQueryWithConn dbConf conn $ DB.findRow (sqlSelect ! #where_ whereClause ! defaults)

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
findAllWithConn dbConf conn whereClause = runQueryWithConn dbConf conn $ DB.findRows (sqlSelect ! #where_ whereClause ! defaults)