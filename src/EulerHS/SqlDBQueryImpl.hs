{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-error=unused-top-binds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedStrings   #-}

module EulerHS.SqlDBQueryImpl
  ( updateOneSqlWoReturningImpl
  , findAllImpl
  , createMultiSqlWoReturningImpl
  , DeleteReturning(..)
  )
where

import           EulerHS.PIIEncryption
import qualified Data.Text as T
import qualified Database.Beam as B
import qualified Database.Beam.MySQL as BM
import qualified Database.Beam.Postgres as BP
import qualified Database.Beam.Sqlite as BS
import qualified Database.Beam.Backend.SQL.BeamExtensions as BExt
import           EulerHS.Extra.Redis (rGetWithART, withArtRecOptionsForKVDB)
import qualified EulerHS.Framework.Language as L
import           EulerHS.Prelude
import qualified EulerHS.SqlDB.Language as DB
import           Named (defaults, (!))
import           Sequelize (Model, Set, Where, sqlSelect, sqlUpdate, sqlDelete)
import           EulerHS.SqlDB.Helper
import EulerHS.Types
import EulerHS.KVConnector.PIIUtils
import EulerHS.CachedSqlDBQuery
--------------- Core API ---------------

-- | Create a new database entry with the given value.
--   Cache the value if the DB insert succeeds.

class DeleteReturning (beM :: Type -> Type) (be :: Type) where
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


instance DeleteReturning BM.MySQLM BM.MySQL where
  deleteAllReturning = deleteAllSqlMySQLImpl

instance DeleteReturning BP.Pg BP.Postgres where
  deleteAllReturning = deleteAllSqlImpl

instance DeleteReturning BS.SqliteM BS.Sqlite where
  deleteAllReturning = deleteAllSqlImpl

updateOneSqlWoReturningImpl ::
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
  Maybe PIIKeyConfig ->
  m (DBResult ())
updateOneSqlWoReturningImpl dbConf newVals whereClause mbKeyConfig = do
  setClause' <- maybe (pure $ Right newVals) (transformSetClause newVals) mbKeyConfig
  case setClause' of
      Left err -> return $ Left $ DBError PIIError err
      Right setClause -> do
        let updateQuery = DB.updateRows . (sqlUpdate
              ! #set setClause
              ! #where_ whereClause)
        res <- runQuery dbConf updateQuery
        case res of
          Right x -> do
            L.logDebug @Text "updateOneSqlWoReturningImpl" "query executed"
            return $ Right x
          Left e -> return $ Left e

findAllImpl :: forall table be beM m.
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
findAllImpl dbConf mCacheKey whereClause = withArtRecOptionsForKVDB @table $ do
  case mCacheKey of
    Just cacheKey -> do
      mRes <- rGetWithART (T.pack cacheName) cacheKey
      case join mRes of
        (Just res) -> return $ Right res
        Nothing -> lookupInDB
    Nothing -> lookupInDB
  where 
    lookupInDB = withArtRecOptionsForKVDB @table $ do
      mDBRes <- findAllSqlImpl dbConf whereClause
      findAllDecryptUtility mDBRes mCacheKey

findAllSqlImpl ::
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
findAllSqlImpl dbConf whereClause = do
  let findQuery = DB.findRows . (sqlSelect ! #where_ whereClause ! defaults)
  runQuery dbConf findQuery

createMultiSqlWoReturningImpl ::
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
  [Maybe PIIKeyConfig] ->
  m (Either DBError ())
createMultiSqlWoReturningImpl dbConf value ignoreDuplicates arrKeys = do
  maybeUpdatedValue <- decryptOrEncryptAllUtility value arrKeys True
  case maybeUpdatedValue of 
    Left err -> return $ Left $ err 
    Right updatedValue -> runQuery dbConf $ DB.insertRows . bool sqlMultiCreate sqlMultiCreateIgnoringDuplicates ignoreDuplicates updatedValue

deleteAllSqlImpl ::
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
deleteAllSqlImpl dbConf value = do
  res <- runQuery dbConf $ DB.deleteRowsReturningList . (sqlDelete ! #where_ value ! defaults)
  return res

deleteAllSqlMySQLImpl ::
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
deleteAllSqlMySQLImpl dbConf value = do
  findRes <- findAllSqlImpl dbConf value
  case findRes of
    Left err  -> return $ Left err
    Right res -> do
      delRes  <- runQuery dbConf $ DB.deleteRows . (sqlDelete ! #where_ value ! defaults)
      case delRes of
        Left err -> return $ Left err
        Right _  -> return $ Right res