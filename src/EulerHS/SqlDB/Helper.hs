{-# LANGUAGE PackageImports #-}
module EulerHS.SqlDB.Helper
   (
    runQuery,
    runQueryMySQL,
    runQueryWithConn,
    runDB,
    runDBWithConn,
    runQuery',
    runQueryWithConn',
   ) where

import qualified Database.Beam.MySQL as BM
import           EulerHS.Extra.Language (getOrInitSqlConn)
import qualified EulerHS.Framework.Language as L
import           EulerHS.Prelude
import qualified EulerHS.ART.EnvVars as EnvVars
import qualified EulerHS.ART.V2.Utils as ArtUtils
import qualified EulerHS.SqlDB.Language as DB
import           EulerHS.SqlDB.Types (BeamRunner, BeamRuntime, DBConfig(..),
                                      DBError (..), NativeSqlConn, getSchemaName)
import           EulerHS.Framework.Language (runDB, runDBWithConn)


runQuery ::
  ( HasCallStack,
    BeamRuntime be beM, BeamRunner beM,
    L.MonadFlow m
  ) =>
  DBConfig beM -> (Text -> DB.SqlDB beM a) -> m (Either DBError a)
runQuery dbConf query = do
  conn <- getOrInitSqlConn dbConf
  case conn of
    Right c -> do
      schemaName <- getSchemaNameWithART dbConf
      result <- L.runDB c (query schemaName)
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
  DBConfig BM.MySQLM -> (Text -> DB.SqlDB BM.MySQLM a) -> m (Either DBError a)
runQueryMySQL dbConf query = do
  conn <- getOrInitSqlConn dbConf
  case conn of
    Right c -> do
      schemaName <- getSchemaNameWithART dbConf
      rows <- L.runTransaction c (query schemaName)
      case rows of
        Left err -> L.incrementDbMetric err dbConf *> pure rows
        Right _ -> pure rows
    Left  e -> return $ Left e

runQueryWithConn ::
  ( HasCallStack,
    BeamRuntime be beM, BeamRunner beM,
    L.MonadFlow m
  ) =>
  DBConfig beM -> NativeSqlConn -> (Text -> DB.SqlDB beM a) -> m (Either DBError a)
runQueryWithConn dbConf c query = do
  schemaName <- getSchemaNameWithART dbConf
  result <- L.runDBWithConn c (query schemaName)
  case result of
    Right _ -> pure result
    Left err -> do
      L.incrementDbMetric err dbConf
      pure result

-- Use these funcs if your connection is made with a schema or SqlDB has proper dbEntitySchema
runQuery' ::
  ( HasCallStack,
    BeamRuntime be beM, BeamRunner beM,
    L.MonadFlow m
  ) =>
  DBConfig beM -> DB.SqlDB beM a -> m (Either DBError a)
runQuery' dbConf query = do
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

runQueryWithConn' ::
  ( HasCallStack,
    BeamRuntime be beM, BeamRunner beM,
    L.MonadFlow m
  ) =>
  DBConfig beM -> NativeSqlConn -> DB.SqlDB beM a -> m (Either DBError a)
runQueryWithConn' dbConf c query = do
  result <- L.runDBWithConn c query
  case result of
    Right _ -> pure result
    Left err -> do
      L.incrementDbMetric err dbConf
      pure result

getSchemaNameWithART :: (L.MonadFlow m) => DBConfig beM -> m Text
getSchemaNameWithART cfg = 
  if EnvVars.isArtV2ReplayEnabled
    then do
      artReplayPrefix <- ArtUtils.getArtReplayDBPrefix
      pure $ artReplayPrefix <> getSchemaName cfg
    else pure $ getSchemaName cfg