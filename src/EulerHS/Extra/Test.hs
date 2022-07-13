{-# LANGUAGE OverloadedStrings #-}

module EulerHS.Extra.Test where

import           EulerHS.Prelude

import qualified Database.Beam.Postgres as BP
import qualified Database.MySQL.Base as MySQL
import qualified Database.PostgreSQL.Simple as PG (execute_)
import           EulerHS.Interpreters
import           EulerHS.Language
import           EulerHS.Runtime (FlowRuntime)
import           EulerHS.Types
import qualified EulerHS.Types as T
import           System.Process


mwhen :: Monoid m => Bool -> m -> m
mwhen True  = id
mwnen False = const mempty


withMysqlDb :: String -> String -> MySQLConfig -> IO a -> IO a
withMysqlDb dbName filePath msRootCfg next =
    bracket_
      (dropTestDbIfExist >> createTestDb)
      (dropTestDbIfExist)
      (loadMySQLDump >> next)
  where
    T.MySQLConfig
      { connectPort
      , connectHost
      , connectUser
      , connectPassword
      } = msRootCfg

    loadMySQLDump :: IO ()
    loadMySQLDump =
        void $ system $
          "mysql " <> options <> " " <> dbName <> " 2> /dev/null < " <> filePath
      where
        options =
          intercalate " "
            [                                      "--port="     <> show connectPort
            , mwhen (not $ null connectHost    ) $ "--host="     <> connectHost
            , mwhen (not $ null connectUser    ) $ "--user="     <> connectUser
            , mwhen (not $ null connectPassword) $ "--password=" <> connectPassword
            ]

    dropTestDbIfExist :: IO ()
    dropTestDbIfExist =
      bracket (T.createMySQLConn msRootCfg) T.closeMySQLConn $ \rootConn -> do
        void . MySQL.execute_ rootConn . MySQL.Query $ "drop database if exists " <> encodeUtf8 dbName

    createTestDb :: IO ()
    createTestDb =
      bracket (T.createMySQLConn msRootCfg) T.closeMySQLConn $ \rootConn -> do
        _ <- MySQL.execute_ rootConn . MySQL.Query $ "create database " <> encodeUtf8 dbName
        void . MySQL.execute_ rootConn . MySQL.Query $ "grant all privileges on " <> encodeUtf8 dbName <> ".* to 'cloud'@'%'"


preparePostgresDB
    :: FilePath
    -> T.PostgresConfig
    -> T.PostgresConfig
    -> (T.PostgresConfig -> DBConfig BP.Pg)
    -> (forall a . (FlowRuntime -> IO a) -> IO a)
    -> (FlowRuntime -> IO ())
    -> IO()
preparePostgresDB filePath pgRootCfg pgCfg@T.PostgresConfig{..} pgCfgToDbCfg withRt next =
    withRt $ \flowRt ->
      bracket (T.createPostgresConn pgRootCfg) T.closePostgresConn $ \rootConn -> do
        let
          dropTestDbIfExist :: IO ()
          dropTestDbIfExist = do
            void $ PG.execute_ rootConn "drop database if exists euler_test_db"

          createTestDb :: IO ()
          createTestDb = do
            void $ PG.execute_ rootConn "create database euler_test_db"
            -- void $ execute_ rootConn "grant all privileges on euler_test_db.* to 'cloud'@'%'"

        bracket_
          (dropTestDbIfExist >> createTestDb)
          (dropTestDbIfExist)
          (loadPgDump >> prepareDBConnections flowRt >> next flowRt)
  where
    prepareDBConnections :: FlowRuntime -> IO ()
    prepareDBConnections flowRuntime = runFlow flowRuntime $ do
        ePool <- initSqlDBConnection $ pgCfgToDbCfg pgCfg
        either (error "Failed to connect to PG") (const $ pure ()) ePool

    loadPgDump :: IO ()
    loadPgDump =
         void $ system $
           "psql -q " <> uri <> " 1> /dev/null < " <> filePath
      where
        uri = "postgresql://"
          <> connectUser <> ":" <> connectPassword  <> "@"
          <> connectHost <> ":" <> show connectPort <> "/"
          <> connectDatabase
