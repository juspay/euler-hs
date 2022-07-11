{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}

module SQLDB.Tests.SQLiteDBSpec where

import           EulerHS.Prelude

import           EulerHS.Interpreters
import qualified EulerHS.Language as L
import           EulerHS.Types hiding (error)
import qualified EulerHS.Types as T

import           SQLDB.TestData.Connections
import           SQLDB.TestData.Scenarios.SQLite
import           SQLDB.TestData.Types

import qualified Database.Beam.Sqlite as BS
import           Test.Hspec hiding (runIO)


-- Configurations

sqliteCfg :: DBConfig BS.SqliteM
sqliteCfg = T.mkSQLiteConfig "eulerSQliteDB" testDBName

poolConfig :: T.PoolConfig
poolConfig = T.PoolConfig
  { stripes = 1
  , keepAlive = 10
  , resourcesPerStripe = 50
  }


sqlitePoolCfg :: T.DBConfig BS.SqliteM
sqlitePoolCfg = T.mkSQLitePoolConfig "eulerSQliteDB" testDBName poolConfig

-- Tests

spec :: Spec
spec = do
  let
      test sqliteCfg = do
        it "Double connection initialization should fail" $ \rt -> do
          eRes <- runFlow rt $ do
            eConn1 <- L.initSqlDBConnection sqliteCfg
            eConn2 <- L.initSqlDBConnection sqliteCfg
            case (eConn1, eConn2) of
              (Left err, _) -> pure $ Left $ "Failed to connect 1st time: " <> show err
              (_, Left (T.DBError T.ConnectionAlreadyExists msg))
                | msg == "Connection for eulerSQliteDB already created." -> pure $ Right ()
              (_, Left err) -> pure $ Left $ "Unexpected error type on 2nd connect: " <> show err
          eRes `shouldBe` Right ()

        it "Get uninialized connection should fail" $ \rt -> do
          eRes <- runFlow rt $ do
            eConn <- L.getSqlDBConnection sqliteCfg
            case eConn of
              Left (T.DBError T.ConnectionDoesNotExist msg)
                | msg == "Connection for eulerSQliteDB does not exists." -> pure $ Right ()
              Left err -> pure $ Left $ "Unexpected error: " <> show err
              Right _ -> pure $ Left "Unexpected connection success"
          eRes `shouldBe` Right ()

        it "Init and get connection should succeed" $ \rt -> do
          eRes <- runFlow rt $ do
            eConn1 <- L.initSqlDBConnection sqliteCfg
            eConn2 <- L.getSqlDBConnection sqliteCfg
            case (eConn1, eConn2) of
              (Left err, _) -> pure $ Left $ "Failed to connect: " <> show err
              (_, Left err) -> pure $ Left $ "Unexpected error on get connection: " <> show err
              _             -> pure $ Right ()
          eRes `shouldBe` Right ()

        it "Init and double get connection should succeed" $ \rt -> do
          eRes <- runFlow rt $ do
            eConn1 <- L.initSqlDBConnection sqliteCfg
            eConn2 <- L.getSqlDBConnection sqliteCfg
            eConn3 <- L.getSqlDBConnection sqliteCfg
            case (eConn1, eConn2, eConn3) of
              (Left err, _, _) -> pure $ Left $ "Failed to connect: " <> show err
              (_, Left err, _) -> pure $ Left $ "Unexpected error on 1st get connection: " <> show err
              (_, _, Left err) -> pure $ Left $ "Unexpected error on 2nd get connection: " <> show err
              _                -> pure $ Right ()
          eRes `shouldBe` Right ()

        it "getOrInitSqlConn should succeed" $ \rt -> do
          eRes <- runFlow rt $ do
            eConn <- L.getOrInitSqlConn sqliteCfg
            case eConn of
              Left err -> pure $ Left $ "Failed to connect: " <> show err
              _        -> pure $ Right ()
          eRes `shouldBe` Right ()

        it "Prepared connection should be available" $ \rt -> do
          void $ runFlow rt $ do
            eConn <- L.initSqlDBConnection sqliteCfg
            when (isLeft eConn) $ error "Failed to prepare connection."
          void $ runFlow rt $ do
            eConn <- L.getSqlDBConnection sqliteCfg
            when (isLeft eConn) $ error "Failed to get prepared connection."

        it "Unique Constraint Violation" $ \rt -> do
          eRes <- runFlow rt (uniqueConstraintViolationDbScript sqliteCfg)
          eRes `shouldBe`
            ( Left $ DBError
                ( SQLError $ SqliteError $
                    SqliteSqlError
                      { sqlError        = SqliteErrorConstraint
                      , sqlErrorDetails = "UNIQUE constraint failed: users.id"
                      , sqlErrorContext = "step"
                      }
                )
                "SQLite3 returned ErrorConstraint while attempting to perform step: UNIQUE constraint failed: users.id"
            )

        it "Select one, row not found" $ \rt -> do
          eRes <- runFlow rt (selectUnknownDbScript sqliteCfg)
          eRes `shouldBe` (Right Nothing)

        it "Select one, row found" $ \rt -> do
          eRes <- runFlow rt (selectOneDbScript sqliteCfg)
          eRes `shouldSatisfy` (someUser "John" "Doe")

        it "Update / Select, row found & changed" $ \rt -> do
          eRes <- runFlow rt (updateAndSelectDbScript sqliteCfg)
          eRes `shouldSatisfy` (someUser "Leo" "San")

        it "Insert returning should return list of rows" $ \rt -> do
          eRes <- runFlow rt (insertReturningScript sqliteCfg)

          case eRes of
            Left  _  -> expectationFailure "Left DBResult"
            Right us -> do
              length us `shouldBe` 2
              let u1 = us !! 0
              let u2 = us !! 1

              _userFirstName u1 `shouldBe` "John"
              _userLastName  u1 `shouldBe` "Doe"

              _userFirstName u2 `shouldBe` "Doe"
              _userLastName  u2 `shouldBe` "John"

  around (withEmptyDB insertTestValues sqliteCfg) $
    describe "EulerHS SQLite DB tests" $ test sqliteCfg

  around (withEmptyDB insertTestValues sqlitePoolCfg) $
    describe "EulerHS SQLite DB tests. Pool cfg." $ test sqlitePoolCfg
