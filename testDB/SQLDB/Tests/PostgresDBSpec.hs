{-# LANGUAGE RecordWildCards #-}

module SQLDB.Tests.PostgresDBSpec where

import           Database.Beam.Postgres (Pg)
import           EulerHS.Extra.Test (preparePostgresDB)
import           EulerHS.Interpreters (runFlow)
import           EulerHS.Language ()
import           EulerHS.Prelude
import           EulerHS.Runtime (withFlowRuntime)
import qualified EulerHS.Types as T
import           SQLDB.TestData.Scenarios.Postgres (selectOneDbScript,
                                                    selectUnknownDbScript,
                                                    uniqueConstraintViolationDbScript,
                                                    updateAndSelectDbScript)
import           SQLDB.TestData.Types (someUser)
import           System.Process ()
import           Test.Hspec hiding (runIO)

-- Configurations

pgCfg :: T.PostgresConfig
pgCfg = T.PostgresConfig
  { connectHost = "postgres" --String
  , connectPort = 5432 --Word16
  , connectUser = "cloud" -- String
  , connectPassword = "scape" -- String
  , connectDatabase = "euler_test_db" --  String
  }

pgRootCfg :: T.PostgresConfig
pgRootCfg =
    T.PostgresConfig
      { connectUser     = "cloud"
      , connectPassword = "scape"
      , connectDatabase = "hpcldb"
      , ..
      }
  where
    T.PostgresConfig {..} = pgCfg

mkPgCfg :: T.PostgresConfig -> T.DBConfig Pg
mkPgCfg = T.mkPostgresConfig "eulerPGDB"

poolConfig :: T.PoolConfig
poolConfig = T.PoolConfig
  { stripes = 1
  , keepAlive = 10
  , resourcesPerStripe = 50
  }

mkPgPoolCfg :: T.PostgresConfig -> T.DBConfig Pg
mkPgPoolCfg cfg = T.mkPostgresPoolConfig "eulerPGDB" cfg poolConfig

-- Tests

spec :: Spec
spec = do
    let
        test pgCfg' = do
          it "Unique Constraint Violation" $ \rt -> do
            eRes <- runFlow rt $ uniqueConstraintViolationDbScript pgCfg'

            eRes `shouldBe`
              Left (T.DBError
                  ( T.SQLError $ T.PostgresError $
                      T.PostgresSqlError
                        { sqlState = "23505"
                        , sqlExecStatus = T.PostgresFatalError
                        , sqlErrorMsg = "duplicate key value violates unique constraint \"users_pkey\""
                        , sqlErrorDetail = "Key (id)=(2) already exists."
                        , sqlErrorHint = ""
                        }
                  )
                  "SqlError {sqlState = \"23505\", sqlExecStatus = FatalError, sqlErrorMsg = \"duplicate key value violates unique constraint \\\"users_pkey\\\"\", sqlErrorDetail = \"Key (id)=(2) already exists.\", sqlErrorHint = \"\"}"
              )

          it "Select one, row not found" $ \rt -> do
            eRes <- runFlow rt $ selectUnknownDbScript pgCfg'
            eRes `shouldBe` Right Nothing

          it "Select one, row found" $ \rt -> do
            eRes <- runFlow rt $ selectOneDbScript pgCfg'
            eRes `shouldSatisfy` someUser "John" "Doe"

          it "Update / Select, row found & changed" $ \rt -> do
            eRes <- runFlow rt $ updateAndSelectDbScript pgCfg'
            eRes `shouldSatisfy` someUser "Leo" "San"

    let prepare pgCfgToDbCfg =
          preparePostgresDB
            "testDB/SQLDB/TestData/PostgresDBSpec.sql"
            pgRootCfg
            pgCfg
            pgCfgToDbCfg
            (withFlowRuntime Nothing)

    around (prepare mkPgCfg) $
      describe "EulerHS Postgres DB tests" $ test $ mkPgCfg pgCfg

    around (prepare mkPgPoolCfg) $
      describe "EulerHS Postgres DB tests. Pool" $ test $ mkPgPoolCfg pgCfg
