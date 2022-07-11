module SQLDB.Tests.PostgresDBSpec where

import           EulerHS.Prelude

import           EulerHS.Interpreters
import           EulerHS.Runtime (withFlowRuntime)
import           EulerHS.Types hiding (error)
import qualified EulerHS.Types as T

import           SQLDB.TestData.Connections (connectOrFail)
import           SQLDB.TestData.Scenarios.Postgres
import           SQLDB.TestData.Types

import qualified Database.Beam.Postgres as BP
import           Database.PostgreSQL.Simple (execute_)
import           EulerHS.Extra.Test
import           EulerHS.Language
import           EulerHS.Runtime (FlowRuntime, withFlowRuntime)
import           System.Process
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

mkPgCfg = mkPostgresConfig "eulerPGDB"

poolConfig = T.PoolConfig
  { stripes = 1
  , keepAlive = 10
  , resourcesPerStripe = 50
  }

mkPgPoolCfg cfg = mkPostgresPoolConfig "eulerPGDB" cfg poolConfig

-- Tests

spec :: Spec
spec = do
    let
        test pgCfg = do
          it "Unique Constraint Violation" $ \rt -> do
            eRes <- runFlow rt $ uniqueConstraintViolationDbScript pgCfg

            eRes `shouldBe`
              ( Left $ DBError
                  ( SQLError $ PostgresError $
                      PostgresSqlError
                        { sqlState = "23505"
                        , sqlExecStatus = PostgresFatalError
                        , sqlErrorMsg = "duplicate key value violates unique constraint \"users_pkey\""
                        , sqlErrorDetail = "Key (id)=(2) already exists."
                        , sqlErrorHint = ""
                        }
                  )
                  "SqlError {sqlState = \"23505\", sqlExecStatus = FatalError, sqlErrorMsg = \"duplicate key value violates unique constraint \\\"users_pkey\\\"\", sqlErrorDetail = \"Key (id)=(2) already exists.\", sqlErrorHint = \"\"}"
              )

          it "Select one, row not found" $ \rt -> do
            eRes <- runFlow rt $ selectUnknownDbScript pgCfg
            eRes `shouldBe` (Right Nothing)

          it "Select one, row found" $ \rt -> do
            eRes <- runFlow rt $ selectOneDbScript pgCfg
            eRes `shouldSatisfy` (someUser "John" "Doe")

          it "Update / Select, row found & changed" $ \rt -> do
            eRes <- runFlow rt $ updateAndSelectDbScript pgCfg
            eRes `shouldSatisfy` (someUser "Leo" "San")

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
