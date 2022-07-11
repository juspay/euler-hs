module SQLDB.Tests.MySQLDBSpec where

import           EulerHS.Prelude

import           EulerHS.Interpreters
import           EulerHS.Runtime (FlowRuntime, withFlowRuntime)
import           EulerHS.Types hiding (error)

import           SQLDB.TestData.Connections (connectOrFail)
import           SQLDB.TestData.Scenarios.MySQL
import           SQLDB.TestData.Types

import           Test.Hspec hiding (runIO)

import qualified Database.Beam.MySQL as BM
import           Database.MySQL.Base
import           EulerHS.Language
import qualified EulerHS.Types as T
import           System.Process

import           EulerHS.Extra.Test


testDBName :: String
testDBName = "mysql_db_spec_test_db"

mySQLCfg :: T.MySQLConfig
mySQLCfg = T.MySQLConfig
  { connectHost     = "mysql"
  , connectPort     = 3306
  , connectUser     = "cloud"
  , connectPassword = "scape"
  , connectDatabase = testDBName
  , connectOptions  = [T.CharsetName "utf8"]
  , connectPath     = ""
  , connectSSL      = Nothing
  , connectCharset  = Latin1
  }

mySQLRootCfg :: T.MySQLConfig
mySQLRootCfg =
    T.MySQLConfig
      { connectUser     = "root"
      , connectPassword = "root"
      , connectDatabase = ""
      , ..
      }
  where
    T.MySQLConfig {..} = mySQLCfg

mkMysqlConfig = T.mkMySQLConfig "eulerMysqlDB"

poolConfig = T.PoolConfig
  { stripes = 1
  , keepAlive = 10
  , resourcesPerStripe = 50
  }

mkMysqlPoolConfig mySQLCfg = mkMySQLPoolConfig "eulerMysqlDB" mySQLCfg poolConfig

spec :: Spec
spec = do
  let test dbCfg = do
        it "Unique Constraint Violation" $ \rt -> do
          eRes <- runFlow rt $ uniqueConstraintViolationDbScript dbCfg
          eRes `shouldBe`
            ( Left $ DBError
                ( SQLError $ MysqlError $
                    MysqlSqlError
                      { errCode   = 1062
                      , errMsg  = "Duplicate entry '2' for key 'PRIMARY'"
                      }
                )
                "ConnectionError {errFunction = \"query\", errNumber = 1062, errMessage = \"Duplicate entry '2' for key 'PRIMARY'\"}"
            )

        it "Txn should be commited in both cases in one connection (Eva)" $ \rt -> do
          _ <- runFlow rt $ uniqueConstraintViolationEveDbScript dbCfg
          eRes2 <- runFlow rt $ selectRowDbScript 2 dbCfg
          eRes3 <- runFlow rt $ selectRowDbScript 3 dbCfg
          (eRes2, eRes3) `shouldBe`
            ( Right (Just (User {_userId = 2, _userFirstName = "Eve", _userLastName = "Beon"}))
            , Right (Just (User {_userId = 3, _userFirstName = "Eve", _userLastName = "Beon"}))
            )

        it "First insert success, last insert resolved on DB side (Mickey)" $ \rt -> do
          _ <- runFlow rt $ uniqueConstraintViolationMickeyDbScript dbCfg
          eRes <- runFlow rt $ selectRowDbScript 4 dbCfg
          eRes `shouldSatisfy` (someUser "Mickey" "Mouse")

        it "Txn should be completely rollbacked on exception (Billy)" $ \rt -> do
          _ <- runFlow rt $ throwExceptionFlowScript dbCfg
          eRes6 <- runFlow rt $ selectRowDbScript 6 dbCfg
          eRes7 <- runFlow rt $ selectRowDbScript 7 dbCfg
          (eRes6, eRes7) `shouldBe` (Right Nothing, Right Nothing)

        it "Insert and Select in one db connection (Milky way)" $ \rt -> do
          eRes <- runFlow rt $ insertAndSelectWithinOneConnectionScript dbCfg
          eRes `shouldSatisfy` (someUser "Milky" "Way")

        it "Select one, row not found" $ \rt -> do
          eRes <- runFlow rt $ selectUnknownDbScript dbCfg
          eRes `shouldBe` (Right Nothing)

        it "Select one, row found" $ \rt -> do
          eRes <- runFlow rt $ selectOneDbScript dbCfg
          eRes `shouldSatisfy` (someUser "John" "Doe")

        it "Update / Select, row found & changed" $ \rt -> do
          eRes <- runFlow rt $ updateAndSelectDbScript dbCfg
          eRes `shouldSatisfy` (someUser "Leo" "San")

        it "Insert returning should return list of rows" $ \rt -> do
          eRes <- runFlow rt $ insertReturningScript dbCfg

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

  let prepare msCfgToDbCfg next =
        withMysqlDb testDBName "testDB/SQLDB/TestData/MySQLDBSpec.sql" mySQLRootCfg $
          withFlowRuntime Nothing $ \rt -> do
            runFlow rt $ do
              ePool <- initSqlDBConnection $ msCfgToDbCfg mySQLCfg
              either (error "Failed to connect to MySQL") (const $ pure ()) ePool
            next rt

  around (prepare mkMysqlConfig) $
    describe "EulerHS MySQL DB tests" $ test $ mkMysqlConfig mySQLCfg

  around (prepare mkMysqlPoolConfig) $
    describe "EulerHS MySQL DB tests. Pool" $ test $ mkMysqlPoolConfig mySQLCfg


