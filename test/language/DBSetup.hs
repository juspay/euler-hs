{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module DBSetup where

import           Data.Aeson as A
import qualified Database.Beam as B
import           Database.Beam.Sqlite.Connection (SqliteM)
import           EulerHS.Interpreters as I
import           EulerHS.Language as L
import           EulerHS.Prelude
import           EulerHS.Runtime
import           EulerHS.Types as T
import           Sequelize


data UserT f = User
    { _userGUID  :: B.C f Int
    , _firstName :: B.C f Text
    , _lastName  :: B.C f Text
    } deriving (Generic, B.Beamable)

instance B.Table UserT where
  data PrimaryKey UserT f =
    UserId (B.C f Int) deriving (Generic, B.Beamable)
  primaryKey = UserId . _userGUID

instance ModelMeta UserT where
  modelFieldModification = userTMod
  modelTableName = "users"

type User = UserT Identity

type UserId = B.PrimaryKey UserT Identity

deriving instance Show UserId
deriving instance Eq UserId
deriving instance ToJSON UserId
deriving instance FromJSON UserId

deriving instance Show User
deriving instance Eq User
deriving instance ToJSON User
deriving instance FromJSON User

userTMod :: UserT (B.FieldModification (B.TableField UserT))
userTMod =
  B.tableModification
    { _userGUID = B.fieldNamed "id"
    , _firstName = B.fieldNamed "first_name"
    , _lastName = B.fieldNamed "last_name"
    }

userEMod :: B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity UserT)
userEMod = B.modifyTableFields userTMod

newtype UserDB f = UserDB
    { users :: f (B.TableEntity UserT)
    } deriving stock (Generic)
      deriving anyclass (B.Database be)

userDB :: B.DatabaseSettings be UserDB
userDB = B.defaultDbSettings `B.withDbModification`
  B.dbModification
    { users = userEMod
    }

-- Prepare connection to database file

testDBName :: String
testDBName = "test/language/EulerHS/TestData/test.db"

testDBTemplateName :: String
testDBTemplateName = "test/language/EulerHS/TestData/test.db.template"

poolConfig :: PoolConfig
poolConfig = T.PoolConfig
  { stripes = 1
  , keepAlive = 10
  , resourcesPerStripe = 50
  }

sqliteCfg :: DBConfig SqliteM
sqliteCfg = T.mkSQLitePoolConfig "SQliteDB" testDBName poolConfig

rmTestDB :: L.Flow ()
rmTestDB = void $ L.runSysCmd $ "rm -f " <> testDBName

prepareTestDB :: L.Flow ()
prepareTestDB = do
  rmTestDB
  -- L.runSysCmd "pwd" >>= L.runIO . print
  void $ L.runSysCmd $ "cp " <> testDBTemplateName <> " " <> testDBName

withEmptyDB :: (FlowRuntime -> IO ()) -> IO ()
withEmptyDB act = withFlowRuntime Nothing (\rt -> do
  try (runFlow rt prepareTestDB) >>= \case
    Left (e :: SomeException) ->
      runFlow rt rmTestDB
      `finally` error ("Preparing test values failed: " <> show e)
    Right _ -> act rt `finally` runFlow rt rmTestDB
    )


connectOrFail :: T.DBConfig beM -> Flow (T.SqlConn beM)
connectOrFail cfg = L.getOrInitSqlConn cfg >>= \case
    Left e     -> error $ show e
    Right conn -> pure conn


