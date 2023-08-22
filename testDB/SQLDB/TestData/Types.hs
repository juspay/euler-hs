{-# OPTIONS_GHC -fclear-plugins #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module SQLDB.TestData.Types where

import qualified Database.Beam as B
import           Database.Beam.Backend.SQL (BeamSqlBackend)
import           EulerHS.Prelude
import qualified EulerHS.Types as T

-- sqlite3 db
-- CREATE TABLE users (id INTEGER PRIMARY KEY AUTOINCREMENT, first_name VARCHAR NOT NULL, last_name VARCHAR NOT NULL);
data UserT f = User
    { _userId        :: B.C f Int
    , _userFirstName :: B.C f Text
    , _userLastName  :: B.C f Text
    } deriving (Generic, B.Beamable)

instance B.Table UserT where
  data PrimaryKey UserT f =
    UserId (B.C f Int) deriving (Generic, B.Beamable)
  primaryKey = UserId . _userId

type User = UserT Identity

type UserId = B.PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User
deriving instance ToJSON User
deriving instance FromJSON User

newtype EulerDb f = EulerDb
    { _users :: f (B.TableEntity UserT)
    } deriving stock (Generic)
      deriving anyclass (B.Database be)

eulerDb :: B.DatabaseSettings be EulerDb
eulerDb = B.defaultDbSettings


data SqliteSequenceT f = SqliteSequence
    { _name :: B.C f Text
    , _seq  :: B.C f Int
    } deriving (Generic, B.Beamable)

instance B.Table SqliteSequenceT where
  data PrimaryKey SqliteSequenceT f =
    SqliteSequenceId (B.C f Text) deriving (Generic, B.Beamable)
  primaryKey = SqliteSequenceId . _name

type SqliteSequence = SqliteSequenceT Identity
type SqliteSequenceId = B.PrimaryKey SqliteSequenceT Identity

newtype SqliteSequenceDb f = SqliteSequenceDb
    { _sqlite_sequence :: f (B.TableEntity SqliteSequenceT)
    } deriving stock (Generic)
      deriving anyclass (B.Database be)

sqliteSequenceDb :: B.DatabaseSettings be SqliteSequenceDb
sqliteSequenceDb = B.defaultDbSettings

data SimpleUser = SimpleUser {first :: Text, last :: Text}

susers :: [SimpleUser]
susers =
  [ SimpleUser  "John" "Doe"
  , SimpleUser  "Doe" "John"
  ]

mkUser ::
  (BeamSqlBackend be,
    B.SqlValable (B.Columnar f Text),
    B.Columnar f Int ~ B.QGenExpr ctxt be s a,
    B.HaskellLiteralForQExpr (B.Columnar f Text) ~ Text) =>
    SimpleUser ->
    UserT f
mkUser (SimpleUser first' last') = User B.default_ (B.val_ first') (B.val_ last')

someUser :: Text -> Text -> T.DBResult (Maybe User) -> Bool
someUser f l (Right (Just u)) = _userFirstName u == f && _userLastName u == l
someUser _ _ _                = False

