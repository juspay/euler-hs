{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE RecordWildCards            #-}

{- |
Module      :  EulerHS.Core.Types.DB
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable

This module contains general DB-related types and helper functions.

This module is internal and should not imported in the projects.

Import 'EulerHS.Types' instead.

Types and helpers for specific databases can be found in separate modules:

'EulerHS.Core.Types.MySQL'
'EulerHS.Core.Types.Postgres'
-}

module EulerHS.Core.Types.DB
  (
    -- * Core DB
    -- ** Types
    BeamRuntime(..)
  , deleteReturningListPG
  , updateReturningListPG
  , BeamRunner(..)
  , NativeSqlPool(..)
  , NativeSqlConn(..)
  , ConnTag
  , SQliteDBname
  , SqlConn(..)
  , DBConfig
  , PoolConfig(..)
  , DBErrorType(..)
  , DBError(..)
  , DBResult
  -- ** Methods
  , bemToNative
  , nativeToBem
  , mkSqlConn
  , mkSQLiteConfig
  , mkSQLitePoolConfig
  , mkPostgresConfig
  , mkPostgresPoolConfig
  , mkMySQLConfig
  , mkMySQLPoolConfig
  , getDBName
  -- ** Helpers
  , withTransaction
  , mysqlErrorToDbError
  , sqliteErrorToDbError
  , postgresErrorToDbError
  , PostgresSqlError(..)
  , PostgresExecStatus(..)
  , MysqlSqlError(..)
  , SqliteSqlError(..)
  , SqliteError(..)
  , SQLError(..)
  ) where

import           EulerHS.Prelude

import qualified Data.Pool as DP
import           Data.Time.Clock (NominalDiffTime)
import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL as B
import qualified Database.Beam.Backend.SQL.BeamExtensions as B
import qualified Database.Beam.MySQL as BM
import qualified Database.Beam.Postgres as BP
import qualified Database.Beam.Sqlite as BS
import qualified Database.Beam.Sqlite.Connection as SQLite
import qualified Database.MySQL.Base as MySQL
import qualified Database.PostgreSQL.Simple as PGS
import qualified Database.SQLite.Simple as SQLite

import           EulerHS.Core.Types.MySQL (MySQLConfig(..), createMySQLConn)
import           EulerHS.Core.Types.Postgres (PostgresConfig(..),
                                              createPostgresConn)



class (B.BeamSqlBackend be, B.MonadBeam be beM) => BeamRuntime be beM
  | be -> beM, beM -> be where
  rtSelectReturningList :: B.FromBackendRow be a => B.SqlSelect be a -> beM [a]
  rtSelectReturningOne  :: B.FromBackendRow be a => B.SqlSelect be a -> beM (Maybe a)
  rtInsert              :: B.SqlInsert be table -> beM ()
  rtInsertReturningList :: forall table . (B.Beamable table, B.FromBackendRow be (table Identity)) => B.SqlInsert be table -> beM [table Identity]
  rtUpdate              :: B.SqlUpdate be table -> beM ()
  rtUpdateReturningList :: forall table. (B.Beamable table, B.FromBackendRow be (table Identity)) => B.SqlUpdate be table -> beM [table Identity]
  rtDelete              :: B.SqlDelete be table -> beM ()

-- TODO: move somewhere (it's implementation)
instance BeamRuntime BS.Sqlite BS.SqliteM where
  rtSelectReturningList = B.runSelectReturningList
  rtSelectReturningOne = B.runSelectReturningOne
  rtInsert = B.runInsert
  rtInsertReturningList = B.runInsertReturningList
  rtUpdate = B.runUpdate
  rtUpdateReturningList = error "Not implemented"
  rtDelete = B.runDelete

-- TODO: move somewhere (it's implementation)
instance BeamRuntime BP.Postgres BP.Pg where
  rtSelectReturningList = B.runSelectReturningList
  rtSelectReturningOne = B.runSelectReturningOne
  rtInsert = B.runInsert
  rtInsertReturningList = B.runInsertReturningList
  rtUpdate = B.runUpdate
  rtUpdateReturningList = updateReturningListPG
  rtDelete = B.runDelete

deleteReturningListPG
  :: (B.Beamable table, B.FromBackendRow BP.Postgres (table Identity))
  => B.SqlDelete BP.Postgres table
  -> BP.Pg [table Identity]
deleteReturningListPG = B.runDeleteReturningList


updateReturningListPG
  :: (B.Beamable table, B.FromBackendRow BP.Postgres (table Identity))
  => B.SqlUpdate BP.Postgres table
  -> BP.Pg [table Identity]
updateReturningListPG = B.runUpdateReturningList

instance BeamRuntime BM.MySQL BM.MySQLM where
  rtSelectReturningList = B.runSelectReturningList
  rtSelectReturningOne = B.runSelectReturningOne
  rtInsert = B.runInsert
  rtInsertReturningList = error "Not implemented"
  rtUpdate = B.runUpdate
  rtUpdateReturningList = error "Not implemented"
  rtDelete = B.runDelete

class BeamRunner beM where
  getBeamDebugRunner :: NativeSqlConn -> beM a -> ((Text -> IO ()) -> IO a)


instance BeamRunner BS.SqliteM where
  getBeamDebugRunner (NativeSQLiteConn conn) beM =
    \logger -> SQLite.runBeamSqliteDebug logger conn beM
  getBeamDebugRunner _ _ = \_ -> error "Not a SQLite connection"


instance BeamRunner BP.Pg where
  getBeamDebugRunner (NativePGConn conn) beM =
    \logger -> BP.runBeamPostgresDebug logger conn beM
  getBeamDebugRunner _ _ = \_ -> error "Not a Postgres connection"

instance BeamRunner BM.MySQLM where
  getBeamDebugRunner (NativeMySQLConn conn) beM =
    \logger -> BM.runBeamMySQLDebug logger conn beM
  getBeamDebugRunner _ _ = \_ -> error "Not a MySQL connection"

withTransaction :: forall beM a .
  SqlConn beM -> (NativeSqlConn -> IO a) -> IO (Either SomeException a)
withTransaction conn f = tryAny $ case conn of
  MockedPool _ -> error "Mocked pool connections are not supported."
  PostgresPool _ pool -> DP.withResource pool (go PGS.withTransaction NativePGConn)
  MySQLPool _ pool -> DP.withResource pool (go MySQL.withTransaction NativeMySQLConn)
  SQLitePool _ pool -> DP.withResource pool (go SQLite.withTransaction NativeSQLiteConn)
  where
    go :: forall b . (b -> IO a -> IO a) -> (b -> NativeSqlConn) -> b -> IO a
    go hof wrap conn' = hof conn' (f . wrap $ conn')

-- | Representation of native DB pools that we store in FlowRuntime
data NativeSqlPool
  = NativePGPool (DP.Pool BP.Connection)         -- ^ 'Pool' with Postgres connections
  | NativeMySQLPool (DP.Pool MySQL.MySQLConn)   -- ^ 'Pool' with MySQL connections
  | NativeSQLitePool (DP.Pool SQLite.Connection) -- ^ 'Pool' with SQLite connections
  | NativeMockedPool

instance Show NativeSqlPool

-- | Representation of native DB connections that we use in implementation.
data NativeSqlConn
  = NativePGConn BP.Connection
  | NativeMySQLConn MySQL.MySQLConn
  | NativeSQLiteConn SQLite.Connection

-- | Transform 'SqlConn' to 'NativeSqlPool'
bemToNative :: SqlConn beM -> NativeSqlPool
bemToNative (MockedPool _)        = NativeMockedPool
bemToNative (PostgresPool _ pool) = NativePGPool pool
bemToNative (MySQLPool _ pool)    = NativeMySQLPool pool
bemToNative (SQLitePool _ pool)   = NativeSQLitePool pool

-- | Create 'SqlConn' from 'DBConfig'
mkSqlConn :: DBConfig beM -> IO (SqlConn beM)
mkSqlConn (PostgresPoolConf connTag cfg PoolConfig {..}) =  PostgresPool connTag
  <$> DP.createPool (createPostgresConn cfg) BP.close stripes keepAlive resourcesPerStripe

mkSqlConn (MySQLPoolConf connTag cfg PoolConfig {..}) =  MySQLPool connTag
  <$> DP.createPool (createMySQLConn cfg) MySQL.close stripes keepAlive resourcesPerStripe

mkSqlConn (SQLitePoolConf connTag dbname PoolConfig {..}) =  SQLitePool connTag
  <$> DP.createPool (SQLite.open dbname) SQLite.close stripes keepAlive resourcesPerStripe

mkSqlConn (MockConfig connTag) = pure $ MockedPool connTag


-- | Tag for SQL connections
type ConnTag = Text

-- | Represents path to the SQLite DB
type SQliteDBname = String

-- | Represents SQL connection that we use in flow.
--   Parametrised by BEAM monad corresponding to the certain DB (MySQL, Postgres, SQLite)
data SqlConn (beM :: Type -> Type)
  = MockedPool ConnTag
  | PostgresPool ConnTag (DP.Pool BP.Connection)
  -- ^ 'Pool' with Postgres connections
  | MySQLPool ConnTag (DP.Pool MySQL.MySQLConn)
  -- ^ 'Pool' with MySQL connections
  | SQLitePool ConnTag (DP.Pool SQLite.Connection)
  -- ^ 'Pool' with SQLite connections
  deriving (Generic)


-- | Represents DB configurations
data DBConfig (beM :: Type -> Type)
  = MockConfig ConnTag
  | PostgresPoolConf ConnTag PostgresConfig PoolConfig
  -- ^ config for 'Pool' with Postgres connections
  | MySQLPoolConf ConnTag MySQLConfig PoolConfig
  -- ^ config for 'Pool' with MySQL connections
  | SQLitePoolConf ConnTag SQliteDBname PoolConfig
  -- ^ config for 'Pool' with SQlite connections
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Represents 'Pool' parameters
data PoolConfig = PoolConfig
  { stripes            :: Int
  -- ^ a number of sub-pools
  , keepAlive          :: NominalDiffTime
  -- ^ the amount of time the connection will be stored
  , resourcesPerStripe :: Int
  -- ^ maximum number of connections to be stored in each sub-pool
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

defaultPoolConfig :: PoolConfig
defaultPoolConfig = PoolConfig
  { stripes = 1
  , keepAlive = 100
  , resourcesPerStripe = 1
  }

-- | Create SQLite 'DBConfig'
mkSQLiteConfig :: ConnTag -> SQliteDBname -> DBConfig BS.SqliteM
mkSQLiteConfig connTag dbName = SQLitePoolConf connTag dbName defaultPoolConfig

-- | Create SQLite 'Pool' 'DBConfig'
mkSQLitePoolConfig :: ConnTag -> SQliteDBname -> PoolConfig -> DBConfig BS.SqliteM
mkSQLitePoolConfig = SQLitePoolConf

-- | Create Postgres 'DBConfig'
mkPostgresConfig :: ConnTag -> PostgresConfig -> DBConfig BP.Pg
mkPostgresConfig connTag dbName = PostgresPoolConf connTag dbName defaultPoolConfig

-- | Create Postgres 'Pool' 'DBConfig'
mkPostgresPoolConfig :: ConnTag -> PostgresConfig -> PoolConfig -> DBConfig BP.Pg
mkPostgresPoolConfig = PostgresPoolConf

-- | Create MySQL 'DBConfig'
mkMySQLConfig :: ConnTag -> MySQLConfig -> DBConfig BM.MySQLM
mkMySQLConfig connTag dbName = MySQLPoolConf connTag dbName defaultPoolConfig

-- | Create MySQL 'Pool' 'DBConfig'
mkMySQLPoolConfig :: ConnTag -> MySQLConfig -> PoolConfig -> DBConfig BM.MySQLM
mkMySQLPoolConfig = MySQLPoolConf

getDBName :: DBConfig beM -> String
getDBName (PostgresPoolConf _ (PostgresConfig{..}) _) = connectDatabase
getDBName (MySQLPoolConf _ (MySQLConfig{..}) _) = connectDatabase
getDBName (SQLitePoolConf _ dbName _) = dbName
getDBName (MockConfig _) = error "Can't get DB name of MockConfig"

----------------------------------------------------------------------

data SqliteError
  = SqliteErrorOK
  | SqliteErrorError
  | SqliteErrorInternal
  | SqliteErrorPermission
  | SqliteErrorAbort
  | SqliteErrorBusy
  | SqliteErrorLocked
  | SqliteErrorNoMemory
  | SqliteErrorReadOnly
  | SqliteErrorInterrupt
  | SqliteErrorIO
  | SqliteErrorCorrupt
  | SqliteErrorNotFound
  | SqliteErrorFull
  | SqliteErrorCantOpen
  | SqliteErrorProtocol
  | SqliteErrorEmpty
  | SqliteErrorSchema
  | SqliteErrorTooBig
  | SqliteErrorConstraint
  | SqliteErrorMismatch
  | SqliteErrorMisuse
  | SqliteErrorNoLargeFileSupport
  | SqliteErrorAuthorization
  | SqliteErrorFormat
  | SqliteErrorRange
  | SqliteErrorNotADatabase
  | SqliteErrorNotice
  | SqliteErrorWarning
  | SqliteErrorRow
  | SqliteErrorDone
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

toSqliteError :: SQLite.Error -> SqliteError
toSqliteError SQLite.ErrorOK                 = SqliteErrorOK
toSqliteError SQLite.ErrorError              = SqliteErrorError
toSqliteError SQLite.ErrorInternal           = SqliteErrorInternal
toSqliteError SQLite.ErrorPermission         = SqliteErrorPermission
toSqliteError SQLite.ErrorAbort              = SqliteErrorAbort
toSqliteError SQLite.ErrorBusy               = SqliteErrorBusy
toSqliteError SQLite.ErrorLocked             = SqliteErrorLocked
toSqliteError SQLite.ErrorNoMemory           = SqliteErrorNoMemory
toSqliteError SQLite.ErrorReadOnly           = SqliteErrorReadOnly
toSqliteError SQLite.ErrorInterrupt          = SqliteErrorInterrupt
toSqliteError SQLite.ErrorIO                 = SqliteErrorIO
toSqliteError SQLite.ErrorCorrupt            = SqliteErrorCorrupt
toSqliteError SQLite.ErrorNotFound           = SqliteErrorNotFound
toSqliteError SQLite.ErrorFull               = SqliteErrorFull
toSqliteError SQLite.ErrorCan'tOpen          = SqliteErrorCantOpen
toSqliteError SQLite.ErrorProtocol           = SqliteErrorProtocol
toSqliteError SQLite.ErrorEmpty              = SqliteErrorEmpty
toSqliteError SQLite.ErrorSchema             = SqliteErrorSchema
toSqliteError SQLite.ErrorTooBig             = SqliteErrorTooBig
toSqliteError SQLite.ErrorConstraint         = SqliteErrorConstraint
toSqliteError SQLite.ErrorMismatch           = SqliteErrorMismatch
toSqliteError SQLite.ErrorMisuse             = SqliteErrorMisuse
toSqliteError SQLite.ErrorNoLargeFileSupport = SqliteErrorNoLargeFileSupport
toSqliteError SQLite.ErrorAuthorization      = SqliteErrorAuthorization
toSqliteError SQLite.ErrorFormat             = SqliteErrorFormat
toSqliteError SQLite.ErrorRange              = SqliteErrorRange
toSqliteError SQLite.ErrorNotADatabase       = SqliteErrorNotADatabase
toSqliteError SQLite.ErrorNotice             = SqliteErrorNotice
toSqliteError SQLite.ErrorWarning            = SqliteErrorWarning
toSqliteError SQLite.ErrorRow                = SqliteErrorRow
toSqliteError SQLite.ErrorDone               = SqliteErrorDone

data SqliteSqlError
  = SqliteSqlError
    { sqlError        :: !SqliteError
    , sqlErrorDetails :: Text
    , sqlErrorContext :: Text
    }
    deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

toSqliteSqlError :: SQLite.SQLError -> SqliteSqlError
toSqliteSqlError sqlErr = SqliteSqlError
    { sqlError        = toSqliteError $ SQLite.sqlError sqlErr
    , sqlErrorDetails = SQLite.sqlErrorDetails sqlErr
    , sqlErrorContext = SQLite.sqlErrorContext sqlErr
    }

sqliteErrorToDbError :: Text -> SQLite.SQLError -> DBError
sqliteErrorToDbError descr e = DBError (SQLError $ SqliteError $ toSqliteSqlError e) descr

data SQLError
  = PostgresError PostgresSqlError
  | MysqlError    MysqlSqlError
  | SqliteError   SqliteSqlError
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

----------------------------------------------------------------------

data MysqlSqlError =
  MysqlSqlError
  { errCode :: {-# UNPACK #-} !Word16,
    errMsg :: {-# UNPACK #-} !Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

toMysqlSqlError :: MySQL.ERR -> MysqlSqlError
toMysqlSqlError err = MysqlSqlError { errCode = MySQL.errCode err,
                                      errMsg = decodeUtf8 . MySQL.errMsg $ err }

mysqlErrorToDbError :: Text -> MySQL.ERRException -> DBError
mysqlErrorToDbError desc (MySQL.ERRException e) =
  DBError (SQLError . MysqlError . toMysqlSqlError $ e) desc

----------------------------------------------------------------------

data PostgresExecStatus
  = PostgresEmptyQuery
  | PostgresCommandOk
  | PostgresTuplesOk
  | PostgresCopyOut
  | PostgresCopyIn
  | PostgresCopyBoth
  | PostgresBadResponse
  | PostgresNonfatalError
  | PostgresFatalError
  | PostgresSingleTuple
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


toPostgresExecStatus :: PGS.ExecStatus -> PostgresExecStatus
toPostgresExecStatus PGS.EmptyQuery    = PostgresEmptyQuery
toPostgresExecStatus PGS.CommandOk     = PostgresCommandOk
toPostgresExecStatus PGS.TuplesOk      = PostgresTuplesOk
toPostgresExecStatus PGS.CopyOut       = PostgresCopyOut
toPostgresExecStatus PGS.CopyIn        = PostgresCopyIn
toPostgresExecStatus PGS.CopyBoth      = PostgresCopyBoth
toPostgresExecStatus PGS.BadResponse   = PostgresBadResponse
toPostgresExecStatus PGS.NonfatalError = PostgresNonfatalError
toPostgresExecStatus PGS.FatalError    = PostgresFatalError
toPostgresExecStatus PGS.SingleTuple   = PostgresSingleTuple


data PostgresSqlError =
  PostgresSqlError
    { sqlState       :: Text
    , sqlExecStatus  :: PostgresExecStatus
    , sqlErrorMsg    :: Text
    , sqlErrorDetail :: Text
    , sqlErrorHint   :: Text
    }
    deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


toPostgresSqlError :: PGS.SqlError -> PostgresSqlError
toPostgresSqlError e = PostgresSqlError
    { sqlState       = decodeUtf8 $ PGS.sqlState e
    , sqlExecStatus  = toPostgresExecStatus $ PGS.sqlExecStatus e
    , sqlErrorMsg    = decodeUtf8 $ PGS.sqlErrorMsg e
    , sqlErrorDetail = decodeUtf8 $ PGS.sqlErrorDetail e
    , sqlErrorHint   = decodeUtf8 $ PGS.sqlErrorHint e
    }


postgresErrorToDbError :: Text -> PGS.SqlError -> DBError
postgresErrorToDbError descr e = DBError (SQLError $ PostgresError $ toPostgresSqlError e) descr

----------------------------------------------------------------------


-- TODO: more informative typed error.
-- | Represents failures that may occur while working with the database
data DBErrorType
  = ConnectionFailed
  | ConnectionAlreadyExists
  | ConnectionDoesNotExist
  | TransactionRollbacked
  | SQLError SQLError
  | UnexpectedResult
  | UnrecognizedError
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Represents DB error
data DBError
  = DBError DBErrorType Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Represents resulting type for DB actions
type DBResult a = Either DBError a


-- | Transforms 'NativeSqlPool' to 'SqlConn'
nativeToBem :: ConnTag -> NativeSqlPool -> SqlConn beM
nativeToBem connTag NativeMockedPool        = MockedPool connTag
nativeToBem connTag (NativePGPool conn)     = PostgresPool connTag conn
nativeToBem connTag (NativeMySQLPool conn)  = MySQLPool connTag conn
nativeToBem connTag (NativeSQLitePool conn) = SQLitePool connTag conn
