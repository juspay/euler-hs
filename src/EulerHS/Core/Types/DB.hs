{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE RecordWildCards            #-}

{- |
Module      :  EulerHS.Core.Types.DB
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2021
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

-- TODO: separate runtime, implementation and public interface.
module EulerHS.Core.Types.DB
  (
    -- * Core DB
    -- ** Public types
    ConnTag
  , SQliteDBname
  , SqlConn(..)
  , DBConfig
  , PoolConfig(..)
  , DBErrorType(..)
  , DBError(..)
  , DBResult
  , PostgresSqlError(..)
  , PostgresExecStatus(..)
  , MysqlSqlError(..)
  , SqliteSqlError(..)
  , SqliteError(..)
  , SQLError(..)

  -- ** Private types
  , BeamRuntime(..)
  , BeamRunner(..)
  , NativeSqlPool(..)
  , NativeSqlConn(..)

  -- ** Public helpers
  , mkSqlConn
  , mkSQLiteConfig
  , mkSQLitePoolConfig
  , mkPostgresConfig
  , mkPostgresPoolConfig
  , mkMySQLConfig
  , mkMySQLPoolConfig
  , getDBName
  , deleteReturningListPG
  , updateReturningListPG
  , defaultPoolConfig

  -- ** Private helpers
  , bemToNative
  , nativeToBem
  , withTransaction
  , mysqlErrorToDbError
  , sqliteErrorToDbError
  , postgresErrorToDbError
  ) where

import           EulerHS.Prelude

import qualified Data.Pool as DP
import           Data.Time.Clock (NominalDiffTime)
import qualified Data.Text as T
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

-- * Public types and helpers

-- | Creates 'SqlConn' from 'DBConfig'.
--
-- You can use this function to prepare your DB connections before running a flow.
-- It's also possible to call this function during the flow evaluation
-- (using 'runIO' or 'runUntracedIO').
mkSqlConn :: DBConfig beM -> IO (SqlConn beM)
mkSqlConn (PostgresPoolConf connTag cfg PoolConfig {..}) = PostgresPool connTag
  <$> DP.createPool (createPostgresConn cfg) BP.close stripes keepAlive resourcesPerStripe

mkSqlConn (MySQLPoolConf connTag cfg PoolConfig {..}) = MySQLPool connTag
  <$> DP.createPool (createMySQLConn cfg) MySQL.close stripes keepAlive resourcesPerStripe

mkSqlConn (SQLitePoolConf connTag dbname PoolConfig {..}) = SQLitePool connTag
  <$> DP.createPool (SQLite.open dbname) SQLite.close stripes keepAlive resourcesPerStripe

mkSqlConn (MockConfig connTag) = pure $ MockedPool connTag

-- | Special version of DELETE query specified for Postgres.
-- TODO: unify this with other backends.
deleteReturningListPG
  :: (B.Beamable table, B.FromBackendRow BP.Postgres (table Identity))
  => B.SqlDelete BP.Postgres table
  -> BP.Pg [table Identity]
deleteReturningListPG = B.runDeleteReturningList

-- | Tag for SQL connections
type ConnTag = Text

-- | Represents path to the SQLite DB
type SQliteDBname = String

-- | Represents SQL connection that we use in flow.
--   Parametrised by BEAM monad corresponding to the certain DB (MySQL, Postgres, SQLite)
data SqlConn (beM :: Type -> Type)
  = MockedPool ConnTag
  -- ^ This mocked connection is not related to any DBs. Used in the ART system and tests.
  | PostgresPool ConnTag (DP.Pool BP.Connection)
  -- ^ 'Pool' with Postgres connections.
  | MySQLPool ConnTag (DP.Pool MySQL.MySQLConn)
  -- ^ 'Pool' with MySQL connections.
  | SQLitePool ConnTag (DP.Pool SQLite.Connection)
  -- ^ 'Pool' with SQLite connections.
  deriving (Generic)

-- | Represents DB configurations
data DBConfig (beM :: Type -> Type)
  = MockConfig ConnTag
  -- ^ This mocked configs is not related to any DBs. Used in the ART system and tests.
  | PostgresPoolConf ConnTag PostgresConfig PoolConfig
  -- ^ Config for 'Pool' with Postgres connections
  | MySQLPoolConf ConnTag MySQLConfig PoolConfig
  -- ^ Config for 'Pool' with MySQL connections
  | SQLitePoolConf ConnTag SQliteDBname PoolConfig
  -- ^ Config for 'Pool' with SQlite connections
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Represents 'Pool' parameters.
--
-- All the DB connections use a pool internally.
-- Configure pools according to your needs.
data PoolConfig = PoolConfig
  { stripes            :: Int
  -- ^ The number of stripes (distinct sub-pools) to maintain. The smallest acceptable value is 1.
  , keepAlive          :: NominalDiffTime
  -- ^ Amount of time for which an unused resource is kept open. The smallest acceptable value is 0.5 seconds.
  --
  -- The elapsed time before destroying a resource may be a little longer than requested, as the reaper thread wakes at 1-second intervals.
  --
  -- Conversion functions will treat it as seconds.
  -- For example, (0.010 :: NominalDiffTime) corresponds to 10 milliseconds.
  , resourcesPerStripe :: Int
  -- ^ Maximum number of resources to keep open per stripe. The smallest acceptable value is 1.
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Default pool config.
--
-- stripes = 1
-- keepAlive = 100 (seconds)
-- resourcesPerStripe = 1
--
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

-- | Obtains a DB name from 'DBConfig'.
--
-- For a mocked config, returns ConnTag as a DB name.
getDBName :: DBConfig beM -> String
getDBName (PostgresPoolConf _ (PostgresConfig{..}) _) = connectDatabase
getDBName (MySQLPoolConf _ (MySQLConfig{..}) _) = connectDatabase
getDBName (SQLitePoolConf _ dbName _) = dbName
getDBName (MockConfig tag) = T.unpack tag

----------------------------------------------------------------------

-- | Abstracted type for SQLite error types.
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

-- | Abstracted type for SQLite error.
data SqliteSqlError
  = SqliteSqlError
    { sqlError        :: !SqliteError
      -- ^ Error type
    , sqlErrorDetails :: Text
      -- ^ Additional error details
    , sqlErrorContext :: Text
      -- ^ Error context
    }
    deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Abstracted type for any errors occurring when dealing with the SQL DB subsystem.
data SQLError
  = PostgresError PostgresSqlError
    -- ^ Contains a Postgres abstracted error
  | MysqlError    MysqlSqlError
    -- ^ Contains a MySQL abstracted error
  | SqliteError   SqliteSqlError
    -- ^ Contains a SQLite abstracted error
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

----------------------------------------------------------------------

-- | Abstracted type for MySQL error.
data MysqlSqlError =
  MysqlSqlError
  { errCode :: {-# UNPACK #-} !Word16,
    errMsg :: {-# UNPACK #-} !Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

----------------------------------------------------------------------

-- | Abstracted type for Postgress exec status.
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

-- | Abstracted type for Postgress SQL error.
data PostgresSqlError =
  PostgresSqlError
    { sqlState       :: Text
    , sqlExecStatus  :: PostgresExecStatus
    , sqlErrorMsg    :: Text
    , sqlErrorDetail :: Text
    , sqlErrorHint   :: Text
    }
    deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

----------------------------------------------------------------------

-- | Represents different failures that the SQL subsystem may return
data DBErrorType
  = ConnectionFailed
    -- ^ Connection problem. Can be anything that causes the connection to break.
  | ConnectionAlreadyExists
    -- ^ This error indicates that the connection for this particular config already exist.
  | ConnectionDoesNotExist
    -- ^ This error indicates that the connection for this particular config is not found.
  | TransactionRollbacked
    -- ^ This error indicates about a rollbacked transaction.
    --
    -- (Not supported yet)
  | SQLError SQLError
    -- ^ Some specific error the DB backend has returned.
  | UnexpectedResult
    -- ^ An unexpected error happened in the SQL DB subsystem.
    --
    -- (Not supported yet)
  | UnrecognizedError
    -- ^ Unknown error from a native DB backend or from the SQL DB subsystem.
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Represents DB error
data DBError
  = DBError DBErrorType Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Represents resulting type for DB actions
type DBResult a = Either DBError a

-- * Internal types and functions

-- | This type class ties internal beam-related type classes
-- and implementation details.
--
-- In typical scenarios, you won't be needing this type class or its methods,
-- because the 'SqlDB' language provides a more high level interface to the
-- SQL DB subsystem.
--
-- It's not guaranteed that this type class will remain public.
--
-- This type class helps to support multiple DB backends.
-- 3 different backends are supported out of the box:
--
-- - SQLite
-- - MySQL
-- - Postgres
class (B.BeamSqlBackend be, B.MonadBeam be beM) => BeamRuntime be beM
  | be -> beM, beM -> be where
  rtSelectReturningList :: B.FromBackendRow be a => B.SqlSelect be a -> beM [a]
  rtSelectReturningOne  :: B.FromBackendRow be a => B.SqlSelect be a -> beM (Maybe a)
  rtInsert              :: B.SqlInsert be table -> beM ()
  rtInsertReturningList :: forall table . (B.Beamable table, B.FromBackendRow be (table Identity)) => B.SqlInsert be table -> beM [table Identity]
  rtUpdate              :: B.SqlUpdate be table -> beM ()
  rtUpdateReturningList :: forall table. (B.Beamable table, B.FromBackendRow be (table Identity)) => B.SqlUpdate be table -> beM [table Identity]
  rtDelete              :: B.SqlDelete be table -> beM ()

-- | Implements 'BeamRuntime' for SQLite.
instance BeamRuntime BS.Sqlite BS.SqliteM where
  rtSelectReturningList = B.runSelectReturningList
  rtSelectReturningOne = B.runSelectReturningOne
  rtInsert = B.runInsert
  rtInsertReturningList = B.runInsertReturningList
  rtUpdate = B.runUpdate
  rtUpdateReturningList = error "Not implemented"
  rtDelete = B.runDelete

-- | Implements 'BeamRuntime' for Postgres.
instance BeamRuntime BP.Postgres BP.Pg where
  rtSelectReturningList = B.runSelectReturningList
  rtSelectReturningOne = B.runSelectReturningOne
  rtInsert = B.runInsert
  rtInsertReturningList = B.runInsertReturningList
  rtUpdate = B.runUpdate
  rtUpdateReturningList = updateReturningListPG
  rtDelete = B.runDelete

-- | Implements 'BeamRuntime' for MySQL.
instance BeamRuntime BM.MySQL BM.MySQLM where
  rtSelectReturningList = B.runSelectReturningList
  rtSelectReturningOne = B.runSelectReturningOne
  rtInsert = B.runInsert
  rtInsertReturningList = error "Not implemented"
  rtUpdate = B.runUpdate
  rtUpdateReturningList = error "Not implemented"
  rtDelete = B.runDelete

-- | Special version of UPDATE query specified for Postgres.
-- TODO: unify this with other backends.
updateReturningListPG
  :: (B.Beamable table, B.FromBackendRow BP.Postgres (table Identity))
  => B.SqlUpdate BP.Postgres table
  -> BP.Pg [table Identity]
updateReturningListPG = B.runUpdateReturningList

-- | This type class ties native connections, beam and native SQL backends.
--
-- In typical scenarios, you won't be needing this type class or its methods,
-- because the 'SqlDB' language provides a more high level interface to the
-- SQL DB subsystem.
--
-- It's not guaranteed that this type class will remain public.
--
-- This type class helps to support multiple SQL backends.
-- 3 different backends are supported out of the box:
--
-- - SQLite
-- - MySQL
-- - Postgres
class BeamRunner beM where
  getBeamDebugRunner :: NativeSqlConn -> beM a -> ((Text -> IO ()) -> IO a)

-- | Implements 'BeamRunner' for SQLite.
instance BeamRunner BS.SqliteM where
  getBeamDebugRunner (NativeSQLiteConn conn) beM =
    \logger -> SQLite.runBeamSqliteDebug logger conn beM
  getBeamDebugRunner _ _ = \_ -> error "Not a SQLite connection"

-- | Implements 'BeamRunner' for Postgres.
instance BeamRunner BP.Pg where
  getBeamDebugRunner (NativePGConn conn) beM =
    \logger -> BP.runBeamPostgresDebug logger conn beM
  getBeamDebugRunner _ _ = \_ -> error "Not a Postgres connection"

-- | Implements 'BeamRunner' for MySQL.
instance BeamRunner BM.MySQLM where
  getBeamDebugRunner (NativeMySQLConn conn) beM =
    \logger -> BM.runBeamMySQLDebug logger conn beM
  getBeamDebugRunner _ _ = \_ -> error "Not a MySQL connection"

-- | Evaluates an action over a native connection within a native transaction.
-- All the backends have this support of transactions:
--
-- - SQLite
-- - MySQL
-- - Postgres
--
-- This is an internal function. Don't use it in the BL code.
withTransaction :: forall beM a .
  SqlConn beM -> (NativeSqlConn -> IO a) -> IO (Either SomeException a)
withTransaction conn f = case conn of
  MockedPool _ -> error "Mocked pool connections are not supported."
  PostgresPool _ pool -> DP.withResource pool (go PGS.withTransaction NativePGConn)
  MySQLPool _ pool -> DP.withResource pool (go MySQL.withTransaction NativeMySQLConn)
  SQLitePool _ pool -> DP.withResource pool (go SQLite.withTransaction NativeSQLiteConn)
  where
    go :: forall b . (b -> IO a -> IO a) -> (b -> NativeSqlConn) -> b -> IO (Either SomeException a)
    go hof wrap conn' = tryAny (hof conn' (f . wrap $ conn'))

-- | Representation of native DB pools that we store in FlowRuntime.
--
-- This is an internal type. Don't use it in the BL code.
data NativeSqlPool
  = NativePGPool     (DP.Pool BP.Connection)     -- ^ 'Pool' with Postgres connections
  | NativeMySQLPool  (DP.Pool MySQL.MySQLConn)   -- ^ 'Pool' with MySQL connections
  | NativeSQLitePool (DP.Pool SQLite.Connection) -- ^ 'Pool' with SQLite connections
  | NativeMockedPool
  deriving Show

-- | Representation of native DB connections that we use in the implementation.
--
-- This is an internal type. Don't use it in the BL code.
data NativeSqlConn
  = NativePGConn BP.Connection
  | NativeMySQLConn MySQL.MySQLConn
  | NativeSQLiteConn SQLite.Connection

-- | Transform 'SqlConn' to 'NativeSqlPool'.
--
-- This is an internal function. Don't use it in the BL code.
bemToNative :: SqlConn beM -> NativeSqlPool
bemToNative (MockedPool _)        = NativeMockedPool
bemToNative (PostgresPool _ pool) = NativePGPool pool
bemToNative (MySQLPool _ pool)    = NativeMySQLPool pool
bemToNative (SQLitePool _ pool)   = NativeSQLitePool pool

-- | Transforms 'NativeSqlPool' to 'SqlConn'.
--
-- This is an internal function. Don't use it in the BL code.
nativeToBem :: ConnTag -> NativeSqlPool -> SqlConn beM
nativeToBem connTag NativeMockedPool        = MockedPool connTag
nativeToBem connTag (NativePGPool conn)     = PostgresPool connTag conn
nativeToBem connTag (NativeMySQLPool conn)  = MySQLPool connTag conn
nativeToBem connTag (NativeSQLitePool conn) = SQLitePool connTag conn

----

-- | Transforms a native Postgres SQL error into an abstracted error type 'PostgresSqlError'.
--
-- This is an internal function. Don't use it in the BL code.
toPostgresSqlError :: PGS.SqlError -> PostgresSqlError
toPostgresSqlError e = PostgresSqlError
    { sqlState       = decodeUtf8 $ PGS.sqlState e
    , sqlExecStatus  = toPostgresExecStatus $ PGS.sqlExecStatus e
    , sqlErrorMsg    = decodeUtf8 $ PGS.sqlErrorMsg e
    , sqlErrorDetail = decodeUtf8 $ PGS.sqlErrorDetail e
    , sqlErrorHint   = decodeUtf8 $ PGS.sqlErrorHint e
    }

-- | Transforms a native Postgres SQL error into the general DB error type 'DBError'.
--
-- This is an internal function. Don't use it in the BL code.
postgresErrorToDbError :: Text -> PGS.SqlError -> DBError
postgresErrorToDbError descr e = DBError (SQLError $ PostgresError $ toPostgresSqlError e) descr

-- | Transforms a native Postgres exec status into an abstracted type 'PostgresExecStatus'.
--
-- This is an internal function. Don't use it in the BL code.
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

-- | Transforms a native SQLite SQL error type into an abstracted error type 'SqliteError'.
--
-- This is an internal function. Don't use it in the BL code.
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

-- | Transforms a native SQLite SQL error into an abstracted error type 'SqliteSqlError'.
--
-- This is an internal function. Don't use it in the BL code.
toSqliteSqlError :: SQLite.SQLError -> SqliteSqlError
toSqliteSqlError sqlErr = SqliteSqlError
    { sqlError        = toSqliteError $ SQLite.sqlError sqlErr
    , sqlErrorDetails = SQLite.sqlErrorDetails sqlErr
    , sqlErrorContext = SQLite.sqlErrorContext sqlErr
    }

-- | Transforms a native SQLite SQL error into the general DB error type 'DBError'.
--
-- This is an internal function. Don't use it in the BL code.
sqliteErrorToDbError :: Text -> SQLite.SQLError -> DBError
sqliteErrorToDbError descr e = DBError (SQLError $ SqliteError $ toSqliteSqlError e) descr

-- | Transforms a native MySQL error into an abstracted error type 'MysqlSqlError'.
--
-- This is an internal function. Don't use it in the BL code.
toMysqlSqlError :: MySQL.ERR -> MysqlSqlError
toMysqlSqlError err = MysqlSqlError { errCode = MySQL.errCode err,
                                      errMsg = decodeUtf8 . MySQL.errMsg $ err }

-- | Transforms a native MySQL error into the general DB error type 'DBError'.
--
-- This is an internal function. Don't use it in the BL code.
mysqlErrorToDbError :: Text -> MySQL.ERRException -> DBError
mysqlErrorToDbError desc (MySQL.ERRException e) =
  DBError (SQLError . MysqlError . toMysqlSqlError $ e) desc
