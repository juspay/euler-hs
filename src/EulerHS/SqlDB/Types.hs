{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE DeriveDataTypeable     #-}

module EulerHS.SqlDB.Types
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
  , DBConfig(..) -- NOTE: Ensure this is not exported publically. - Koz
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

import           Data.Data (Data)
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
import qualified Data.Text as T
import           EulerHS.Prelude
import           EulerHS.SqlDB.MySQL (MySQLConfig (..), createMySQLConn)
import           EulerHS.SqlDB.Postgres (PostgresConfig (..),
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
  rtDeleteReturningList :: forall table. (B.Beamable table, B.FromBackendRow be (table Identity)) => B.SqlDelete be table -> beM [table Identity]

-- TODO: move somewhere (it's implementation)
instance BeamRuntime BS.Sqlite BS.SqliteM where
  rtSelectReturningList = B.runSelectReturningList
  rtSelectReturningOne = B.runSelectReturningOne
  rtInsert = B.runInsert
  rtInsertReturningList = B.runInsertReturningList
  rtUpdate = B.runUpdate
  rtUpdateReturningList = error "Not implemented"
  rtDelete = B.runDelete
  rtDeleteReturningList = error "Not implemented"

-- TODO: move somewhere (it's implementation)
instance BeamRuntime BP.Postgres BP.Pg where
  rtSelectReturningList = B.runSelectReturningList
  rtSelectReturningOne = B.runSelectReturningOne
  rtInsert = B.runInsert
  rtInsertReturningList = B.runInsertReturningList
  rtUpdate = B.runUpdate
  rtUpdateReturningList = updateReturningListPG
  rtDelete = B.runDelete
  rtDeleteReturningList = deleteReturningListPG

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
  rtDeleteReturningList = error "Not implemented"

class BeamRunner beM where
  getBeamDebugRunner :: NativeSqlConn -> beM a -> ((Text -> IO ()) -> IO a)

instance BeamRunner BS.SqliteM where
  getBeamDebugRunner (NativeSQLiteConn conn) beM =
    \logger -> SQLite.runBeamSqliteDebug (logger . T.pack) conn beM
  getBeamDebugRunner _ _ = \_ -> error "Not a SQLite connection"

instance BeamRunner BP.Pg where
  getBeamDebugRunner (NativePGConn conn) beM =
    \logger -> BP.runBeamPostgresDebug (logger . T.pack) conn beM
  getBeamDebugRunner _ _ = \_ -> error "Not a Postgres connection"

instance BeamRunner BM.MySQLM where
  getBeamDebugRunner (NativeMySQLConn conn) beM =
    \logger -> BM.runBeamMySQLDebug logger conn beM
  getBeamDebugRunner _ _ = \_ -> error "Not a MySQL connection"

withTransaction :: forall beM a .
  SqlConn beM -> (NativeSqlConn -> IO a) -> IO (Either SomeException a)
withTransaction conn f = tryAny $ case conn of
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
  deriving stock (Show)

-- | Representation of native DB connections that we use in implementation.
data NativeSqlConn
  = NativePGConn BP.Connection
  | NativeMySQLConn MySQL.MySQLConn
  | NativeSQLiteConn SQLite.Connection

-- | Transform 'SqlConn' to 'NativeSqlPool'
bemToNative :: SqlConn beM -> NativeSqlPool
bemToNative = \case
  PostgresPool _ pool -> NativePGPool pool
  MySQLPool _ pool    -> NativeMySQLPool pool
  SQLitePool _ pool   -> NativeSQLitePool pool

-- | Create 'SqlConn' from 'DBConfig'
mkSqlConn :: DBConfig beM -> IO (SqlConn beM)
mkSqlConn = \case
  PostgresPoolConf connTag cfg PoolConfig {..} -> PostgresPool connTag
    <$> DP.createPool (createPostgresConn cfg) BP.close stripes keepAlive resourcesPerStripe
  MySQLPoolConf connTag cfg PoolConfig {..} -> MySQLPool connTag
    <$> DP.createPool (createMySQLConn cfg) MySQL.close stripes keepAlive resourcesPerStripe
  SQLitePoolConf connTag dbname PoolConfig {..} -> SQLitePool connTag
    <$> DP.createPool (SQLite.open dbname) SQLite.close stripes keepAlive resourcesPerStripe

-- | Tag for SQL connections
type ConnTag = Text

-- | Represents path to the SQLite DB
type SQliteDBname = String

-- | Represents SQL connection that we use in flow.
--   Parametrised by BEAM monad corresponding to the certain DB (MySQL, Postgres, SQLite)
data SqlConn (beM :: Type -> Type)
  = PostgresPool ConnTag (DP.Pool BP.Connection)
  -- ^ 'Pool' with Postgres connections
  | MySQLPool ConnTag (DP.Pool MySQL.MySQLConn)
  -- ^ 'Pool' with MySQL connections
  | SQLitePool ConnTag (DP.Pool SQLite.Connection)
  -- ^ 'Pool' with SQLite connections
  deriving stock (Generic)

-- | Represents DB configurations
data DBConfig (beM :: Type -> Type)
  = PostgresPoolConf ConnTag PostgresConfig PoolConfig
  -- ^ config for 'Pool' with Postgres connections
  | MySQLPoolConf ConnTag MySQLConfig PoolConfig
  -- ^ config for 'Pool' with MySQL connections
  | SQLitePoolConf ConnTag SQliteDBname PoolConfig
  -- ^ config for 'Pool' with SQlite connections
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Represents 'Pool' parameters
data PoolConfig = PoolConfig
  { stripes            :: Int
  -- ^ a number of sub-pools
  , keepAlive          :: NominalDiffTime
  -- ^ the amount of time the connection will be stored
  , resourcesPerStripe :: Int
  -- ^ maximum number of connections to be stored in each sub-pool
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
getDBName = \case
  PostgresPoolConf _ PostgresConfig{..} _ -> connectDatabase
  MySQLPoolConf _ MySQLConfig{..} _       -> connectDatabase
  SQLitePoolConf _ dbName _               -> dbName

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
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

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
toSqliteError _                              = SqliteErrorError

data SqliteSqlError
  = SqliteSqlError
    { sqlError        :: !SqliteError
    , sqlErrorDetails :: Text
    , sqlErrorContext :: Text
    }
    deriving stock (Show, Eq, Ord, Generic, Data)
    deriving anyclass (ToJSON, FromJSON)

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
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data MysqlSqlError =
  MysqlSqlError
  { errCode :: {-# UNPACK #-} !Word16,
    errMsg  :: {-# UNPACK #-} !Text
  }
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

toMysqlSqlError :: MySQL.ERR -> MysqlSqlError
toMysqlSqlError err = MysqlSqlError { errCode = MySQL.errCode err,
                                      errMsg = decodeUtf8 . MySQL.errMsg $ err }

mysqlErrorToDbError :: Text -> MySQL.ERRException -> DBError
mysqlErrorToDbError desc (MySQL.ERRException e) =
  DBError (SQLError . MysqlError . toMysqlSqlError $ e) desc

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
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

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
    deriving stock (Show, Eq, Ord, Generic, Data)
    deriving anyclass (ToJSON, FromJSON)

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
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

-- | Represents DB error
data DBError
  = DBError DBErrorType Text
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON, Exception)

-- | Represents resulting type for DB actions
type DBResult a = Either DBError a

-- | Transforms 'NativeSqlPool' to 'SqlConn'
nativeToBem :: ConnTag -> NativeSqlPool -> SqlConn beM
nativeToBem connTag = \case
  NativePGPool conn     -> PostgresPool connTag conn
  NativeMySQLPool conn  -> MySQLPool connTag conn
  NativeSQLitePool conn -> SQLitePool connTag conn
