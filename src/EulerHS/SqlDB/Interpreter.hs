module EulerHS.SqlDB.Interpreter
  (
  -- * SQL DB Interpreter
  runSqlDB
  ) where

import           Control.Exception (throwIO)
import           EulerHS.Prelude
import           EulerHS.SqlDB.Language (SqlDB,
                                         SqlDBMethodF (SqlDBMethod, SqlThrowException))
import qualified EulerHS.SqlDB.Types as T

-- TODO: The runner runner gets composed in in `sqlDBMethod`. Move it into the interpreter!
interpretSqlDBMethod
  :: T.NativeSqlConn
  -> (Text -> IO ())
  -> SqlDBMethodF beM a
  -> IO a
interpretSqlDBMethod conn logger = \case
  SqlDBMethod runner next   -> next <$> runner conn logger
  SqlThrowException ex next -> next <$> throwIO ex

runSqlDB  :: T.NativeSqlConn -> (Text -> IO ()) -> SqlDB beM a -> IO a
runSqlDB sqlConn logger = foldF (interpretSqlDBMethod sqlConn logger)
