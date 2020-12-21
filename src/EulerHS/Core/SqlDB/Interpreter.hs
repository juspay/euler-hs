module EulerHS.Core.SqlDB.Interpreter
  (
  -- * SQL DB Interpreter
  runSqlDB
  ) where

import           EulerHS.Prelude

import qualified EulerHS.Core.Language as L
import qualified EulerHS.Core.Types as T


interpretSqlDBMethod
  :: T.NativeSqlConn
  -> (Text -> IO ())
  -> L.SqlDBMethodF beM a
  -> IO a
interpretSqlDBMethod conn logger (L.SqlDBMethod runner next) =
  next <$> runner conn logger

runSqlDB  :: T.NativeSqlConn -> (Text -> IO ()) -> L.SqlDB beM a -> IO a
runSqlDB sqlConn logger = foldF (interpretSqlDBMethod sqlConn logger)
