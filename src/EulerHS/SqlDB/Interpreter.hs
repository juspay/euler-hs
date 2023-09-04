{- |
Module      :  EulerHS.SqlDB.Interpreter
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable

This module contains interpreters and methods for running `SqlDB` scenarios.
Import this module qualified as `SqlDB`.
-}

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
