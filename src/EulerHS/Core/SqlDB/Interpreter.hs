{- |
Module      :  EulerHS.Core.SqlDB.Interpreter
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module EulerHS.Core.SqlDB.Interpreter
  (
  -- * SQL DB Interpreter
  runSqlDB
  ) where

import           EulerHS.Prelude

import qualified EulerHS.Core.Language as L
import qualified EulerHS.Core.Types as T

import           Control.Exception (throwIO)

interpretSqlDBMethod
  :: T.NativeSqlConn
  -> (Text -> IO ())
  -> L.SqlDBMethodF beM a
  -> IO a
interpretSqlDBMethod conn logger (L.SqlDBMethod runner next) =
  next <$> runner conn logger


interpretSqlDBMethod _ _ (L.SqlThrowException ex next) = do
  next <$> throwIO ex

runSqlDB  :: T.NativeSqlConn -> (Text -> IO ()) -> L.SqlDB beM a -> IO a
runSqlDB sqlConn logger = foldF (interpretSqlDBMethod sqlConn logger)
