{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

{- |
Module      :  EulerHS.Core.SqlDB.Language
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2021
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable

Language of the SQL DB subsystem.

Uses `beam` as relational DBs connector.

This module is internal and should not imported in the projects.
Import 'EulerHS.Language' instead.
-}

module EulerHS.Core.SqlDB.Language
  (
  -- * SQLDB language
  -- ** Types
    SqlDB
  , SqlDBMethodF(..)
  -- ** Methods
  , findRow
  , findRows
  , insertRows
  , insertRowsReturningList
  , updateRows
  , updateRowsReturningList
  , deleteRows
  , deleteRowsReturningListPG
  , updateRowsReturningListPG
  , insertRowReturningMySQL
  ) where

import qualified Database.Beam as B
import qualified Database.Beam.MySQL as BM
import qualified Database.Beam.Postgres as BP
import qualified EulerHS.Core.Types as T
import           EulerHS.Prelude

-- | Language of the SQL DB subsytem.
type SqlDB beM = F (SqlDBMethodF beM)

-- | Algebra of the SQL DB subsytem.
data SqlDBMethodF (beM :: Type -> Type) next where
  SqlDBMethod :: HasCallStack => (T.NativeSqlConn -> (Text -> IO ()) -> IO a) -> (a -> next) -> SqlDBMethodF beM next

instance Functor (SqlDBMethodF beM) where
  fmap f (SqlDBMethod runner next) = SqlDBMethod runner (f . next)

-- | Wrapping helper
sqlDBMethod
  :: (HasCallStack, T.BeamRunner beM)
  => beM a
  -> SqlDB beM a
sqlDBMethod act = liftFC $ SqlDBMethod (flip T.getBeamDebugRunner act) id

-- Convenience interface

-- | Select many rows query
findRows
  :: (HasCallStack, T.BeamRunner beM, T.BeamRuntime be beM, B.FromBackendRow be a)
  => B.SqlSelect be a
  -> SqlDB beM [a]
findRows = sqlDBMethod . T.rtSelectReturningList

-- | Select one row query
findRow
  :: (HasCallStack, T.BeamRunner beM, T.BeamRuntime be beM, B.FromBackendRow be a)
  => B.SqlSelect be a
  -> SqlDB beM (Maybe a)
findRow = sqlDBMethod . T.rtSelectReturningOne

-- | Insert query
insertRows
  :: (HasCallStack, T.BeamRunner beM, T.BeamRuntime be beM)
  => B.SqlInsert be table
  -> SqlDB beM ()
insertRows = sqlDBMethod . T.rtInsert

-- | Insert returning list query
insertRowsReturningList
  :: (HasCallStack, B.Beamable table, B.FromBackendRow be (table Identity), T.BeamRuntime be beM, T.BeamRunner beM)
  => B.SqlInsert be table
  -> SqlDB beM [table Identity]
insertRowsReturningList = sqlDBMethod . T.rtInsertReturningList

-- | Update query
updateRows
  :: (HasCallStack, T.BeamRunner beM, T.BeamRuntime be beM)
  => B.SqlUpdate be table
  -> SqlDB beM ()
updateRows = sqlDBMethod . T.rtUpdate

-- | Update returning list query
updateRowsReturningList
  :: (HasCallStack, T.BeamRunner beM, T.BeamRuntime be beM,
      B.Beamable table, B.FromBackendRow be (table Identity))
  => B.SqlUpdate be table
  -> SqlDB beM [table Identity]
updateRowsReturningList = sqlDBMethod . T.rtUpdateReturningList

-- | Delete query
deleteRows
  :: (HasCallStack, T.BeamRunner beM, T.BeamRuntime be beM)
  => B.SqlDelete be table
  -> SqlDB beM ()
deleteRows = sqlDBMethod . T.rtDelete


-- Postgres-only extra methods

-- | Postgres-only DELETE query (returning list)
deleteRowsReturningListPG
  :: (HasCallStack, B.Beamable table, B.FromBackendRow BP.Postgres (table Identity))
  => B.SqlDelete BP.Postgres table
  -> SqlDB BP.Pg [table Identity]
deleteRowsReturningListPG = sqlDBMethod . T.deleteReturningListPG

-- | Postgres-only UPDATE query (returning list)
updateRowsReturningListPG
  :: (HasCallStack, B.Beamable table, B.FromBackendRow BP.Postgres (table Identity))
  => B.SqlUpdate BP.Postgres table
  -> SqlDB BP.Pg [table Identity]
updateRowsReturningListPG = sqlDBMethod . T.updateReturningListPG

-- MySQL only extra methods
-- NOTE: This should be run inside a SQL transaction!

-- | MySQL-only INSERT query (returning list)
--
-- NOTE: This should be run inside a SQL transaction!
insertRowReturningMySQL :: (HasCallStack, B.FromBackendRow BM.MySQL (table Identity))
                        => B.SqlInsert BM.MySQL table
                        -> SqlDB BM.MySQLM (Maybe (table Identity))
insertRowReturningMySQL =
    sqlDBMethod . BM.runInsertRowReturning
