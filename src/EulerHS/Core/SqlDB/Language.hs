{- |
Module      :  EulerHS.Core.SqlDB.Language
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable

Language of the SQL DB subsystem.

Uses `beam` as relational DBs connector.

This module is internal and should not imported in the projects.
Import 'EulerHS.Language' instead.
-}

{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

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
  , sqlThrowException -- for tests
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

  SqlThrowException :: (HasCallStack, Exception e) => e -> (a -> next) -> SqlDBMethodF beM next

instance Functor (SqlDBMethodF beM) where
  fmap f (SqlDBMethod runner next) = SqlDBMethod runner (f . next)
  fmap f (SqlThrowException message next) = SqlThrowException message (f . next)

-- | Wrapping helper
sqlDBMethod
  :: (HasCallStack, T.BeamRunner beM, T.BeamRuntime be beM)
  => beM a
  -> SqlDB beM a
sqlDBMethod act = liftFC $ SqlDBMethod (flip T.getBeamDebugRunner act) id

-- For testing purpose
sqlThrowException :: forall a e beM be . (HasCallStack, Exception e, T.BeamRunner beM, T.BeamRuntime be beM) => e -> SqlDB beM a
sqlThrowException ex = liftFC $ SqlThrowException ex id

-- Convenience interface

-- | Select many
findRows
  :: (HasCallStack, T.BeamRunner beM, T.BeamRuntime be beM, B.FromBackendRow be a)
  => B.SqlSelect be a
  -> SqlDB beM [a]
findRows = sqlDBMethod . T.rtSelectReturningList

-- | Select one
findRow
  :: (HasCallStack, T.BeamRunner beM, T.BeamRuntime be beM, B.FromBackendRow be a)
  => B.SqlSelect be a
  -> SqlDB beM (Maybe a)
findRow = sqlDBMethod . T.rtSelectReturningOne

-- | Insert
insertRows
  :: (HasCallStack, T.BeamRunner beM, T.BeamRuntime be beM)
  => B.SqlInsert be table
  -> SqlDB beM ()
insertRows = sqlDBMethod . T.rtInsert

-- | Insert returning list
insertRowsReturningList
  :: (HasCallStack, B.Beamable table, B.FromBackendRow be (table Identity), T.BeamRuntime be beM, T.BeamRunner beM)
  => B.SqlInsert be table
  -> SqlDB beM [table Identity]
insertRowsReturningList = sqlDBMethod . T.rtInsertReturningList

-- | Update
updateRows
  :: (HasCallStack, T.BeamRunner beM, T.BeamRuntime be beM)
  => B.SqlUpdate be table
  -> SqlDB beM ()
updateRows = sqlDBMethod . T.rtUpdate

-- | Update returning list
updateRowsReturningList
  :: (HasCallStack, T.BeamRunner beM, T.BeamRuntime be beM,
      B.Beamable table, B.FromBackendRow be (table Identity))
  => B.SqlUpdate be table
  -> SqlDB beM [table Identity]
updateRowsReturningList = sqlDBMethod . T.rtUpdateReturningList

-- | Delete
deleteRows
  :: (HasCallStack, T.BeamRunner beM, T.BeamRuntime be beM)
  => B.SqlDelete be table
  -> SqlDB beM ()
deleteRows = sqlDBMethod . T.rtDelete


-- Postgres only extra methods

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
