
{- |
Module      :  EulerHS.Types
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
This is a top module that reexports all the public types of the framework
along with some helper functions.
This module is better imported as qualified.
@
import qualified EulerHS.Types as T
import Database.Beam.MySQL (MySQLM)
mySQLDevConfig :: T.'DBConfig' MySQLM
mySQLDevConfig = T.'mkMySQLPoolConfig' "MySQL dev DB" cfg poolCfg
  where
    cfg :: T.'MySQLConfig'
    cfg = T.'defaultMySQLConfig'
      { T.connectPassword = "my pass"
      , T.connectDatabase = "my db"
      }
    poolCfg = T.'defaultPoolConfig'
      { T.keepAlive = 1000000
      }
@
-}

{-# LANGUAGE DeriveAnyClass #-}

module EulerHS.Types
  ( module X,
    HttpManagerNotFound(..),
    AwaitingError(..),
    Operation(..)
  ) where
import           EulerHS.Prelude
import           EulerHS.Api as X
import           EulerHS.BinaryString as X
import           EulerHS.Common as X
import           EulerHS.HttpAPI as X
import           EulerHS.KVDB.Types as X
import           EulerHS.Logger.Types as X
import           EulerHS.Masking as X
import           EulerHS.Options as X
import           EulerHS.SqlDB.MySQL as X
import           EulerHS.SqlDB.Postgres as X
import           EulerHS.SqlDB.Types as X hiding (withTransaction)

data Operation
  = CREATE
  | CREATE_RETURNING
  | UPDATE
  | UPDATE_RETURNING
  | UPDATE_ALL
  | UPDATE_ALL_RETURNING
  | FIND
  | FIND_ALL
  | FIND_ALL_WITH_OPTIONS
  | DELETE_ONE
  | DELETE_ONE_RETURNING
  | DELETE_ALL_RETURNING
  deriving (Generic, Show, Eq, ToJSON,FromJSON,Hashable,FromJSONKey,ToJSONKey)