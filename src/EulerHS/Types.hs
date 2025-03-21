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