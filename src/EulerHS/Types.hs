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

-- Beam imports
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

module EulerHS.Types
  ( module X
  ) where


import           EulerHS.Core.Types as X
