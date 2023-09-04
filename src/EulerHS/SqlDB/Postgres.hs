{- |
Module      :  EulerHS.SqlDB.Postgres
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE RecordWildCards #-}

module EulerHS.SqlDB.Postgres
  (
    -- * Core Postgres
    -- ** Types
    PostgresConfig(..)
    -- ** Methods
  , createPostgresConn
  , closePostgresConn
  ) where

import qualified Database.Beam.Postgres as BP
import           EulerHS.Prelude

data PostgresConfig = PostgresConfig
  { connectHost     :: String
  , connectPort     :: Word16
  , connectUser     :: String
  , connectPassword :: String
  , connectDatabase :: String
  } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Transform PostgresConfig to the Postgres ConnectInfo.
toBeamPostgresConnectInfo :: PostgresConfig -> BP.ConnectInfo
toBeamPostgresConnectInfo PostgresConfig {..} = BP.ConnectInfo {..}

-- | Connect with the given config to the database.
createPostgresConn :: PostgresConfig -> IO BP.Connection
createPostgresConn = BP.connect . toBeamPostgresConnectInfo

-- | Close the given connection.
closePostgresConn :: BP.Connection -> IO ()
closePostgresConn = BP.close

