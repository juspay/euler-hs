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

