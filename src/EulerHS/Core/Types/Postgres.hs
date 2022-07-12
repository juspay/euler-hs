{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

{- |
Module      :  EulerHS.Core.Types.Postgres
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable

Types and helper functions to wrap a Postgres-related stuff.

This module is internal and should not imported in the projects.
Import 'EulerHS.Types' instead.
-}

module EulerHS.Core.Types.Postgres
  (
    -- * Core Postgres
    -- ** Types
    PostgresConfig(..)
    -- ** Methods
  , createPostgresConn
  , closePostgresConn
  ) where

import           EulerHS.Prelude

import qualified Database.Beam.Postgres as BP


data PostgresConfig = PostgresConfig
  { connectHost     :: String
  , connectPort     :: Word16
  , connectUser     :: String
  , connectPassword :: String
  , connectDatabase :: String
  } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Transform PostgresConfig to the Postgres ConnectInfo.
toBeamPostgresConnectInfo :: PostgresConfig -> BP.ConnectInfo
toBeamPostgresConnectInfo (PostgresConfig {..}) = BP.ConnectInfo {..}

-- | Connect with the given config to the database.
createPostgresConn :: PostgresConfig -> IO BP.Connection
createPostgresConn = BP.connect . toBeamPostgresConnectInfo

-- | Close the given connection.
closePostgresConn :: BP.Connection -> IO ()
closePostgresConn = BP.close

