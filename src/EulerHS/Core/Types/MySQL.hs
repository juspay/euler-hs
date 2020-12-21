{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass     #-}

{- |
Module      :  EulerHS.Core.Types.MySQL
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2021
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable

Types and helper functions to wrap a MySQL-related stuff.

This module is internal and should not imported in the projects.
Import 'EulerHS.Types' instead.
-}

module EulerHS.Core.Types.MySQL
  (
    -- * Core MySQL
    -- ** Types
    MySQLConfig(..)
  , MySqlOption(..)
    -- ** Methods
  , createMySQLConn
  , closeMySQLConn
    -- ** Defaults
  , defaultMySQLConfig
  ) where

import Prelude
import Data.Word (Word16)
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Database.MySQL.Base (MySQLConn, close, ConnectInfo (..), connect, defaultConnectInfoMB4)
import Data.ByteString.UTF8 (fromString)

-- | MySQL connection protocol
data MySqlProtocol
  = TCP
  | Socket
  | Pipe
  | Memory
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | MySQL options
data MySqlOption
  = ConnectTimeout Word
  | Compress
  | NamedPipe
  -- | InitCommand ByteString       -- TODO
  | ReadDefaultFile FilePath
  -- | ReadDefaultGroup ByteString  -- TODO
  | CharsetDir FilePath
  | CharsetName String
  | LocalInFile Bool
  | Protocol MySqlProtocol
  -- | SharedMemoryBaseName ByteString  -- TODO
  | ReadTimeout Word
  | WriteTimeout Word
  -- | UseRemoteConnection
  -- | UseEmbeddedConnection
  -- | GuessConnection
  -- | ClientIP ByteString
  | SecureAuth Bool
  | ReportDataTruncation Bool
  | Reconnect Bool
  -- | SSLVerifyServerCert Bool
  | FoundRows
  | IgnoreSIGPIPE
  | IgnoreSpace
  | Interactive
  | LocalFiles
  | MultiResults
  | MultiStatements
  | NoSchema
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Auth credentials
data SSLInfo = SSLInfo
  { sslKey     :: FilePath
  , sslCert    :: FilePath
  , sslCA      :: FilePath
  , sslCAPath  :: FilePath
  , sslCiphers :: String
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | MySQL config
data MySQLConfig = MySQLConfig
  { connectHost     :: String
  , connectPort     :: Word16
  , connectUser     :: String
  , connectPassword :: String
  , connectDatabase :: String
  , connectOptions  :: [MySqlOption]
  , connectPath     :: FilePath
  , connectSSL      :: Maybe SSLInfo
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | Default MySQL config.

connectHost = "127.0.0.1"
connectPort = 3306
connectUser = "root"
connectPassword = ""
connectDatabase = "test"
connectOptions = [CharsetName "utf8"]
connectPath = ""
connectSSL = Nothing
-}
defaultMySQLConfig :: MySQLConfig
defaultMySQLConfig = MySQLConfig
  { connectHost = "127.0.0.1"
  , connectPort = 3306
  , connectUser = "root"
  , connectPassword = ""
  , connectDatabase = "test"
  , connectOptions = [CharsetName "utf8"]
  , connectPath = ""
  , connectSSL = Nothing
  }

-- | Connect with the given config to the database.
createMySQLConn :: MySQLConfig -> IO MySQLConn
createMySQLConn conf = do
  let dbConf = ConnectInfo
        { ciHost = connectHost conf
        , ciPort = fromIntegral . connectPort $ conf
        , ciDatabase = fromString . connectDatabase $ conf
        , ciUser = fromString . connectUser $ conf
        , ciPassword = fromString . connectPassword $ conf
        , ciCharset = ciCharset defaultConnectInfoMB4
        }
  connect dbConf

-- | Close the given connection.
closeMySQLConn :: MySQLConn -> IO ()
closeMySQLConn = close
