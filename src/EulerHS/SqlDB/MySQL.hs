{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}

module EulerHS.SqlDB.MySQL
  (
    -- * Core MySQL
    -- ** Types
    MySQLConfig(..)
  , MySqlOption(..)
  , MySQLCharset(..)
    -- ** Methods
  , createMySQLConn
  , closeMySQLConn
    -- ** Defaults
  , defaultMySQLConfig
  ) where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.ByteString.UTF8 (fromString)
import           Data.Word (Word16, Word8)
import           Database.MySQL.Base (ConnectInfo (..), MySQLConn, close,
                                      connect)
import           GHC.Generics (Generic)
import           Prelude

data MySqlProtocol
  = TCP
  | Socket
  | Pipe
  | Memory
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

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

data SSLInfo = SSLInfo
  { sslKey     :: FilePath
  , sslCert    :: FilePath
  , sslCA      :: FilePath
  , sslCAPath  :: FilePath
  , sslCiphers :: String
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Describes the character set to be used with the database. This also
-- includes collation information.
--
-- Currently, only a limited number of these are provided.
--
-- /See also:/ [MySQL documentation on character
-- sets](https://dev.mysql.com/doc/refman/5.7/en/charset-mysql.html)
--
-- @since 2.0.3.0
data MySQLCharset =
  -- | Corresponds to the @latin1@ character set, with the @latin1_swedish_ci@
  -- collation.
  --
  -- @since 2.0.3.0
  Latin1
  -- | Corresponds to the @utf8@ character set, with the @utf8_general_ci@
  -- collation.
  --
  -- @since 2.0.3.0
  | UTF8General
  -- | Corresponds to the @utf8mb@ character set, with the @unicode_ci@
  -- collation.
  --
  -- @since 2.0.3.0
  | UTF8Full
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | @since 2.0.3.0
data MySQLConfig = MySQLConfig
  { connectHost     :: String
  , connectPort     :: Word16
  , connectUser     :: String
  , connectPassword :: String
  , connectDatabase :: String
  , connectOptions  :: [MySqlOption]
  , connectPath     :: FilePath
  , connectSSL      :: Maybe SSLInfo
  , connectCharset  :: !MySQLCharset -- ^ @since 2.0.3.0
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | @since 2.0.3.0
defaultMySQLConfig :: MySQLConfig
defaultMySQLConfig = MySQLConfig {
  connectHost = "localhost",
  connectPort = 3306,
  connectUser = "root",
  connectPassword = "",
  connectDatabase = "test",
  connectOptions = [CharsetName "utf8"],
  connectPath = "",
  connectSSL = Nothing,
  connectCharset = Latin1
  }

-- | Connect with the given config to the database.
--
-- @since 2.0.3.0
createMySQLConn :: MySQLConfig -> IO MySQLConn
createMySQLConn conf = do
  let dbConf = ConnectInfo {
    ciHost = connectHost conf,
    ciPort = fromIntegral . connectPort $ conf,
    ciDatabase = fromString . connectDatabase $ conf,
    ciUser = fromString . connectUser $ conf,
    ciPassword = fromString . connectPassword $ conf,
    ciCharset = charsetToDBCharset . connectCharset $ conf
    }
  connect dbConf

-- | Close the given connection.
closeMySQLConn :: MySQLConn -> IO ()
closeMySQLConn = close

-- Helpers

charsetToDBCharset :: MySQLCharset -> Word8
charsetToDBCharset = \case
  Latin1      -> 8
  UTF8General -> 33
  UTF8Full    -> 224
