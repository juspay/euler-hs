{-# LANGUAGE DeriveAnyClass #-}

module EulerHS.Logger.Types
    (
    -- * Core Logger
    -- ** Types
      LogLevel(..)
    , BufferSize
    , MessageBuilder (..)
    , MessageFormatter
    , FlowFormatter
    , LoggerConfig(..)
    , Message(..)
    , Tag
    , PendingMsg(..)
    , ShouldLogSQL(..)
    , LogEntry (..)
    , Log
    , LogContext
    , LogCounter
    , LogMaskingConfig (..)
    , MaskKeyType (..)
    -- ** defaults
    , defaultLoggerConfig
    , defaultMessageFormatter
    , showingMessageFormatter
    , defaultFlowFormatter
    , builderToByteString
    ) where

import qualified EulerHS.Common as T
import           EulerHS.Prelude
-- Currently, TinyLogger is highly coupled with the interface.
-- Reason: unclear current practice of logging that affects design and performance.
import qualified Data.Aeson as A
import qualified Data.Text.Lazy.Encoding as TE
import Data.Text.Lazy.Builder
import qualified Data.ByteString.Lazy as LBS
import Formatting.Buildable (Buildable(..))
import qualified System.Logger.Message as LogMsg

-- | Logging level.
data LogLevel = Debug | Info | Warning | Error
    deriving (Generic, Eq, Ord, Show, Read, Enum, ToJSON, FromJSON)

data LogMaskingConfig =
  LogMaskingConfig
    { _maskKeys :: HashSet Text -- Check : Better to make this case insensitive
    , _maskText :: Maybe Text
    , _keyType  :: MaskKeyType
    } deriving (Generic, Show, Read)

data MessageBuilder
  = SimpleString String
  | SimpleText Text
  | SimpleBS ByteString
  | SimpleLBS LBS.ByteString
  | MsgBuilder LogMsg.Builder
  | MsgTransformer (LogMsg.Msg -> LogMsg.Msg)

data MaskKeyType =
    WhiteListKey
  | BlackListKey
  deriving (Generic, Show, Read)

data ShouldLogSQL
  -- | Log SQL queries, including sensitive data and API keys. Do NOT PR code
  -- with this enabled, and make sure this doesn't make it into production
  = UnsafeLogSQL_DO_NOT_USE_IN_PRODUCTION
  -- | omit SQL logs
  | SafelyOmitSqlLogs
  deriving (Generic, Show, Read)

type LogCounter = IORef Int         -- No race condition: atomicModifyIORef' is used.

data Message = Message
  { msgMessage :: Maybe A.Value
  , msgValue :: Maybe A.Value
  }
  deriving (Show)

instance Buildable Message where
  build = fromText . decodeUtf8 . showMessage
    where
      showMessage msg = case (msgMessage msg, msgValue msg) of
        -- This is compatibility code, so we added values as an extesnsion
        -- and we can safely ignore it here if needed.
        (Just message, _) -> A.encode message
        (_, Just value) -> A.encode value
        (_, _) -> ""
  {-# INLINE build #-}

type Tag = Text
type MessageNumber = Int
type BufferSize = Int
type MessageFormatter = PendingMsg -> MessageBuilder
type FlowFormatter = Maybe T.FlowGUID -> IO MessageFormatter
type LogContext = HashMap Text Text

data LoggerConfig
  = LoggerConfig
    { _isAsync          :: Bool
    , _logLevel         :: LogLevel
    , _logFilePath      :: FilePath
    , _logToConsole     :: Bool
    , _logToFile        :: Bool
    , _maxQueueSize     :: Word
    , _logRawSql        :: ShouldLogSQL
    , _logAPI           :: Bool
    , _logMaskingConfig :: Maybe LogMaskingConfig
    } deriving (Generic, Show, Read)

data PendingMsg = PendingMsg
  !(Maybe T.FlowGUID)
  !LogLevel
  !Tag
  !Message
  !MessageNumber
  !LogContext
  deriving (Show)

data LogEntry = LogEntry !LogLevel !Message
type Log = [LogEntry]

defaultMessageFormatter :: MessageFormatter
defaultMessageFormatter (PendingMsg _ lvl tag msg _ _) =
  SimpleString $ "[" +|| lvl ||+ "] <" +| tag |+ "> " +| TE.decodeUtf8 (A.encode (msgMessage msg)) |+ ""

showingMessageFormatter :: MessageFormatter
showingMessageFormatter = SimpleString . show

defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig = LoggerConfig
    { _isAsync = False
    , _logLevel = Debug
    , _logFilePath = ""
    , _logToConsole = True
    , _logToFile = False
    , _maxQueueSize = 1000
    , _logRawSql = SafelyOmitSqlLogs
    , _logAPI = True
    , _logMaskingConfig = Nothing
    }

defaultFlowFormatter :: FlowFormatter
defaultFlowFormatter _ = pure defaultMessageFormatter

builderToByteString :: LogMsg.Builder -> LBS.ByteString
builderToByteString = LogMsg.eval
