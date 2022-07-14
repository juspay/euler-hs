{-# LANGUAGE DeriveAnyClass #-}

{- |
Module      :  EulerHS.Core.Types.Logger
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2021
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable

Types and helper functions of the Logging subsystem.

This module is internal and should not imported in the projects.
Import 'EulerHS.Types' instead.
-}

module EulerHS.Core.Types.Logger
    (
    -- * Core Logger
    -- ** Types
      LogLevel(..)
    , BufferSize
    , MessageBuilder (..)
    , MessageFormatter
    , FlowFormatter
    , LoggerConfig(..)
    , Message
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

import           EulerHS.Prelude
import           Data.HashSet(HashSet)

import qualified EulerHS.Core.Types.Common as T

-- Currently, TinyLogger is highly coupled with the interface.
-- Reason: unclear current practice of logging that affects design and performance.
import qualified System.Logger.Message as LogMsg
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS

-- | Logging level.
data LogLevel = Debug | Info | Warning | Error
    deriving (Generic, Eq, Ord, Show, Read, Enum, ToJSON, FromJSON)

data LogMaskingConfig =
  LogMaskingConfig
    { _maskKeys      :: HashSet Text -- Check : Better to make this case insensitive
    , _maskText      :: Maybe Text
    , _keyType       :: MaskKeyType
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
type Message = Text
type Tag = Text
type MessageNumber = Int
type BufferSize = Int
type MessageFormatter = PendingMsg -> MessageBuilder
type FlowFormatter = Maybe T.FlowGUID -> IO MessageFormatter
type LogContext = HashMap Text Text

data LoggerConfig
  = LoggerConfig
    { _isAsync      :: Bool
    , _logLevel     :: LogLevel
    , _logFilePath  :: FilePath
    , _logToConsole :: Bool
    , _logToFile    :: Bool
    , _maxQueueSize :: Word
    , _logRawSql    :: ShouldLogSQL
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
  SimpleString $ "[" +|| lvl ||+ "] <" +| tag |+ "> " +| msg |+ ""

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
    , _logMaskingConfig = Nothing
    }

defaultFlowFormatter :: FlowFormatter
defaultFlowFormatter _ = pure defaultMessageFormatter

builderToByteString :: LogMsg.Builder -> LBS.ByteString
builderToByteString = LogMsg.eval
