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
    , MessageFormatter
    , FlowFormatter
    , LoggerConfig(..)
    , Message
    , Tag
    , PendingMsg(..)
    , LogEntry (..)
    , Log
    -- ** defaults
    , defaultLoggerConfig
    , defaultMessageFormatter
    , defaultFlowFormatter
    ) where

import           EulerHS.Prelude

import qualified EulerHS.Core.Types.Common as T

-- | Logging level.
data LogLevel
  = Debug
  | Info
  | Warning
  | Error
    deriving (Generic, Eq, Ord, Show, Read, Enum, ToJSON, FromJSON)

-- | Message type
type Message = Text

-- | Tag that accompanies every call of `logMessage`, `logInfo` and other functions.
type Tag = Text

-- | The number of a log message in the this run.
--
-- It's 0 for a fresh `LoggerRuntime`, and increases on each logging call.
type MessageNumber = Int

-- | Buffer size of a logger. Can be important in some cases.
type BufferSize = Int

{- | Formatter of a message.

Can be used to format a particular logging message (wrapped into `PendingMsg`).

The simplest formatter is just `show`.
@
import qualified EulerHS.Types as T

simplestFormatter :: T.MessageFormatter
simplestFormatter = show
@
-}

type MessageFormatter = PendingMsg -> String

{- | A flow-specific message formatter.

It's a callback that should return a `MessageFormatter` before a message
goes to the underlying logging library.

In the simplest case, you return the same message formatter for any flow.
The @FlowGUID@ argument then has no effect.

@
-- flowFormatter :: T.FlowFormatter
flowFormatter :: Maybe T.FlowGUID -> IO T.MessageFormatter
flowFormatter _ = pure simplestFormatter
@

In fact, you can setup your own message formatter for each new flow.
To do this, you define a callback which is able to track your flows
and return a particular message formatter.

This logic should be thread-safe.

@
type FlowsFormatters = MVar (Map T.FlowGUID T.MessageFormatter)

flowsFormatter
  :: FlowsFormatters
  -> Maybe T.FlowGUID
  -> IO T.MessageFormatter
flowsFormatter flowFsVar Nothing = pure simplestFormatter
flowsFormatter flowFsVar (Just flowGuid) = do
  flowFs <- readMVar flowFsVar
  case Map.lookup flowGuid flowFS of
    Nothing -> pure simplestFormatter   -- You should decide on what to return here
    Just formatter -> pure formatter
@

You can update your formatters map right before and after running a flow.
There is a special function `runFlow'` to run a flow with a particular GUID.

GUID string can be anything unique across your flows.
-}

type FlowFormatter = Maybe T.FlowGUID -> IO MessageFormatter

-- | Config of a logger
data LoggerConfig
  = LoggerConfig
    { _isAsync      :: Bool
      -- ^ Is it async.
      --
      -- N.B. The async logger feature is not well-tested.
    , _logLevel     :: LogLevel
      -- ^ System log level
    , _logFilePath  :: FilePath
      -- ^ Log file path if a file logger is enabled
    , _logToConsole :: Bool
      -- ^ Enable / disable a console logging.
    , _logToFile    :: Bool
      -- ^ Enable / disable a file logging
    , _maxQueueSize :: Word
      -- ^ Allows to configure a logging queue.
    , _logRawSql    :: Bool
      -- ^ Enable / disable logging of SQL queries in the SQL DB subsystem.
      --
      -- SQL queries will be written as Debug messages.
      --
      -- N.B. Enabling this feature slows down the performance of the SQL DB subsystem.
    } deriving (Generic, Show, Read)

-- | A message to send to the underlying logger subsystem.
--
-- Can be formatted with `MessageFormatter`.
data PendingMsg = PendingMsg
  !(Maybe T.FlowGUID)
  !LogLevel
  !Tag
  !Message
  !MessageNumber
  deriving (Show)

{- | Default message formatter:
@
defaultMessageFormatter (PendingMsg _ lvl tag msg _) =
  "[" +|| lvl ||+ "] <" +| tag |+ "> " +| msg |+ ""
@
-}
defaultMessageFormatter :: MessageFormatter
defaultMessageFormatter (PendingMsg _ lvl tag msg _) =
  "[" +|| lvl ||+ "] <" +| tag |+ "> " +| msg |+ ""

{- | Default logger config:
  isAsync = False
  logLevel = Debug
  logFilePath = ""
  logToConsole = True
  logToFile = False
  maxQueueSize = 1000
  logRawSql = True
-}
defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig = LoggerConfig
    { _isAsync = False
    , _logLevel = Debug
    , _logFilePath = ""
    , _logToConsole = True
    , _logToFile = False
    , _maxQueueSize = 1000
    , _logRawSql = True
    }

-- | Default flow formatter.
-- Ignores the flow GUID and just returns `defaultMessageFormatter`.
defaultFlowFormatter :: FlowFormatter
defaultFlowFormatter _ = pure defaultMessageFormatter

-- * Internal types

-- | Service type for tracking log entries
data LogEntry = LogEntry !LogLevel !Message

-- | Service type for tracking log entries
type Log = [LogEntry]
