{- |
Module      :  EulerHS.Core.Runtime
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2021
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable

This module contains functions and types to work with `CoreRuntime`.

This is an internal module. Import EulerHS.Runtime instead.
-}

module EulerHS.Core.Runtime
  (
    -- * Core Runtime
    CoreRuntime(..)
  , LoggerRuntime(..)
  , shouldLogRawSql
  , incLogCounter
  , createCoreRuntime
  , createVoidLoggerRuntime
  , createMemoryLoggerRuntime
  , createLoggerRuntime
  , createLoggerRuntime'
  , clearCoreRuntime
  , clearLoggerRuntime
  , module X
  ) where

import           EulerHS.Prelude

-- Currently, TinyLogger is highly coupled with the Runtime.
-- Fix it if an interchangable implementations are needed.
import qualified EulerHS.Core.Logger.Impl.TinyLogger as Impl
import qualified EulerHS.Core.Types as T
import           EulerHS.Core.Types.DB as X (withTransaction)
import qualified System.Logger as Log

-- | A counter of log messages sent since the `LoggerRuntime` creation.
type LogCounter = IORef Int         -- No race condition: atomicModifyIORef' is used.

-- | Runtime sturcture holding all the necessary operational information
-- for the logging subsystem.
data LoggerRuntime
  -- | Runtime structure of a regular logger.
  = LoggerRuntime
      { _flowFormatter   :: T.FlowFormatter
        -- ^ A callback for obtaining a flow-specific formatter.
      , _logLevel        :: T.LogLevel
        -- ^ Log level
      , _logRawSql       :: !Bool
        -- ^ Whether to log raw SQL as Debug messages.
      , _logCounter      :: !LogCounter
        -- ^ Log messages counter variable.
      , _logLoggerHandle :: Impl.LoggerHandle
        -- ^ Internal logging subsystem handler.
      }
  -- | Runtime structure for a memory logger.
  | MemoryLoggerRuntime !T.FlowFormatter !T.LogLevel !(MVar [Text]) !LogCounter

-- | Runtime that keeps all the operational info for the core subsystems.
data CoreRuntime = CoreRuntime
    { _loggerRuntime :: LoggerRuntime
      -- ^ Logger runtime
    }

-- | Create a memory logger runtime.
--
-- This function can be passed to `createFlowRuntime'`.
createMemoryLoggerRuntime :: T.FlowFormatter -> T.LogLevel -> IO LoggerRuntime
createMemoryLoggerRuntime flowFormatter logLevel =
  MemoryLoggerRuntime flowFormatter logLevel <$> newMVar [] <*> initLogCounter

-- | Create a regular logger runtime according to the config passed.
--
-- This function can be passed to `createFlowRuntime'`.
createLoggerRuntime :: T.FlowFormatter -> T.LoggerConfig -> IO LoggerRuntime
createLoggerRuntime flowFormatter cfg = do
  counter <- initLogCounter
  LoggerRuntime flowFormatter (T._logLevel cfg) (T._logRawSql cfg) counter
    <$> Impl.createLogger flowFormatter cfg

-- | The same as `createLoggerRuntime` but allows to setup different tweaks
-- of the specific tiny-logger subsystem.
--
-- This function can be passed to `createFlowRuntime'`.
createLoggerRuntime'
  :: Maybe Log.DateFormat
  -> Maybe Log.Renderer
  -> T.BufferSize
  -> T.FlowFormatter
  -> T.LoggerConfig
  -> IO LoggerRuntime
createLoggerRuntime' mbDateFormat mbRenderer bufferSize flowFormatter cfg = do
  counter <- initLogCounter
  loggerHandle <- Impl.createLogger' mbDateFormat mbRenderer bufferSize flowFormatter cfg
  pure $ LoggerRuntime flowFormatter (T._logLevel cfg) (T._logRawSql cfg) counter loggerHandle

-- | Create a void logger: nothing will be logged.
createVoidLoggerRuntime :: IO LoggerRuntime
createVoidLoggerRuntime = do
  counter <- initLogCounter
  LoggerRuntime (const $ pure show)  T.Debug True counter <$> Impl.createVoidLogger

-- | Clear a logger runtime and dispose the logger.
--
-- This function flushes the last log messages existing in the log queue.
clearLoggerRuntime :: LoggerRuntime -> IO ()
clearLoggerRuntime (LoggerRuntime flowFormatter _ _ _ handle) = Impl.disposeLogger flowFormatter handle
clearLoggerRuntime (MemoryLoggerRuntime _ _ msgsVar _) = void $ swapMVar msgsVar []

-- | Creates a core runtime.
createCoreRuntime :: LoggerRuntime -> IO CoreRuntime
createCoreRuntime = pure . CoreRuntime

-- | Clears the core runtime.
clearCoreRuntime :: CoreRuntime -> IO ()
clearCoreRuntime _ = pure ()

-- | Returns True if debug logging of raw SQL queries was set.
shouldLogRawSql :: LoggerRuntime -> Bool
shouldLogRawSql = \case
  (LoggerRuntime _ _ logRawSql _ _) -> logRawSql
  _ -> True

-- | Init log messages counter.
--
-- Internal function, should not be used in the BL.
initLogCounter :: IO LogCounter
initLogCounter = newIORef 0

-- | Incremenet log messages counter.
--
-- Internal function, should not be used in the BL.
incLogCounter :: LogCounter -> IO Int
incLogCounter = flip atomicModifyIORef' (\cnt -> (cnt + 1, cnt))
