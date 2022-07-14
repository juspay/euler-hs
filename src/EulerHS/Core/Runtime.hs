{- |
Module      :  EulerHS.Core.Runtime
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
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
  , getLogMaskingConfig
  , module X
  ) where

import           EulerHS.Prelude
import           EulerHS.Core.Types
  ( LogCounter
  , LogLevel(..)
  , LoggerConfig(..)
  , LogMaskingConfig(..)
  , ShouldLogSQL(SafelyOmitSqlLogs, UnsafeLogSQL_DO_NOT_USE_IN_PRODUCTION)
  )
-- Currently, TinyLogger is highly coupled with the Runtime.
-- Fix it if an interchangable implementations are needed.
import qualified EulerHS.Core.Logger.Impl.TinyLogger as Impl
import qualified EulerHS.Core.Types as T
import           EulerHS.Core.Types.DB as X (withTransaction)
import qualified System.Logger as Log


-- TODO: add StaticLoggerRuntimeContext if we'll need more than a single Bool
data LoggerRuntime
  = LoggerRuntime
    { _flowFormatter    :: T.FlowFormatter
    , _logContext       :: T.LogContext
    , _logLevel         :: T.LogLevel
    , _logRawSql        :: ShouldLogSQL
    , _logCounter       :: !T.LogCounter
    , _logMaskingConfig :: Maybe T.LogMaskingConfig
    , _logLoggerHandle  :: Impl.LoggerHandle
    }
  | MemoryLoggerRuntime !T.FlowFormatter T.LogContext !T.LogLevel !(MVar [Text]) !T.LogCounter

data CoreRuntime = CoreRuntime
    { _loggerRuntime :: LoggerRuntime
    }

-- createLoggerRuntime :: LoggerConfig -> IO LoggerRuntime
-- createLoggerRuntime (MemoryLoggerConfig cfgLogLevel) =
--   MemoryLoggerRuntime cfgLogLevel <$> newMVar []
-- createLoggerRuntime cfg = do
--   counter <- initLogCounter
--   LoggerRuntime (_level cfg) (_logRawSql cfg) counter Nothing Nothing (_logMaskingConfig cfg)<$> Impl.createLogger cfg

createMemoryLoggerRuntime :: T.FlowFormatter -> T.LogLevel -> IO LoggerRuntime
createMemoryLoggerRuntime flowFormatter logLevel =
  MemoryLoggerRuntime flowFormatter mempty logLevel <$> newMVar [] <*> initLogCounter

createLoggerRuntime :: T.FlowFormatter -> T.LoggerConfig -> IO LoggerRuntime
createLoggerRuntime flowFormatter cfg = do
  counter <- initLogCounter
  LoggerRuntime flowFormatter mempty (T._logLevel cfg) (T._logRawSql cfg) counter Nothing
    <$> Impl.createLogger flowFormatter cfg

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
  pure $ LoggerRuntime flowFormatter mempty (T._logLevel cfg) (T._logRawSql cfg) counter Nothing loggerHandle

createVoidLoggerRuntime :: IO LoggerRuntime
createVoidLoggerRuntime = do
  counter <- initLogCounter
  LoggerRuntime (const $ pure T.showingMessageFormatter) mempty T.Debug SafelyOmitSqlLogs counter Nothing <$> Impl.createVoidLogger

clearLoggerRuntime :: LoggerRuntime -> IO ()
clearLoggerRuntime (LoggerRuntime flowFormatter _ _ _ _ _ handle) = Impl.disposeLogger flowFormatter handle
clearLoggerRuntime (MemoryLoggerRuntime _ _ _ msgsVar _) = void $ swapMVar msgsVar []

createCoreRuntime :: LoggerRuntime -> IO CoreRuntime
createCoreRuntime = pure . CoreRuntime

clearCoreRuntime :: CoreRuntime -> IO ()
clearCoreRuntime _ = pure ()

shouldLogRawSql :: LoggerRuntime -> Bool
shouldLogRawSql = \case
  (LoggerRuntime _ _ _ UnsafeLogSQL_DO_NOT_USE_IN_PRODUCTION _ _ _) -> True
  _ -> False

getLogMaskingConfig :: LoggerRuntime -> Maybe T.LogMaskingConfig
getLogMaskingConfig = \case
  (LoggerRuntime _ _ _ _ _ mbMaskConfig _) -> mbMaskConfig
  _ -> Nothing

initLogCounter :: IO T.LogCounter
initLogCounter = newIORef 0

incLogCounter :: T.LogCounter -> IO Int
incLogCounter = flip atomicModifyIORef' (\cnt -> (cnt + 1, cnt))
