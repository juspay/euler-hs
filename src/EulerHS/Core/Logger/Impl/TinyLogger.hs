module EulerHS.Core.Logger.Impl.TinyLogger
  (
    -- * TinyLogger Implementation
    -- ** Types
    LoggerHandle
    -- ** Methods
  , sendPendingMsg
  , createLogger
  , createLogger'
  , createVoidLogger
  , disposeLogger
  , withLogger
  , defaultDateFormat
  , defaultRenderer
  , defaultBufferSize
  ) where

import           EulerHS.Prelude hiding ((.=))

import           Control.Concurrent (forkOn, getNumCapabilities)
import qualified Control.Concurrent.Chan.Unagi.Bounded as Chan
import qualified System.Logger as Log
import qualified System.Logger.Message as LogMsg

import qualified EulerHS.Core.Types as T

type LogQueue = (Chan.InChan T.PendingMsg, Chan.OutChan T.PendingMsg)

type Loggers = [Log.Logger]

data LoggerHandle
  = AsyncLoggerHandle [ThreadId] LogQueue Loggers
  | SyncLoggerHandle Loggers
  | VoidLoggerHandle

-- | Maps the custom log level type 'T.LogLevel' to the standard log level type 'Log.Level'.
dispatchLogLevel :: T.LogLevel -> Log.Level
dispatchLogLevel T.Debug   = Log.Debug
dispatchLogLevel T.Info    = Log.Info
dispatchLogLevel T.Warning = Log.Warn
dispatchLogLevel T.Error   = Log.Error

-- | Logs a pending message using the provided flow formatter and loggers.
logPendingMsg :: T.FlowFormatter -> Loggers -> T.PendingMsg -> IO ()
logPendingMsg flowFormatter loggers pendingMsg@(T.PendingMsg mbFlowGuid lvl tag msg msgNum logContext) = do
  formatter <- flowFormatter mbFlowGuid
  let msgBuilder = formatter pendingMsg
  let lvl' = dispatchLogLevel lvl
  let msg' = case msgBuilder of
        T.SimpleString str -> Log.msg str
        T.SimpleText txt -> Log.msg txt
        T.SimpleBS bs -> Log.msg bs
        T.SimpleLBS lbs -> Log.msg lbs
        T.MsgBuilder bld -> Log.msg bld
        T.MsgTransformer f -> f
  mapM_ (\logger -> Log.log logger lvl' msg') loggers

-- | Worker function for the logger, reads pending messages from the channel and logs them.
loggerWorker :: T.FlowFormatter -> Chan.OutChan T.PendingMsg -> Loggers -> IO ()
loggerWorker flowFormatter outChan loggers = do
  pendingMsg <- Chan.readChan outChan
  logPendingMsg flowFormatter loggers pendingMsg

-- | Sends a pending message to the appropriate logger based on the logger handle.
sendPendingMsg :: T.FlowFormatter -> LoggerHandle -> T.PendingMsg -> IO ()
sendPendingMsg _ VoidLoggerHandle = const (pure ())
sendPendingMsg flowFormatter (SyncLoggerHandle loggers) = logPendingMsg flowFormatter loggers
sendPendingMsg _ (AsyncLoggerHandle _ (inChan, _) _) = Chan.writeChan inChan

createVoidLogger :: IO LoggerHandle
createVoidLogger = pure VoidLoggerHandle

{-|
  Create a logger with the provided flow formatter and logger configuration.
  This is the main entry point for creating loggers.

  === Parameters:

  * @T.FlowFormatter@: Flow formatter used for formatting log messages.

  * @T.LoggerConfig@: Logger configuration specifying various settings for the logger.

  === Returns:

  An 'IO' action that produces a 'LoggerHandle'.

-}
createLogger :: T.FlowFormatter -> T.LoggerConfig -> IO LoggerHandle
createLogger = createLogger' defaultDateFormat defaultRenderer defaultBufferSize

{-|
  Create a logger with additional settings such as date format, renderer, and buffer size.

  NOTE : a Renderer typically refers to a component or function responsible for rendering log messages into a specific format or representation. It determines how the log messages are formatted before being output to the desired output channel, such as a console, file, or another logging endpoint.

  === Parameters:

  * @Maybe Log.DateFormat@: Optional date format for log messages.

  * @Maybe Log.Renderer@: Optional renderer for log messages.

  * @T.BufferSize@: Buffer size for log messages.

  * @T.FlowFormatter@: Flow formatter used for formatting log messages.

  * @T.LoggerConfig@: Logger configuration specifying various settings for the logger.

  === Returns:

  An 'IO' action that produces a 'LoggerHandle'.

-}
createLogger'
  :: Maybe Log.DateFormat
  -> Maybe Log.Renderer
  -> T.BufferSize
  -> T.FlowFormatter
  -> T.LoggerConfig
  -> IO LoggerHandle
createLogger'
  mbDateFormat
  mbRenderer
  bufferSize
  flowFormatter
  (T.LoggerConfig isAsync _ logFileName isConsoleLog isFileLog maxQueueSize _ _) = do

    let fileSettings
          = Log.setFormat mbDateFormat
          $ maybe id Log.setRenderer mbRenderer
          $ Log.setBufSize bufferSize
          $ Log.setOutput (Log.Path logFileName)
          $ Log.defSettings

    let consoleSettings
          = Log.setFormat mbDateFormat
          $ maybe id Log.setRenderer mbRenderer
          $ Log.setBufSize bufferSize
          $ Log.setOutput Log.StdOut
          $ Log.defSettings

    let fileH    = [Log.new fileSettings    | isFileLog]
    let consoleH = [Log.new consoleSettings | isConsoleLog]
    let loggersH = fileH ++ consoleH

    when (not $ null loggersH) $
      if isAsync then putStrLn @String "Creating async loggers..."
                 else putStrLn @String "Creating sync loggers..."
    when isFileLog    $ putStrLn @String $ "Creating file logger (" +| logFileName |+ ")..."
    when isConsoleLog $ putStrLn @String "Creating console logger..."

    loggers <- sequence loggersH
    startLogger isAsync loggers
  where
    startLogger :: Bool -> Loggers -> IO LoggerHandle
    startLogger _ [] = pure VoidLoggerHandle
    startLogger False loggers = pure $ SyncLoggerHandle loggers
    startLogger True  loggers = do
      caps <- getNumCapabilities
      chan@(_, outChan) <- Chan.newChan (fromIntegral maxQueueSize)
      threadIds <- traverse ((flip forkOn) (forever $ loggerWorker flowFormatter outChan loggers)) [1..caps]
      pure $ AsyncLoggerHandle threadIds chan loggers

{-
Disposes resources associated with a logger handle.

This function handles the disposal of resources based on the type of logger handle:
- For 'VoidLoggerHandle', no action is performed.
- For 'SyncLoggerHandle', it flushes and closes each logger in the handle.
- For 'AsyncLoggerHandle', it kills the associated threads, logs any remaining messages in the output channel,
  flushes and closes each logger in the handle.
-}
disposeLogger :: T.FlowFormatter -> LoggerHandle -> IO ()
disposeLogger _ VoidLoggerHandle = pure ()
disposeLogger _ (SyncLoggerHandle loggers) = do
  putStrLn @String "Disposing sync logger..."
  mapM_ Log.flush loggers
  mapM_ Log.close loggers
disposeLogger flowFormatter (AsyncLoggerHandle threadIds (_, outChan) loggers) = do
  putStrLn @String "Disposing async logger..."
  traverse_ killThread threadIds
  logRemaining outChan
  mapM_ Log.flush loggers
  mapM_ Log.close loggers
  where
    logRemaining :: Chan.OutChan T.PendingMsg -> IO ()
    logRemaining oc = do
      (el,_) <- Chan.tryReadChan oc
      mPendingMsg <- Chan.tryRead el
      case mPendingMsg of
        Just pendingMsg -> do
            logPendingMsg flowFormatter loggers pendingMsg
            logRemaining oc
        Nothing -> pure ()

{-

Convenience function to create a logger with specified settings and run an action with the obtained logger.

Acquires a logger using 'createLogger'' or 'createLogger', runs the provided
action, and ensures proper disposal of the logger afterward. Handles exceptions
that might occur during resource acquisition, action execution, or resource release.

NOTE: 'bracket' is used here to manage the lifecycle of the logger. The first
argument acquires the resource (creates the logger), the second argument releases
the resource (disposes of the logger).

-}
withLogger'
  :: Maybe Log.DateFormat
  -> Maybe Log.Renderer
  -> T.BufferSize
  -> T.FlowFormatter
  -> T.LoggerConfig
  -> (LoggerHandle -> IO a)
  -> IO a
withLogger' mbDateFormat mbRenderer bufSize flowFormatter cfg =
  bracket (createLogger' mbDateFormat mbRenderer bufSize flowFormatter cfg) (disposeLogger flowFormatter)

withLogger
  :: T.FlowFormatter
  -> T.LoggerConfig
  -> (LoggerHandle -> IO a)
  -> IO a
withLogger flowFormatter cfg = bracket (createLogger flowFormatter cfg) (disposeLogger flowFormatter)

defaultBufferSize :: T.BufferSize
defaultBufferSize = 4096

defaultDateFormat :: Maybe Log.DateFormat
defaultDateFormat = Nothing

defaultRenderer :: Maybe Log.Renderer
defaultRenderer = Nothing
