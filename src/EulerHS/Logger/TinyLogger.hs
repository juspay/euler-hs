module EulerHS.Logger.TinyLogger
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

import           Control.Concurrent (forkOn, getNumCapabilities)
import qualified Control.Concurrent.Chan.Unagi.Bounded as Chan
import           EulerHS.Logger.Types (BufferSize, FlowFormatter,
                                       LogLevel (Debug, Error, Info, Warning),
                                       LoggerConfig (LoggerConfig),
                                       MessageBuilder (MsgBuilder, MsgTransformer, SimpleBS, SimpleLBS, SimpleString, SimpleText),
                                       PendingMsg (PendingMsg))
import           GHC.Conc (labelThread)
import           EulerHS.Prelude
import qualified System.Logger as Log

type LogQueue = (Chan.InChan PendingMsg, Chan.OutChan PendingMsg)

type Loggers = [Log.Logger]

data LoggerHandle
  = AsyncLoggerHandle [ThreadId] LogQueue Loggers
  | SyncLoggerHandle Loggers
  | VoidLoggerHandle

dispatchLogLevel :: LogLevel -> Log.Level
dispatchLogLevel Debug   = Log.Debug
dispatchLogLevel Info    = Log.Info
dispatchLogLevel Warning = Log.Warn
dispatchLogLevel Error   = Log.Error

logPendingMsg :: FlowFormatter -> Loggers -> PendingMsg -> IO ()
logPendingMsg flowFormatter loggers pendingMsg@(PendingMsg mbFlowGuid lvl _ _ _ _) = do
  formatter <- flowFormatter mbFlowGuid
  let msgBuilder = formatter pendingMsg
  let lvl' = dispatchLogLevel lvl
  let msg' = case msgBuilder of
        SimpleString str -> Log.msg str
        SimpleText txt   -> Log.msg txt
        SimpleBS bs      -> Log.msg bs
        SimpleLBS lbs    -> Log.msg lbs
        MsgBuilder bld   -> Log.msg bld
        MsgTransformer f -> f
  mapM_ (\logger -> Log.log logger lvl' msg') loggers

loggerWorker :: FlowFormatter -> Chan.OutChan PendingMsg -> Loggers -> IO ()
loggerWorker flowFormatter outChan loggers = do
  pendingMsg <- Chan.readChan outChan
  logPendingMsg flowFormatter loggers pendingMsg

sendPendingMsg :: FlowFormatter -> LoggerHandle -> PendingMsg -> IO ()
sendPendingMsg _ VoidLoggerHandle = const (pure ())
sendPendingMsg flowFormatter (SyncLoggerHandle loggers) = logPendingMsg flowFormatter loggers
sendPendingMsg _ (AsyncLoggerHandle _ (inChan, _) _) = Chan.writeChan inChan

createVoidLogger :: IO LoggerHandle
createVoidLogger = pure VoidLoggerHandle

createLogger :: FlowFormatter -> LoggerConfig -> IO LoggerHandle
createLogger = createLogger' defaultDateFormat defaultRenderer defaultBufferSize

createLogger'
  :: Maybe Log.DateFormat
  -> Maybe Log.Renderer
  -> BufferSize
  -> FlowFormatter
  -> LoggerConfig
  -> IO LoggerHandle
createLogger'
  mbDateFormat
  mbRenderer
  bufferSize
  flowFormatter
  (LoggerConfig isAsync _ logFileName isConsoleLog isFileLog maxQueueSize _ _ _) = do

    let fileSettings
          = Log.setFormat mbDateFormat
          $ maybe id Log.setRenderer mbRenderer
          $ Log.setBufSize bufferSize
          $ Log.setOutput (Log.Path logFileName)
          Log.defSettings

    let consoleSettings
          = Log.setFormat mbDateFormat
          $ maybe id Log.setRenderer mbRenderer
          $ Log.setBufSize bufferSize
          $ Log.setOutput Log.StdOut
          Log.defSettings

    let fileH    = [Log.new fileSettings    | isFileLog]
    let consoleH = [Log.new consoleSettings | isConsoleLog]
    let loggersH = fileH ++ consoleH

    unless (null loggersH) $
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
      threadIds <- traverse (`forkOn` (forever $ loggerWorker flowFormatter outChan loggers)) [1..caps]
      mapM_ (\tid -> labelThread tid "euler-createLogger") threadIds
      pure $ AsyncLoggerHandle threadIds chan loggers

disposeLogger :: FlowFormatter -> LoggerHandle -> IO ()
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
    logRemaining :: Chan.OutChan PendingMsg -> IO ()
    logRemaining oc = do
      (el,_) <- Chan.tryReadChan oc
      mPendingMsg <- Chan.tryRead el
      case mPendingMsg of
        Just pendingMsg -> do
            logPendingMsg flowFormatter loggers pendingMsg
            logRemaining oc
        Nothing -> pure ()

withLogger
  :: FlowFormatter
  -> LoggerConfig
  -> (LoggerHandle -> IO a)
  -> IO a
withLogger flowFormatter cfg = bracket (createLogger flowFormatter cfg) (disposeLogger flowFormatter)

defaultBufferSize :: BufferSize
defaultBufferSize = 4096

defaultDateFormat :: Maybe Log.DateFormat
defaultDateFormat = Nothing

defaultRenderer :: Maybe Log.Renderer
defaultRenderer = Nothing
