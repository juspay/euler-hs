{-# LANGUAGE BangPatterns #-}

module EulerHS.Core.Logger.Interpreter
  (
    -- * Core Logger Interpreter
    runLogger
  )
where

import           EulerHS.Prelude

import qualified EulerHS.Core.Language as L
import qualified EulerHS.Core.Logger.Impl.TinyLogger as Impl
import qualified EulerHS.Core.Runtime as R
import qualified EulerHS.Core.Types as T
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS


interpretLogger :: Maybe T.FlowGUID -> R.LoggerRuntime -> L.LoggerMethod a -> IO a

-- Memory logger
interpretLogger
  mbFlowGuid
  (R.MemoryLoggerRuntime flowFormatter logContext logLevel logsVar cntVar)
  (L.LogMessage msgLogLvl tag msg next) =

  fmap next $
    case compare logLevel msgLogLvl of
      GT -> pure ()
      _  -> do
        formatter <- flowFormatter mbFlowGuid
        !msgNum   <- R.incLogCounter cntVar
        let msgBuilder = formatter $ T.PendingMsg mbFlowGuid msgLogLvl tag msg msgNum logContext
        let !m = case msgBuilder of
              T.SimpleString str -> T.pack str
              T.SimpleText txt -> txt
              T.SimpleBS bs -> T.decodeUtf8 bs
              T.SimpleLBS lbs -> T.decodeUtf8 $ LBS.toStrict lbs
              T.MsgBuilder bld -> T.decodeUtf8 $ LBS.toStrict $ T.builderToByteString bld
              T.MsgTransformer _ -> error "Msg -> Msg not supported for memory logger."
        MVar.modifyMVar logsVar $ \(!lgs) -> pure (m : lgs, ())

-- Regular logger
interpretLogger
  mbFlowGuid
  (R.LoggerRuntime flowFormatter logContext logLevel _ cntVar _ handle)
  (L.LogMessage msgLogLevel tag msg next) =

  fmap next $
    case compare logLevel msgLogLevel of
      GT -> pure ()
      _  -> do
        msgNum    <- R.incLogCounter cntVar
        Impl.sendPendingMsg flowFormatter handle $ T.PendingMsg mbFlowGuid msgLogLevel tag msg msgNum logContext

runLogger :: Maybe T.FlowGUID -> R.LoggerRuntime -> L.Logger a -> IO a
runLogger mbFlowGuid loggerRt = foldF (interpretLogger mbFlowGuid loggerRt)
