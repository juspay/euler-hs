{-# OPTIONS_GHC -Werror #-}

module EulerHS.Tests.Framework.Common
  (
    withServer,
    initRTWithManagers
    -- runFlowWithArt, initPlayerRT, initRecorderRT, initRegularRT,
    -- withServer, runFlowRecording, initRTWithManagers, replayRecording,
    -- emptyMVarWithWatchDog
  ) where

import           EulerHS.TestData.API.Client (api, port, server)
import           Control.Concurrent.Async (withAsync)
import qualified Data.Map as Map
-- import qualified Data.Vector as V
-- import           EulerHS.Interpreters (runFlow)
-- import           EulerHS.Language as L
import           EulerHS.Prelude
import           EulerHS.Runtime (FlowRuntime, _httpClientManagers,
                                  withFlowRuntime)
-- import           EulerHS.Types as T
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.Wai.Handler.Warp (run)
import           Servant.Server (serve)
-- import           Test.Hspec (shouldBe)

-- runFlowWithArt :: (Show b, Eq b) => Flow b -> IO b
-- runFlowWithArt flow = do
--   (recording, recResult) <- runFlowRecording ($) flow
--   (errors   , repResult) <- runFlowReplaying recording flow
--   flattenErrors errors `shouldBe` []
--   recResult `shouldBe` repResult
--   pure recResult

-- runFlowRecording ::
--   (forall b . (FlowRuntime -> IO b) -> FlowRuntime -> IO b) ->
--   Flow a ->
--   IO (ResultRecording, a)
-- runFlowRecording mod' flow = do
--   let next flowRuntime = do
--         result <- runFlow flowRuntime flow
--         case _runMode flowRuntime of
--           T.RecordingMode T.RecorderRuntime{recording} -> do
--             entries <- awaitRecording recording
--             pure (entries, result)
--           _ -> fail "wrong mode"
--   initRecorderRT >>= mod' next

-- runFlowReplaying :: ResultRecording -> Flow a -> IO (ResultReplayError, a)
-- runFlowReplaying recording flow  = do
--   playerRuntime <- initPlayerRT recording
--   result <- runFlow playerRuntime flow
--   case _runMode playerRuntime of
--     T.ReplayingMode T.PlayerRuntime{rerror} -> do
--       errors <- awaitErrors rerror
--       pure (errors, result)
--     _ -> fail "wrong mode"

withServer :: IO () -> IO ()
withServer action = withAsync (run port . serve api $ server)
                              (const action)

initRTWithManagers :: IO FlowRuntime
initRTWithManagers = do
  flowRt <- withFlowRuntime Nothing pure
  m1 <- newManager tlsManagerSettings
  m2 <- newManager tlsManagerSettings
  let managersMap = Map.fromList
        [ ("manager1", m1)
        , ("manager2", m2)
        ]
  pure $ flowRt { _httpClientManagers = managersMap }

-- initRegularRT :: IO FlowRuntime
-- initRegularRT = do
--   flowRt <- withFlowRuntime Nothing pure
--   pure $ flowRt { _runMode = T.RegularMode }

-- initRecorderRT :: IO FlowRuntime
-- initRecorderRT = do
--   recMVar       <- newMVar V.empty
--   safeRecMVar   <- newMVar Map.empty
--   forkedRecMVar <- newMVar Map.empty
--   let
--     recorderRuntime = T.RecorderRuntime
--       { flowGUID = "testFlow"
--       , recording = T.Recording recMVar safeRecMVar forkedRecMVar
--       , disableEntries = []
--       }
--   flowRuntime <- withFlowRuntime Nothing pure
--   pure $ flowRuntime { _runMode = T.RecordingMode recorderRuntime }


-- initPlayerRT :: ResultRecording -> IO FlowRuntime
-- initPlayerRT recEntries = do
--   step              <- newMVar 0
--   freshReplayErrors <- T.ReplayErrors <$> newMVar Nothing <*> newMVar Map.empty <*> newMVar Map.empty

--   let
--     playerRuntime = T.PlayerRuntime
--       { resRecording    = recEntries
--       , stepMVar        = step
--       , rerror          = freshReplayErrors
--       , disableVerify   = []
--       , disableMocking  = []
--       , skipEntries     = []
--       , entriesFiltered = False
--       , flowGUID        = "testFlow"
--       }

--   flowRuntime <- withFlowRuntime Nothing pure
--   pure $ flowRuntime { _runMode = T.ReplayingMode playerRuntime }

-- replayRecording :: ResultRecording -> Flow a -> IO a
-- replayRecording rec flow = do
--   (errors, result) <- runFlowReplaying rec flow
--   flattenErrors errors `shouldBe` []
--   pure result

-- emptyMVarWithWatchDog :: Int -> IO (MVar a, IO (Maybe a), IO ())
-- emptyMVarWithWatchDog t = do
--     guard $ t >= 0
--     targetMVar <- newEmptyMVar
--     finalMVar  <- newEmptyMVar
--     let watch = forkIO $ do
--           let loop n = do
--                 mresult <- tryTakeMVar targetMVar

--                 case mresult of
--                   Just a -> do
--                     putMVar targetMVar a
--                     putMVar finalMVar $ Just a

--                   Nothing -> do
--                     if n > 0
--                         then do
--                           threadDelay 100000
--                           loop $ n - 1

--                         else putMVar finalMVar Nothing


--           loop $ t * 10

--     let reset = void $ tryTakeMVar targetMVar


--     pure (targetMVar, watch >> takeMVar finalMVar, reset)


