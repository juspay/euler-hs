{-# OPTIONS_GHC -Werror -Wwarn=deprecations #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE NamedFieldPuns #-}

module Common
  ( withServer
  , withSecureServer
  , withClientTlsAuthServer
  , withCertV1SecureServer
  , initRTWithManagers
  , clientHttpCert
    -- runFlowWithArt, initPlayerRT, initRecorderRT, initRegularRT,
    -- withServer, runFlowRecording, initRTWithManagers, replayRecording,
    -- emptyMVarWithWatchDog
  ) where


import           Data.ByteString (readFile)
import           Client (api, serverPort, server)
import           Control.Concurrent.Async (withAsync)
import           EulerHS.Prelude hiding (readFile, empty)
import           EulerHS.Runtime (FlowRuntime, _httpClientManagers,
                                  withFlowRuntime)
import           EulerHS.Types as T
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.Wai.Handler.Warp (Settings, runSettings, defaultSettings, setPort, setBeforeMainLoop)
import           Network.Wai.Handler.WarpTLS (TLSSettings, runTLS, tlsSettingsChain, tlsWantClientCert, tlsServerHooks)
import           Network.TLS (CertificateUsage (..), onClientCertificate)
import           Data.Default
import           Servant.Server (serve)
-- import           Test.Hspec (shouldBe)
import           Data.X509.CertificateStore (readCertificateStore)


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

readyHandlerSetter :: MVar () -> Settings -> Settings
readyHandlerSetter sem = setBeforeMainLoop $ readyHandler sem
  where
    readyHandler = flip putMVar ()

portSetter :: Settings -> Settings
portSetter = setPort serverPort

mkSettings :: MVar () -> Settings -> Settings
mkSettings sem = portSetter . (readyHandlerSetter  sem)

withServer :: IO () -> IO ()
withServer action = do
  sem <- newEmptyMVar
  let settings = mkSettings sem defaultSettings
  let runServer = runSettings settings . serve api $ server
  let serverIsUpCallback = \_ -> takeMVar sem >> action
  withAsync runServer serverIsUpCallback

tlsSettingsWithCert :: TLSSettings
tlsSettingsWithCert = tlsSettingsChain
                          "test/tls/server/server.cert.pem"
                          ["test/tls/intermediate/certs/ca-chain-bundle.cert.pem"]
                          "test/tls/server/server.key.pem"

tlsSettingsWithCertV1 :: TLSSettings
tlsSettingsWithCertV1 = tlsSettingsChain
                          "test/tls/server/server.v1.cert.pem"
                          ["test/tls/intermediate/certs/ca-chain-bundle.v1.cert.pem"]
                          "test/tls/server/server.v1.key.pem"

withCertV1SecureServer :: IO () -> IO ()
withCertV1SecureServer action = do
  sem <- newEmptyMVar
  let settings = mkSettings sem defaultSettings
  let tlsSettings = tlsSettingsWithCertV1
                      { tlsWantClientCert = False
                      }
  let runServer = runTLS tlsSettings settings . serve api $ server
  let serverIsUpCallback = \_ -> takeMVar sem >> action
  withAsync runServer serverIsUpCallback

withSecureServer :: IO () -> IO ()
withSecureServer action = do
  sem <- newEmptyMVar
  let settings = mkSettings sem defaultSettings
  let tlsSettings = tlsSettingsWithCert
                      { tlsWantClientCert = False
                      }
  let runServer = runTLS tlsSettings settings . serve api $ server
  let serverIsUpCallback = \_ -> takeMVar sem >> action
  withAsync runServer serverIsUpCallback

withClientTlsAuthServer :: IO () -> IO ()
withClientTlsAuthServer action = do
  sem <- newEmptyMVar
  let settings = mkSettings sem defaultSettings
  let tlsSettings = tlsSettingsWithCert
                      { tlsWantClientCert = True
                      , tlsServerHooks = def
                        -- test server doesn't validate client's certificates
                        { onClientCertificate = \ _ -> pure $ CertificateUsageAccept
                        }
                      }

  let runServer = runTLS tlsSettings settings . serve api $ server
  let serverIsUpCallback = \_ -> takeMVar sem >> action
  withAsync runServer serverIsUpCallback


initRTWithManagers :: IO FlowRuntime
initRTWithManagers = do
  flowRt <- withFlowRuntime Nothing pure
  -- default managers
  m1 <- newManager tlsManagerSettings
  m2 <- newManager tlsManagerSettings

  -- custom managers built with euler's builder

  -- sample proxying
  m3 <- newManager $ buildSettings $
          withProxy ("localhost", 3306)

  -- custom CA
  mbStore <- readCertificateStore "test/tls/ca-certificates"
  let store = fromMaybe (error "cannot read store") mbStore

  m4 <- newManager $  buildSettings $
          withCustomCA store

  cert <- readFile "test/tls/client/client.cert.pem"
  key <- readFile "test/tls/client/client.key.pem"

  -- let managerBuilder = newManager $ extract $ buildSettings
  -- with client certificate
  m5 <- newManager $ buildSettings $
             withCustomCA store
          <> withClientTls (HTTPCert cert [] "localhost" key)

  m6 <- newManager $ buildSettings $
             withCustomCA store
          <> withNoCheckLeafV3

  --
  let managersMap =
        [ ("manager1", m1)
        , ("manager2", m2)
        , ("proxying", m3)
        , ("tlsWithCustomCA", m4)
        , ("tlsWithClientCertAndCustomCA", m5)
        , ("v1CertsSupport", m6)
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


clientHttpCert:: IO T.HTTPCert
clientHttpCert = do
  -- let _ = empty
  cert <- readFile "test/tls/client/client.cert.pem"
  key  <- readFile "test/tls/client/client.key.pem"
  return $ HTTPCert cert [] "server01" key -- (Just "test/tls/ca-certificates")
