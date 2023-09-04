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
                          ""
                          [""]
                          ""

tlsSettingsWithCertV1 :: TLSSettings
tlsSettingsWithCertV1 = tlsSettingsChain
                          ""
                          [""]
                          ""

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
  mbStore <- readCertificateStore ""
  let store = fromMaybe (error "") mbStore

  m4 <- newManager $  buildSettings $
          withCustomCA store

  cert <- readFile ""
  key <- readFile ""

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



clientHttpCert:: IO T.HTTPCert
clientHttpCert = do
  cert <- readFile ""
  key  <- readFile ""
  return $ HTTPCert cert [] "server01" key 
