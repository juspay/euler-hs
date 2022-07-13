{-# OPTIONS_GHC -Werror #-}

module EulerHS.Tests.Framework.Common
  (
    withServer,
    initRTWithManagers
  ) where

import           EulerHS.TestData.API.Client (api, port, server)
import           Control.Concurrent.Async (withAsync)
import qualified Data.Map as Map
import           EulerHS.Prelude
import           EulerHS.Runtime (FlowRuntime, _httpClientManagers,
                                  withFlowRuntime)
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.Wai.Handler.Warp (run)
import           Servant.Server (serve)

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
