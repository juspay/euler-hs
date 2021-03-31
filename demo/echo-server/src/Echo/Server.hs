module Echo.Server where

import           EulerHS.Prelude

import qualified EulerHS.Runtime      as R
import qualified EulerHS.Language     as L
import qualified EulerHS.Interpreters as I

import Echo.API
import Echo.Domain
import Echo.Logic

import Network.Wai.Handler.Warp (run)
import Servant

data AppState = AppState
  { easterEgg :: Text
  }

data Env = Env !R.FlowRuntime !AppState
type MethodHandler = ReaderT Env (ExceptT ServerError IO)
type AppServer  = ServerT EchoAPI MethodHandler


-- Handlers connected to the API
echoServer' :: AppServer
echoServer'
     = getEcho
  :<|> postEcho


-- Conversion between handlers and server monad stacks
echoServer :: Env -> Server EchoAPI
echoServer env = hoistServer echoAPI f echoServer'
  where
    f :: ReaderT Env (ExceptT ServerError IO) a -> Handler a
    f r = do
      eResult <- liftIO $ runExceptT $ runReaderT r env
      case eResult of
        Left err  -> throwError err
        Right res -> pure res


-- Warp HTTP application
echoApp :: Env -> Application
echoApp = serve echoAPI . echoServer


-- Wrapper to run a flow with a predefined flow runtime.
runFlow :: L.Flow a -> MethodHandler a
runFlow flow = do
  Env flowRt _ <- ask
  eRes <- lift $ lift $ try $ I.runFlow flowRt flow
  case eRes of
    Left (err :: SomeException) -> do
      liftIO $ putStrLn @String $ "Exception handled: " <> show err
      throwError err500
    Right res -> pure res


-- Method handlers
getEcho :: Maybe Text -> Maybe Int -> MethodHandler EchoMessage
getEcho mbPhrase mbNumber = do
  Env _ (AppState easterEgg) <- ask
  runFlow $ echoFlow easterEgg mbPhrase mbNumber

postEcho :: EchoRequest -> MethodHandler EchoMessage
postEcho (EchoRequest phrase number) = do
  Env _ (AppState easterEgg) <- ask
  runFlow $ echoFlow easterEgg (Just phrase) (Just number)

-- Echo server entry point
runEchoServer :: Text -> R.FlowRuntime -> Int -> IO ()
runEchoServer easterEgg flowRt port = do
  putStrLn @String $ "Starting Echo Server on port " ++ show port ++ "..."
  let appSt = AppState easterEgg
  let env = Env flowRt appSt
  run port $ echoApp env
