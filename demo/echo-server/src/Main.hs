module Main where

import           EulerHS.Prelude

import qualified EulerHS.Types         as T
import qualified EulerHS.Runtime       as R
import qualified EulerHS.Interpreters  as I
import qualified EulerHS.Language      as L

import EchoServer.API

import Network.Wai.Handler.Warp (run)
import Servant

data AppState = AppState
  { easterEgg :: Text
  }

data Env = Env !R.FlowRuntime !AppState
type AppHandler = ReaderT Env (ExceptT ServerError IO)
type AppServer = ServerT EchoAPI AppHandler


echoServer' :: AppServer
echoServer'
     = getEcho
  :<|> postEcho




echoServer :: Env -> Server EchoAPI
echoServer env = hoistServer echoAPI f echoServer'
  where
    f :: ReaderT Env (ExceptT ServerError IO) a -> Handler a
    f r = do
      eResult <- liftIO $ runExceptT $ runReaderT r env
      case eResult of
        Left err  -> throwError err
        Right res -> pure res

echoBackendApp :: Env -> Application
echoBackendApp = serve echoAPI . echoServer


runFlow :: L.Flow a -> AppHandler a
runFlow flow = do
  Env rt _ <- ask
  eRes <- lift $ lift $ try $ I.runFlow rt flow
  case eRes of
    Left (err :: SomeException) -> do
      liftIO $ putStrLn @String $ "Exception handled: " <> show err
      throwError err500
    Right res -> pure res



echoFlow :: Text -> Maybe Text -> Maybe Int -> L.Flow EchoMessage
echoFlow easterEgg mbPhrase mbNumber = do
  L.logDebug ("echoFlow" :: Text) $ "Prase and number got: " <> show (mbPhrase, mbNumber)
  let phrase = fromMaybe "" mbPhrase
  let number = fromMaybe 0 mbNumber
  pure $ EchoMessage phrase number easterEgg


getEcho :: Maybe Text -> Maybe Int -> AppHandler EchoMessage
getEcho mbPhrase mbNumber = do
  Env _ (AppState easterEgg) <- ask
  runFlow $ echoFlow easterEgg mbPhrase mbNumber


postEcho :: EchoRequest -> AppHandler EchoMessage
postEcho (EchoRequest phrase number) = do
  Env _ (AppState easterEgg) <- ask
  runFlow $ echoFlow easterEgg (Just phrase) (Just number)


loggerConfig :: T.LoggerConfig
loggerConfig = T.LoggerConfig
    { T._isAsync = False
    , T._logLevel = T.Debug
    , T._logFilePath = "/tmp/logs/myFlow.log"
    , T._logToConsole = True
    , T._logToFile = False
    , T._maxQueueSize = 1000
    , T._logRawSql = False
    }

runEchoServer :: Text -> Int -> IO ()
runEchoServer easterEgg port = do
  putStrLn @String $ "Starting Echo Server on port " ++ show port ++ "..."

  let mkLoggerRt = R.createLoggerRuntime T.defaultFlowFormatter loggerConfig

  R.withFlowRuntime (Just mkLoggerRt) $ \flowRt -> do
    let appSt = AppState easterEgg
    let env = Env flowRt appSt
    run port $ echoBackendApp env


main :: IO ()
main = runEchoServer "This is an easter egg." 8080
