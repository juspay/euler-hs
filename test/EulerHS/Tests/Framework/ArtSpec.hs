{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving    #-}

module EulerHS.Tests.Framework.ArtSpec where

import           Control.Monad (void)
import           Data.Aeson as A
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Map as Map
import qualified Data.String.Conversions as Conversions
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Encoding.Error as Encoding
import qualified Data.UUID as UUID (toText)
import qualified Data.UUID.V4 as UUID (nextRandom)
import qualified Data.Vector as V
import           Network.Wai.Handler.Warp
import           Servant.Client
import           Servant.Server
import qualified System.IO.Error as Error
import           Test.Hspec

import           EulerHS.Interpreters
import           EulerHS.Language as L
import           EulerHS.Prelude
import           EulerHS.Runtime
import           EulerHS.TestData.API.Client
import           EulerHS.TestData.Types
import           EulerHS.Tests.Framework.Common
import           EulerHS.Types as T


spec :: Spec
spec = do
  describe "ART Test" $ do
    it "Regular mode" $ do
      rt <- initRegularRT
      res <- runFlow rt $ mainScript
      res `shouldBe` "hello\n"

    it "Recorder mode" $ do
      flowRuntime <- initRecorderRT
      result      <- runFlow flowRuntime mainScript
      case _runMode flowRuntime of
        T.RecordingMode T.RecorderRuntime{recording} -> do
          T.ResultRecording{..} <- awaitRecording recording
          V.length recording        `shouldBe` 10
          Map.size forkedRecordings `shouldBe` 2
          result                    `shouldBe` "hello\n"
        _ -> fail "wrong mode"

    it "Player mode: replaying incorrect flow returns error (main flow)" $ do
      flowRuntime <- initRecorderRT
      _           <- runFlow flowRuntime mainScript
      case _runMode flowRuntime of
        T.RecordingMode T.RecorderRuntime{recording} -> do
          entries              <- awaitRecording recording
          playerRuntime        <- initPlayerRT entries
          -- TODO runFlow shoul catch all exceptions internally
          _ <- try @_ @SomeException $ runFlow playerRuntime mainScriptWrong
          case _runMode playerRuntime of
            T.ReplayingMode T.PlayerRuntime{rerror} -> do
              errors <- awaitErrors rerror
              flattenErrors errors `shouldNotBe` []
            _ -> fail "wrong mode"
        _ -> fail "wrong mode"

    it "Player mode: replaying incorrect flow returns error (fork flow)" $ do
      flowRuntime <- initRecorderRT
      _           <- runFlow flowRuntime mainScript
      case _runMode flowRuntime of
        T.RecordingMode T.RecorderRuntime{recording} -> do
          entries              <- awaitRecording recording
          playerRuntime        <- initPlayerRT entries
          -- TODO runFlow shoul catch all exceptions internally
          _ <- try @_ @SomeException $ runFlow playerRuntime mainScriptWrongFork
          case _runMode playerRuntime of
            T.ReplayingMode T.PlayerRuntime{rerror} -> do
              errors <- awaitErrors rerror
              flattenErrors errors `shouldNotBe` []
            _ -> fail "wrong mode"
        _ -> fail "wrong mode"

    it "Player mode: missing fork recording returns error (fork flow)" $ do
      flowRuntime <- initRecorderRT
      _           <- runFlow flowRuntime mainScript
      case _runMode flowRuntime of
        T.RecordingMode T.RecorderRuntime{recording} -> do
          entries              <- awaitRecording recording
          playerRuntime        <- initPlayerRT $ entries {forkedRecordings = Map.empty}
          -- TODO runFlow shoul catch all exceptions internally
          _ <- try @_ @SomeException $ runFlow playerRuntime mainScript
          case _runMode playerRuntime of
            T.ReplayingMode T.PlayerRuntime{rerror} -> do
              errors <- awaitErrors rerror
              flattenErrors errors `shouldNotBe` []
            _ -> fail "wrong mode"
        _ -> fail "wrong mode"

----------------------------------------------------------------------

    it "Set/Get Option" $ do
      let testOptionValue = "testOptionValue" :: String
      mopt <- runFlowWithArt $ do
        L.setOption TestStringKey testOptionValue
        L.getOption TestStringKey
      mopt `shouldBe` Just testOptionValue

    it "Generate distinct GUID" $ do
      (guid1, guid2) <- runFlowWithArt $ do
        guid1 <- L.generateGUID
        guid2 <- L.generateGUID
        pure (guid1, guid2)
      guid1 `shouldNotBe` guid2

    it "RunIO" $ do
      res <- runFlowWithArt $ do
        L.runIO $ pure ()
      res `shouldBe` ()

    it "RunIO" $ do
      res <- runFlowWithArt $ do
        L.runIO $ pure ()
      res `shouldBe` ()

    it "RunIO also works with Serializable types" $ do
      let bs :: ByteString = "Hello"
      res <- runFlowWithArt $ do
        L.runIO $ pure bs
      res `shouldBe` bs

    it "RunUntracedIO" $ do
      res <- runFlowWithArt $ do
        L.runUntracedIO $ pure ()
      res `shouldBe` ()

    -- run an example with non-deterministic outputs
    it "RunUntracedIO with UUID" $ do
      runFlowWithArt $ do
        L.runUntracedIO (UUID.toText <$> UUID.nextRandom)
        pure ()

    it "RunSysCmd" $ do
      let value = "hello"
      res <- runFlowWithArt $ do
        L.runSysCmd $ "echo " <> value
      res `shouldBe` "hello\n"

    it "Logging" $ runFlowWithArt $ do
      L.logInfo    "Info"    "L.logInfo"
      L.logError   "Error"   "L.logError"
      L.logDebug   "Debug"   "L.logDebug"
      L.logWarning "Warning" "L.logWarning"

    it "SafeFlow, throwException" $ do
      res <- runFlowWithArt $ do
        runSafeFlow $ (throwException err403 {errBody = "403"} :: Flow Text)
      res `shouldBe` (Left $ show err403{errBody = "403"})

    it "SafeFlow, RunSysCmd" $ do
      res <- runFlowWithArt $ do
        runSafeFlow $ L.runSysCmd $ "echo " <> "safe hello"
        runSafeFlow $ L.runSysCmd $ "echo " <> "safe hello2"
      res `shouldBe` (Right "safe hello2\n")

    it "Fork" $ runFlowWithArt $ do
      L.forkFlow "Fork" $
        L.logInfo "Fork" "Hello"

    it "SafeFlow and Fork" $ runFlowWithArt $ do
      runSafeFlow $ L.runSysCmd $ "echo " <> "safe hello"
      L.forkFlow "Fork" $
        L.logInfo "Fork" "Hello"

    it "SafeFlow exception and Fork" $ runFlowWithArt $ do
      runSafeFlow $ (throwException err403 {errBody = "403"} :: Flow Text)
      L.forkFlow "Fork" $
        L.logInfo "Fork" "Hello"

    it "Fork by fork" $ runFlowWithArt $ do
      L.forkFlow "Fork" $
        L.logInfo "Fork" "Hello"
      L.forkFlow "Fork 2" $
        L.logInfo "Fork 2" "Bye"

    it "SafeFlow and Fork" $ runFlowWithArt $ do
      runSafeFlow $ L.runSysCmd $ "echo " <> "safe hello"
      L.forkFlow "Fork" $
        L.logInfo "Fork" "Hello"

    it "Fork and flow from SafeFlow" $ do
      res <- runFlowWithArt $ do
        runSafeFlow $ do
          L.runSysCmd $ "echo " <> "safe hello"
          L.forkFlow "Fork" $
            L.logInfo "Fork" "Hello"
      res `shouldBe` (Right ())

    it "Flow and fork from SafeFlow" $ do
      res <- runFlowWithArt $ do
        runSafeFlow $ do
          L.forkFlow "Fork" $
            L.logInfo "Fork" "Hello"
          L.runSysCmd $ "echo " <> "safe hello"
      res `shouldBe` (Right "safe hello\n")

    it "Fork from Fork" $ runFlowWithArt $ do
      L.forkFlow "ForkOne" $ do
        L.logInfo "ForkOne" "Hello"
        L.forkFlow "ForkTwo" $
          L.forkFlow "ForkThree" $ do
            L.forkFlow "ForkFour" $
              L.logInfo "ForkFour" "Hello"

    it "Fork and safeFlow from Fork" $ runFlowWithArt $ do
      L.forkFlow "ForkOne" $ do
        L.logInfo "ForkOne" "Hello"
        runSafeFlow $ L.runSysCmd $ "echo " <> "safe hello"
        L.forkFlow "ForkTwo" $
          L.forkFlow "ForkThree" $ do
            L.forkFlow "ForkFour" $
              L.logInfo "ForkFour" "Hello"

    around_ withServer $ do
      describe "CallServantAPI tests" $ do
        it "Simple request (book)" $ do
          let url = BaseUrl Http "127.0.0.1" port ""
          bookEither <- runFlowWithArt $ callServantAPI Nothing url getBook
          bookEither `shouldSatisfy` isRight

        it "Simple request (user)" $ do
          let url = BaseUrl Http "127.0.0.1" port ""
          userEither <- runFlowWithArt $ callServantAPI Nothing url getUser
          userEither `shouldSatisfy` isRight

    xit "Untyped HTTP API Calls" $ do
      let url = "https://google.com"
      (statusCode, status, body, headers) <- runFlowWithArt $ do
        eResponse <- L.callHTTP $ T.httpGet "https://google.com" :: Flow (Either Text T.HTTPResponse)
        response <- case eResponse of
          Left err -> throwException err403 {errBody = "Expected a response"}
          Right response -> pure response
        return
          ( getResponseCode    response
          , getResponseStatus  response
          , getResponseBody    response
          , getResponseHeaders response
          )
      -- check status code
      statusCode `shouldBe` 200
      status `shouldBe` "OK"
      -- check body
      -- Lazy.putStr (getLBinaryString body)
      -- seem to be non-breaking latin-1 encoded spaces in what is supposed to
      -- be a UTF-8 output xD; show some leniency
      let
        body' =
          Encoding.decodeUtf8With
            Encoding.lenientDecode
            (Conversions.convertString body)
      Text.isInfixOf "google" body' `shouldBe` True
      Text.isInfixOf "<html" body' `shouldBe` True
      -- -- check headers
      case Map.lookup "content-type" headers of
        Nothing          ->
          throwM $ Error.userError "Expected a Content-Type header"
        Just headerValue -> do
          Text.isInfixOf "text/html" headerValue `shouldBe` True

    xit "Untyped HTTP API Calls" $ do
      let url = "https://127.0.0.1:666/fourohhhfour"
      result <- runFlowWithArt $ do
        L.callHTTP $ T.httpGet url :: Flow (Either Text T.HTTPResponse)

      err <- extractLeft result
      -- putStrLn $ "ERROR" <> err
      pure ()


extractLeft :: Either a b -> IO a
extractLeft eitherVal =
  case eitherVal of
    Left val ->
      pure val
    Right res ->
      throwM $ Error.userError "Expected Left from erroneous call!"

mainScript :: Flow String
mainScript = do
  guid1 <- generateGUID
  guid2 <- generateGUID
  -- This should re-execute each time and not break replay
  runUntracedIO (UUID.toText <$> UUID.nextRandom)
  forkFlow guid1 (void forkScript)
  forkFlow guid2 (void forkScript)
  runSysCmd "echo hello"

mainScriptWrong :: Flow String
mainScriptWrong = do
  guid1 <- generateGUID
  forkFlow guid1 (void forkScript)
  runSysCmd "echo hello"

mainScriptWrongFork :: Flow String
mainScriptWrongFork = do
  guid1 <- generateGUID
  guid2 <- generateGUID
  forkFlow guid1 (void forkScript)
  forkFlow guid2 (void forkScriptWrong)
  runSysCmd "echo hello"

forkScript :: Flow String
forkScript = do
  _ <- generateGUID
  runSysCmd "echo hello"

forkScriptWrong :: Flow String
forkScriptWrong = do
  runSysCmd "echo hello"
