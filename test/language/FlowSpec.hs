{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wwarn=deprecations #-}

module FlowSpec (spec) where

import           Client (externalServerPort, getBook, getUser,
                         port)
import           Common (clientHttpCert, initRTWithManagers,
                         withCertV1SecureServer, withClientTlsAuthServer,
                         withSecureServer, withServer)
import qualified Control.Exception as E
import           Data.Either.Extra (fromLeft')
import           Data.Maybe (fromJust)
import qualified Data.Text as Text
import qualified Data.UUID as UUID (fromText)
import           Data.X509.CertificateStore (readCertificateStore)
import           Servant.Client (BaseUrl (..), ClientError (..), Scheme (..))
import           Servant.Server (err403, errBody)
import           Test.Hspec (Spec, around, around_, describe, it, shouldBe,
                             shouldSatisfy)

import           EulerHS.Interpreters (runFlow)
import           EulerHS.Language as L
import           EulerHS.Prelude hiding (get)
import           EulerHS.Runtime (createLoggerRuntime, withFlowRuntime)
import           EulerHS.TestData.Types (NTTestKeyWithIntPayload (NTTestKeyWithIntPayload),
                                         NTTestKeyWithIntPayloadAnotherEnc (NTTestKeyWithIntPayloadAnotherEnc),
                                         NTTestKeyWithStringPayload (NTTestKeyWithStringPayload),
                                         NTTestKeyWithStringPayloadAnotherEnc (NTTestKeyWithStringPayloadAnotherEnc),
                                         TestIntKey (TestIntKey),
                                         TestIntKey2 (TestIntKey2),
                                         TestKVals (TestKVals),
                                         TestKeyWithIntPayload (TestKeyWithIntPayload),
                                         TestKeyWithIntPayloadAnotherEnc (TestKeyWithIntPayloadAnotherEnc),
                                         TestKeyWithStringPayload (TestKeyWithStringPayload),
                                         TestKeyWithStringPayloadAnotherEnc (TestKeyWithStringPayloadAnotherEnc),
                                         TestStringKey (TestStringKey),
                                         TestStringKey2 (TestStringKey2),
                                         TestStringKey2AnotherEnc (TestStringKey2AnotherEnc),
                                         TestStringKeyAnotherEnc (TestStringKeyAnotherEnc),
                                         mbNTTestKeyWithIntPayloadAnotherEncS1,
                                         mbNTTestKeyWithIntPayloadAnotherEncS2,
                                         mbNTTestKeyWithIntPayloadS1,
                                         mbNTTestKeyWithIntPayloadS2,
                                         mbNTTestKeyWithStringPayloadAnotherEncS1,
                                         mbNTTestKeyWithStringPayloadAnotherEncS2,
                                         mbNTTestKeyWithStringPayloadS1,
                                         mbNTTestKeyWithStringPayloadS2,
                                         mbTestIntKey, mbTestIntKey2,
                                         mbTestKeyWithIntPayloadAnotherEncS1,
                                         mbTestKeyWithIntPayloadAnotherEncS2,
                                         mbTestKeyWithIntPayloadS1,
                                         mbTestKeyWithIntPayloadS2,
                                         mbTestKeyWithStringPayloadAnotherEncS1,
                                         mbTestKeyWithStringPayloadAnotherEncS2,
                                         mbTestKeyWithStringPayloadS1,
                                         mbTestKeyWithStringPayloadS2,
                                         mbTestStringKey, mbTestStringKey2,
                                         mbTestStringKey2AnotherEnc,
                                         mbTestStringKeyAnotherEnc)
import           EulerHS.Types (HttpManagerNotFound (..), defaultFlowFormatter,
                                getResponseCode)
import qualified EulerHS.Types as T

-- import           EulerHS.Testing.Types (FlowMockedValues' (..))
-- import           EulerHS.Testing.Flow.Interpreter (runFlowWithTestInterpreter)
-- import           Scenario1 (testScenario1)
-- import           Unsafe.Coerce (unsafeCoerce)

-- import Debug.Trace


spec :: Maybe T.LoggerConfig -> Spec
spec loggerCfg = do
  describe "EulerHS flow language tests" $ do
    around (withFlowRuntime (map (createLoggerRuntime defaultFlowFormatter Nothing) loggerCfg)) $ do

      -- describe "TestInterpreters" $ do
      --   xit "testScenario1" $ \rt -> do
      --     mv <- newMVar scenario1MockedValues
      --     res <- runFlowWithTestInterpreter mv rt testScenario1
      --     res `shouldBe` User "John" "Snow" "00000000-0000-0000-0000-000000000000"

      around_ withCertV1SecureServer $ do
        describe "support for V1 certificates" $ do
          it "manager with V1 support connects well" $ \ _ -> do
            rt <- initRTWithManagers
            let req = T.httpGet $ "https://localhost:" <> show port
            -- TODO use correct manager
            resEither <- runFlow rt $ callHTTP' (Just "v1CertsSupport") req
            resEither `shouldSatisfy` isRight
            let code = getResponseCode $ fromRight (error "res is left") resEither
            code `shouldBe` 404
          it "by default there is no support for V1 certificates" $ \ rt -> do
            let req = T.httpGet $ "https://localhost:" <> show port
            resEither <- runFlow rt $ callHTTP req
            resEither `shouldSatisfy` isLeft

      around_ withServer $ do
        describe "callAPI tests with server" $ do
          it "Simple request (book) with default manager" $ \rt -> do
            let url = BaseUrl Http "localhost" port ""
            bookEither <- runFlow rt $ callAPI url getBook
            bookEither `shouldSatisfy` isRight
          it "Simple request (user) with default manager" $ \rt -> do
            let url = BaseUrl Http "localhost" port ""
            userEither <- runFlow rt $ callAPI url getUser
            userEither `shouldSatisfy` isRight
          it "Simple request (book) with manager1" $ \_ -> do
            rt <- initRTWithManagers
            let url = BaseUrl Http "localhost" port ""
            bookEither <- runFlow rt $ callAPI' (Just "manager1") url getBook
            bookEither `shouldSatisfy` isRight
          it "Simple request (user) with manager2" $ \_ -> do
            rt <- initRTWithManagers
            let url = BaseUrl Http "localhost" port ""
            userEither <- runFlow rt $ callAPI' (Just "manager2") url getUser
            userEither `shouldSatisfy` isRight
          it "Simple request with not existing manager" $ \_ -> do
            rt <- initRTWithManagers
            let url = BaseUrl Http "localhost" port ""
            let err = displayException (ConnectionError (toException $ HttpManagerNotFound "notexist"))
            userEither <- runFlow rt $ callAPI' (Just "notexist") url getUser
            case userEither of
              Left e  -> displayException e `shouldBe` err
              Right _ -> fail "Success result not expected"

      describe "callAPI tests without server" $ do
        it "Simple request (book)" $ \rt -> do
          let url = BaseUrl Http "localhost" port ""
          bookEither <- runFlow rt $ callAPI url getBook
          bookEither `shouldSatisfy` isLeft
        it "Simple request (user)" $ \rt -> do
          let url = BaseUrl Http "localhost" port ""
          userEither <- runFlow rt $ callAPI url getUser
          userEither `shouldSatisfy` isLeft

      describe "calling external TLS services with untyped API" $ do
        around_ withSecureServer $ do
          it "calling secure service using unsecured protocol fails" $ \ rt -> do
            let req = T.httpGet $ "http://localhost:" <> show port
            resEither <- runFlow rt $ callHTTP req
            resEither `shouldSatisfy` isRight
            let code = getResponseCode $ fromRight (error "res is left") resEither
            code `shouldBe` 426
          it "server certificates with unknown CA gets rejected" $ \ rt -> do
            let req = T.httpGet $ "https://localhost:" <> show port
            resEither <- runFlow rt $ callHTTP req
            resEither `shouldSatisfy` isLeft
            (fromLeft' resEither) `shouldSatisfy` (\m -> Text.count "certificate has unknown CA" m == 1)
          it "validate server certificate with custom CA" $ \ _ -> do
            rt <- initRTWithManagers
            let req = T.httpGet $ "https://localhost:" <> show port
            resEither <- runFlow rt $ callHTTP' (Just "tlsWithCustomCA") req
            resEither `shouldSatisfy` isRight
            let code = getResponseCode $ fromRight (error "res is left") resEither
            code `shouldBe` 404

      describe "TLS client authentication with untyped API" $ do
        around_ withClientTlsAuthServer $ do
          it "server rejects clients without a certificate" $ \ _ -> do
            rt <- initRTWithManagers
            let req = T.httpGet $ "https://localhost:" <> show port
            resEither <- runFlow rt $ callHTTP' (Just "manager1") req
            resEither `shouldSatisfy` isLeft
          it "authenticate client by a certificate" $ \ _ -> do
            rt <- initRTWithManagers
            let req = T.httpGet $ "https://localhost:" <> show port
            resEither <- runFlow rt $ callHTTP' (Just "tlsWithClientCertAndCustomCA")  req
            resEither `shouldSatisfy` isRight
            let code = getResponseCode $ fromRight (error "res is left") resEither
            code `shouldBe` 404

      describe "calling external TLS services with well-typed API" $ do
        around_ withSecureServer $ do
          it "calling secure service using unsecured protocol fails" $ \ _ -> do
            rt <- initRTWithManagers
            let url = BaseUrl Http "localhost" port ""
            bookEither <- runFlow rt $ callAPI' (Just "manager1") url getBook
            bookEither `shouldSatisfy` isLeft
          it "server certificates with unknown CA gets rejected" $ \ _ -> do
            rt <- initRTWithManagers
            let url = BaseUrl Https "localhost" port ""
            bookEither <- runFlow rt $ callAPI' (Just "manager1") url getBook
            bookEither `shouldSatisfy` isLeft
          it "validate server certificate with custom CA" $ \ _ -> do
            rt <- initRTWithManagers
            let url = BaseUrl Https "localhost" port ""
            bookEither <- runFlow rt $ callAPI' (Just "tlsWithCustomCA") url getBook
            bookEither `shouldSatisfy` isRight

      describe "TLS client authentication" $ do
        around_ withClientTlsAuthServer $ do
          it "server rejects clients without a certificate" $ \ _ -> do
            rt <- initRTWithManagers
            let url = BaseUrl Https "localhost" externalServerPort ""
            bookEither <- runFlow rt $ callAPI' (Just "manager1") url getBook
            bookEither `shouldSatisfy` isLeft
          it "authenticate client by a certificate" $ \ _ -> do
            rt <- initRTWithManagers
            let url = BaseUrl Https "localhost" externalServerPort ""
            bookEither <- runFlow rt $ callAPI' (Just "tlsWithClientCertAndCustomCA") url getBook
            bookEither `shouldSatisfy` isRight

          it "authenticate client by a ad-hoc certificate using callHTTPWithCert without custom CA store, with cert" $ \ rt -> do
            let req = T.httpGet $ "https://localhost:" <> show externalServerPort
            cert  <- clientHttpCert
            resEither <- runFlow rt $ L.callHTTPWithCert req $ Just cert
            resEither `shouldSatisfy` isLeft
            (fromLeft' resEither) `shouldSatisfy` (\m -> Text.count "certificate has unknown CA" m == 1)

          it "authenticate client by a ad-hoc certificate using callHTTPWithCert without custom CA store, without cert" $ \ rt -> do
            let req = T.httpGet $ "https://localhost:" <> show externalServerPort
            resEither <- runFlow rt $ L.callHTTPWithCert req Nothing
            resEither `shouldSatisfy` isLeft
            (fromLeft' resEither) `shouldSatisfy` (\m -> Text.count "certificate has unknown CA" m == 1)

          it "authenticate client by an ad-hoc certificate with callHTTP" $ \ rt -> do
            let req = T.httpGet $ "https://localhost:" <> show externalServerPort
            cert  <- clientHttpCert
            store <- fromJust <$> readCertificateStore "test/tls/ca-certificates"
            resEither <- runFlow rt $ do
              -- Here we call getHTTPManager twice as a smoke test for LRU cache,
              -- Eq and Ord insatnces for CertificateStore'
              let settings = T.withClientTls cert <> T.withCustomCA store
              mgr <- L.getHTTPManager settings
              _ <- L.getHTTPManager settings
              L.callHTTPUsingManager mgr req
            resEither `shouldSatisfy` isRight

          it "authenticate client by an ad-hoc certificate with callAPI" $ \ rt -> do
            cert  <- clientHttpCert
            store <- fromJust <$> readCertificateStore "test/tls/ca-certificates"
            resEither <- runFlow rt $ do
              -- Here we call getHTTPManager twice as a smoke test for LRU cache,
              -- Eq and Ord insatnces for CertificateStore'
              let settings = T.withClientTls cert <> T.withCustomCA store
              mgr <- L.getHTTPManager settings
              _ <- L.getHTTPManager settings
              let url = BaseUrl Https "localhost" externalServerPort ""
              L.callAPIUsingManager mgr url getBook
            resEither `shouldSatisfy` isRight

      describe "runIO tests" $ do
        it "RunIO" $ \rt -> do
          result <- runFlow rt $ runIO (pure ("hi" :: String))
          result `shouldBe` "hi"
        it "RunIO with exception" $ \rt -> do
          result <- E.catch
            (runFlow rt $ do
              _ <- runIO ioActWithException
              pure ("Never returned" :: Text))
            (\e -> do let err = show (e :: E.AssertionFailed)
                      pure err)
          result `shouldBe` ("Exception from IO" :: Text)
        it "RunIO with catched exception" $ \rt -> do
          result <-runFlow rt $ do
              runIO $
                E.catch
                  ioActWithException
                  (\e -> do let err = show (e :: E.AssertionFailed)
                            pure err)
          result `shouldBe` ("Exception from IO" :: Text)
        it "RunUntracedIO" $ \rt -> do
          result <- runFlow rt $ runIO (pure ("hi" :: String))
          result `shouldBe` "hi"
      describe "withRunFlow" $ do
        it "works" $ \rt -> do
          let withResource :: (Int -> IO a) -> IO a
              withResource act = do
                threadDelay 10
                act 42

              action :: Int -> Flow Int
              action res = runIO $ do
                threadDelay 10
                pure res
          result <- runFlow rt $ do
            L.withRunFlow $ \run -> do
              withResource $ \res -> do
                run (action res)
          result `shouldBe` 42
      describe "STM tests" $ do
        it "STM Test" $ \rt -> do
          result <- runFlow rt $ do
            countVar <- runIO $ newTVarIO (0 :: Int)
            let
              updateCount = do
                count <- readTVar countVar
                when (count < 100) (writeTVar countVar (count + 1))
                readTVar countVar
            let
              countTo100 = do
                count <- atomically updateCount
                if count < 100
                  then countTo100
                  else return count
            awaitable1 <- forkFlow' "counter1" $ runIO $ void countTo100
            awaitable2 <- forkFlow' "counter2" $ runIO $ void countTo100
            _ <- await Nothing awaitable1 >> await Nothing awaitable2
            runIO $ readTVarIO countVar
          result `shouldBe` 100

      describe "Options" $ do
        it "One key" $ \rt -> do
          result <- runFlow rt $ do
            _ <- setOption TestStringKey "lore ipsum"
            getOption TestStringKey
          result `shouldBe` Just "lore ipsum"
        it "Not found" $ \rt -> do
          result <- runFlow rt $ do
            _ <- setOption TestStringKey "lore ipsum"
            getOption TestStringKey2
          result `shouldBe` Nothing
        it "Two keys" $ \rt -> do
          result <- runFlow rt $ do
            _ <- setOption TestStringKey "lore ipsum"
            _ <- setOption TestStringKey2 "lore ipsum2"
            s1 <- getOption TestStringKey
            s2 <- getOption TestStringKey2
            pure (s1,s2)
          result `shouldBe` (Just "lore ipsum", Just "lore ipsum2")
        it "Delete Key" $ \rt -> do
          result <- runFlow rt $ do
            _ <- setOption TestStringKey "lorem ipsum"
            s1 <- getOption TestStringKey
            _ <- delOption TestStringKey
            s2 <- getOption TestStringKey
            pure (s1, s2)
          result `shouldBe` (Just "lorem ipsum", Nothing)
        it "Different encoding, types & payload" $ \rt -> do
          testKVals <- runFlow rt $ do
            _     <- setOption TestStringKey "mbTestStringKey"
            _     <- setOption TestStringKey2 "mbTestStringKey2"
            _     <- setOption TestIntKey 1001
            _     <- setOption TestIntKey2 2002
            _     <- setOption TestStringKeyAnotherEnc "mbTestStringKeyAnotherEnc"
            _     <- setOption TestStringKey2AnotherEnc "mbTestStringKey2AnotherEnc"
            _     <- setOption (TestKeyWithStringPayload "SP1") "mbTestKeyWithStringPayloadS1"
            _     <- setOption (TestKeyWithStringPayload "SP2") "mbTestKeyWithStringPayloadS2"
            _     <- setOption (TestKeyWithIntPayload 1001) "mbTestKeyWithIntPayloadS1"
            _     <- setOption (TestKeyWithIntPayload 2002) "mbTestKeyWithIntPayloadS2"
            _     <- setOption (TestKeyWithStringPayloadAnotherEnc "SP1") "mbTestKeyWithStringPayloadAnotherEncS1"
            _     <- setOption (TestKeyWithStringPayloadAnotherEnc "SP2") "mbTestKeyWithStringPayloadAnotherEncS2"
            _     <- setOption (TestKeyWithIntPayloadAnotherEnc 1001) "mbTestKeyWithIntPayloadAnotherEncS1"
            _     <- setOption (TestKeyWithIntPayloadAnotherEnc 2002) "mbTestKeyWithIntPayloadAnotherEncS2"
            _     <- setOption (NTTestKeyWithStringPayload "SP1") "mbNTTestKeyWithStringPayloadS1"
            _     <- setOption (NTTestKeyWithStringPayload "SP2") "mbNTTestKeyWithStringPayloadS2"
            _     <- setOption (NTTestKeyWithIntPayload 1001) 2333
            _     <- setOption (NTTestKeyWithIntPayload 2002) 3322
            _     <- setOption (NTTestKeyWithStringPayloadAnotherEnc "SP1") "mbNTTestKeyWithStringPayloadAnotherEncS1"
            _     <- setOption (NTTestKeyWithStringPayloadAnotherEnc "SP2") "mbNTTestKeyWithStringPayloadAnotherEncS2"
            _     <- setOption (NTTestKeyWithIntPayloadAnotherEnc 1001) 9009
            _     <- setOption (NTTestKeyWithIntPayloadAnotherEnc 2002) 1001
            TestKVals
               <$> getOption TestStringKey
               <*> getOption TestStringKey2
               <*> getOption TestIntKey
               <*> getOption TestIntKey2
               <*> getOption TestStringKeyAnotherEnc
               <*> getOption TestStringKey2AnotherEnc
               <*> getOption (TestKeyWithStringPayload "SP1")
               <*> getOption (TestKeyWithStringPayload "SP2")
               <*> getOption (TestKeyWithIntPayload 1001)
               <*> getOption (TestKeyWithIntPayload 2002)
               <*> getOption (TestKeyWithStringPayloadAnotherEnc "SP1")
               <*> getOption (TestKeyWithStringPayloadAnotherEnc "SP2")
               <*> getOption (TestKeyWithIntPayloadAnotherEnc 1001)
               <*> getOption (TestKeyWithIntPayloadAnotherEnc 2002)
               <*> getOption (NTTestKeyWithStringPayload "SP1")
               <*> getOption (NTTestKeyWithStringPayload "SP2")
               <*> getOption (NTTestKeyWithIntPayload 1001)
               <*> getOption (NTTestKeyWithIntPayload 2002)
               <*> getOption (NTTestKeyWithStringPayloadAnotherEnc "SP1")
               <*> getOption (NTTestKeyWithStringPayloadAnotherEnc "SP2")
               <*> getOption (NTTestKeyWithIntPayloadAnotherEnc 1001)
               <*> getOption (NTTestKeyWithIntPayloadAnotherEnc 2002)

          testKVals `shouldBe` TestKVals
                  { mbTestStringKey                          = Just "mbTestStringKey"
                  , mbTestStringKey2                         = Just "mbTestStringKey2"
                  , mbTestIntKey                             = Just 1001
                  , mbTestIntKey2                            = Just 2002
                  , mbTestStringKeyAnotherEnc                = Just "mbTestStringKeyAnotherEnc"
                  , mbTestStringKey2AnotherEnc               = Just "mbTestStringKey2AnotherEnc"
                  , mbTestKeyWithStringPayloadS1             = Just "mbTestKeyWithStringPayloadS1"
                  , mbTestKeyWithStringPayloadS2             = Just "mbTestKeyWithStringPayloadS2"
                  , mbTestKeyWithIntPayloadS1                = Just "mbTestKeyWithIntPayloadS1"
                  , mbTestKeyWithIntPayloadS2                = Just "mbTestKeyWithIntPayloadS2"
                  , mbTestKeyWithStringPayloadAnotherEncS1   = Just "mbTestKeyWithStringPayloadAnotherEncS1"
                  , mbTestKeyWithStringPayloadAnotherEncS2   = Just "mbTestKeyWithStringPayloadAnotherEncS2"
                  , mbTestKeyWithIntPayloadAnotherEncS1      = Just "mbTestKeyWithIntPayloadAnotherEncS1"
                  , mbTestKeyWithIntPayloadAnotherEncS2      = Just "mbTestKeyWithIntPayloadAnotherEncS2"
                  , mbNTTestKeyWithStringPayloadS1           = Just "mbNTTestKeyWithStringPayloadS1"
                  , mbNTTestKeyWithStringPayloadS2           = Just "mbNTTestKeyWithStringPayloadS2"
                  , mbNTTestKeyWithIntPayloadS1              = Just 2333
                  , mbNTTestKeyWithIntPayloadS2              = Just 3322
                  , mbNTTestKeyWithStringPayloadAnotherEncS1 = Just "mbNTTestKeyWithStringPayloadAnotherEncS1"
                  , mbNTTestKeyWithStringPayloadAnotherEncS2 = Just "mbNTTestKeyWithStringPayloadAnotherEncS2"
                  , mbNTTestKeyWithIntPayloadAnotherEncS1    = Just 9009
                  , mbNTTestKeyWithIntPayloadAnotherEncS2    = Just 1001
                  }
      it "RunSysCmd" $ \rt -> do
        result <- runFlow rt $ runSysCmd "echo test"
        result `shouldBe` "test\n"
      it "RunSysCmd with bad command" $ \rt -> do
        putStrLn ("" :: Text)
        result <- E.catch
          (runFlow rt $ runSysCmd "badEcho test")
          (\e -> do let err = show (e :: E.SomeException)
                    pure err)
        result `shouldBe` ("readCreateProcess: badEcho test (exit 127): failed" :: String)
      it "GenerateGUID" $ \rt -> do
        guid <- runFlow rt generateGUID
        let maybeGUID = UUID.fromText guid
        maybeGUID `shouldSatisfy` isJust
      it "ThrowException" $ \rt -> do
        result <- E.catch
          (runFlow rt $ do
            _ <- throwException (E.AssertionFailed "Exception message")
            pure @_ @Text "Never returned")
          (\e -> do let err = show (e :: E.AssertionFailed)
                    pure err)
        result `shouldBe` "Exception message"

      describe "ForkFlow" $ do
        let i :: Int = 101
        it "Fork and successful await infinitely" $ \rt -> do
          let flow = do
                awaitable <- forkFlow' "101" (pure i)
                await Nothing awaitable
          result <- runFlow rt flow
          result `shouldBe` Right 101
        it "SafeFlow, fork and successful await infinitely" $ \rt -> do
          let flow = do
                awaitable <- forkFlow' "101" $ runSafeFlow (pure i :: Flow Int)
                await Nothing awaitable
          result <- runFlow rt flow
          result `shouldBe` Right (Right 101)
        it "SafeFlow with exception, fork and successful await infinitely" $ \rt -> do
          let flow = do
                awaitable <- forkFlow' "101" (throwException err403 {errBody = "403"} :: Flow Text)
                await Nothing awaitable
          result <- runFlow rt flow
          result `shouldBe` Left (T.ForkedFlowError $ show err403 {errBody = "403"})
        it "Safe flow with exception and return power" $ \rt -> do
          let flow = do
                void $ runSafeFlow (throwException err403 {errBody = "403"} :: Flow Text)
                runIO (pure ("hi" :: String))
          result <- runFlow rt flow
          result `shouldBe` "hi"
        it "Safe flow, RunSysCmd" $ \rt -> do
          let flow = do
                runSafeFlow $ L.runSysCmd $ "echo " <> "safe hello"
          result <- runFlow rt flow
          result `shouldBe` Right "safe hello\n"
        it "Fork and successful await with a sufficient timeout 1" $ \rt -> do
          let flow = do
                awaitable <- forkFlow' "101" (pure i)
                await (Just $ T.Microseconds 1000000) awaitable
          result <- runFlow rt flow
          result `shouldBe` Right 101
        it "Fork and successful await with a sufficient timeout 2" $ \rt -> do
          let flow = do
                awaitable <- forkFlow' "101" (runIO (threadDelay 1000) >> pure i)
                await (Just $ T.Microseconds 1000000) awaitable
          result <- runFlow rt flow
          result `shouldBe` Right 101
        it "Fork and successful await with an unsufficient timeout" $ \rt -> do
          let flow = do
                awaitable <- forkFlow' "101" (runIO (threadDelay 1000000) >> pure i)
                await (Just $ T.Microseconds 1000) awaitable
          result <- runFlow rt flow
          result `shouldBe` Left T.AwaitingTimeout
        it "Fork and successful await for 2 flows" $ \rt -> do
          let flow = do
                awaitable1 <- forkFlow' "101" (runIO (threadDelay 10000) >> pure i)
                awaitable2 <- forkFlow' "102" (runIO (threadDelay 100000) >> pure (i+1))
                mbRes1 <- await Nothing awaitable1
                mbRes2 <- await Nothing awaitable2
                pure (mbRes1, mbRes2)
          result <- runFlow rt flow
          result `shouldBe` (Right 101, Right 102)
        it "Fork and successful await 1 of 2 flows" $ \rt -> do
          let flow = do
                awaitable1 <- forkFlow' "101" (runIO (threadDelay 10000) >> pure i)
                awaitable2 <- forkFlow' "102" (runIO (threadDelay 1000000) >> pure (i+1))
                mbRes1 <- await Nothing awaitable1
                mbRes2 <- await (Just $ T.Microseconds 1000) awaitable2
                pure (mbRes1, mbRes2)
          result <- runFlow rt flow
          result `shouldBe` (Right 101, Left T.AwaitingTimeout)

-- Helpers

-- user :: Any
-- user = unsafeCoerce $ Right $ User "John" "Snow" "00000000-0000-0000-0000-000000000000"

-- localGUID :: Any
-- localGUID = unsafeCoerce ("FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF" :: String)

-- lhost :: ByteString
-- lhost = "localhost"

-- scenario1MockedValues :: FlowMockedValues'
-- scenario1MockedValues = FlowMockedValues'
--   { mockedCallServantAPI = [user]
--   , mockedRunIO = [localGUID]
--   , mockedGetOption = [lhost]
--   , mockedGenerateGUID = ["00000000-0000-0000-0000-000000000000"]
--   , mockedRunSysCmd = ["Neo"]
--   }

ioActWithException :: IO Text
ioActWithException = do
  _ <- E.throw (E.AssertionFailed "Exception from IO")
  pure "Text from IO"
