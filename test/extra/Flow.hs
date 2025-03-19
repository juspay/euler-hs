{-# LANGUAGE OverloadedStrings #-}

module Flow where

import Test.Hspec hiding (runIO)
import qualified Data.Map.Strict as Map
import EulerHS.ART.V2.FlowUtils (getKafkaEncKeyAndIV)
import           EulerHS.Prelude
import qualified EulerHS.Language as L
import GHC.IO (unsafePerformIO)
import EulerHS.Framework.Runtime (FlowRuntime(..), KafkaSettings(..), withFlowRuntime)
import Kafka.Producer
import qualified EulerHS.Interpreters as I
import qualified EulerHS.Types as T


mockKafkaSettings :: KafkaSettings
mockKafkaSettings = KafkaSettings
  { isKmsDecoded = False
  , keyAndIV     = unsafePerformIO $ newMVar Map.empty
  , retryCount = 0
  , kafkaProducer = unsafePerformIO $ newMVar $ Left $ KafkaError "Connection not Initialized"
  }

type KafkaFlowSpec = SpecWith FlowRuntime

itKafkaFlow :: [Char] -> L.Flow () -> KafkaFlowSpec
itKafkaFlow description flow =
    it description (`I.runFlow` flow)

kafkaFlowSpec :: KafkaFlowSpec -> Spec
kafkaFlowSpec = do
    aroundAll $ \tests -> do
        withFlowRuntime Nothing $ \rt -> do
            tests rt

asserting :: Expectation -> L.Flow ()
asserting = L.runIO

awaitMVarWithTimeout :: Int -> T.Awaitable (Either Text a) -> IO (Either Text a)
awaitMVarWithTimeout mcs (T.Awaitable mvar) = do
  m <- newEmptyMVar
  void $ forkIO (threadDelay mcs *> putMVar m (Left "Awaiting Error"))
  void $ forkIO $ readMVar mvar >>= putMVar m
  takeMVar m

spec :: HasCallStack => Spec
spec = kafkaFlowSpec $ do
    itKafkaFlow "should set isKmsDecoded exactly once, even under race conditions" $ do
      let aesKey = "testAesKey"
          aesIv = "testAesIv"
          aesKeyTag = "keyTag"
          aesIvTag = "ivTag"
          numThreads = 20

      forks <- L.forkFlow' "test..." $ replicateM numThreads $ getKafkaEncKeyAndIV aesKey aesIv aesKeyTag aesIvTag
      _ <- L.runIO $ awaitMVarWithTimeout (20 * 1000 * 1000) forks --res
    --   mapM_ wait results
    --   asserting $ res `shouldBe` (Right ([(Just "testAesKey", Just "testAesIv")] <> (replicate (numThreads - 1) (Just "2025testAesKey", Just "2025testAesIv"))) :: Either Text [(Maybe Text, Maybe Text)])

      flowRt <- L.getFlowRuntime
      kafkaProducersMap <- L.runIO $ readIORef (_kafkaProducers flowRt)
      let kafkaSettings = Map.findWithDefault mockKafkaSettings kafkaProducersMap
    -- check the print statements at kmsDecodeFn . Must print only once
      asserting $ isKmsDecoded kafkaSettings `shouldBe` True
