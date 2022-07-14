module KVDB.KVDBSpec where

import           Test.Hspec hiding (runIO)

import           EulerHS.Interpreters
import qualified EulerHS.Language as L
import           EulerHS.Prelude
import           EulerHS.Runtime
import qualified EulerHS.Types as T

redisName = "eulerKVDB"

redisCfg = T.mkKVDBConfig redisName T.defaultKVDBConnConfig

spec :: Spec
spec =
  around (withFlowRuntime Nothing) $

    describe "EulerHS KVDB tests" $ do

      it "Double connection initialization should fail" $ \rt -> do
        eRes <- runFlow rt $ do
          eConn1 <- L.initKVDBConnection redisCfg
          eConn2 <- L.initKVDBConnection redisCfg
          case (eConn1, eConn2) of
            (Left err, _) -> pure $ Left $ "Failed to connect 1st time: " <> show err
            (_, Left (T.KVDBError T.KVDBConnectionAlreadyExists msg)) -> pure $ Right ()
            (_, Left err) -> pure $ Left $ "Unexpected error type on 2nd connect: " <> show err
        eRes `shouldBe` Right ()

      it "Get uninialized connection should fail" $ \rt -> do
        eRes <- runFlow rt $ do
          eConn <- L.getKVDBConnection redisCfg
          case eConn of
            Left (T.KVDBError T.KVDBConnectionDoesNotExist msg) -> pure $ Right ()
            Left err -> pure $ Left $ "Unexpected error: " <> show err
            Right _ -> pure $ Left "Unexpected connection success"
        eRes `shouldBe` Right ()

      it "Init and get connection should succeed" $ \rt -> do
        eRes <- runFlow rt $ do
          eConn1 <- L.initKVDBConnection redisCfg
          eConn2 <- L.getKVDBConnection redisCfg
          case (eConn1, eConn2) of
            (Left err, _) -> pure $ Left $ "Failed to connect: " <> show err
            (_, Left err) -> pure $ Left $ "Unexpected error on get connection: " <> show err
            _             -> pure $ Right ()
        eRes `shouldBe` Right ()

      it "Init and double get connection should succeed" $ \rt -> do
        eRes <- runFlow rt $ do
          eConn1 <- L.initKVDBConnection redisCfg
          eConn2 <- L.getKVDBConnection redisCfg
          eConn3 <- L.getKVDBConnection redisCfg
          case (eConn1, eConn2, eConn3) of
            (Left err, _, _) -> pure $ Left $ "Failed to connect: " <> show err
            (_, Left err, _) -> pure $ Left $ "Unexpected error on 1st get connection: " <> show err
            (_, _, Left err) -> pure $ Left $ "Unexpected error on 2nd get connection: " <> show err
            _                -> pure $ Right ()
        eRes `shouldBe` Right ()

      it "getOrInitKVDBConn should succeed" $ \rt -> do
        eRes <- runFlow rt $ do
          eConn <- L.getOrInitKVDBConn redisCfg
          case eConn of
            Left err -> pure $ Left $ "Failed to connect: " <> show err
            _        -> pure $ Right ()
        eRes `shouldBe` Right ()

      it "Prepared connection should be available" $ \rt -> do
        void $ runFlow rt $ do
          eConn <- L.initKVDBConnection redisCfg
          when (isLeft eConn) $ error "Failed to prepare connection."
        void $ runFlow rt $ do
          eConn <- L.getKVDBConnection redisCfg
          when (isLeft eConn) $ error "Failed to get prepared connection."

      it "Redis binary strings 1" $ \rt -> do
        let key = "a\xfcज" :: ByteString
        let value = "bbbex\xfc\xffकखगघङचछज" :: ByteString
        result <- runFlow rt $ do
          eConn <- L.initKVDBConnection redisCfg
          case eConn of
            Left err ->
              error $ "Failed to get prepared connection: " <> show err
            Right conn -> do
              let hour = 60 * 60
              L.runKVDB redisName $ do
                L.setex key hour value
                res <- L.get key
                L.del [key]
                pure res
        result `shouldBe` Right (Just value)

      it "Redis binary strings 2" $ \rt -> do
        let key = "a\xfcज" :: ByteString
        let value = "bbbex\xfc\xffकखगघङचछज" :: ByteString
        result <- runFlow rt $ do
          eConn <- L.initKVDBConnection redisCfg
          case eConn of
            Left err ->
              error $ "Failed to get prepared connection: " <> show err
            Right conn -> do
              L.rSetB redisName key value
              L.rGetB redisName key
        result `shouldBe` Just value

      it "Redis unicode" $ \rt -> do
        let key = "a\xfcज" :: Text
        let value = "bbbex\xfc\xffकखगघङचछज" :: Text
        result <- runFlow rt $ do
          eConn <- L.initKVDBConnection redisCfg
          case eConn of
            Left err ->
              error $ "Failed to get prepared connection: " <> show err
            Right conn -> do
              L.rSetT redisName key value
              L.rGetT redisName key
        result `shouldBe` Just value

      it "Redis unicode + json" $ \rt -> do
        let key = "a\xfcज" :: Text
        let value = "bbbex\xfc\xffकखगघङचछज" :: Text
        result <- runFlow rt $ do
          eConn <- L.initKVDBConnection redisCfg
          case eConn of
            Left err ->
              error $ "Failed to get prepared connection: " <> show err
            Right conn -> do
              L.rSet redisName key value
              L.rGet redisName key
        result `shouldBe` Just value
      it "Redis set functions" $ \rt -> do
        let key = "abc" :: ByteString
        let value = ["hello", "world"] :: [ByteString]
        result <- runFlow rt $ do
          eConn <- L.initKVDBConnection redisCfg
          case eConn of
            Left err ->
              error $ "Failed to get prepared connection: " <> show err
            Right conn -> do
              void $ L.rSadd redisName key value
              L.rSismember redisName key (head value)
        result `shouldBe` (Right True)
