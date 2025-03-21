module PubSubSpec
  (
    -- spec
  ) where

-- import           Common (emptyMVarWithWatchDog, replayRecording)
-- import           Data.Aeson
-- import qualified Database.Redis as R
-- import           EulerHS.Language as L
-- import           EulerHS.Prelude
-- import           EulerHS.Types as T
-- import           Test.Hspec

-- connectInfo :: R.ConnectInfo
-- connectInfo = R.defaultConnectInfo {R.connectHost = "redis"}

-- runWithRedisConn_ :: ResultRecording -> Flow b -> IO b
-- runWithRedisConn_ = replayRecording
-- -- runWithRedisConn_ = runWithRedisConn connectInfo

-- spec :: Spec
-- spec = do
--   describe "Publish/Subscribe subsystem tests" $ do
--     it "Callback receives messages from channel it subscribed to" $ do
--       let testMsg = "Hello, Tests"
--       let testCh  = "test"
--       (targetMVar, watch, _) <- emptyMVarWithWatchDog 1

--       result <- runWithRedisConn_ rr1 $ do
--         subscribe [Channel testCh] $ \msg -> L.runIO $
--           putMVar targetMVar msg

--         publish (Channel testCh) $ Payload testMsg

--         L.runIO watch
--       result `shouldBe` Just testMsg

    -- TODO: This test is brittle if replayed with pre-recorded ART-traces,
    --       as this ties it deeply to the implementation; instead we should
    --       test the externally-observable behaviour only
    --
    -- TODO: rework this test
    -- it "Pub/Sub works the same way if run in fork" $ do
    --   let testMsg = "Hello, Tests"
    --   let testCh  = "test"
    --   (targetMVar, watch, _) <- emptyMVarWithWatchDog 1

    --   waitSubscribe <- newEmptyMVar
    --   result <- runWithRedisConn_ rr2 $ do
    --   -- result <- runWithRedisConn connectInfo rr2 $ do
    --     L.forkFlow "Fork" $ do
    --       void $ subscribe [Channel testCh] $ \msg -> L.runIO $
    --         putMVar targetMVar msg
    --       void $ L.runIO $ putMVar waitSubscribe ()

    --     void $ L.runIO $ takeMVar waitSubscribe

    --     publish (Channel testCh) $ Payload testMsg

    --     L.runIO watch

    --   result `shouldBe` Just testMsg

--     it "Callback does not receive messages from channel after unsubscribe (subscribe method)" $ do
--       let testMsg = "Hello, Tests"
--       let testCh  = "test"
--       (targetMVar, watch, _) <- emptyMVarWithWatchDog 1

--       result <- runWithRedisConn_ rr3 $ do
--         unsubscribe <- subscribe [Channel testCh] $ \msg -> L.runIO $
--           putMVar targetMVar msg

--         unsubscribe

--         publish (Channel testCh) $ Payload testMsg

--         L.runIO watch

--       result `shouldBe` Nothing

--     it "Callback receives messages from channel it subscribed to, if pattern matches" $ do
--       let testMsg  = "Hello, Tests"
--       let testCh0  = "0test"
--       let testCh1  = "1test"
--       let testPatt = "?test"
--       (targetMVar, watch, reset) <- emptyMVarWithWatchDog 1

--       result <- runWithRedisConn_ rr4 $ do
--         void $ psubscribe [ChannelPattern testPatt] $ \ch msg -> L.runIO $
--           putMVar targetMVar (ch, msg)

--         L.publish (Channel testCh0) $ Payload testMsg
--         result0 <- L.runIO $ watch <* reset

--         L.publish (Channel testCh1) $ Payload testMsg
--         result1 <- L.runIO $ watch <* reset

--         pure (result0, result1)

--       result `shouldBe`
--         ( Just (testCh0, testMsg)
--         , Just (testCh1, testMsg)
--         )

--     it "Callback does not receive messages from channel after unsubscribe (psubscribe method)" $ do
--       let testMsg  = "Hello, Tests"
--       let testCh   = "ptest"
--       let testPatt = "?test"
--       (targetMVar, watch, _) <- emptyMVarWithWatchDog 1

--       result <- runWithRedisConn_ rr5 $ do
--         unsubscribe <- psubscribe [ChannelPattern testPatt] $ \ch msg -> L.runIO $
--           putMVar targetMVar (ch, msg)

--         unsubscribe

--         publish (Channel testCh) $ Payload testMsg

--         L.runIO watch

--       result `shouldBe` Nothing

--     it "Callback receive messages from all subscribed channels" $ do
--       let testMsg0 = "Hello, Tests_0"
--       let testMsg1 = "Hello, Tests_1"
--       let testCh0  = "test_0"
--       let testCh1  = "test_1"
--       (targetMVar, watch, reset) <- emptyMVarWithWatchDog 1

--       result <- runWithRedisConn_ rr6 $ do
--         void $ L.subscribe [Channel testCh0, Channel testCh1] $ \msg -> L.runIO $
--           putMVar targetMVar msg

--         L.publish (Channel testCh0) $ Payload testMsg0
--         result0 <- L.runIO $ watch <* reset

--         L.publish (Channel testCh1) $ Payload testMsg1
--         result1 <- L.runIO $ watch <* reset

--         pure (result0, result1)

--       result `shouldBe` (Just testMsg0, Just testMsg1)

--     it "Unsubscribe unsubscribes from all subscribed channels" $ do
--       let testMsg0 = "Hello, Tests_0"
--       let testMsg1 = "Hello, Tests_1"
--       let testCh0  = "test_0"
--       let testCh1  = "test_1"
--       (targetMVar, watch, reset) <- emptyMVarWithWatchDog 1

--       result <- runWithRedisConn_ rr7 $ do
--         unsubscribe <- L.subscribe [Channel testCh0, Channel testCh1] $ \msg -> L.runIO $
--           putMVar targetMVar msg

--         unsubscribe

--         L.publish (Channel testCh0) $ Payload testMsg0
--         result0 <- L.runIO $ watch <* reset

--         L.publish (Channel testCh1) $ Payload testMsg1
--         result1 <- L.runIO $ watch <* reset

--         pure (result0, result1)

--       result `shouldBe` (Nothing, Nothing)


-- -- Callback receives messages from channel it subscribed to
-- rr1 :: ResultRecording
-- rr1 = fromJust $ decode $ "{\"recording\":[{\"_entryName\":\"SubscribeEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonChannels\":[{\"b64\":\"dGVzdA==\"}]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"PublishEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"jsonChannel\":{\"b64\":\"dGVzdA==\"},\"jsonPayload\":{\"b64\":\"SGVsbG8sIFRlc3Rz\"},\"jsonResult\":{\"Right\":1}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonResult\":{\"b64\":\"SGVsbG8sIFRlc3Rz\"},\"description\":\"\"},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{},\"safeRecordings\":{}}"

-- Pub/Sub works the same way if run in fork
-- rr2 :: ResultRecording
-- rr2 = fromJust $ decode $ "{\"recording\":[{\"_entryName\":\"GenerateGUIDEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"guid\":\"c9239b0c-083f-4711-94af-46972f31864d\"},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"LogMessageEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"tag\":\"\\\"ForkFlow\\\"\",\"msg\":\"Flow forked. Description: Fork GUID: c9239b0c-083f-4711-94af-46972f31864d\",\"level\":\"Info\"},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"ForkEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"guid\":\"c9239b0c-083f-4711-94af-46972f31864d\",\"description\":\"Fork\"},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":3,\"_entryPayload\":{\"jsonResult\":[],\"description\":\"\"},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"PublishEntry\",\"_entryIndex\":4,\"_entryPayload\":{\"jsonChannel\":{\"utf8\":\"test\",\"b64\":\"dGVzdA==\"},\"jsonPayload\":{\"utf8\":\"Hello, Tests\",\"b64\":\"SGVsbG8sIFRlc3Rz\"},\"jsonResult\":{\"Right\":1}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":5,\"_entryPayload\":{\"jsonResult\":{\"utf8\":\"Hello, Tests\",\"b64\":\"SGVsbG8sIFRlc3Rz\"},\"description\":\"\"},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{\"c9239b0c-083f-4711-94af-46972f31864d\":{\"recording\":[{\"_entryName\":\"GenerateGUIDEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"guid\":\"eed089ff-3a93-4768-8b56-c9cb6d7ce26e\"},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunSafeFlowEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"jsonResult\":{\"Right\":[]},\"guid\":\"eed089ff-3a93-4768-8b56-c9cb6d7ce26e\"},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{},\"safeRecordings\":{\"eed089ff-3a93-4768-8b56-c9cb6d7ce26e\":[{\"_entryName\":\"SubscribeEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonChannels\":[{\"utf8\":\"test\",\"b64\":\"dGVzdA==\"}]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"jsonResult\":[],\"description\":\"\"},\"_entryReplayMode\":\"Normal\"}]}}},\"safeRecordings\":{}}"

-- Callback does not receive messages from channel after unsubscribe (subscribe method)
-- rr3 :: ResultRecording
-- rr3 = fromJust $ decode $ "{\"recording\":[{\"_entryName\":\"SubscribeEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonChannels\":[{\"b64\":\"dGVzdA==\"}]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"jsonResult\":[],\"description\":\"subscribe\"},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"PublishEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonChannel\":{\"b64\":\"dGVzdA==\"},\"jsonPayload\":{\"b64\":\"SGVsbG8sIFRlc3Rz\"},\"jsonResult\":{\"Right\":1}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":3,\"_entryPayload\":{\"jsonResult\":null,\"description\":\"\"},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{},\"safeRecordings\":{}}"

-- -- Callback receives messages from channel it subscribed to, if pattern matches
-- rr4 :: ResultRecording
-- rr4 = fromJust $ decode $ "{\"recording\":[{\"_entryName\":\"PSubscribeEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonPatterns\":[{\"b64\":\"P3Rlc3Q=\"}]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"PublishEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"jsonChannel\":{\"b64\":\"MHRlc3Q=\"},\"jsonPayload\":{\"b64\":\"SGVsbG8sIFRlc3Rz\"},\"jsonResult\":{\"Right\":1}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonResult\":[{\"b64\":\"MHRlc3Q=\"},{\"b64\":\"SGVsbG8sIFRlc3Rz\"}],\"description\":\"\"},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"PublishEntry\",\"_entryIndex\":3,\"_entryPayload\":{\"jsonChannel\":{\"b64\":\"MXRlc3Q=\"},\"jsonPayload\":{\"b64\":\"SGVsbG8sIFRlc3Rz\"},\"jsonResult\":{\"Right\":1}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":4,\"_entryPayload\":{\"jsonResult\":[{\"b64\":\"MXRlc3Q=\"},{\"b64\":\"SGVsbG8sIFRlc3Rz\"}],\"description\":\"\"},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{},\"safeRecordings\":{}}"

-- -- Callback does not receive messages from channel after unsubscribe (psubscribe method)
-- rr5 :: ResultRecording
-- rr5 = fromJust $ decode $ "{\"recording\":[{\"_entryName\":\"PSubscribeEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonPatterns\":[{\"b64\":\"P3Rlc3Q=\"}]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"jsonResult\":[],\"description\":\"psubscribe\"},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"PublishEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonChannel\":{\"b64\":\"cHRlc3Q=\"},\"jsonPayload\":{\"b64\":\"SGVsbG8sIFRlc3Rz\"},\"jsonResult\":{\"Right\":1}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":3,\"_entryPayload\":{\"jsonResult\":null,\"description\":\"\"},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{},\"safeRecordings\":{}}"

-- -- Callback receive messages from all subscribed channels
-- rr6 :: ResultRecording
-- rr6 = fromJust $ decode $ "{\"recording\":[{\"_entryName\":\"SubscribeEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonChannels\":[{\"b64\":\"dGVzdF8w\"},{\"b64\":\"dGVzdF8x\"}]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"PublishEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"jsonChannel\":{\"b64\":\"dGVzdF8w\"},\"jsonPayload\":{\"b64\":\"SGVsbG8sIFRlc3RzXzA=\"},\"jsonResult\":{\"Right\":1}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonResult\":{\"b64\":\"SGVsbG8sIFRlc3RzXzA=\"},\"description\":\"\"},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"PublishEntry\",\"_entryIndex\":3,\"_entryPayload\":{\"jsonChannel\":{\"b64\":\"dGVzdF8x\"},\"jsonPayload\":{\"b64\":\"SGVsbG8sIFRlc3RzXzE=\"},\"jsonResult\":{\"Right\":1}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":4,\"_entryPayload\":{\"jsonResult\":{\"b64\":\"SGVsbG8sIFRlc3RzXzE=\"},\"description\":\"\"},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{},\"safeRecordings\":{}}"

-- -- Unsubscribe unsubscribes from all subscribed channels
-- rr7 :: ResultRecording
-- rr7 = fromJust $ decode $ "{\"recording\":[{\"_entryName\":\"SubscribeEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonChannels\":[{\"b64\":\"dGVzdF8w\"},{\"b64\":\"dGVzdF8x\"}]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"jsonResult\":[],\"description\":\"subscribe\"},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"PublishEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonChannel\":{\"b64\":\"dGVzdF8w\"},\"jsonPayload\":{\"b64\":\"SGVsbG8sIFRlc3RzXzA=\"},\"jsonResult\":{\"Right\":1}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":3,\"_entryPayload\":{\"jsonResult\":null,\"description\":\"\"},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"PublishEntry\",\"_entryIndex\":4,\"_entryPayload\":{\"jsonChannel\":{\"b64\":\"dGVzdF8x\"},\"jsonPayload\":{\"b64\":\"SGVsbG8sIFRlc3RzXzE=\"},\"jsonResult\":{\"Right\":0}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":5,\"_entryPayload\":{\"jsonResult\":null,\"description\":\"\"},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{},\"safeRecordings\":{}}"
