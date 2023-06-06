{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module KV.InsertSpec where

import           EulerHS.Prelude hiding(id)

import           KV.FlowHelper
import           KV.TestSchema.ServiceConfiguration
import qualified EulerHS.KVConnector.Flow as DB
import           Test.Hspec
import           Sequelize (Clause(..), Term(..))
import           KV.TestSchema.Mesh
import           EulerHS.KVConnector.Utils (getPKeyWithShard, getSecondaryLookupKeys)
import           EulerHS.KVConnector.Types hiding (kvRedis)
import           KV.TestHelper

{-
Things to test against insert
1. The value should be present in the KV againt the right primary id KEY
2. Testing auto increment
3. All the secondary keys should be present in KV and should be pointing to the primary key
4. The insert command should be present in the redis stream

Things to test againt find
1. Right value should be fetched with primary key
2. Right value should be fetched with secondary keys (including composite key)
3. Right value should be fetched with secondary keys for a query which requires extra filtering of data in the application
4.
-}

spec :: HasCallStack => Spec
spec = flowSpec $ do
    itFlow "Should add/increment value for auto increment id in KV" $ do
        sc <- dummyServiceConfig
        prevAutoId <- fromJustErr <$> peekAutoIncrId (tableName @ServiceConfiguration)
        withTableEntry sc $ (\_serviceConfig _dbConf -> do
          newAutoId <- fromJustErr <$> peekAutoIncrId (tableName @ServiceConfiguration)
          asserting $ (prevAutoId + 1) `shouldBe` newAutoId
          )
    itFlow "Should fetch a created entry using secondary key" $ do
        sc <- dummyServiceConfig
        withTableEntry sc $ (\serviceConfig dbConf -> do
            eitherSC <- DB.findWithKVConnector dbConf meshConfig [Is name (Eq $ serviceConfig.name)]
            when (isLeft eitherSC) $ error $ show eitherSC
            asserting $ (join $ hush eitherSC) `shouldBe` (Just serviceConfig)
          )
    itFlow "Should add primary key and secondary keys to redis on insert command" $ do
        sc <- dummyServiceConfig
        withTableEntry sc $ (\serviceConfig _dbConf -> do
            let pKey = getPKeyWithShard serviceConfig
            let secKeys = getSecondaryLookupKeys serviceConfig
            (valueFromPrimaryKey :: Maybe ServiceConfiguration) <- getValueFromPrimaryKey pKey
            valueFromSecondaryKeys <- (snd . partialHead) <$> getValueFromSecondaryKeys secKeys
            asserting $ valueFromPrimaryKey `shouldBe` valueFromSecondaryKeys
          )
    itFlow "Should fetch a created entry using primary key" $ do
        sc <- dummyServiceConfig
        withTableEntry sc $ (\serviceConfig dbConf -> do
            eitherSC <- DB.findWithKVConnector dbConf meshConfig [Is id (Eq $ serviceConfig.id)]
            when (isLeft eitherSC) $ error $ show eitherSC
            asserting $ (join $ hush eitherSC) `shouldBe` (Just serviceConfig)
          )
    xitFlow "[Feature to be impl] Should reject creation of duplicate entry based on the unique key" $ do
        sc <- dummyServiceConfig
        -- Assuming name column of service config table has a unique key constraint
        withTableEntry sc $ (\serviceConfig dbConf -> do
            eitherEntry <- DB.createWithKVConnector dbConf meshConfig serviceConfig
            asserting $ (isLeft eitherEntry) `shouldBe` True
          )
