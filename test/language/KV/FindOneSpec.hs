module KV.FindOneSpec where

import           EulerHS.Prelude hiding (id)
import           KV.FlowHelper
import           KV.TestHelper
import qualified EulerHS.KVConnector.Flow as DB
import qualified EulerHS.Language as L
import qualified Data.Text as Text
-- import           Database.Beam.MySQL (MySQLM)
-- import qualified EulerHS.Types as T
import           Test.Hspec
import           KV.TestSchema.ServiceConfiguration
import           KV.TestSchema.Mesh
import           Sequelize (Clause(..), Term(..))

{-
mandate use of either primary key or sec key , since not eq won't work
-}

spec :: HasCallStack => Spec
spec = flowSpec $ do
    itFlow "Should be able to fetch created entry using secondary key and filter in application with Eq" $ do
      randomName <- Text.take 5 <$> L.generateGUID
      let value1 = "value1" <> randomName
      let value2 = "value2" <> randomName
      let serviceConfig1 = mkServiceConfig "name1" value1
      let serviceConfig2 = mkServiceConfig "name1" value2
      withTableEntry serviceConfig1 $ \serviceConfig dbConf -> do
        _eitherSc2 <- fromRightErr <$> DB.createWithKVConnector dbConf meshConfig serviceConfig2
        maybeRes  <- fromRightErr <$> DB.findWithKVConnector dbConf meshConfig [Is name (Eq serviceConfig.name),Is value (Eq $ serviceConfig.value)]
        asserting $ maybeRes `shouldBe` (Just serviceConfig)
    itFlow "Should be able to fetch created entry using secondary key and filter in application with not Eq" $ do
      randomName <- Text.take 5 <$> L.generateGUID
      let value1 = "value1" <> randomName
      let value2 = "value2" <> randomName
      let serviceConfig1 = mkServiceConfig "name1" value1
      let serviceConfig2 = mkServiceConfig "name1" value2
      withTableEntry serviceConfig1 $ \serviceConfig dbConf -> do
        sc2 <- fromRightErr <$> DB.createWithKVConnector dbConf meshConfig serviceConfig2
        maybeRes  <- fromRightErr <$> DB.findWithKVConnector dbConf meshConfig [Is name (Eq serviceConfig.name),Is value (Not $ Eq $ serviceConfig.value)]
        asserting $ maybeRes `shouldBe` (Just sc2)
    itFlow "Should be able to fetch created entry using secondary key and filter in application with geq_" $ do
      randomName <- Text.take 5 <$> L.generateGUID
      let value1 = "value1" <> randomName
      let value2 = "value2" <> randomName
      let serviceConfig1 = mkServiceConfig "name1" value1
      let serviceConfig2 = mkServiceConfig "name1" value2
      withTableEntry serviceConfig1 $ \_ dbConf -> do
        sc2 <- fromRightErr <$> DB.createWithKVConnector dbConf meshConfig serviceConfig2
        maybeRes  <- fromRightErr <$> DB.findWithKVConnector dbConf meshConfig [Is name (Eq sc2.name),Is id (GreaterThanOrEq sc2.id)]
        asserting $ maybeRes `shouldBe` (Just sc2)
    itFlow "Should be able to fetch created entry using secondary key and filter in application with gt_" $ do
      randomName <- Text.take 5 <$> L.generateGUID
      let value1 = "value1" <> randomName
      let value2 = "value2" <> randomName
      let serviceConfig1 = mkServiceConfig "name1" value1
      let serviceConfig2 = mkServiceConfig "name1" value2
      withTableEntry serviceConfig1 $ \serviceConfig dbConf -> do
        sc2 <- fromRightErr <$> DB.createWithKVConnector dbConf meshConfig serviceConfig2
        maybeRes  <- fromRightErr <$> DB.findWithKVConnector dbConf meshConfig [Is name (Eq serviceConfig.name),Is id (GreaterThan serviceConfig.id)]
        asserting $ maybeRes `shouldBe` (Just sc2)
    itFlow "Should be able to fetch created entry using secondary key and filter in application with leq_" $ do
      randomName <- Text.take 5 <$> L.generateGUID
      let value1 = "value1" <> randomName
      let value2 = "value2" <> randomName
      let serviceConfig1 = mkServiceConfig "name1" value1
      let serviceConfig2 = mkServiceConfig "name1" value2
      withTableEntry serviceConfig1 $ \serviceConfig dbConf -> do
        _ <- fromRightErr <$> DB.createWithKVConnector dbConf meshConfig serviceConfig2
        maybeRes  <- fromRightErr <$> DB.findWithKVConnector dbConf meshConfig [Is name (Eq serviceConfig.name),Is id (LessThanOrEq serviceConfig.id)]
        asserting $ maybeRes `shouldBe` (Just serviceConfig)
    itFlow "Should be able to fetch created entry using secondary key and filter in application with lt_" $ do
      randomName <- Text.take 5 <$> L.generateGUID
      let value1 = "value1" <> randomName
      let value2 = "value2" <> randomName
      let serviceConfig1 = mkServiceConfig "name1" value1
      let serviceConfig2 = mkServiceConfig "name1" value2
      withTableEntry serviceConfig1 $ \serviceConfig dbConf -> do
        sc2 <- fromRightErr <$> DB.createWithKVConnector dbConf meshConfig serviceConfig2
        maybeRes  <- fromRightErr <$> DB.findWithKVConnector dbConf meshConfig [Is name (Eq sc2.name),Is id (LessThan sc2.id)]
        asserting $ maybeRes `shouldBe` (Just serviceConfig)