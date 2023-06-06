{-# LANGUAGE ScopedTypeVariables #-}

module CachedDBSpec where

import           DBSetup as DBS
import qualified Database.Beam as B
import           EulerHS.CachedSqlDBQuery
import           EulerHS.Interpreters as I
import           EulerHS.Language as L
import           EulerHS.Prelude
import           EulerHS.Types as T
import           Sequelize
import           Test.Hspec

redisCfg :: KVDBConfig
redisCfg = T.mkKVDBConfig "eulerKVDB" T.defaultKVDBConnConfig

spec :: Spec
spec = do
  around withEmptyDB $

    describe "Cached sequelize layer" $ do

      it "findOne returns Nothing for empty table" $ \rt -> do
        let testKey = "key1"
        res <- runFlow rt $ do
          _ <- L.initKVDBConnection redisCfg
          findOne sqliteCfg (Just testKey) []
        (res :: Either DBError (Maybe User)) `shouldBe` Right Nothing

      it "findOne returns first row from table" $ \rt -> do
        let testKey = "key2"
        res <- runFlow rt $ do
          _ <- L.initKVDBConnection redisCfg
          conn <- connectOrFail sqliteCfg
          _ <- L.runDB conn $ L.insertRows $
            B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates"]
          findOne sqliteCfg (Just testKey) []
        res `shouldBe` Right (Just (User 1 "Bill" "Gates"))

      it "findOne successfully reads `Nothing` from cache" $ \rt -> do
        let testKey = "key3"
        -- Test with `Nothing`
        res <- runFlow rt $ do
          _ <- L.initKVDBConnection redisCfg
          -- This read should write `Nothing` to the cache
          _ <- findOne sqliteCfg (Just testKey) []
                :: Flow (Either DBError (Maybe User))
          -- Read `Nothing` from the cache
          findOne sqliteCfg (Just testKey) []
        (res :: Either DBError (Maybe User)) `shouldBe` Right Nothing
        -- Also test with a value (Just ...)

      it "findOne reads (Just result) from cache" $ \rt -> do
        let testKey = "key4"
        res <- runFlow rt $ do
          _ <- L.initKVDBConnection redisCfg
          conn <- connectOrFail sqliteCfg
          _ <- L.runDB conn $ L.insertRows $
            B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates"]
          _ <- findOne sqliteCfg (Just testKey) []
                :: Flow (Either DBError (Maybe User))
          -- Delete value to ensure the cache is used
          _ <- L.runDB conn $ L.deleteRows $
            B.delete (users userDB) (\u -> _userGUID u B.==. 1)
          findOne sqliteCfg (Just testKey) []
        res `shouldBe` Right (Just (User 1 "Bill" "Gates"))

      it "findAll finds all values in the database" $ \rt -> do
        let testKey = "key5"
        res <- runFlow rt $ do
          _ <- L.initKVDBConnection redisCfg
          conn <- connectOrFail sqliteCfg
          _ <- L.runDB conn $ L.insertRows $
            B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates"]
          _ <- L.runDB conn $ L.insertRows $
            B.insert (users userDB) $ B.insertValues [User 2 "Steve" "Jobs"]
          _ <- findAll sqliteCfg (Just testKey) []
                :: Flow (Either DBError [User])
          findAll sqliteCfg (Just testKey) []
        res `shouldSatisfy` \case
          Right xs -> User 1 "Bill" "Gates" `elem` xs
                      && User 2 "Steve" "Jobs" `elem` xs
          Left _ -> False

      it "findAll successfully reads `[]` from cache" $ \rt -> do
        let testKey = "key6"
        res <- runFlow rt $ do
          _ <- L.initKVDBConnection redisCfg
          -- This read should write `Nothing` to the cache
          _ <- findAll sqliteCfg (Just testKey) []
                :: Flow (Either DBError [User])
          -- Read `Nothing` from the cache
          findAll sqliteCfg (Just testKey) []
        (res :: Either DBError [User]) `shouldBe` Right []
        -- Also test with a value (Just ...)

      it "findAll reads nonempty list from cache after writing to it" $ \rt -> do
        let testKey = "key7"
        res <- runFlow rt $ do
          _ <- L.initKVDBConnection redisCfg
          conn <- connectOrFail sqliteCfg
          _ <- L.runDB conn $ L.insertRows $
            B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates"]
          _ <- L.runDB conn $ L.insertRows $
            B.insert (users userDB) $ B.insertValues [User 2 "Steve" "Jobs"]
          _ <- findAll sqliteCfg (Just testKey) []
                :: Flow (Either DBError [User])
          -- Delete everything to ensure the cache is used
          _ <- L.runDB conn $ L.deleteRows $
            B.delete (users userDB) (\u -> _userGUID u B.<. 3)
          findAll sqliteCfg (Just testKey) []
        res `shouldSatisfy` \case
          Right xs -> User 1 "Bill" "Gates" `elem` xs
                      && User 2 "Steve" "Jobs" `elem` xs
          Left _ -> False

      it "create inserts into the DB" $ \rt -> do
        let user = User 10 "Alonzo" "Church"
        res <- runFlow rt $ do
          _ <- initKVDBConnection redisCfg
          _ <- create sqliteCfg user Nothing
          findOne sqliteCfg Nothing []
        res `shouldBe` Right (Just user)

      it "create writes to the cache and findOne can read it" $ \rt -> do
        let testKey = "key8"
        let user = User 10 "Alan" "Turing"
        res <- runFlow rt $ do
          _ <- initKVDBConnection redisCfg
          conn <- connectOrFail sqliteCfg
          _ <- create sqliteCfg user (Just testKey)
          -- Delete from DB to ensure the cache is used
          _ <- L.runDB conn $ L.deleteRows $
            B.delete (users userDB) (\u -> _userGUID u B.==. 10)
          findOne sqliteCfg (Just testKey) []
        res `shouldBe` Right (Just user)

      it "updateOne updates the DB" $ \rt -> do
        let user1 :: User = User 10 "Alan" "Turing"
        res <- runFlow rt $ do
          _ <- initKVDBConnection redisCfg
          _ <- create sqliteCfg user1 Nothing
          _ <- updateOne sqliteCfg Nothing [Sequelize.Set DBS._firstName "Kurt"] [Is _userGUID (Eq 10)]
          findOne sqliteCfg Nothing []
        res `shouldBe` Right (Just user1 {DBS._firstName = "Kurt"})

      it "updateOne updates the cache" $ \rt -> do
        let user1 :: User = User 10 "Alan" "Turing"
        let testKey = "key9"
        res <- runFlow rt $ do
          _ <- initKVDBConnection redisCfg
          conn <- connectOrFail sqliteCfg
          _ <- create sqliteCfg user1 (Just testKey)
          _ <- updateOne sqliteCfg (Just testKey) [Sequelize.Set DBS._firstName "Kurt"] [Is _userGUID (Eq 10)]
          -- Delete from DB to ensure the cache is used
          _ <- L.runDB conn $ L.deleteRows $
            B.delete (users userDB) (\u -> _userGUID u B.==. 10)
          findOne sqliteCfg (Just testKey) []
        res `shouldBe` Right (Just user1 {DBS._firstName = "Kurt"})
