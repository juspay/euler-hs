{-# LANGUAGE RecordWildCards #-}

module SnowflakesSpec where

import EulerHS.Extra.Snowflakes.Types
import EulerHS.KVConnector.Utils (foldEither)
import           EulerHS.Prelude
import qualified EulerHS.Interpreters as I
import qualified EulerHS.Language as L
import qualified EulerHS.Runtime as R
import Data.Set as Set (fromList, intersection, size)
import Test.Hspec 

type SnowflakeFlowSpec = SpecWith R.FlowRuntime

asserting :: Expectation -> L.Flow ()
asserting = L.runIO

snowflakeFlowSpec :: SnowflakeFlowSpec -> Spec
snowflakeFlowSpec = do
    aroundAll $ \tests -> do
        R.withFlowRuntime Nothing $ \rt -> do
            tests rt

setSnowflakePrerequisites :: L.Flow ()
setSnowflakePrerequisites = 
    void . sequence_ $  [L.setOption StackID 1, L.setOption PodID 1]

purgeSnowflakePrerequisites :: L.Flow ()
purgeSnowflakePrerequisites =
    void . sequence_ $  [ L.delOption StackID, L.delOption PodID] 

itSnowflakeFlow :: [Char] -> L.Flow () -> SnowflakeFlowSpec
itSnowflakeFlow description flow =
    it description (`I.runFlow` flow)

spec :: HasCallStack => Spec
spec = snowflakeFlowSpec $ do
    itSnowflakeFlow "Snowflake concurrency test: Snowflake generation is thread-safe"
        $ do
            setSnowflakePrerequisites
            a1 <- L.forkFlow' "Snowflake Generation thread 1"$ (replicateM 2000 ( L.generateSnowflake "Sample"))
            a2 <- L.forkFlow' "Snowflake Generation thread 2" $ (replicateM 2000 ( L.generateSnowflake "Sample"))
            flakes1 <- L.await Nothing a1
            flakes2 <- L.await Nothing a2
                -- res :: Either String Bool
            res <- case (flakes1, flakes2) of
                (Right ef1, Right ef2) -> case (foldEither ef1, foldEither ef2) of
                    (Right f1, Right f2) -> return $ Right $ checkCollision f1 f2
                    _ -> return $ Left ("Error generating snowflakes" :: String)
                _ -> return $ Left ("Error generating snowflakes" :: String)
            asserting $ res `shouldBe` (Right True)

    where
        checkCollision :: [Snowflake] -> [Snowflake] -> Bool
        checkCollision a b = Set.size (Set.intersection (Set.fromList a) (Set.fromList b)) == 0