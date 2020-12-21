{-# OPTIONS -fno-warn-deprecations #-}
module EulerHS.TestData.Scenarios.Scenario1 where

import qualified EulerHS.Language as L
import           EulerHS.Prelude hiding (getOption)
import           Servant.Client (BaseUrl (..), Scheme (..))

import           EulerHS.TestData.API.Client
import           EulerHS.TestData.Types

testScenario1 :: L.Flow User
testScenario1 = do
  localUserName <- L.runSysCmd "whoami"
  localGUID     <- L.runIO (undefined :: IO String)
  guid          <- L.generateGUID
  url           <- maybe (mkUrl "127.0.0.1") mkUrl <$> L.getOption UrlKey
  res           <- L.callServantAPI Nothing url getUser
  pure $ case res of
    Right u | localGUID /= userGUID u -> u
    Right u | otherwise -> User localUserName "" $ toString guid
    _ -> User localUserName "Smith" $ toString guid
  where
    mkUrl :: String -> BaseUrl
    mkUrl host = BaseUrl Http host port ""
