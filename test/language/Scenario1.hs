{-# OPTIONS_GHC -fno-warn-deprecations -Werror #-}

module Scenario1
  (
    mkUrl, testScenario1
  ) where

import           Client (User (User), getUser, port, userGUID)
import           Data.Text (pack)
import           EulerHS.Language
import           EulerHS.Prelude hiding (pack)
import           EulerHS.TestData.Types
import           Servant.Client (BaseUrl (..), Scheme (..))

mkUrl :: String -> BaseUrl
mkUrl host = BaseUrl Http host port ""

testScenario1 :: Flow User
testScenario1 = do
  localUserName <- pack <$> runSysCmd "whoami"
  localGUID <- runIO (undefined :: IO Text)
  guid <- generateGUID
  url <- maybe (mkUrl "localhost") mkUrl <$> getOption UrlKey
  res <- callServantAPI Nothing url getUser
  case res of
    Right u ->  if localGUID /= userGUID u then pure u
                     else pure $ User localUserName "" guid
    _ -> pure $ User localUserName "Smith" guid
