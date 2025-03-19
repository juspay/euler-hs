{-# OPTIONS_GHC -Wwarn=deprecations #-}

module HttpAPISpec (spec) where

--import           EulerHS.Interpreters (runFlow)
--import           EulerHS.Language
import           EulerHS.Prelude hiding (get)
--import           EulerHS.Runtime (createLoggerRuntime, withFlowRuntime)
import           Test.Hspec (Spec, describe, it, shouldBe)
import qualified EulerHS.Types as T
import qualified Data.Map as Map

spec :: Spec
spec = do
    describe "Building HTTP requests" $ do
      it "building a JSON request" $ do
        let req = T.httpGet "http://localhost:8080/" &
                  T.withJSONBody @String "foo"
        let req' = defRequest
              { T.getRequestHeaders = Map.singleton "content-type" "application/json"
              , T.getRequestBody = Just $ T.LBinaryString "\"foo\""
              }
        req `shouldBe` req'

      it "building a Form-based request" $ do
        let req = T.httpGet "http://localhost:8080/" &
                  T.withFormBody
                    [ ("foo", "bar")
                    , ("baz", "qux")
                    ]
        let req' = defRequest
              { T.getRequestBody = Just $ T.LBinaryString "foo=bar&baz=qux"
              -- Not sure whether it's always "application/x-www-form-urlencoded"
--              , T.getRequestHeaders = Map.singleton "content-type" "application/x-www-form-urlencoded"
              }
        req `shouldBe` req'

  where
    defRequest = T.HTTPRequest
                { getRequestMethod = T.Get
                , getRequestHeaders = Map.empty
                , getRequestBody = Nothing
                , getRequestURL = "http://localhost:8080/"
                , getRequestTimeout = Just T.defaultTimeout
                , getRequestRedirects = Just 10
                }