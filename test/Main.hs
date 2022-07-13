{-# OPTIONS_GHC -Werror #-}

module Main (main) where

import           EulerHS.Prelude hiding (bracket)
import qualified EulerHS.Types as T
import qualified EulerHS.Tests.Framework.FlowSpec as Flow
import qualified EulerHS.Tests.Framework.MaskingSpec as MaskSpec
import           Test.Hspec (hspec)

main :: IO ()
main = do
  -- Redis not works on CI
  -- withRedis $
  hspec $ do
    Flow.spec logsDisabled
    MaskSpec.spec

    -- Wait for Redis on CI
    -- CachedSqlDBQuery.spec

    -- ART removed and these tests not work anymore
    -- Art.spec
    -- KVDB.spec
    -- SQL.spec
    -- PubSub.spec




-- Helpers

logsDisabled :: Maybe T.LoggerConfig
logsDisabled = Nothing
