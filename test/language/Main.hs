{-# OPTIONS_GHC -Werror #-}

module Main (main) where


import           EulerHS.Prelude hiding (bracket)
import qualified EulerHS.Types as T
import qualified MaskingSpec as MaskSpec
import qualified FlowSpec as Flow
import qualified HttpAPISpec as HttpAPISpec
import           Test.Hspec.Core.Runner

main :: IO ()
main = do

    hspecWith defaultConfig{ configPrintCpuTime = True, configColorMode = ColorAlways} $ do
      HttpAPISpec.spec
      MaskSpec.spec
      Flow.spec logsDisabled

  where
    logsDisabled :: Maybe T.LoggerConfig
    logsDisabled = Nothing


