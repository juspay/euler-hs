{-# OPTIONS_GHC -Werror #-}

module Main (main) where


import           EulerHS.Prelude
import qualified Envs as Envs
import qualified Options as Options
import qualified XMLMasking as XMLMasking
import qualified SnowflakesSpec as Snowflakes
import qualified Flow as Flow
import           Test.Hspec (hspec)

main :: IO ()
main = do
  hspec $ do
    Envs.spec
    Options.spec
    Snowflakes.spec
    XMLMasking.spec
    Flow.spec

