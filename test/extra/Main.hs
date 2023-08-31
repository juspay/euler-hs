{-# OPTIONS_GHC -Werror #-}

module Main (main) where


import           EulerHS.Prelude
import qualified Options as Options
import qualified SnowflakesSpec as Snowflakes
import           Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  Options.spec
  Snowflakes.spec
