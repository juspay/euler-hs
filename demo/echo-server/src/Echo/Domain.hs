{-# LANGUAGE DeriveAnyClass #-}

module Echo.Domain where

import EulerHS.Prelude


data EchoMessage = EchoMessage
  { phrase :: Text
  , number :: Int
  , easterEgg :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
