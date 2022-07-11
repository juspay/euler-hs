{-# LANGUAGE DeriveAnyClass #-}

module EulerHS.Core.Types.Exceptions
  ( -- * Exceptions
    HttpManagerNotFound(..)
  , AwaitingError (..)
  ) where

import           EulerHS.Prelude


data HttpManagerNotFound = HttpManagerNotFound String
 deriving (Show, Eq, Exception)


data AwaitingError = AwaitingTimeout | ForkedFlowError Text
  deriving (Show, Eq, Ord, Generic)
