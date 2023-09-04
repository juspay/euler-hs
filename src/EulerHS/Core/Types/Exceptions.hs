{- |
Module      :  EulerHS.Core.Types.Exceptions
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable

This module contains some exceptions and error types used in the framework.

This module is internal and should not imported in the projects.
Import 'EulerHS.Types' instead.
-}

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
