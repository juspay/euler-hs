{-# LANGUAGE DeriveAnyClass #-}

{- |
Module      :  EulerHS.Core.Types.Exceptions
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2021
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable

This module contains some exceptions and error types used in the framework.

This module is internal and should not imported in the projects.
Import 'EulerHS.Types' instead.
-}

module EulerHS.Core.Types.Exceptions
  ( -- * Exceptions
    HttpManagerNotFound(..)
  , AwaitingError (..)
  ) where

import           EulerHS.Prelude

-- | Exception type for indicating that a named Http manager is not set.
data HttpManagerNotFound = HttpManagerNotFound String
 deriving (Show, Eq, Exception)


-- | This error may be returned on some problem with an awaitable value.
data AwaitingError
  = AwaitingTimeout
    -- ^ Awaiting period has expired.
  | ForkedFlowError Text
    -- ^ A forked flow has finished with this error.
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
