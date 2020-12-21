{- |
Module      :  EulerHS.Core.Types.Common
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2021
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable

Common types and helper functions.

This module is internal and should not imported in the projects.
Import 'EulerHS.Types' instead.
-}

module EulerHS.Core.Types.Common
  (
    FlowGUID
  , ForkGUID
  , SafeFlowGUID
  , ManagerSelector
  , Description
  , Awaitable (..)
  , Microseconds (..)
  ) where

import qualified Data.Word as W
import           EulerHS.Prelude

  -- | Guid for any flow.
  -- This type can be used to specify a separate logger formatting
  -- for each flow.
type FlowGUID = Text

-- | Guid for a forked flow.
-- Service type, rarely needed in the business logic.
type ForkGUID = Text

  -- | Guid for a safe flow.
  -- Service type, rarely needed in business logic.
type SafeFlowGUID = Text

  -- | Network manager selector.
  -- Allows to have a set of named managers with own configs
  -- when the default one is not enough.
type ManagerSelector = String

  -- | Description type
type Description = Text

  -- | Awaitable object. Ask it for results from forked flow.
data Awaitable s = Awaitable (MVar s)

  -- | Wrapper for microseconds.
newtype Microseconds
  = Microseconds W.Word32
  -- ^ Max timeout ~71 minutes with Word32
