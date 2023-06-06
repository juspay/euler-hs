{-# LANGUAGE DerivingVia #-}

module EulerHS.Common
  (
    -- * Guid for any flow
    FlowGUID
    -- * Guid for a forked flow
  , ForkGUID
    -- * Guid for a safe flow
  , SafeFlowGUID
    -- * Network manager selector
  , ManagerSelector(..)
    -- * Description type
  , Description
    -- * A variable for await results from a forked flow
  , Awaitable (..)
  , Microseconds (..)
  ) where

import qualified Data.Word as W
import           EulerHS.Prelude

type FlowGUID = Text
type ForkGUID = Text
type SafeFlowGUID = Text

newtype ManagerSelector = ManagerSelector Text
  deriving (Eq, IsString) via Text
  deriving stock (Show)

type Description = Text
data Awaitable s = Awaitable (MVar s)
data Microseconds = Microseconds W.Word32 -- Max timeout ~71 minutes with Word32
