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
  , keyMapToHashMap
  , hashMapToKeyMap
  ) where

import qualified Data.Word as W
import           EulerHS.Prelude
import qualified Data.Aeson.Key as AK
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson.KeyMap as KM

type FlowGUID = Text
type ForkGUID = Text
type SafeFlowGUID = Text

newtype ManagerSelector = ManagerSelector Text
  deriving (Eq, IsString) via Text
  deriving stock (Show)

type Description = Text
data Awaitable s = Awaitable (MVar s)
data Microseconds = Microseconds W.Word32 -- Max timeout ~71 minutes with Word32

keyMapToHashMap :: KM.KeyMap Text -> HashMap Text Text
keyMapToHashMap = HM.fromList . map (\(k, v) -> (AK.toText k, v)) . KM.toList

hashMapToKeyMap :: HashMap Text Text -> KM.KeyMap Text
hashMapToKeyMap = KM.fromList . map (\(k, v) -> (AK.fromText k, v)) . HM.toList