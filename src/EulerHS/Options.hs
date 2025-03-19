{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, DeriveAnyClass    #-}

module EulerHS.Options
  (
    -- * Options
    -- | Determine the relationship between key & value
    OptionEntity
    -- | Type of the option key
  , OptionKey
    -- * Make option key
  , mkOptionKey
  ) where

import           EulerHS.Prelude
import Streamly.Data.Array (Array)
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Data.MutByteArray as MBA
import Data.Typeable (typeOf, TypeRep)
import EulerHS.ART.V2.Types (ArtRecordable)

class (Typeable k, ToJSON k, ArtRecordable v, MBA.Serialize k)
  => OptionEntity k v |  k -> v

type W8Arr = Array Word8
type OptionKey = (TypeRep, W8Arr)

mkOptionKey :: forall k v. OptionEntity k v => k -> OptionKey
mkOptionKey k = (typeOf k, Array.serialize k)
