{- |
Module      :  EulerHS.Core.Types.Options
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2021
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable

Options can be used as a stateful mutable KV storage.

One should be careful in mutating it from different threads.

This module is internal and should not imported in the projects.
Import 'EulerHS.Types' instead.
-}

module EulerHS.Core.Types.Options
  (
    -- * Options
    -- | Determine the relationship between key & value
    OptionEntity
    -- * Make option key
  , mkOptionKey
  ) where

import           Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import           EulerHS.Prelude
import           Type.Reflection (typeRep)

-- | This type class helps to tie a key to a value.
--
-- You can't have different values for the same key.
class (Typeable k, FromJSON k, ToJSON k, FromJSON v, ToJSON v)
  => OptionEntity k v | k -> v

-- | Converts a value-like key into a string.
mkOptionKey :: forall k v. OptionEntity k v => k -> Text
mkOptionKey k = show (typeRep @k) <> (decodeUtf8 $ BSL.toStrict $ encode k)
