{- |
Module      :  EulerHS.Core.Types.Options
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
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

class (Typeable k, ToJSON k)
  => OptionEntity k v |  k -> v

mkOptionKey :: forall k v. OptionEntity k v => k -> Text
mkOptionKey k = show (typeRep @k) <> (decodeUtf8 $ BSL.toStrict $ encode k)
