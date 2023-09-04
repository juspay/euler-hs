{- |
Module      :  EulerHS.Options
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable

This module contains the definition of the `OptionEntity` typeclass.
OptionEntity is a typeclass that is used to define the relationship between
the key and value of an option.
-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module EulerHS.Options
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
mkOptionKey k = show (typeRep @k) <> decodeUtf8 (BSL.toStrict $ encode k)
