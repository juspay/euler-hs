{- |
Module      :  EulerHS.Extra.Aeson
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable

This module contains utility functions for munging JSON data with Aeson.
-}

{-# LANGUAGE TypeApplications #-}

-- | Utility functions for munging JSON data with Aeson.
module EulerHS.Extra.Aeson
  ( -- * Common utility functions
    obfuscate

    -- * Aeson options presets
  , aesonOmitNothingFields
  , stripAllLensPrefixOptions
  , stripLensPrefixOptions
  , unaryRecordOptions
  , untaggedOptions
  , aesonOptions
  , aesonOmitNothingOption
  ) where

import           Data.Aeson (Options (..), SumEncoding (..), Value (..),
                             defaultOptions)
import           Prelude


{-------------------------------------------------------------------------------
  Common utility functions
-------------------------------------------------------------------------------}

-- | Rip away all __simple__ values from a JSON Value.
obfuscate :: Value -> Value
obfuscate v = go v where
    go (Object o)  = Object $ go <$> o
    go (Array a)   = Array $ go <$> a
    go (String  _) = String "***"
    go (Number _)  = Number 0
    go (Bool _)    = Bool False
    go Null        = Null



{- | Use it to omit 'Nothing' fields.

Also previously known as @aesonOrderCreateOptions@, @aesonOmitNothingOption@ and
broken @aesonOptions@. The latter is broken because of using @omitNothingFields = False@,
which is default in Aeson.

If you want to show 'Nothing' fields then please just use stock 'defaultOptions'!

>>> encode $ Person "Omar" Nothing
"{\"name\":\"Omar\"}"

whereas the default behavior is:

>>> encode $ Person "Omar" Nothing
"{\"age\":null,\"name\":\"Omar\"}"

-}
aesonOmitNothingFields :: Options
aesonOmitNothingFields = defaultOptions
  { omitNothingFields = True 
  }

{- | Drops all leading characters while they are the same.

@
data Wooolf = Wooolf
  { cccName :: Text
  , cccColour :: Maybe Text
  }
@

>>> encode $ Wooolf "Boooss" (Just "grey")
"{\"Name\":\"Boooss\",\"Colour\":\"grey\"}"

-}
stripAllLensPrefixOptions :: Options
stripAllLensPrefixOptions = defaultOptions { fieldLabelModifier = dropPrefix}
  where
    dropPrefix :: String -> String
    dropPrefix field = if length field > 0
                         then dropWhile (== head field) field
                         else field

{- | Strips lens-style one-character prefixes (usually @_@) from field names.

@
data Dog = Dog
  { cName :: Text
  , cColour :: Maybe Text
  }
@

>>> encode $ Dog "Buddy" (Just "white")
"{\"Name\":\"Buddy\",\"Colour\":\"white\"}"

-}
stripLensPrefixOptions :: Options
stripLensPrefixOptions = defaultOptions { fieldLabelModifier = drop 1 }


unaryRecordOptions :: Options
unaryRecordOptions = defaultOptions
  { unwrapUnaryRecords = True
  }


untaggedOptions :: Options
untaggedOptions = defaultOptions { sumEncoding = UntaggedValue }

aesonOptions :: Options
aesonOptions = defaultOptions
  { omitNothingFields = False
  }

aesonOmitNothingOption :: Options
aesonOmitNothingOption = defaultOptions
  { omitNothingFields = True
  }

