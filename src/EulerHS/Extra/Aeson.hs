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

{-------------------------------------------------------------------------------
  Aeson options presets
  tests in test/extra/Options.hs
-------------------------------------------------------------------------------}

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
  { omitNothingFields = True -- hey! It should be True. We relay on it.
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

{- | When a record wrapped in a constructor with one argument, like @newtype@ ones,
the record goes into @contents@ key and the constructor's name into @tag@ key.

If you want to encode\/decode JSON which doesn't contain @tag@ and @contents@, but just
the value itself use this preset. See docs for 'unwrapUnaryRecords'.

See tests for more examples: test\/extra\/Options.hs
-}
unaryRecordOptions :: Options
unaryRecordOptions = defaultOptions
  { unwrapUnaryRecords = True
  }

{- | This preset throws away a constructor of sum type while encoding and uses the
first compatible constructor when decoding. See docs for 'sumEncoding' and 'UntaggedValue'.

See tests for more examples: test\/extra\/Options.hs
-}
untaggedOptions :: Options
untaggedOptions = defaultOptions { sumEncoding = UntaggedValue }

