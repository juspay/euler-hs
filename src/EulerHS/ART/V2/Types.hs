{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE ImplicitParams     #-}
{-# LANGUAGE ConstraintKinds     #-}

module EulerHS.ART.V2.Types where

import Data.Aeson
import Prelude
import GHC.Generics (Generic)

class ArtRecordable a where
  isRecordable :: Bool
  isRecordable = False
  toArtRecordingValue :: a -> Either String Value
  toArtRecordingValue _ = if isRecordable @a then Left "SERIALIZATION_NOT_IMPLEMENTED" else Left "NON_RECORDABLE"
  fromArtRecordedValue :: Value -> Either String a
  fromArtRecordedValue _ = if isRecordable @a then Left "DESERIALIZATION_NOT_IMPLEMENTED" else Left "NON_RECORDABLE"

instance {-# OVERLAPPABLE #-} (ToJSON a, FromJSON a) => ArtRecordable a where
  isRecordable = True
  toArtRecordingValue val = Right (toJSON val)
  fromArtRecordedValue = eitherDecode . encode

data ShouldRecordJoin = RECORD_JOIN | NO_RECORD_JOINS
  deriving (Show, Eq)

data ArtRecOptions = ArtRecOptions {
    shouldRecordForART :: Bool,
    shouldReplayForART :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

defaultArtRecOptions :: ArtRecOptions
defaultArtRecOptions = ArtRecOptions {
    shouldRecordForART = True,
    shouldReplayForART = True
  }

type HasArtRecOptions = ?artRecOptions :: ArtRecOptions
