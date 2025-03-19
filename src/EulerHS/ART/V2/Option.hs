{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingStrategies  #-}

module EulerHS.ART.V2.Option where

import           EulerHS.Prelude
import qualified EulerHS.Types as T
import qualified Streamly.Data.MutByteArray as MBA


data ArtRecordingEnabled = ArtRecordingEnabled
  deriving (Show, Eq, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

$(MBA.deriveSerialize [d|instance MBA.Serialize ArtRecordingEnabled|])
instance T.OptionEntity ArtRecordingEnabled Bool

data ArtReplayDBPrefix = ArtReplayDBPrefix
  deriving (Show, Eq, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

$(MBA.deriveSerialize [d|instance MBA.Serialize ArtReplayDBPrefix|])
instance T.OptionEntity ArtReplayDBPrefix Text

data ArtReplayRedisPrefix = ArtReplayRedisPrefix
  deriving (Show, Eq, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

$(MBA.deriveSerialize [d|instance MBA.Serialize ArtReplayRedisPrefix|])
instance T.OptionEntity ArtReplayRedisPrefix Text