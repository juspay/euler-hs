{- |
Module      :  EulerHS.Extra.Snowflakes.Types
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

{-# LANGUAGE  ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass     #-}

module EulerHS.Extra.Snowflakes.Types where

import Data.Aeson as A
import Data.Word
import Prelude
import Data.Map.Strict as Map
import Control.Concurrent.MVar
import EulerHS.Prelude
import EulerHS.Options (OptionEntity)




data SnowflakeMetadata = SnowflakeMetadata {
    lastCalledAt       :: Word32,
    incrementalPayload :: Word16
}

type SnowflakeGenerator = MVar (Map Text SnowflakeMetadata)
type Snowflake = Word64

data SnowflakeError = NonFatal Text | Fatal Text
  deriving stock (Eq, Show, Generic)

data StackID = StackID
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
instance OptionEntity StackID Word8


data PodID = PodID
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
instance OptionEntity PodID Word16
