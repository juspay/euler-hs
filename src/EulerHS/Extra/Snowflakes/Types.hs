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

{-
Juspay Snowflakes
This 64 Bit Snowflake can be broken down as
29 Bits - Epoch time calculated from 1st Jan 2023, 12:00 AM (GMT + 5:30) in seconds.
7 Bits  - Stack ID
15 Bits - Pod ID
12 Bits - Incremental Payload

Notes -- 
This utility guarantees generation of 4096 unique snowflakes per second per stack per pod per key, which
should be enough for most usecases since this is a bigger number than what most of our systems handle.

Usage: 
generator <- getSnowflakeGenerator
snowflake <- generateSnowflakeID stackID podID "Sample Table" generator

Possible changes? 
Implementation of errors when more than 4096 snowflakes are attempted (or buffer the function call using thread delay.)
-}


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
