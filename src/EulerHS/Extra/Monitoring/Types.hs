{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE RecordWildCards  #-}

module EulerHS.Extra.Monitoring.Types where

import           EulerHS.Prelude
import qualified Data.Aeson as A
import           EulerHS.Types (OptionEntity)


data DBMetricInfo = DBMetricInfo {
    _latencyInfo :: LatencyInfo 
}
  deriving stock (Show)

data RedisMetricInfo = RedisMetricInfo {
    _latencyInfo :: LatencyInfo 
}
  deriving stock (Show)

data APIMetricInfo = APIMetricInfo {
    _latencyInfo :: LatencyInfo 
}
  deriving stock (Show)

data LatencyInfo = LatencyInfo {
    _latency    :: Double
,   _requests   :: Int
}
  deriving stock (Show)

instance ToJSON LatencyInfo where
  toJSON (LatencyInfo {..}) = A.object [
      ("latency", A.toJSON _latency)
    , ("requests", A.toJSON _requests)
    ]

data DBMetricInfoKey = DBMetricInfoKey
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (FromJSON, ToJSON)


data RedisMetricInfoKey = RedisMetricInfoKey
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (FromJSON, ToJSON)

data APIMetricInfoKey = APIMetricInfoKey
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (FromJSON, ToJSON)

instance OptionEntity DBMetricInfoKey DBMetricInfo

instance OptionEntity RedisMetricInfoKey RedisMetricInfo

instance OptionEntity APIMetricInfoKey APIMetricInfo

data LatencyHandle = DB | REDIS | API
  deriving stock (Show)

data LatencyMetricHandler = LatencyMetricHandler
  { latencyCounter :: (LatencyHandle, Text, Text, LatencyInfo) -> IO ()
  }

data LatencyMetricCfg = LatencyMetricCfg
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity LatencyMetricCfg LatencyMetricHandler
