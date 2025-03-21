{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE RecordWildCards  #-}

module EulerHS.Extra.Monitoring.Types where

import           EulerHS.Prelude
import           EulerHS.Options
import qualified Streamly.Data.MutByteArray as MBA

data DBMetricInfo = DBMetricInfo {
    latencyInfo :: LatencyInfo 
}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)


data RedisMetricInfo = RedisMetricInfo {
    latencyInfo :: LatencyInfo 
}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ExternalMetricInfo = ExternalMetricInfo {
    latencyInfo :: LatencyInfo 
}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data KMSMetricInfo = KMSMetricInfo {
    latencyInfo :: LatencyInfo 
}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data InternalAPIMetricInfo = InternalAPIMetricInfo {
    outgoingLatency :: Double 
,   requestCount :: Int
,   latency :: Double
}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)  

data DBMetricInfoKey = DBMetricInfoKey
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (FromJSON, ToJSON)


data RedisMetricInfoKey = RedisMetricInfoKey
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (FromJSON, ToJSON)

data InternalAPIMetricInfoKey = InternalAPIMetricInfoKey
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (FromJSON, ToJSON)

data ExternalMetricInfoKey = ExternalMetricInfoKey
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (FromJSON, ToJSON)

data KMSMetricInfoKey = KMSMetricInfoKey
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (FromJSON, ToJSON)

data LatencyHandle = DB | REDIS
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  
data LatencyMetricCfg = LatencyMetricCfg
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

data LatencyInfo = LatencyInfo {
    latency    :: Double
,   requestCount   :: Int
}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)


data IOLatencyMetric = IOLatencyMetric {
    dbMetric :: Maybe LatencyInfo
,   redisMetric :: Maybe LatencyInfo
,   internalApiMetric :: Maybe InternalAPIMetricInfo
,   externalApiMetric :: Maybe LatencyInfo
,   kmsMetric :: Maybe LatencyInfo
,   eulerOverhead :: Maybe Double
}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- TH Instances
--------------------------------------------------------------------------------

$(MBA.deriveSerialize [d|instance MBA.Serialize DBMetricInfoKey|])
$(MBA.deriveSerialize [d|instance MBA.Serialize RedisMetricInfoKey|])
$(MBA.deriveSerialize [d|instance MBA.Serialize InternalAPIMetricInfoKey|])
$(MBA.deriveSerialize [d|instance MBA.Serialize ExternalMetricInfoKey|])
$(MBA.deriveSerialize [d|instance MBA.Serialize KMSMetricInfoKey|])

instance OptionEntity DBMetricInfoKey DBMetricInfo
instance OptionEntity RedisMetricInfoKey RedisMetricInfo
instance OptionEntity InternalAPIMetricInfoKey InternalAPIMetricInfo
instance OptionEntity ExternalMetricInfoKey ExternalMetricInfo
instance OptionEntity KMSMetricInfoKey KMSMetricInfo
