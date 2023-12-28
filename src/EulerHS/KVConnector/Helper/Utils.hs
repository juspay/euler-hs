module EulerHS.KVConnector.Helper.Utils where

import           Data.Time.Clock(getCurrentTime, NominalDiffTime, UTCTime, diffUTCTime)
import EulerHS.Prelude

latency :: UTCTime -> IO NominalDiffTime
latency time = do
  currentTime <- getCurrentTime
  return $ diffUTCTime currentTime time