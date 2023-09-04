{- |
Module      :  EulerHS.Extra.Time
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module EulerHS.Extra.Time
  ( readToLocalTime
  , convertLocalToUTC
  , junkUTC
  , getCurrentTimeUTC
  , getCurrentDateInMillis
  , getCurrentDateInSeconds
  ) where

import           Data.Time (Day (ModifiedJulianDay), LocalTime,
                            UTCTime (UTCTime), localTimeToUTC, utc,
                            utcToLocalTime, zonedTimeToLocalTime, getCurrentTime, utcToZonedTime)
import           Data.Time.Clock.POSIX (getPOSIXTime)

import           EulerHS.Prelude
import           EulerHS.Language (MonadFlow, runIO)


readToLocalTime :: Maybe UTCTime -> Maybe LocalTime
readToLocalTime = fmap (utcToLocalTime utc)

convertLocalToUTC :: LocalTime -> UTCTime
convertLocalToUTC = localTimeToUTC utc

junkUTC :: UTCTime
junkUTC = UTCTime (ModifiedJulianDay 0) 0

getCurrentTimeUTC :: (MonadFlow m) => m LocalTime
getCurrentTimeUTC = runIO go
  where
    go :: IO LocalTime
    go = zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime

getCurrentDateInMillis :: (MonadFlow m) => m Int
getCurrentDateInMillis = runIO $ do
   t <- (* 1000) <$> getPOSIXTime
   pure . floor $ t

getCurrentDateInSeconds :: (MonadFlow m) => m Int
getCurrentDateInSeconds = runIO $ floor <$> getPOSIXTime
