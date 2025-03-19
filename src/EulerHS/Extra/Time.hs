module EulerHS.Extra.Time
  ( readToLocalTime
  , convertLocalToUTC
  , junkUTC
  , getCurrentTimeUTC
  , getCurrentDateInMillis
  , getCurrentDateInSeconds
  , getNewLocalTimeAfterAddingDays
  ) where

import           Data.Time (Day( ModifiedJulianDay ), LocalTime, UTCTime (UTCTime), localTimeToUTC, utc,
                            utcToLocalTime, zonedTimeToLocalTime, utcToZonedTime ,addUTCTime)
import           Data.Time.Clock (NominalDiffTime)

import           EulerHS.Prelude
import           EulerHS.Framework.Language (MonadFlow, getCurrentTime, getPOSIXTime)


readToLocalTime :: Maybe UTCTime -> Maybe LocalTime
readToLocalTime = fmap (utcToLocalTime utc)

convertLocalToUTC :: LocalTime -> UTCTime
convertLocalToUTC = localTimeToUTC utc

junkUTC :: UTCTime
junkUTC = UTCTime (ModifiedJulianDay 0) 0

getCurrentTimeUTC :: (MonadFlow m) => m LocalTime
getCurrentTimeUTC = go
  where
    go :: (MonadFlow m) => m LocalTime
    go = zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime

getCurrentDateInMillis :: (MonadFlow m) => m Int
getCurrentDateInMillis = do
   t <- (* 1000) <$> getPOSIXTime
   pure . floor $ t

getCurrentDateInSeconds :: (MonadFlow m) => m Int
getCurrentDateInSeconds = floor <$> getPOSIXTime

getNewLocalTimeAfterAddingDays :: (MonadFlow m) => Int -> m LocalTime
getNewLocalTimeAfterAddingDays days = do
  currTime <- getCurrentTime
  let timeDiff = fromIntegral (days * 24 * 60 * 60) :: NominalDiffTime
      updatedTime = addUTCTime timeDiff currTime
  pure (zonedTimeToLocalTime (utcToZonedTime utc updatedTime))
