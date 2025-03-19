{-# LANGUAGE  ScopedTypeVariables #-}

module EulerHS.Extra.Snowflakes.Flow where

import Data.Bits
import Data.Either.Extra (mapRight)
import Data.Word
import Data.Text as Text
import Data.Time.Clock.POSIX as Time
import Control.Applicative
import Prelude
import Data.Bifunctor (first, second)
import Data.Map.Strict as Map
import Control.Concurrent.MVar
import EulerHS.Extra.Snowflakes.Types

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


getSnowflakeGenerator :: IO SnowflakeGenerator
getSnowflakeGenerator = newMVar Map.empty

generateSnowflake' :: Word8 -> Word16 -> String -> SnowflakeGenerator -> IO (Either SnowflakeError Snowflake)
generateSnowflake' stackID podID key generator = do
    let 
        firstJan2023Midnight = 1672511400 :: Word32           -- 1st Jan 2023, 12:00 AM (GMT + 5:30)
        keyText = Text.pack key
    currentPosixTime :: Word32 <- round <$> Time.getPOSIXTime -- Current Posix time (GMT + 5:30)
    eUpdatedPayload <- modifyMVar generator (\snowflakeMetadataMap -> do
        pure $ case snowflakeMetadataMap !? keyText of
            Just value -> updateSnowflakeMetadata currentPosixTime keyText value snowflakeMetadataMap
            Nothing    -> createMetadataAgainstKey keyText currentPosixTime snowflakeMetadataMap
        )
    return $ (flip mapRight) eUpdatedPayload (\updatedPayload ->
        let 
            timeElapsed        :: Word64 = fromIntegral $ currentPosixTime - firstJan2023Midnight
            timeElapsedSetAt64 :: Word64 = shiftL timeElapsed 34
            stackIDSetAt64     :: Word64 = shiftL (fromIntegral (stackID .&. 127) :: Word64) 27
            podIDSetAt64       :: Word64 = shiftL (fromIntegral (podID .&. 32767) :: Word64) 12
            updatedPayloadAt64 :: Word64 = fromIntegral updatedPayload
        in timeElapsedSetAt64 .|. stackIDSetAt64 .|. podIDSetAt64 .|. updatedPayloadAt64)

        where
            createMetadataAgainstKey :: Text -> Word32 -> Map Text SnowflakeMetadata -> (Map Text SnowflakeMetadata, Either SnowflakeError Word16)
            createMetadataAgainstKey key' currentPosixTime = second Right . first (insert key' (SnowflakeMetadata currentPosixTime 0)) . (, 0)

            updateSnowflakeMetadata :: Word32 -> Text -> SnowflakeMetadata -> Map Text SnowflakeMetadata -> (Map Text SnowflakeMetadata, Either SnowflakeError Word16)
            updateSnowflakeMetadata currentPosixTime key' currentMetadata snowflakeMetadataMap =
                let 
                    updatedIncrementalPayload = if (currentPosixTime - currentMetadata.lastCalledAt) > 0 then 0 else (currentMetadata.incrementalPayload + 1) .&. 4095
                    currentIncrementalPayload = incrementalPayload  currentMetadata
                    errorString :: Text
                    errorString = "generateSnowflake':: Incremental Payload reached limit for timeStamp: " <> (pack . show $ currentPosixTime) <> ", key: " <> key'
                in if currentIncrementalPayload == 4095
                      then second Left (snowflakeMetadataMap, NonFatal errorString)
                      else second Right (insert key' (SnowflakeMetadata currentPosixTime updatedIncrementalPayload) snowflakeMetadataMap, updatedIncrementalPayload)