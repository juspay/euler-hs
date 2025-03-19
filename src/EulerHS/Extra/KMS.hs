{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module EulerHS.Extra.KMS where

import           Data.ByteString.Base64 (decodeLenient)
import           Data.Time.Clock (diffTimeToPicoseconds)
import           Data.Time.Clock.System (getSystemTime, systemToTAITime)
import           Data.Time.Clock.TAI (diffAbsoluteTime)
import           EulerHS.Framework.Language as L
import           EulerHS.Prelude
import           Data.Either.Extra (mapLeft)
import qualified Amazonka as AWS
import qualified Amazonka.Data as AWS
import qualified Amazonka.KMS.Decrypt as AWS
import qualified Amazonka.KMS.Encrypt as AWS
import qualified Control.Lens as AWS
import qualified Data.ByteString.Char8 as BSC (pack, unpack)
import qualified Data.Generics.Product as AWS
import qualified Data.Text as DT
import qualified EulerHS.Extra.Monitoring.Flow as EEMF
import qualified EulerHS.Extra.Monitoring.Types as EEMT
import qualified Juspay.Extra.Env as Env

-- Common AWS-KMS related functions

picoMilliDiff :: Integer
picoMilliDiff = 1000000000

getAwsRegion :: AWS.Region
getAwsRegion = fromMaybe AWS.Mumbai lookupRegion

awsLogLevel :: Text
awsLogLevel =
    let envType = Env.JuspayEnv
                    { key = "AWS_LOG_LEVEL"
                    , actionLeft = Env.mkDefaultEnvAction ("error" :: Text)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

lookupRegion :: Maybe AWS.Region
lookupRegion =
    let envType1 = Env.JuspayEnv
                    { key = "KMS_AWS_REGION"
                    , actionLeft = Env.mkDefaultEnvAction (Nothing :: Maybe Text)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
        envType2 = Env.JuspayEnv
                    { key = "AWS_REGION"
                    , actionLeft = Env.mkDefaultEnvAction (Nothing :: Maybe Text)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
        region1 = Env.lookupEnv envType1
        region2 = Env.lookupEnv envType2
    in case region1 <|> region2 of
        Just "" -> Nothing
        Just t  -> Just $ AWS.Region' t
        Nothing -> Nothing


-- AWS-KMS Encryption Functions

encryptKMS :: (HasCallStack, L.MonadFlow m) => Text -> ByteString -> m (Either Text (Maybe ByteString))
encryptKMS kmsKeyId value = do
  let region = getAwsRegion
  encryptKMSWithRegion region kmsKeyId value

encryptKMSWithRegion :: (HasCallStack, L.MonadFlow m) => AWS.Region -> Text -> ByteString -> m (Either Text (Maybe ByteString))
encryptKMSWithRegion awsRegion kmsKeyId value = do
  let loggerLevel = either (const AWS.Error) id (AWS.fromText awsLogLevel)
  L.runIO $ encryptWithKMS awsRegion loggerLevel kmsKeyId value

encryptWithKMS :: AWS.Region -> AWS.LogLevel -> Text -> ByteString -> IO (Either Text (Maybe ByteString))
encryptWithKMS awsRegion loggerLevel kmsKeyId value = mapLeft handleException <$> (try $ do
    lgr <- AWS.newLogger loggerLevel stdout
    env <- AWS.newEnv AWS.discover
        <&> AWS.set (AWS.field @"logger") lgr
          . AWS.set (AWS.field @"region") awsRegion
    AWS.runResourceT $ do
      decResp <- AWS.send env $ AWS.newEncrypt kmsKeyId value 
      pure $ decResp ^. AWS.encryptResponse_ciphertextBlob
  )
  where
    handleException :: SomeException -> Text
    handleException = DT.pack . displayException

-- AWS-KMS Decryption Functions
  
decryptKMS :: (HasCallStack, L.MonadFlow m) => ByteString -> m (Either Text (Maybe ByteString))
decryptKMS value = do
  let reg = getAwsRegion
  decryptKmsWithRegion reg value
  
decryptKmsWithRegion :: (HasCallStack, L.MonadFlow m) => AWS.Region -> ByteString ->  m (Either Text (Maybe ByteString))
decryptKmsWithRegion awsRegion value = do
  let loggerLevel =
          case AWS.fromText awsLogLevel of
            Left _ -> AWS.Error
            Right val -> val
  L.runIO $ decryptWithKMS awsRegion loggerLevel value

decryptWithKMS :: AWS.Region -> AWS.LogLevel -> ByteString -> IO (Either Text (Maybe ByteString))
decryptWithKMS awsRegion loggerLevel value = mapLeft handleException <$> (try $ do
    lgr <- AWS.newLogger loggerLevel stdout
    env <- AWS.newEnv AWS.discover
        <&> AWS.set (AWS.field @"logger") lgr
          . AWS.set (AWS.field @"region") awsRegion
    AWS.runResourceT $ do
      decResp <- AWS.send env $ AWS.newDecrypt value
      pure $ decResp ^. AWS.decryptResponse_plaintext  
  )
  where
    handleException :: SomeException -> Text
    handleException = DT.pack . displayException

decodeKMS :: (HasCallStack, L.MonadFlow m) => String -> String -> m (Either Text (Maybe String))
decodeKMS cipherText envName = do
  start <- L.runIO $ systemToTAITime <$> getSystemTime
  let preparedText = decodeLenient $ BSC.pack cipherText
  decryptedBS <- decryptKMS preparedText
  resp <- case decryptedBS of
    Right (Just v) -> pure . Right . Just $ BSC.unpack v
    Right Nothing -> do
      L.logError @Text "decryptKMS" ("drsPlaintext returned as Nothing" <> "Env: " <> (DT.pack envName))
      pure . Right $ Nothing
    Left err -> do
      L.logError @Text "decryptKMS" ("Env: " <> (DT.pack envName) <> " : " <> err)
      pure . Left $ err
  end <- L.runIO $ systemToTAITime <$> getSystemTime
  let latency = div (diffTimeToPicoseconds $ diffAbsoluteTime end start) picoMilliDiff
  (EEMT.LatencyInfo oldLatency count) <- maybe EEMF.defaultLatencyMetric (\(EEMT.KMSMetricInfo x) -> x) <$> L.getOptionLocal EEMT.KMSMetricInfoKey
  L.setOptionLocal EEMT.KMSMetricInfoKey $ EEMT.KMSMetricInfo (EEMT.LatencyInfo (oldLatency + fromInteger latency) (count + 1))
  pure resp
