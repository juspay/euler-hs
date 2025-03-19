{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module EulerHS.ART.V2.FlowUtils
  ( producePayload,
  ) where

import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import           EulerHS.ART.EnvVars
import           EulerHS.Extra.KafkaClient.Utils
import qualified EulerHS.Extra.KMS as KMS
import qualified EulerHS.Framework.Language as L
import           EulerHS.Prelude
import           EulerHS.Framework.Runtime (FlowRuntime(..), KafkaSettings(..))
import qualified Data.Map as Map
import           EulerHS.EnvVars


tryBoth :: L.MonadFlow m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
tryBoth action1 action2 = do
    result1 <- action1
    case result1 of
        Just _  -> return result1
        Nothing -> action2

getARTEncKeyAndIV :: L.MonadFlow m => m (Maybe Text, Maybe Text)
getARTEncKeyAndIV = do
  flowRt <- L.getFlowRuntime
  kafkaSettings@KafkaSettings{..} <- L.runIO . takeMVar $ _kafkaProducer flowRt
  if isKmsDecoded
    then do
      L.runIO $ putMVar (_kafkaProducer flowRt) kafkaSettings
      pure (encKey, encIV)
    else do
      let kmsDecodeFn = 
            if (DT.toLower getEnv == "development")
              then (\val _ -> pure . Right $ Just val)
              else KMS.decodeKMS
      decArtAesKey <- either (const Nothing) (fmap DT.pack) <$> kmsDecodeFn getARTAesKey "ART_AES_KEY"
      decArtAesIv  <- either (const Nothing) (fmap DT.pack) <$> kmsDecodeFn getARTAesIv "ART_AES_IV"
      L.runIO $ putMVar (_kafkaProducer flowRt) (kafkaSettings {encKey = decArtAesKey, encIV = decArtAesIv, isKmsDecoded = True})
      pure (decArtAesKey, decArtAesIv)

runProducerWithRetries :: L.MonadFlow m => (KafkaProducer -> IO (Either SomeException ())) -> m (Either SomeException ()) 
runProducerWithRetries kafkaFlow = do
  flowRt <- L.getFlowRuntime
  kafkaSettings@KafkaSettings{..} <- L.runIO . takeMVar $ _kafkaProducer flowRt
  case kafkaProducer of
    Left kafkaErr -> 
      case retryCount <= getARTKafkaMaxProducerConnectAttempts of
        True -> do
          let updatedRetryCount = retryCount + 1
              kafkaGlobalProps = 
                  Map.insert "request.timeout.ms" (show getARTKafkaProducerRequestTimeout)
                $ Map.insert "broker.address.family" getARTKafkaBrokerAddressFamily
                $ Map.insert "acks" (show getARTKafkaBrokerAcks)
                $ Map.insert "message.send.max.retries" (show getARTKafkaMaxMessageRetries)
                $ parseKafkaProps getARTKafkaGlobalProps
              kafkaTopicProps = parseKafkaProps getARTKafkaTopicProps
              kafkaProps = mkKafkaProps getARTKafkaBrokersList getARTKafkaTimeout getARTKafkaLogLevel getARTKafkaMaxQueueSizeKB kafkaGlobalProps kafkaTopicProps
          L.runIO $ do
            newKafkaProducer <- newProducer kafkaProps
            putMVar (_kafkaProducer flowRt) (kafkaSettings {retryCount = updatedRetryCount, kafkaProducer = newKafkaProducer})
          runProducerWithRetries kafkaFlow
        False -> do
          L.runIO $ putMVar (_kafkaProducer flowRt) kafkaSettings
          pure . Left $ toException kafkaErr
    Right producer -> 
      L.runIO $ do
        putMVar (_kafkaProducer flowRt) (kafkaSettings {retryCount = retryCount, kafkaProducer = Right producer})
        kafkaFlow producer

producePayload :: L.MonadFlow m => ValueType -> ByteString -> m ()
producePayload vType plainValue = do
  mbSessionID <- L.getLoggerContext "x-request-id"
  mbMerchantId <- tryBoth (L.getLoggerContext "merchantId") (L.getLoggerContext "merchant_id") 
  mbOrderId <- tryBoth (L.getLoggerContext "orderId") (L.getLoggerContext "order_id")
  msgNumber <- L.incrementArtCounter
  (mbArtAesKey, mbArtAesIv) <- getARTEncKeyAndIV  
  case (mbSessionID, mbArtAesKey, mbArtAesIv) of
    (Just sessId, Just artAesKey, Just artAesIv) -> 
      case mkEncPayload artAesKey artAesIv sessId (fromMaybe "merchantId" mbMerchantId) (fromMaybe "orderId" mbOrderId) msgNumber vType plainValue of
        Left err -> L.logError @Text "ART_MAKE_PAYLOAD" (err)
        Right payload -> do
          eres <- runProducerWithRetries $ \kafkaProducer -> runProducer kafkaProducer getARTKafkaTopic payload (DTE.encodeUtf8 sessId)
          case eres of
            Left e -> L.logErrorV @Text "ART_PRODUCE_PAYLOAD" (displayException e)
            Right _ -> pure ()
    _ -> pure ()