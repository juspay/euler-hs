{-# LANGUAGE OverloadedStrings #-}

module EulerHS.Extra.KafkaClient.Utils 
  ( module EulerHS.Extra.KafkaClient.Utils
  , newProducer
  , KafkaProducer
  , ProducerProperties
  )
where

import EulerHS.Encryption
import EulerHS.Prelude
import Kafka.Producer
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8
import           Data.Either.Extra (mapRight)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

data ValueType = DB | DB_JOINS | DB_AGGREGATE | UUID | REDIS | IMC | INCOMING_API | OUTGOING_API | DROP_RECORDING | TIMESTAMP | RANDOM_RIO | RANDOM_BYTES | FORK_FLOW | SET_OPTIONS | GET_OPTIONS | PT_START | RUN_IO
    deriving (Show)

type KafkaMaxProducerConnectAttempts = Int

data Payload = Payload
    { sessionId :: Text
    , mid :: Text
    , orderId :: Text
    , messageNumber :: Int
    , valueType :: ValueType
    , value :: ByteString
    } deriving (Show)

mkEncPayload :: Text -> Text -> Text -> Text -> Text -> Int -> ValueType -> ByteString -> Either Text ByteString
mkEncPayload aesKey aesIV sessId mId oId num vType plainValue =
    let eitherEncryptedValue = runAES (encodeUtf8 aesKey) (encodeUtf8 aesIV) plainValue
    in mapRight (\encPayload -> serializePayload $ Payload sessId mId oId num vType encPayload) eitherEncryptedValue

serializePayload :: Payload -> ByteString
serializePayload (Payload sessId mId oId msgNum vType encValue) =
    C8.pack $ T.unpack $ T.intercalate ","
        [ sessId, mId, oId
        , T.pack (show msgNum)
        , T.pack (show vType)
        , decodeUtf8 $ B64.encode encValue
        ]

mkKafkaProps :: [Text] -> Int -> Text -> Int -> Map.Map Text Text -> Map.Map Text Text -> ProducerProperties
mkKafkaProps brokers kafkaMsgTimeout kafkaLogLevel maxQueueBufferSizeInKb otherGlobalProps otherTopicProps = 
     extraTopicProps otherTopicProps
  <> extraProps otherGlobalProps
  <> brokersList (BrokerAddress <$> brokers)
  <> logLevel (getKafkaLogLevel kafkaLogLevel)
  <> sendTimeout (Timeout kafkaMsgTimeout)
  <> extraProps (Map.singleton "queue.buffering.max.kbytes" (show maxQueueBufferSizeInKb))
  where
    getKafkaLogLevel "DEBUG" = KafkaLogDebug
    getKafkaLogLevel "INFO"  = KafkaLogInfo
    getKafkaLogLevel "ERROR" = KafkaLogErr
    getKafkaLogLevel _       = KafkaLogCrit

runProducer :: KafkaProducer -> Text -> ByteString -> ByteString -> IO (Either SomeException ())
runProducer kafkaProducer topicName payload key = do
  let topic = TopicName topicName
  eRes <- produceMessage kafkaProducer (mkMessage topic payload key)
  case eRes of
    Just err -> pure $ Left $ toException err
    _ -> pure $ Right ()

mkMessage :: TopicName -> ByteString -> ByteString -> ProducerRecord
mkMessage topic payload key =
    ProducerRecord
        { prTopic = topic
        , prPartition = UnassignedPartition
        , prKey = Just $ key
        , prValue = Just $ payload
        }

parseKafkaProps :: Text -> Map.Map Text Text
parseKafkaProps "" = Map.empty
parseKafkaProps s  = Map.fromList (fmap T.tail <$> (T.breakOn "=" <$> T.splitOn "&" s))

