{-# LANGUAGE ScopedTypeVariables #-}

module EulerHS.ART.EnvVars where

import           EulerHS.Prelude
import qualified Juspay.Extra.Env as Env

getDirPath :: Text
getDirPath =
    let envType =  Env.JuspayEnv
                    { key = "RECORDER_RECORDINGS_DIR"
                    , actionLeft = Env.mkDefaultEnvAction ("/" :: Text)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

shouldRecordForkFLow :: Bool
shouldRecordForkFLow =
    let envType =  Env.JuspayEnv
                    { key = "RECORD_FORK_FLOW"
                    , actionLeft = Env.mkDefaultEnvAction (False :: Bool)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

isArtReplayEnabled :: Bool
isArtReplayEnabled =
    let envType =  Env.JuspayEnv
                    { key = "IS_REPLAY_ENABLED"
                    , actionLeft = Env.mkDefaultEnvAction (False :: Bool)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

dbRetryTime :: Int
dbRetryTime =
    let envType =  Env.JuspayEnv
                    { key = "DB_RETRY_THREAD_DELAY_TIME"
                    , actionLeft = Env.mkDefaultEnvAction (1000000 :: Int)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

dbRetryAttemps :: Integer
dbRetryAttemps =
    let envType = Env.JuspayEnv
                    { key = "DB_RETRY_ATTEMPTS"
                    , actionLeft = Env.mkDefaultEnvAction (1 :: Integer)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

mockServerURL :: Text
mockServerURL =
    let envType =  Env.JuspayEnv
                    { key = "MOCK_SERVER_URL"
                    , actionLeft = Env.mkDefaultEnvAction ("http://euler-hs-art-__VERSION__.art-recordings.svc.cluster.local" :: Text)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

mockServerURLV2 :: Text
mockServerURLV2 =
    let envType =  Env.JuspayEnv
                    { key = "MOCK_SERVER_URL_V2"
                    , actionLeft = Env.mkDefaultEnvAction ("http://euler-hs-art-__VERSION__.art-recordings.svc.cluster.local" :: Text)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

replayVersion :: Text
replayVersion =
    let envType =  Env.JuspayEnv
                    { key = "REPLAY_VERSION"
                    , actionLeft = Env.mkDefaultEnvAction ("00" :: Text)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

shouldLogCallStackART :: Bool
shouldLogCallStackART =
    let envType =  Env.JuspayEnv
                    { key = "SHOULD_LOG_CALLSTACK_ART"
                    , actionLeft = Env.mkDefaultEnvAction (False :: Bool)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

logTenantErrors :: Bool
logTenantErrors =
    let envType =  Env.JuspayEnv
                    { key = "LOG_TENANT_ERRORS"
                    , actionLeft = Env.mkDefaultEnvAction (False :: Bool)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

getHostName :: Text
getHostName =
    let envType =  Env.JuspayEnv
                    { key = "HOSTNAME"
                    , actionLeft = Env.mkDefaultEnvAction ("" :: Text)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

isArtV2ReplayEnabled :: Bool
isArtV2ReplayEnabled = 
    let envType =  Env.JuspayEnv
                    { key = "IS_V2_REPLAY_ENABLED"
                    , actionLeft = Env.mkDefaultEnvAction (False :: Bool)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

shouldRecordTrackerTables :: Bool
shouldRecordTrackerTables = 
    let envType =  Env.JuspayEnv
                    { key = "RECORD_TRACKER_TABLES"
                    , actionLeft = Env.mkDefaultEnvAction (False :: Bool)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

shouldReplayConfigTables :: Bool
shouldReplayConfigTables = 
    let envType =  Env.JuspayEnv
                    { key = "REPLAY_CONFIG_TABLES"
                    , actionLeft = Env.mkDefaultEnvAction (True :: Bool)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

shouldReplayAllJoins :: Bool
shouldReplayAllJoins = 
    let envType =  Env.JuspayEnv
                    { key = "REPLAY_ALL_JOINS"
                    , actionLeft = Env.mkDefaultEnvAction (True :: Bool)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

shouldReplayCommonConfigTables :: Bool
shouldReplayCommonConfigTables = 
    let envType =  Env.JuspayEnv
                    { key = "REPLAY_COMMON_CONFIG_TABLES"
                    , actionLeft = Env.mkDefaultEnvAction (False :: Bool)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

shouldReplayNonKVRedis :: Bool
shouldReplayNonKVRedis = 
    let envType =  Env.JuspayEnv
                    { key = "REPLAY_NON_KV_REDIS"
                    , actionLeft = Env.mkDefaultEnvAction (True :: Bool)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

shouldRecordTimestamp :: Bool
shouldRecordTimestamp = 
    let envType =  Env.JuspayEnv
                    { key = "RECORD_TIMESTAMP"
                    , actionLeft = Env.mkDefaultEnvAction (False :: Bool)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

shouldRecordSetOption :: Bool
shouldRecordSetOption = 
    let envType =  Env.JuspayEnv
                    { key = "RECORD_SET_OPTIONS"
                    , actionLeft = Env.mkDefaultEnvAction (False :: Bool)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

shouldBlacklistTagsForRecordingGlobalOptions :: Bool
shouldBlacklistTagsForRecordingGlobalOptions = 
    let envType =  Env.JuspayEnv
                    { key = "BLACKLIST_TAGS_FOR_RECORDING_GLOBAL_OPTIONS"
                    , actionLeft = Env.mkDefaultEnvAction (True :: Bool)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

recordGlobalOptionsTagsList :: [String]
recordGlobalOptionsTagsList = 
    let envType =  Env.JuspayEnv
                    { key = "TAGS_FOR_RECORDING_GLOBAL_OPTIONS_VIA_JSON"
                    , actionLeft = Env.mkDefaultEnvAction ([] :: [String])
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

getARTAesKey :: String
getARTAesKey = 
  let envType =  Env.JuspayEnv
                  { key = "ART_AES_KEY"
                  , actionLeft = Env.mkDefaultEnvAction (mempty :: String)
                  , decryptFunc = pure
                  , logWhenThrowException = Nothing
                  }
  in Env.lookupEnv envType

getARTAesIv :: String
getARTAesIv = 
  let envType =  Env.JuspayEnv
                  { key = "ART_AES_IV"
                  , actionLeft = Env.mkDefaultEnvAction (mempty :: String)
                  , decryptFunc = pure
                  , logWhenThrowException = Nothing
                  }
  in Env.lookupEnv envType

getARTKafkaTimeout :: Int
getARTKafkaTimeout = 
  let envType =  Env.JuspayEnv
                  { key = "ART_KAFKA_TIMEOUT"
                  , actionLeft = Env.mkDefaultEnvAction (1000 :: Int)
                  , decryptFunc = pure
                  , logWhenThrowException = Nothing
                  }
  in Env.lookupEnv envType

getARTKafkaTopic :: Text
getARTKafkaTopic = 
  let envType =  Env.JuspayEnv
                  { key = "ART_KAFKA_TOPIC"
                  , actionLeft = Env.mkDefaultEnvAction ("euler-art" :: Text)
                  , decryptFunc = pure
                  , logWhenThrowException = Nothing
                  }
  in Env.lookupEnv envType

getARTKafkaBrokersList :: [Text]
getARTKafkaBrokersList = 
  let envType =  Env.JuspayEnv
                  { key = "ART_KAFKA_BROKERS_LIST"
                  , actionLeft = Env.mkDefaultEnvAction (mempty :: [Text])
                  , decryptFunc = pure
                  , logWhenThrowException = Nothing
                  }
  in Env.lookupEnv envType

getARTKafkaMaxProducerConnectAttempts :: Int
getARTKafkaMaxProducerConnectAttempts = 
  let envType =  Env.JuspayEnv
                  { key = "ART_KAFKA_MAX_PRODUCER_RETRIES"
                  , actionLeft = Env.mkDefaultEnvAction (3 :: Int)
                  , decryptFunc = pure
                  , logWhenThrowException = Nothing
                  }
  in Env.lookupEnv envType

getARTKafkaMaxMessageRetries :: Int
getARTKafkaMaxMessageRetries = 
  let envType =  Env.JuspayEnv
                  { key = "ART_KAFKA_MAX_MESSAGE_RETRIES"
                  , actionLeft = Env.mkDefaultEnvAction (10 :: Int)
                  , decryptFunc = pure
                  , logWhenThrowException = Nothing
                  }
  in Env.lookupEnv envType

getARTKafkaMaxQueueSizeKB :: Int
getARTKafkaMaxQueueSizeKB = 
  let envType =  Env.JuspayEnv
                  { key = "ART_KAFKA_MAX_QUEUE_SIZE_KB"
                  , actionLeft = Env.mkDefaultEnvAction (5120 :: Int)
                  , decryptFunc = pure
                  , logWhenThrowException = Nothing
                  }
  in Env.lookupEnv envType

getARTKafkaProducerRequestTimeout :: Int
getARTKafkaProducerRequestTimeout =
  let envType =  Env.JuspayEnv
                  { key = "ART_KAFKA_PRODUCER_REQUEST_TIMEOUT"
                  , actionLeft = Env.mkDefaultEnvAction (30000 :: Int)
                  , decryptFunc = pure
                  , logWhenThrowException = Nothing
                  }
  in Env.lookupEnv envType


getARTKafkaLogLevel :: Text
getARTKafkaLogLevel = 
  let envType =  Env.JuspayEnv
                  { key = "ART_KAFKA_LOG_LEVEL"
                  , actionLeft = Env.mkDefaultEnvAction ("ERROR" :: Text)
                  , decryptFunc = pure
                  , logWhenThrowException = Nothing
                  }
  in Env.lookupEnv envType

getARTKafkaBrokerAddressFamily :: Text
getARTKafkaBrokerAddressFamily = 
  let envType =  Env.JuspayEnv
                  { key = "ART_KAFKA_BROKER_ADDRESS_FAMILY"
                  , actionLeft = Env.mkDefaultEnvAction ("any" :: Text)
                  , decryptFunc = pure
                  , logWhenThrowException = Nothing
                  }
  in Env.lookupEnv envType

getARTKafkaBrokerAcks :: Int
getARTKafkaBrokerAcks = 
  let envType =  Env.JuspayEnv
                  { key = "ART_KAFKA_BROKER_ACKS"
                  , actionLeft = Env.mkDefaultEnvAction (1 :: Int)
                  , decryptFunc = pure
                  , logWhenThrowException = Nothing
                  }
  in Env.lookupEnv envType

getARTKafkaGlobalProps :: Text
getARTKafkaGlobalProps = 
  let envType =  Env.JuspayEnv
                  { key = "ART_KAFKA_PRODUCER_PROPS"
                  , actionLeft = Env.mkDefaultEnvAction (mempty :: Text)
                  , decryptFunc = pure
                  , logWhenThrowException = Nothing
                  }
  in Env.lookupEnv envType

getARTKafkaTopicProps :: Text
getARTKafkaTopicProps = 
  let envType =  Env.JuspayEnv
                  { key = "ART_KAFKA_TOPIC_PROPS"
                  , actionLeft = Env.mkDefaultEnvAction (mempty :: Text)
                  , decryptFunc = pure
                  , logWhenThrowException = Nothing
                  }
  in Env.lookupEnv envType

httpReplayAPIList :: [Text]
httpReplayAPIList =
    let envType =  Env.JuspayEnv
                    { key = "HTTP_REPLAY_API_LIST"
                    , actionLeft = Env.mkDefaultEnvAction ([] :: [Text])
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

whitelistedRunIORecordingFunctions :: [String]
whitelistedRunIORecordingFunctions = 
    let envType =  Env.JuspayEnv
                    { key = "WHITELISTED_FUNCTIONS_FOR_RUN_IO_RECORDING"
                    , actionLeft = Env.mkDefaultEnvAction ([] :: [String])
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

blacklistedModuleForRunIORecording :: [String]
blacklistedModuleForRunIORecording = 
    let envType =  Env.JuspayEnv
                    { key = "BLACKLISTED_MODULES_FOR_RUN_IO_RECORDING"
                    , actionLeft = Env.mkDefaultEnvAction ([] :: [String])
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

blacklistedTagsForRunIORecording :: [String]
blacklistedTagsForRunIORecording = 
    let envType =  Env.JuspayEnv
                    { key = "BLACKLISTED_TAGS_FOR_RUN_IO_RECORDING"
                    , actionLeft = Env.mkDefaultEnvAction ([] :: [String])
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

whitelistedRunIOReplayFunctions :: [String]
whitelistedRunIOReplayFunctions = 
    let envType =  Env.JuspayEnv
                    { key = "WHITELISTED_FUNCTIONS_FOR_RUN_IO_REPLAY"
                    , actionLeft = Env.mkDefaultEnvAction ([] :: [String])
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

blacklistedModuleForRunIOReplay :: [String]
blacklistedModuleForRunIOReplay = 
    let envType =  Env.JuspayEnv
                    { key = "BLACKLISTED_MODULES_FOR_RUN_IO_REPLAY"
                    , actionLeft = Env.mkDefaultEnvAction ([] :: [String])
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

blacklistedTagsForRunIOReplay :: [String]
blacklistedTagsForRunIOReplay = 
    let envType =  Env.JuspayEnv
                    { key = "BLACKLISTED_TAGS_FOR_RUN_IO_REPLAY"
                    , actionLeft = Env.mkDefaultEnvAction ([] :: [String])
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType
