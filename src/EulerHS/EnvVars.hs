{-# LANGUAGE ScopedTypeVariables #-}

module EulerHS.EnvVars where

import           EulerHS.Prelude
import qualified Juspay.Extra.Env as Env

getEnv :: Text
getEnv = 
  let envType = Env.JuspayEnv
        { key = "NODE_ENV"
        , actionLeft = Env.mkDefaultEnvAction ("development" :: Text)
        , decryptFunc = pure
        , logWhenThrowException = Nothing
        }
  in Env.lookupEnv envType

forkFLowEnabled :: Bool
forkFLowEnabled =
    let envType =  Env.JuspayEnv
                    { key = "FORK_FLOW_ENABLED_FOR_QUERY"
                    , actionLeft = Env.mkDefaultEnvAction (False :: Bool)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

shouldEnforcePii :: Bool
shouldEnforcePii =
    let envType =  Env.JuspayEnv
                    { key = "SHOULD_ENFORCE_PII"
                    , actionLeft = Env.mkDefaultEnvAction (False :: Bool)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

shouldLogXmlContent :: Bool
shouldLogXmlContent =
    let envType =  Env.JuspayEnv
                    { key = "SHOULD_LOG_XML_CONTENT"
                    , actionLeft = Env.mkDefaultEnvAction (False :: Bool)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

shouldLogEncryptedLogs :: Bool
shouldLogEncryptedLogs =
    let envType = Env.JuspayEnv
                    { key = "SHOULD_PUSH_ENCRYPTED_LOGS"
                    , actionLeft =  Env.mkDefaultEnvAction (False :: Bool)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

getLogEncryptionKey :: String
getLogEncryptionKey =
    let envType =  Env.JuspayEnv
                    { key = "LOG_ENCRYPTION_KEY"
                    , actionLeft = Env.mkDefaultEnvAction ("" :: String)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

getLogEncryptionIV :: String
getLogEncryptionIV =
    let envType =  Env.JuspayEnv
                    { key = "LOG_ENCRYPTION_IV"
                    , actionLeft = Env.mkDefaultEnvAction ("" :: String)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

tenantIdListForLoadTesting :: [Text]
tenantIdListForLoadTesting =
    let envType =  Env.JuspayEnv
                    { key = "LOADTEST_ENABLED_TENANTS"
                    , actionLeft = Env.mkDefaultEnvAction (["test" :: Text])
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

getHeadersToFilterFromOutgoingRequest :: [Text]
getHeadersToFilterFromOutgoingRequest =
    let envType =  Env.JuspayEnv
                    { key = "HEADERS_TO_FILTER_FROM_OUTGOING_REQUEST"
                    , actionLeft = Env.mkDefaultEnvAction ([] :: [Text])
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

disableDBFallBack :: Bool
disableDBFallBack =
    let envType =  Env.JuspayEnv
                    { key = "SHOULD_DISABLE_DB_FALLBACK"
                    , actionLeft = Env.mkDefaultEnvAction (False :: Bool)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType