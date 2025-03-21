{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EulerHS.Logger.Language
  (
    Logger
  , LoggerMethod(..)
  , logMessageFormatted
  , masterLogger
  ) where

import qualified EulerHS.Logger.Types as T
import qualified EulerHS.Extra.Monitoring.Types as T
import           EulerHS.Prelude
import           Type.Reflection
import qualified Juspay.Extra.Env as Env

-- | Language for logging.
data LoggerMethod next where
  -- | Log message with a predefined level.
  LogMessage :: T.LogLevel -> !T.VersionLoggerMessage -> (() -> next) -> LoggerMethod next 

instance Functor LoggerMethod where
  fmap f (LogMessage lvl vMsg next) = LogMessage lvl vMsg $ f . next

type Logger = F LoggerMethod

logMessage' :: forall tag . (Typeable tag, Show tag) => T.LogLevel -> tag -> T.Message -> Logger ()
logMessage' lvl tag msg = liftFC $ LogMessage lvl (T.Ver1 textTag msg) id
  where
    textTag :: Text
    textTag
      | Just HRefl <- eqTypeRep (typeRep @tag) (typeRep @Text  ) = tag
      | Just HRefl <- eqTypeRep (typeRep @tag) (typeRep @String) = toText tag
      | otherwise = show tag

logMessageFormatted :: forall tag. (Typeable tag, Show tag) => T.LogLevel -> T.Category -> Maybe T.Action -> Maybe T.PrimaryKey -> Maybe T.SecondaryKeys -> Maybe T.Entity -> Maybe T.ErrorL -> Maybe T.Latency -> Maybe T.IOLatencyMetric -> Maybe Int64 -> Maybe Int32 -> Maybe Int64 -> Maybe T.RespCode -> T.Message -> tag -> Maybe Bool -> Logger ()
logMessageFormatted logLevel category action primaryKey secondaryKeys entity maybeError maybeLatency maybeIOLatencyMetric maybeCpuTime maybeMemBytes maybeGcTime maybeRespCode message tag artStatus =
  liftFC $ LogMessage logLevel (T.Ver2 category action' primaryKey secondaryKeys entity maybeError maybeLatency maybeIOLatencyMetric maybeCpuTime maybeMemBytes maybeGcTime maybeRespCode message artStatus) id
    where
    action' = action <|> (Just textTag) -- keeping tag as action now, if action not found, going ahead we will remove this by verifying all domain action logs

    textTag :: Text
    textTag
      | Just HRefl <- eqTypeRep (typeRep @tag) (typeRep @Text  ) = tag
      | Just HRefl <- eqTypeRep (typeRep @tag) (typeRep @String) = toText tag
      | otherwise = show tag


{-
based on log config:
V1 - older version of logging
V2 - newer version of logging
V1_V2 - both version of logging
-}

masterLogger :: forall tag. (Typeable tag, Show tag) => T.LogLevel -> tag -> T.Category -> Maybe T.Action -> Maybe T.PrimaryKey -> Maybe T.SecondaryKeys -> Maybe T.Entity -> Maybe T.ErrorL -> Maybe T.Latency -> Maybe T.IOLatencyMetric -> Maybe Int64 -> Maybe Int32 -> Maybe Int64 -> Maybe T.RespCode -> T.Message -> Maybe Bool -> Logger ()
masterLogger logLevel tag category action primaryKey secondaryKeys entity maybeError maybeLatency maybeIOLatencyMetric maybeCpuTime maybeMemBytes maybeGcTime maybeRespCode message artStatus
  | version == "V1" = logMessage' logLevel tag message
  | version == "V2"= logMessageFormatted logLevel category action primaryKey secondaryKeys entity maybeError maybeLatency maybeIOLatencyMetric maybeCpuTime maybeMemBytes maybeGcTime maybeRespCode message tag artStatus
  | version == "V1_V2" = do
    logMessage' logLevel tag message
    logMessageFormatted logLevel category action primaryKey secondaryKeys entity maybeError maybeLatency maybeIOLatencyMetric maybeCpuTime maybeMemBytes maybeGcTime maybeRespCode message tag artStatus
  | otherwise = logMessage' logLevel tag message
  where
    version = getLoggerFormatVersion

getLoggerFormatVersion :: Text
getLoggerFormatVersion =
    let envType =  Env.JuspayEnv
                    { key = "LOGGING_VERSION"
                    , actionLeft = Env.mkDefaultEnvAction ("V2" :: Text)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType