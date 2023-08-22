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
import           EulerHS.Prelude
import           Type.Reflection
import           Juspay.Extra.Config (lookupEnvT)

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

logMessageFormatted :: forall tag. (Typeable tag, Show tag) => T.LogLevel -> T.Category -> Maybe T.Action -> Maybe T.Entity -> Maybe T.ErrorL -> Maybe T.Latency -> Maybe T.RespCode -> T.Message -> tag -> Logger ()
logMessageFormatted logLevel category action entity maybeError maybeLatency maybeRespCode message tag =
  liftFC $ LogMessage logLevel (T.Ver2 category action' entity maybeError maybeLatency maybeRespCode message) id
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

masterLogger :: forall tag. (Typeable tag, Show tag) => T.LogLevel -> tag -> T.Category -> Maybe T.Action -> Maybe T.Entity -> Maybe T.ErrorL -> Maybe T.Latency -> Maybe T.RespCode -> T.Message -> Logger ()
masterLogger logLevel tag category action entity maybeError maybeLatency maybeRespCode message
  | version == "V1" = logMessage' logLevel tag message
  | version == "V2"= logMessageFormatted logLevel category action entity maybeError maybeLatency maybeRespCode message tag
  | version == "V1_V2" = do
    logMessage' logLevel tag message
    logMessageFormatted logLevel category action entity maybeError maybeLatency maybeRespCode message tag
  | otherwise = logMessage' logLevel tag message
  where
    version = getLoggerFormatVersion

getLoggerFormatVersion :: Text
getLoggerFormatVersion = fromMaybe "V1" $ lookupEnvT "LOGGING_VERSION"