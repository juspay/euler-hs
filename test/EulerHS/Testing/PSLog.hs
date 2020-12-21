module EulerHS.Testing.PSLog where

import           Data.Aeson (FromJSON, Value (..), (.:))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (Parser, prependFailure, typeMismatch)
import           Data.Vector ((!?))
import           EulerHS.Prelude

data PSLog
  = LogEntry
  | RunDBEntry
  | RunKVDBEitherEntry
  | DoAffEntry
  | SetOptionEntry
  | GetOptionEntry
  | GenerateGUIDEntry
  | CallAPIEntry
  | ForkFlowEntry
  | ThrowExceptionEntry
  | RunSysCmdEntry
  | GetDBConnEntry
  | GetKVDBConnEntry
  | RunKVDBSimpleEntry
  | UnexpectedRecordingEnd
  | UnknownRRItem
  | ItemMismatch
  | ForkedFlowRecordingsMissed
  | MockDecodingFailed
  | UnknownPlaybackError
  | Other

psLogFromText :: Text -> Maybe PSLog
psLogFromText = \case
  "LogEntry" -> Just LogEntry
  "RunDBEntry" -> Just RunDBEntry
  "RunKVDBEitherEntry" -> Just RunKVDBEitherEntry
  "DoAffEntry" -> Just DoAffEntry
  "SetOptionEntry" -> Just SetOptionEntry
  "GetOptionEntry" -> Just GetOptionEntry
  "GenerateGUIDEntry" -> Just GenerateGUIDEntry
  "CallAPIEntry" -> Just CallAPIEntry
  "ForkFlowEntry" -> Just ForkFlowEntry
  "ThrowExceptionEntry" -> Just ThrowExceptionEntry
  "RunSysCmdEntry" -> Just RunSysCmdEntry
  "GetDBConnEntry" -> Just GetDBConnEntry
  "GetKVDBConnEntry" -> Just GetKVDBConnEntry
  "RunKVDBSimpleEntry" -> Just RunKVDBSimpleEntry
  "UnexpectedRecordingEnd" -> Just UnexpectedRecordingEnd
  "UnknownRRItem" -> Just UnknownRRItem
  "ItemMismatch" -> Just ItemMismatch
  "ForkedFlowRecordingsMissed" -> Just ForkedFlowRecordingsMissed
  "MockDecodingFailed" -> Just MockDecodingFailed
  "UnknownPlaybackError" -> Just UnknownPlaybackError
  "Other" -> Just Other
  _ -> Nothing

instance FromJSON PSLog where
  parseJSON j = Aeson.withArray "PSLog"
    (\a -> do
      logType <- (traverse Aeson.parseJSON $ a !? 2  :: Parser (Maybe Text))
      case psLogFromText =<< logType of
        Nothing -> prependFailure "parsing PSLog failed, "
          (typeMismatch "PSLog" j)
        Just x -> pure x
    )
    j

