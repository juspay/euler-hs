module EulerHS.Testing.HSLog where

import           Data.Aeson (FromJSON, Value (..), (.:))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (prependFailure, typeMismatch)
import           EulerHS.Prelude

data HSLog
  = SetEntry
  | SetExEntry
  | GetEntry
  | ExistsEntry
  | DelEntry
  | ExpireEntry
  | IncrEntry
  | HSetEntry
  | HGetEntry
  | MultiExecEntry
  | ThrowExceptionEntry
  | CallServantApiEntry
  | SetOptionEntry
  | GetOptionEntry
  | RunSysCmdEntry
  | ForkEntry
  | GeneratedGUIDEntry
  | RunIOEntry
  | InitSqlDBConnectionEntry
  | DeInitSqlDBConnectionEntry
  | GetSqlDBConnectionEntry
  | RunDBEntry
  | GetKVDBConnectionEntry
  | AwaitEntry
  | RunSafeFlowEntry
  | LogMessageEntry

hsLogFromText :: Text -> Maybe HSLog
hsLogFromText = \case
  "SetEntry" -> Just SetEntry
  "SetExEntry" -> Just SetExEntry
  "GetEntry" -> Just GetEntry
  "ExistsEntry" -> Just ExistsEntry
  "DelEntry" -> Just DelEntry
  "ExpireEntry" -> Just ExpireEntry
  "IncrEntry" -> Just IncrEntry
  "HSetEntry"  -> Just HSetEntry
  "HGetEntry" -> Just HGetEntry
  "MultiExecEntry" -> Just MultiExecEntry
  "ThrowExceptionEntry" -> Just ThrowExceptionEntry
  "CallServantApiEntry" -> Just CallServantApiEntry
  "SetOptionEntry" -> Just SetOptionEntry
  "GetOptionEntry" -> Just GetOptionEntry
  "RunSysCmdEntry" -> Just RunSysCmdEntry
  "ForkEntry" -> Just ForkEntry
  "GeneratedGUIDEntry" -> Just GeneratedGUIDEntry
  "RunIOEntry" -> Just RunIOEntry
  "InitSqlDBConnectionEntry" -> Just InitSqlDBConnectionEntry
  "DeInitSqlDBConnectionEntry" -> Just DeInitSqlDBConnectionEntry
  "GetSqlDBConnectionEntry"  -> Just GetSqlDBConnectionEntry
  "RunDBEntry" -> Just RunDBEntry
  "GetKVDBConnectionEntry" -> Just GetKVDBConnectionEntry
  "AwaitEntry" -> Just AwaitEntry
  "RunSafeFlowEntry" -> Just RunSafeFlowEntry
  "LogMessageEntry" -> Just LogMessageEntry
  _  -> Nothing

instance FromJSON HSLog where
  parseJSON j = Aeson.withObject "HSLog"
    (\o -> do
      logType <- o .: "_entryName"
      case hsLogFromText logType of
        Nothing -> prependFailure "parsing HSLog failed, "
          (typeMismatch "HSLog" j)
        Just x -> pure x
    )
    j


