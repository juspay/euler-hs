module EulerHS.Testing.CommonLog where

import           EulerHS.Prelude
import           EulerHS.Testing.HSLog (HSLog (..))
import qualified EulerHS.Testing.HSLog as HSLog
import           EulerHS.Testing.PSLog (PSLog (..))
import qualified EulerHS.Testing.PSLog as PSLog

data CommonLog
  = CMLog
  | CMIO
  | CMRunDB
  | CMGetDBConn
  | CMGetKVDBConn
  | CMFork
  | CMSysCmd
  | CMKVDB
  | CMException
  | CMSetOpt
  | CMGetOpt
  | CMGenGuid
  | CMCallApi
  deriving (Eq, Ord, Show)

isLog :: CommonLog -> Bool
isLog = \case
  CMLog -> True
  _     -> False

fromHSLog :: HSLog.HSLog -> Maybe CommonLog
fromHSLog = \case
  SetEntry -> Just CMKVDB
  SetExEntry -> Just CMKVDB
  GetEntry -> Just CMKVDB
  ExistsEntry -> Just CMKVDB
  DelEntry -> Just CMKVDB
  ExpireEntry -> Just CMKVDB
  IncrEntry -> Just CMKVDB
  HSetEntry  -> Just CMKVDB
  HGetEntry -> Just CMKVDB
  MultiExecEntry -> Nothing
  HSLog.ThrowExceptionEntry -> Just CMException
  CallServantApiEntry -> Just CMCallApi
  HSLog.SetOptionEntry -> Just CMSetOpt
  HSLog.GetOptionEntry -> Just CMGetOpt
  HSLog.RunSysCmdEntry -> Just CMSysCmd
  ForkEntry -> Just CMFork
  GeneratedGUIDEntry -> Just CMGenGuid
  RunIOEntry -> Just CMIO
  InitSqlDBConnectionEntry -> Nothing
  DeInitSqlDBConnectionEntry -> Nothing
  GetSqlDBConnectionEntry -> Just CMGetDBConn
  HSLog.RunDBEntry -> Just CMRunDB
  GetKVDBConnectionEntry -> Just CMGetKVDBConn
  AwaitEntry -> Nothing
  RunSafeFlowEntry -> Nothing
  LogMessageEntry -> Just CMLog

fromPSLog :: PSLog.PSLog -> Maybe CommonLog
fromPSLog = \case
  LogEntry -> Just CMLog
  PSLog.RunDBEntry -> Just CMRunDB
  RunKVDBEitherEntry -> Just CMKVDB
  DoAffEntry -> Just CMIO
  PSLog.SetOptionEntry -> Just CMSetOpt
  PSLog.GetOptionEntry -> Just CMGetOpt
  GenerateGUIDEntry -> Just CMGenGuid
  CallAPIEntry -> Just CMCallApi
  ForkFlowEntry -> Just CMFork
  PSLog.ThrowExceptionEntry -> Just CMException
  PSLog.RunSysCmdEntry -> Just CMSysCmd
  GetDBConnEntry -> Just CMGetDBConn
  GetKVDBConnEntry -> Just CMGetKVDBConn
  RunKVDBSimpleEntry -> Just CMKVDB
  UnexpectedRecordingEnd -> Nothing
  UnknownRRItem -> Nothing
  ItemMismatch -> Nothing
  ForkedFlowRecordingsMissed -> Nothing
  MockDecodingFailed -> Nothing
  UnknownPlaybackError -> Nothing
  Other -> Nothing

class HasCommonLog log where
  toCommonLog :: log -> Maybe CommonLog

instance HasCommonLog HSLog.HSLog where
  toCommonLog  = fromHSLog

instance HasCommonLog PSLog.PSLog where
  toCommonLog = fromPSLog
