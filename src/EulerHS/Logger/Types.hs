{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module EulerHS.Logger.Types
    (
    -- * Core Logger
    -- ** Types
      LogLevel(..)
    , BufferSize
    , MessageBuilder (..)
    , MessageFormatter
    , FlowFormatter
    , LoggerConfig(..)
    , Message(..)
    , Tag
    , PendingMsg(..)
    , ShouldLogSQL(..)
    , LogEntry (..)
    , Log
    , LogContext
    , LogCounter
    , LogMaskingConfig (..)
    , MaskKeyType (..)
    , ExceptionEntry (..)
    , VersionLoggerMessage(..)
    , Action
    , Category
    , Entity
    , Latency
    , RespCode
    , ErrorL(..)
    , ErrorInfo(..)
    -- ** defaults
    , defaultLoggerConfig
    , defaultMessageFormatter
    , showingMessageFormatter
    , defaultFlowFormatter
    , builderToByteString
    , getFlowGuuid
    , getLogLevel
    , getLogContext
    , getMessageNumber
    , convertToPendingMsg
    , getErrorLogFromException
    ) where

import qualified EulerHS.Common as T
import           EulerHS.Prelude
-- Currently, TinyLogger is highly coupled with the interface.
-- Reason: unclear current practice of logging that affects design and performance.
import qualified Data.Aeson as A
import qualified Data.Text.Lazy.Encoding as TE
import Data.Text.Lazy.Builder
import qualified Data.ByteString.Lazy as LBS
import Formatting.Buildable (Buildable(..))
import qualified System.Logger.Message as LogMsg
import qualified Control.Exception as Exception
import qualified EulerHS.SqlDB.Types as T
import qualified EulerHS.KVDB.Types as TK
import Data.Typeable (typeOf)
import Data.Data (toConstr, Data)

-- | Logging level.
data LogLevel = Debug | Info | Warning | Error
    deriving (Generic, Eq, Ord, Show, Read, Enum, ToJSON, FromJSON)

data LogMaskingConfig =
  LogMaskingConfig
    { _maskKeys :: HashSet Text -- Check : Better to make this case insensitive
    , _maskText :: Maybe Text
    , _keyType  :: MaskKeyType
    } deriving (Generic, Show, Read)

data MessageBuilder
  = SimpleString String
  | SimpleText Text
  | SimpleBS ByteString
  | SimpleLBS LBS.ByteString
  | MsgBuilder LogMsg.Builder
  | MsgTransformer (LogMsg.Msg -> LogMsg.Msg)

data MaskKeyType =
    WhiteListKey
  | BlackListKey
  deriving (Generic, Show, Read)

data ShouldLogSQL
  -- | Log SQL queries, including sensitive data and API keys. Do NOT PR code
  -- with this enabled, and make sure this doesn't make it into production
  = UnsafeLogSQL_DO_NOT_USE_IN_PRODUCTION
  -- | omit SQL logs
  | SafelyOmitSqlLogs
  deriving (Generic, Show, Read)

type LogCounter = IORef Int         -- No race condition: atomicModifyIORef' is used.

data Message = Message
  { msgMessage :: Maybe A.Value
  , msgValue :: Maybe A.Value
  }
  deriving (Show)

instance Buildable Message where
  build = fromText . decodeUtf8 . showMessage
    where
      showMessage msg = case (msgMessage msg, msgValue msg) of
        -- This is compatibility code, so we added values as an extesnsion
        -- and we can safely ignore it here if needed.
        (Just message, _) -> A.encode message
        (_, Just value) -> A.encode value
        (_, _) -> ""
  {-# INLINE build #-}

type Tag = Text
type MessageNumber = Int
type BufferSize = Int
type MessageFormatter = PendingMsg -> MessageBuilder
type FlowFormatter = Maybe T.FlowGUID -> IO MessageFormatter
type LogContext = HashMap Text Text

data LoggerConfig
  = LoggerConfig
    { _isAsync          :: Bool
    , _logLevel         :: LogLevel
    , _logFilePath      :: FilePath
    , _logToConsole     :: Bool
    , _logToFile        :: Bool
    , _maxQueueSize     :: Word
    , _logRawSql        :: ShouldLogSQL
    , _logAPI           :: Bool
    , _logMaskingConfig :: Maybe LogMaskingConfig
    } deriving (Generic, Show, Read)

data PendingMsg =
  V1 !(Maybe T.FlowGUID) !LogLevel !Tag !Message !MessageNumber !LogContext
  | V2 !(Maybe T.FlowGUID) !LogLevel !Category !(Maybe Action) !(Maybe Entity) !(Maybe ErrorL) !(Maybe Latency) !(Maybe RespCode) !Message !MessageNumber !LogContext
  deriving (Show)

type Category = Text
type Action = Text
type Entity = Text

data ErrorInfo = ErrorInfo
  {
    error_code            :: Text,
    error_message         :: Text,
    error_category        :: Text,
    unified_error_code    :: Text,
    unified_error_message :: Text
  }

  deriving (Show, Generic)

instance ToJSON ErrorInfo where
  toJSON = A.genericToJSON A.defaultOptions

data ErrorL  = ErrorL !(Maybe ErrCode) ErrCategory ErrReason -- kept as maybe till unifiction is done
    deriving Show

type ErrCode = Text
type ErrCategory = Text
type ErrReason = Text
type Latency = Integer
type RespCode = Int

data LogEntry = LogEntry !LogLevel !Message
type Log = [LogEntry]

data ExceptionEntry = ExceptionEntry
  { error_code    :: Text
  , error_message :: String
  , jp_error_code :: Text
  , source        :: Text
  } deriving (Generic, ToJSON)

defaultMessageFormatter :: MessageFormatter
defaultMessageFormatter (V1 _ lvl tag msg _ _) =
  SimpleString $ "[" +|| lvl ||+ "] <" +| tag |+ "> " +| TE.decodeUtf8 (A.encode (msgMessage msg)) |+ ""
defaultMessageFormatter (V2 _ lvl category action _ _ _ _ msg _ _) =
  SimpleString $ "[" +|| lvl ||+ "] <" +| category |+ "> <" +| action |+ "> " +| TE.decodeUtf8 (A.encode (msgMessage msg)) |+ ""

showingMessageFormatter :: MessageFormatter
showingMessageFormatter = SimpleString . show

defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig = LoggerConfig
    { _isAsync = False
    , _logLevel = Debug
    , _logFilePath = ""
    , _logToConsole = True
    , _logToFile = False
    , _maxQueueSize = 1000
    , _logRawSql = SafelyOmitSqlLogs
    , _logAPI = True
    , _logMaskingConfig = Nothing
    }

defaultFlowFormatter :: FlowFormatter
defaultFlowFormatter _ = pure defaultMessageFormatter

builderToByteString :: LogMsg.Builder -> LBS.ByteString
builderToByteString = LogMsg.eval


data VersionLoggerMessage = 
    Ver1 !Tag !Message
  | Ver2 !Category !(Maybe Action) !(Maybe Entity) !(Maybe ErrorL) !(Maybe Latency) !(Maybe RespCode) !Message

getFlowGuuid :: PendingMsg -> Maybe T.FlowGUID
getFlowGuuid (V1 mbFlowGuid _ _ _ _ _)           = mbFlowGuid
getFlowGuuid (V2 mbFlowGuid _ _ _ _ _ _ _ _ _ _) = mbFlowGuid

getLogLevel :: PendingMsg -> LogLevel
getLogLevel (V1 _ lvl _ _ _ _) = lvl
getLogLevel (V2 _ lvl _ _ _ _ _ _ _ _ _) = lvl

getLogContext :: PendingMsg -> LogContext
getLogContext (V1 _ _ _ _ _ lContext) = lContext
getLogContext (V2 _ _ _ _ _ _ _ _ _ _ lContext) = lContext

getMessageNumber :: PendingMsg -> MessageNumber
getMessageNumber (V1 _ _ _ _ msgNumber _) = msgNumber
getMessageNumber (V2 _ _ _ _ _ _ _ _ _ msgNumber _) = msgNumber

convertToPendingMsg :: Maybe T.FlowGUID -> LogLevel -> MessageNumber -> LogContext -> VersionLoggerMessage -> PendingMsg
convertToPendingMsg mbFlowGuid logLevel msgNum lContext (Ver1 tag msg) =
    V1 mbFlowGuid logLevel tag msg msgNum lContext
convertToPendingMsg mbFlowGuid logLevel msgNum lContext (Ver2 category action entity maybeEror maybeLatency maybeRespCode msg) =
    V2 mbFlowGuid logLevel category action entity maybeEror maybeLatency maybeRespCode msg msgNum lContext

deriving instance Data Exception.ArithException
deriving instance Data Exception.ArrayException
deriving instance Data Exception.AsyncException

getErrorLogFromException :: SomeException -> ExceptionEntry
getErrorLogFromException exception =
  fromMaybe (exceptionLogDefault exception)
        $ exceptionLogWithConstructor <$> (fromException exception :: Maybe Exception.ArithException)
        <|> exceptionLogWithConstructor <$> (fromException exception :: Maybe Exception.ArrayException)
        <|> exceptionLogDefault <$> (fromException exception :: Maybe Exception.AssertionFailed)
        <|> exceptionLogWithConstructor <$> (fromException exception :: Maybe Exception.AsyncException)
        <|> exceptionLogDefault <$> (fromException exception :: Maybe Exception.NonTermination)
        <|> exceptionLogDefault <$> (fromException exception :: Maybe Exception.NoMethodError)
        <|> exceptionLogDefault <$> (fromException exception :: Maybe Exception.NestedAtomically)
        <|> exceptionLogDefault <$> (fromException exception :: Maybe Exception.TypeError)
        <|> exceptionLogDefault <$> (fromException exception :: Maybe Exception.BlockedIndefinitelyOnMVar)
        <|> exceptionLogDefault <$> (fromException exception :: Maybe Exception.BlockedIndefinitelyOnSTM)
        <|> exceptionLogDefault <$> (fromException exception :: Maybe Exception.AllocationLimitExceeded)
        <|> exceptionLogDefault <$> (fromException exception :: Maybe Exception.Deadlock)
        <|> exceptionLogDefault <$> (fromException exception :: Maybe Exception.PatternMatchFail)
        <|> exceptionLogDefault <$> (fromException exception :: Maybe Exception.RecConError)
        <|> exceptionLogDefault <$> (fromException exception :: Maybe Exception.RecSelError)
        <|> exceptionLogDefault <$> (fromException exception :: Maybe Exception.RecUpdError)
        <|> exceptionLogDefault <$> (fromException exception :: Maybe Exception.ErrorCall)
        <|> exceptionLogWithConstructor <$> (fromException exception :: Maybe T.DBError)
        <|> exceptionLogWithConstructor <$> (fromException exception :: Maybe TK.MeshError)
  where
  exceptionLogWithConstructor ex = ExceptionEntry (show . toConstr $ ex) (displayException ex) (show $ typeOf ex) "Exception"
  exceptionLogDefault ex = ExceptionEntry (show $ typeOf ex) (displayException ex) (show $ typeOf ex) "Exception"