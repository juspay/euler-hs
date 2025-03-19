{-# OPTIONS_GHC -fclear-plugins #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE PackageImports  #-}

module EulerHS.Framework.Language
  (
  -- * Flow language
    Flow(..)
  , FlowMethod(..)
  , MonadFlow(..)
  , Loggable (..)
  , ReaderFlow
  , HttpManagerNotFound(..)
  -- ** Extra methods
  -- *** Logging
  , logCallStack
  , logExceptionCallStack
  , logInfo
  , logEncryptedInfo
  , logEncryptedError
  , logErrorWithCategoryV
  , logErrorWithCategory
  , logError
  , logDebug
  , logWarning
  , logM
  , log
  , logV
  , logInfoM
  , logInfoV
  , logErrorM
  , logErrorV
  , logDebugM
  , logDebugV
  , logWarningM
  , logWarningV
  , logException
  -- *** PublishSubscribe
  , unpackLanguagePubSub
  -- *** Working with external services
  , callAPI
  , callAPI'
  , callHTTP
  , callHTTP'
  -- *** Legacy
  , callHTTPWithCert
  , callHTTPWithManager
  , callHTTPWithCert' 
  , callHTTPWithManager'
  -- *** Others
  , runIO
  , generateGUIDWithTag
  , withRunFlow
  , forkFlow
  , forkFlow'
  , forkFlow'' 
  -- ** Interpretation
  , foldFlow
  , getMySQLConnection
  -- ** DBAndRedisMetric
  , incrementDbAndRedisMetric
  , DBAndRedisMetricHandler
  , DBAndRedisMetric (..)
  , mkDBAndRedisMetricHandler
  , isDBMetricEnabled
  , DBMetricCfg (..)
  , TenantConfigObj(..)
  , TenantConfig(..)
  , MigrationConfig(..)
  , TenantDBMigrationMode(..)
  , CachePrefixMigrationMode(..)
  , PrefixMigrationMode(..)
  , incrementDbMetric
  , RunDBInfo(..)
  , callHttpWithDigest
  ) where

import           Control.Monad.Catch (ExitCase, MonadCatch (catch),
                                      MonadThrow (throwM))
import           Control.Monad.Free.Church (MonadFree)
import           Control.Monad.Trans.Except (withExceptT)
import           Control.Monad.Trans.RWS.Strict (RWST)
import           Control.Monad.Trans.Writer (WriterT)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe (fromJust)
import qualified Data.Text as Text
import           Network.HTTP.Client (Manager)
import           Servant.Client (BaseUrl, ClientError (ConnectionError))

import           EulerHS.Api (EulerClient)
import           EulerHS.Common (Awaitable, Description, ForkGUID,
                                 ManagerSelector (ManagerSelector),
                                 Microseconds, SafeFlowGUID)
import           EulerHS.Extra.Snowflakes.Types (Snowflake, SnowflakeError)
import           EulerHS.Framework.Runtime (FlowRuntime, ConfigEntry)
import           EulerHS.HttpAPI (HTTPCert, HTTPClientSettings, HTTPRequest,
                                  HTTPResponse, withClientTls, HttpManagerNotFound(..), AwaitingError, MaskReqRespBody)
import           EulerHS.KVDB.Language (KVDB)
import           EulerHS.KVDB.Types (KVDBAnswer, KVDBConfig, KVDBConn,
                                     KVDBReply)
import qualified EulerHS.KVDB.Types as T
import EulerHS.KVConnector.InMemConfig.Types 
import           EulerHS.Logger.Language (Logger, masterLogger)
import           EulerHS.Logger.Types (LogLevel (Debug, Error, Info, Warning),
                                       Message (Message), ExceptionEntry(..), ErrorL(..), 
                                       LogEncryptionKey(..), LogEncryptionIV(..), getErrorLogFromException)
import           EulerHS.Options (OptionEntity, OptionKey, mkOptionKey)
import           EulerHS.ART.Types (RecordingEntry(..))
import           EulerHS.ART.V2.Types (ArtRecordable)
import qualified EulerHS.Encryption as EE
import           EulerHS.Prelude hiding (throwM)
import qualified EulerHS.PubSub.Language as PSL
import           EulerHS.SqlDB.Language (SqlDB)
import           EulerHS.SqlDB.Types (BeamRunner, BeamRuntime, DBConfig,
                                      DBResult, SqlConn)
import qualified EulerHS.SqlDB.Types as T
import           Euler.Events.MetricApi.MetricApi
import qualified Juspay.Extra.Env as Env
import qualified Data.Pool as DP
import qualified Database.MySQL.Base as MySQL
import qualified EulerHS.Logger.Types as Log
import Streamly.Data.Array (Array)
import qualified Streamly.Data.MutByteArray as MBA
import           Type.Reflection
import qualified Data.Time as DT
import qualified Data.Time.Clock.POSIX as DT
import qualified System.Random as SR
import qualified Data.UUID as UUID
import           Data.ByteArray (ByteArray)

data DBAndRedisMetric
  = ConnectionLost
  | ConnectionFailed
  | ConnectionDoesNotExist
  | ConnectionAlreadyExists
  | TransactionRollbacked
--   | SQLQueryError
  | UnrecognizedDBError
  | UnexpectedDBResult
  | RedisExceptionMessage

data DBAndRedisMetricHandler = DBAndRedisMetricHandler
  { dBAndRedisCounter :: (DBAndRedisMetric, Text, Text) -> IO ()
  }
  deriving (ArtRecordable)

---------------------------------------------------------

data DBMetricCfg = DBMetricCfg
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

$(MBA.deriveSerialize [d|instance MBA.Serialize DBMetricCfg|])
instance OptionEntity DBMetricCfg DBAndRedisMetricHandler

---------------------------------------------------------

data TenantDBMigrationMode = NEW_DB_DISABLED | DB_DUAL_WRITE | NEW_DB_READ_ENABLED | OLD_DB_DISABLED
  deriving stock (Generic, Eq, Ord, Show, Read)
  deriving anyclass (ToJSON, FromJSON)

data MigrationConfig = MigrationConfig {
    databaseMigrationMode :: Maybe TenantDBMigrationMode
  }
  deriving stock (Generic, Eq, Ord, Show, Read)
  deriving anyclass (ToJSON, FromJSON)


data TenantConfig = TenantConfig
    { tenantAccountId :: Text
    , tenantIdV2 :: Text
    , tenantName :: Text
    , databasePrefix :: Maybe Text
    , cacheKeyPrefix :: Maybe Text
    , tenantHost :: Maybe Text
    , apiPathPrefix :: Maybe Text
    , migrationConfig :: Maybe MigrationConfig
}
  deriving stock (Generic, Eq, Ord, Show, Read)
  deriving anyclass (ToJSON, FromJSON)

data TenantConfigObj = TenantConfigObj
  deriving stock (Generic, Eq, Ord, Show, Read)
  deriving anyclass (ToJSON, FromJSON)

$(MBA.deriveSerialize [d|instance MBA.Serialize TenantConfigObj|])
instance OptionEntity TenantConfigObj TenantConfig


data PrefixMigrationMode = NEW_WRITE_DISABLED | DUAL_WRITE | NEW_READ_ENABLED | OLD_WRITE_DISABLED | OLD_WRITE_ENABLED
  deriving stock (Generic, Eq, Ord, Show, Read)
  deriving anyclass (ToJSON, FromJSON)

data CachePrefixMigrationMode = CachePrefixMigrationMode
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (ToJSON, FromJSON) 

$(MBA.deriveSerialize [d|instance MBA.Serialize CachePrefixMigrationMode|])
instance OptionEntity CachePrefixMigrationMode PrefixMigrationMode

---------------------------------------------------------

isDBMetricEnabled :: Bool
isDBMetricEnabled =
  let envType =  Env.JuspayEnv
                    { key = "DB_METRIC_ENABLED"
                    , actionLeft = Env.mkDefaultEnvAction (False :: Bool)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
  in Env.lookupEnv envType

--------------------------------------------------------------------------------TRANSACTION BLOCK------------------------------------------------------------------------------------------------------

data RunDBInfo beM a = TransactionFlow (T.NativeSqlConn -> Flow a) | SqlDBFlow (SqlDB beM a) Bool

type W8Arr = Array Word8

-- | Flow language.
data FlowMethod (next :: Type) where

  LookupHTTPManager
    :: HasCallStack
    => (Maybe ManagerSelector)
    -> (Maybe Manager -> next)
    -> FlowMethod next

  GetHTTPManager
    :: HasCallStack
    => HTTPClientSettings
    -> (Manager -> next)
    -> FlowMethod next

  CallServantAPI
    :: (Show apiTag, HasCallStack)
    => Manager
    -> BaseUrl
    -> apiTag
    -> (LBS.ByteString -> Maybe Log.ErrorInfo)
    -> EulerClient a
    -> (Either ClientError a -> next)
    -> FlowMethod next

  CallHTTP
      :: (Show apiTag, HasCallStack)
      => HTTPRequest
      -> apiTag
      -> (HTTPResponse -> Maybe Log.ErrorInfo)
      -> Manager
      -> Maybe MaskReqRespBody
      -> Maybe (Text, Text)
      -> (Either Text HTTPResponse -> next)
      -> FlowMethod next

  EvalLogger
    :: HasCallStack
    => Logger a
    -> (a -> next)
    -> FlowMethod next

  RunIO
    :: HasCallStack
    => Text
    -> IO a
    -> (a -> next)
    -> FlowMethod next

  WithRunFlow
    :: HasCallStack
    => ((forall x. Flow x -> IO x) -> IO next)
    -> FlowMethod next

  GetOption
    :: (HasCallStack, ArtRecordable a)
    => OptionKey
    -> (Maybe a -> next)
    -> FlowMethod next

  SetOption
    :: (HasCallStack, ArtRecordable a)
    => OptionKey
    -> a
    -> (() -> next)
    -> FlowMethod next

  SetLoggerContext
    :: HasCallStack
     => Text
    -> Text
    -> (() -> next)
    -> FlowMethod next

  IncrementArtCounter
    :: HasCallStack
    => ((Int) -> next)
    -> FlowMethod next
  
  GetLoggerContext
    :: HasCallStack
     => Text
    -> ((Maybe Text) -> next)
    -> FlowMethod next

  SetLoggerContextMap
    :: HasCallStack
    => HashMap Text Text
    -> (() -> next)
    -> FlowMethod next

  GetLoggerContextMap
    :: HasCallStack
    => ((HashMap Text Text) -> next)
    -> FlowMethod next
  
  ModifyOption
    :: HasCallStack
    => OptionKey
    -> ( a -> a )
    -> ((Maybe a, Maybe a) -> next)
    -> FlowMethod next

  DelOption
    :: HasCallStack
    => OptionKey
    -> (() -> next)
    -> FlowMethod next

  GetOptionLocal
    :: HasCallStack
    => OptionKey
    -> (Maybe a -> next)
    -> FlowMethod next

  SetOptionLocal
    :: HasCallStack
    => OptionKey
    -> a
    -> (() -> next)
    -> FlowMethod next

  ModifyOptionLocal
    :: HasCallStack
    => OptionKey
    -> ( a -> a )
    -> ((Maybe a, Maybe a) -> next)
    -> FlowMethod next

  DelOptionLocal
    :: HasCallStack
    => OptionKey
    -> (() -> next)
    -> FlowMethod next
  
  GetFlowRuntime
    :: HasCallStack
    => (FlowRuntime -> next)
    -> FlowMethod next

  GetRecordingLocal
    :: HasCallStack
    => ([RecordingEntry] -> next)
    -> FlowMethod next

  AppendRecordingLocal
    :: HasCallStack
    => RecordingEntry
    -> (() -> next)
    -> FlowMethod next

  DelRecordingLocal
    :: HasCallStack
    => (() -> next)
    -> FlowMethod next

  GetConfig
    :: HasCallStack
    => W8Arr
    -> QueryType
    -> (Maybe ConfigEntry -> next)
    -> FlowMethod next

  SetConfig
    :: HasCallStack
    => W8Arr
    -> ConfigEntry
    -> QueryType
    -> (() -> next)
    -> FlowMethod next

  ModifyConfig
    :: HasCallStack
    => W8Arr
    -> (Maybe ConfigEntry -> ConfigEntry)
    -> QueryType
    -> (() -> next)
    -> FlowMethod next

  TrySetConfig
    :: HasCallStack
    => W8Arr
    -> ConfigEntry
    -> QueryType
    -> (Maybe () -> next)
    -> FlowMethod next

  DelConfig
    :: HasCallStack
    => W8Arr
    -> QueryType
    -> (() -> next)
    -> FlowMethod next

  AcquireConfigLock
    :: HasCallStack
    => W8Arr
    -> (Bool -> next)
    -> FlowMethod next
  
  ReleaseConfigLock
    :: HasCallStack
    => W8Arr
    -> (Bool -> next)
    -> FlowMethod next

  GenerateGUID
    ::  HasCallStack
    => Text
    -> (UUID.UUID -> next)
    -> FlowMethod next
  
  GetCurrentTime
    :: HasCallStack
    => (DT.UTCTime -> next)
    -> FlowMethod next
  
  GetPOSIXTime
    :: HasCallStack
    => (DT.POSIXTime -> next)
    -> FlowMethod next

  RandomRIO
    :: (ToJSON a, FromJSON a, SR.Random a, HasCallStack)
    => Text
    -> (a,a)
    -> (a -> next)
    -> FlowMethod next

  GetRandomBytes
    :: (ArtRecordable byteArray, ByteArray byteArray, HasCallStack)
    => Text
    -> Int
    -> (byteArray -> next)
    -> FlowMethod next

  RunSysCmd
    :: HasCallStack
    => String
    -> (String -> next)
    -> FlowMethod next

  Fork
    :: HasCallStack
    => Description
    -> ForkGUID
    -> Flow a
    -> (Awaitable (Either Text a) -> next)
    -> FlowMethod next

  Await
    :: HasCallStack
    => Maybe Microseconds
    -> Awaitable (Either Text a)
    -> (Either AwaitingError a -> next)
    -> FlowMethod next

  ThrowException
    :: forall a e next
     . (HasCallStack, Exception e)
    => e
    -> (a -> next)
    -> FlowMethod next

  CatchException
    :: forall a e next
     . (HasCallStack, Exception e)
    => Flow a
    -> (e -> Flow a)
    -> (a -> next)
    -> FlowMethod next

  Mask
    :: forall b next
     . HasCallStack
    => ((forall a . Flow a -> Flow a) -> Flow b)
    -> (b -> next)
    -> FlowMethod next

  UninterruptibleMask
    :: forall b next
     . HasCallStack
    => ((forall a . Flow a -> Flow a) -> Flow b)
    -> (b -> next)
    -> FlowMethod next

  GeneralBracket
    :: forall a b c next
     . HasCallStack
    => Flow a
    -> (a -> ExitCase b -> Flow c)
    -> (a -> Flow b)
    -> ((b, c) -> next)
    -> FlowMethod next

  -- This is technically redundant - we can implement this using something like
  -- bracket, but better. - Koz
  RunSafeFlow
    :: HasCallStack
    => SafeFlowGUID
    -> Flow a
    -> (Either Text a -> next)
    -> FlowMethod next

  InitSqlDBConnection
    :: HasCallStack
    => DBConfig beM
    -> (DBResult (SqlConn beM) -> next)
    -> FlowMethod next

  DeInitSqlDBConnection
    :: HasCallStack
    => SqlConn beM
    -> (() -> next)
    -> FlowMethod next

  GetSqlDBConnection
    :: HasCallStack
    => DBConfig beM
    -> (DBResult (SqlConn beM) -> next)
    -> FlowMethod next

  InitKVDBConnection
    :: HasCallStack
    => KVDBConfig
    -> (KVDBAnswer KVDBConn -> next)
    -> FlowMethod next

  DeInitKVDBConnection
    :: HasCallStack
    => KVDBConn
    -> (() -> next)
    -> FlowMethod next

  GetKVDBConnection
    :: HasCallStack
    => KVDBConfig
    -> (KVDBAnswer KVDBConn -> next)
    -> FlowMethod next

  RunDB
    :: HasCallStack
    => SqlConn beM
    -> RunDBInfo beM a
    -> (DBResult a -> next)
    -> FlowMethod next

  RunDBWithRetry
    :: HasCallStack
    => SqlConn beM
    -> RunDBInfo beM a
    -> (DBResult a -> next)
    -> FlowMethod next

  RunKVDB
    :: HasCallStack
    => Text
    -> KVDB a
    -> (KVDBAnswer a -> next)
    -> FlowMethod next

  RunPubSub
    :: HasCallStack
    => PubSub a
    -> (a -> next)
    -> FlowMethod next

  WithModifiedRuntime
    :: HasCallStack
    => (FlowRuntime -> FlowRuntime)
    -> Flow a
    -> (a -> next)
    -> FlowMethod next
  
  GetSnowflakeID
    :: HasCallStack
    => Word8
    -> Word16
    -> String
    -> (Either SnowflakeError Snowflake -> next)
    -> FlowMethod next
  RunDBWithConn
    :: HasCallStack
    => T.NativeSqlConn
    -> SqlDB beM a
    -> (DBResult a -> next)
    -> FlowMethod next

-- Needed due to lack of impredicative instantiation (for stuff like Mask). -
-- Koz
instance Functor FlowMethod where
  {-# INLINEABLE fmap #-}
  fmap f = \case
    LookupHTTPManager mSel cont -> LookupHTTPManager mSel (f . cont)
    CallServantAPI mgr url apiTag errFunc client cont -> CallServantAPI mgr url apiTag errFunc client (f . cont)
    GetHTTPManager settings cont -> GetHTTPManager settings (f . cont)
    CallHTTP req mgr apiTag errFunc mskReqRespBody mbDigestDetails cont -> CallHTTP req mgr apiTag errFunc mskReqRespBody mbDigestDetails (f . cont)
    EvalLogger logger cont -> EvalLogger logger (f . cont)
    RunIO t act cont -> RunIO t act (f . cont)
    WithRunFlow ioAct -> WithRunFlow (\runFlow -> f <$> ioAct runFlow)
    GetOption k cont -> GetOption k (f . cont)
    SetOption k v cont -> SetOption k v (f . cont)
    SetLoggerContext k v cont -> SetLoggerContext k v (f . cont)
    IncrementArtCounter cont -> IncrementArtCounter (f . cont)
    GetLoggerContext k cont -> GetLoggerContext k (f . cont)
    SetLoggerContextMap v cont -> SetLoggerContextMap v (f . cont)
    GetLoggerContextMap cont -> GetLoggerContextMap (f . cont)
    ModifyOption k fn cont -> ModifyOption k fn (f . cont)
    DelOption k cont -> DelOption k (f . cont)
    GetOptionLocal k cont -> GetOptionLocal k (f . cont)
    SetOptionLocal k v cont -> SetOptionLocal k v (f . cont)
    ModifyOptionLocal k fn cont -> ModifyOptionLocal k fn (f . cont)
    DelOptionLocal k cont -> DelOptionLocal k (f . cont)
    GetFlowRuntime cont -> GetFlowRuntime (f . cont)
    GetRecordingLocal cont -> GetRecordingLocal (f . cont)
    AppendRecordingLocal v cont -> AppendRecordingLocal v (f . cont)
    DelRecordingLocal cont -> DelRecordingLocal (f . cont)
    GetConfig k queryType cont -> GetConfig k queryType (f . cont)
    SetConfig k v queryType cont -> SetConfig k v queryType (f . cont)
    ModifyConfig k modification queryType cont -> ModifyConfig k modification queryType (f . cont)
    TrySetConfig k v queryType cont -> TrySetConfig k v queryType (f . cont)
    DelConfig k queryType cont -> DelConfig k queryType (f . cont)
    AcquireConfigLock k cont -> AcquireConfigLock k (f . cont)
    ReleaseConfigLock k cont -> ReleaseConfigLock k (f . cont)
    GenerateGUID tag cont -> GenerateGUID tag (f . cont)
    GetCurrentTime cont -> GetCurrentTime (f . cont)
    GetPOSIXTime cont -> GetPOSIXTime (f . cont)
    RandomRIO k p cont -> RandomRIO k p (f . cont)
    GetRandomBytes k p cont -> GetRandomBytes k p (f . cont)
    RunSysCmd cmd cont -> RunSysCmd cmd (f . cont)
    Fork desc guid flow cont -> Fork desc guid flow (f . cont)
    Await time awaitable cont -> Await time awaitable (f . cont)
    ThrowException e cont -> ThrowException e (f . cont)
    CatchException flow handler cont -> CatchException flow handler (f . cont)
    Mask cb cont -> Mask cb (f . cont)
    UninterruptibleMask cb cont -> UninterruptibleMask cb (f . cont)
    GeneralBracket acquire release act cont ->
      GeneralBracket acquire release act (f . cont)
    RunSafeFlow guid flow cont -> RunSafeFlow guid flow (f . cont)
    InitSqlDBConnection conf cont -> InitSqlDBConnection conf (f . cont)
    DeInitSqlDBConnection conn cont -> DeInitSqlDBConnection conn (f . cont)
    GetSqlDBConnection conf cont -> GetSqlDBConnection conf (f . cont)
    InitKVDBConnection conf cont -> InitKVDBConnection conf (f . cont)
    DeInitKVDBConnection conn cont -> DeInitKVDBConnection conn (f . cont)
    GetKVDBConnection conf cont -> GetKVDBConnection conf (f . cont)
    RunDB conn dbI cont -> RunDB conn dbI (f . cont)
    RunDBWithRetry conn dbI cont -> RunDBWithRetry conn dbI (f . cont)
    RunKVDB t db cont -> RunKVDB t db (f . cont)
    RunPubSub pubSub cont -> RunPubSub pubSub (f . cont)
    WithModifiedRuntime g innerFlow cont ->
      WithModifiedRuntime g innerFlow (f . cont)
    GetSnowflakeID sId pId k cont -> GetSnowflakeID sId pId k (f . cont)
    RunDBWithConn conn db cont -> RunDBWithConn conn db (f . cont)


newtype Flow (a :: Type) = Flow (F FlowMethod a)
  deriving newtype (Functor, Applicative, Monad, MonadFree FlowMethod)
  deriving anyclass (ArtRecordable)


instance MonadThrow Flow where
  {-# INLINEABLE throwM #-}
  throwM e = liftFC . ThrowException e $ id

instance MonadCatch Flow where
  {-# INLINEABLE catch #-}
  catch comp handler = liftFC . CatchException comp handler $ id

instance MonadMask Flow where
  {-# INLINEABLE mask #-}
  mask cb = liftFC . Mask cb $ id
  {-# INLINEABLE uninterruptibleMask #-}
  uninterruptibleMask cb = liftFC . UninterruptibleMask cb $ id
  {-# INLINEABLE generalBracket #-}
  generalBracket acquire release act =
    liftFC . GeneralBracket acquire release act $ id

---------------------------------------------------------

incrementRedisMetric :: (HasCallStack, MonadFlow m) => T.KVDBReply -> Text -> m ()
incrementRedisMetric err cName = when (isDBMetricEnabled) $
  case err of
    T.KVDBError T.KVDBConnectionFailed _ -> incrementMetric ConnectionFailed cName
    T.KVDBError T.KVDBConnectionAlreadyExists _ -> incrementMetric ConnectionAlreadyExists cName
    T.KVDBError T.KVDBConnectionDoesNotExist _ -> incrementMetric ConnectionDoesNotExist cName
    T.ExceptionMessage _ ->
      if Text.isInfixOf "Network.Socket.connect" $ show err
        then incrementMetric ConnectionLost cName
        else incrementMetric RedisExceptionMessage cName
    _ -> pure ()

incrementDbMetric :: (HasCallStack, MonadFlow m) => T.DBError -> T.DBConfig beM -> m ()
incrementDbMetric (T.DBError err msg) dbConf = when isDBMetricEnabled $
  case err of
    T.ConnectionFailed -> incrementMetric ConnectionFailed (dbConfigToTag dbConf)
    T.ConnectionAlreadyExists -> incrementMetric ConnectionAlreadyExists (dbConfigToTag dbConf)
    T.ConnectionDoesNotExist -> incrementMetric ConnectionDoesNotExist (dbConfigToTag dbConf)
    T.TransactionRollbacked -> incrementMetric TransactionRollbacked (dbConfigToTag dbConf)
    T.UnexpectedResult -> incrementMetric UnexpectedDBResult (dbConfigToTag dbConf)
    T.UnrecognizedError -> if Text.isInfixOf "Network.Socket.connect" $ show msg
      then incrementMetric ConnectionLost (dbConfigToTag dbConf)
      else incrementMetric UnrecognizedDBError (dbConfigToTag dbConf)
    _ -> pure ()

incrementMetric :: (HasCallStack, MonadFlow m) => DBAndRedisMetric -> Text -> m ()
incrementMetric metric dbName = do
  env <- getOption DBMetricCfg
  case env of
    Just val -> incrementDbAndRedisMetric val metric dbName (Env.lookupEnv envType)
    Nothing -> pure ()
  where
    envType =  Env.JuspayEnv
                    { key = "HOSTNAME"
                    , actionLeft = Env.mkDefaultEnvAction ("" :: Text)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }

dbConfigToTag :: T.DBConfig beM -> Text
dbConfigToTag = \case
  T.PostgresPoolConf t _ _ -> t
  T.MySQLPoolConf t _ _    -> t
  T.SQLitePoolConf t _ _   -> t
  T.MockingConf t _ _      -> t

-- | MonadFlow implementation for the `Flow` Monad. This allows implementation of MonadFlow for
-- `ReaderT` and other monad transformers.
--
-- Omit `forkFlow` as this will break some monads like StateT (you can lift this manually if you
-- know what you're doing)
class (MonadMask m) => MonadFlow m where
  -- | Method for calling external HTTP APIs using the facilities of servant-client.
  -- Allows to specify what manager should be used. If no manager found,
  -- `HttpManagerNotFound` will be returne (as part of `ClientError.ConnectionError`).
  --
  -- Thread safe, exception free.
  --
  -- Takes remote url, servant client for this endpoint
  -- and returns either client error or result.
  --
  -- > data User = User { firstName :: String, lastName :: String , userGUID :: String}
  -- >   deriving (Generic, Show, Eq, ToJSON, FromJSON )
  -- >
  -- > data Book = Book { author :: String, name :: String }
  -- >   deriving (Generic, Show, Eq, ToJSON, FromJSON )
  -- >
  -- > type API = "user" :> Get '[JSON] User
  -- >       :<|> "book" :> Get '[JSON] Book
  -- >
  -- > api :: HasCallStack => Proxy API
  -- > api = Proxy
  -- >
  -- > getUser :: HasCallStack => EulerClient User
  -- > getBook :: HasCallStack => EulerClient Book
  -- > (getUser :<|> getBook) = client api
  -- >
  -- > url = BaseUrl Http "localhost" port ""
  -- >
  -- >
  -- > myFlow = do
  -- >   book <- callServantAPI url getBook
  -- >   user <- callServantAPI url getUser
  callServantAPI
    :: (HasCallStack, Show apiTag)
    => Maybe ManagerSelector     -- ^ name of the connection manager to be used
    -> BaseUrl                   -- ^ remote url 'BaseUrl'
    -> apiTag
    -> (LBS.ByteString -> Maybe Log.ErrorInfo)
    -> EulerClient a             -- ^ servant client 'EulerClient'
    -> m (Either ClientError a)  -- ^ result

  callAPIUsingManager
    :: (HasCallStack, Show apiTag)
    => Manager
    -> BaseUrl
    -> apiTag
    -> (LBS.ByteString  -> Maybe Log.ErrorInfo)
    -> EulerClient a
    -> m (Either ClientError a)

  lookupHTTPManager
    :: (HasCallStack, MonadFlow m)
    => Maybe ManagerSelector
    -> m (Maybe Manager)

  getHTTPManager
    :: HasCallStack
    => HTTPClientSettings
    -> m Manager

  -- | Method for calling external HTTP APIs without bothering with types.
  --
  -- Thread safe, exception free.
  callHTTPUsingManager
    :: (HasCallStack, Show apiTag)
    => Manager
    -> HTTPRequest
    -> apiTag
    -> (HTTPResponse -> Maybe Log.ErrorInfo)
    -> Maybe MaskReqRespBody
    -> Maybe (Text, Text)
    -> m (Either Text.Text HTTPResponse)

  -- | Evaluates a logging action.
  evalLogger' :: HasCallStack => Logger a -> m a

  -- | The same as runIO, but accepts a description which will be written into the ART recordings
  -- for better clarity.
  --
  -- Warning. This method is dangerous and should be used wisely.
  --
  -- > myFlow = do
  -- >   content <- runIO' "reading from file" $ readFromFile file
  -- >   logDebugT "content id" $ extractContentId content
  -- >   pure content
  runIO' :: HasCallStack => Text -> IO a -> m a

  -- | Gets stored a typed option by a typed key.
  --
  -- Thread safe, exception free.
  getOption :: forall k v. (HasCallStack, OptionEntity k v) => k -> m (Maybe v)

  -- Sets a typed option using a typed key (a mutable destructive operation)
  --
  -- Be aware that it's possible to overflow the runtime with options
  -- created uncontrollably.
  --
  -- Also please keep in mind the options are runtime-bound and if you have
  -- several API methods working with the same option key, you'll get a race.
  --
  -- Thread safe, exception free.
  --
  -- >  data MerchantIdKey = MerchantIdKey
  -- >
  -- >  instance OptionEntity MerchantIdKey Text
  -- >
  -- >  myFlow = do
  -- >    _ <- setOption MerchantIdKey "abc1234567"
  -- >    mKey <- getOption MerchantIdKey
  -- >    runIO $ putTextLn mKey
  -- >    delOption MerchantIdKey
  setOption :: forall k v. (HasCallStack, OptionEntity k v) => k -> v -> m ()

  setLoggerContext :: (HasCallStack) => Text -> Text -> m ()

  getLoggerContext :: (HasCallStack) => Text -> m (Maybe Text)

  incrementArtCounter :: (HasCallStack) => m Int

  setLoggerContextMap :: (HasCallStack) => HashMap Text Text -> m ()

  getLoggerContextMap :: (HasCallStack) => m (HashMap Text Text)

  -- > Problem Statement :
  -- > _ <- getOption k 
  -- > ---- <some calculation> ------- 
  -- > ----------------------------------------
  -- > -----<This region is not thread safe>--------
  -- > ----------------------------------------
  -- > _ <- setOption k <result of calculations>
  -- >  
  -- > Since the above block is not thread safe to modify an option
  -- > 
  -- > USE MODIFYOPTION :-)
  -- > It takes a key and function and applies the function if it finds a val with key
  -- >
  -- >
  -- > Sample usage:
  -- > (oldCount,modifiedCount) <- modifyOption MyCounter (\x -> x + 1)

  modifyOption :: forall k v. (HasCallStack, OptionEntity k v) => k -> (v -> v) -> m (Maybe v,Maybe v)

  -- | Deletes a typed option using a typed key.
  delOption :: forall k v. (HasCallStack, OptionEntity k v) => k -> m ()

  getOptionLocal :: forall k v. (HasCallStack, OptionEntity k v) => k -> m (Maybe v)

  modifyOptionLocal :: forall k v. (HasCallStack, OptionEntity k v) => k -> (v -> v) -> m (Maybe v, Maybe v)

  setOptionLocal :: forall k v. (HasCallStack, OptionEntity k v) => k -> v -> m ()

  delOptionLocal :: forall k v. (HasCallStack, OptionEntity k v) => k -> m ()

  getFlowRuntime :: (HasCallStack) => m FlowRuntime

  getRecordingLocal :: (HasCallStack) => m ([RecordingEntry])

  appendRecordingLocal :: (HasCallStack) => RecordingEntry -> m ()

  delRecordingLocal :: (HasCallStack) => m ()

  getConfig :: HasCallStack => W8Arr -> QueryType -> m (Maybe ConfigEntry)

  setConfig :: HasCallStack => W8Arr -> ConfigEntry -> QueryType -> m ()

  modifyConfig :: HasCallStack => W8Arr -> (Maybe ConfigEntry -> ConfigEntry) -> QueryType -> m ()

  trySetConfig :: HasCallStack => W8Arr -> ConfigEntry -> QueryType -> m (Maybe ())

  delConfig :: HasCallStack => W8Arr -> QueryType -> m ()

  acquireConfigLock :: HasCallStack => W8Arr -> m Bool

  releaseConfigLock :: HasCallStack => W8Arr -> m Bool
  -- | Generate a version 4 UUIDs as specified in RFC 4122
  -- e.g. 25A8FC2A-98F2-4B86-98F6-84324AF28611.
  --
  -- Thread safe, exception free.
  generateGUID :: HasCallStack => m Text

  generateRawGUID :: HasCallStack => Text -> m UUID.UUID

  getCurrentTime :: HasCallStack => m DT.UTCTime

  getPOSIXTime :: HasCallStack => m DT.POSIXTime

  randomRIO :: (HasCallStack, ToJSON a, FromJSON a, SR.Random a) => Text -> (a,a) -> m a

  getRandomBytes :: (ArtRecordable byteArray, ByteArray byteArray, HasCallStack) => Text -> Int -> m byteArray

  -- | Runs system command and returns its output.
  --
  -- Warning. This method is dangerous and should be used wisely.
  --
  -- > myFlow = do
  -- >   currentDir <- runSysCmd "pwd"
  -- >   logInfoT "currentDir" $ toText currentDir
  -- >   ...
  runSysCmd :: HasCallStack => String -> m String

  -- | Inits an SQL connection using a config.
  --
  -- Returns an error (Left $ T.DBError T.ConnectionAlreadyExists msg)
  -- if the connection already exists for this config.
  --
  -- Thread safe, exception free.
  initSqlDBConnection :: HasCallStack => DBConfig beM -> m (DBResult (SqlConn beM))

  -- | Deinits an SQL connection.
  -- Does nothing if the connection is not found (might have been closed earlier).
  --
  -- Thread safe, exception free.
  deinitSqlDBConnection :: HasCallStack => SqlConn beM -> m ()

  -- | Gets the existing connection.
  --
  -- Returns an error (Left $ T.DBError T.ConnectionDoesNotExist)
  -- if the connection does not exist.
  --
  -- Thread safe, exception free.
  getSqlDBConnection :: HasCallStack => DBConfig beM -> m (DBResult (SqlConn beM))

  -- | Inits a KV DB connection using a config.
  --
  -- Returns an error (Left $ KVDBError KVDBConnectionAlreadyExists msg)
  -- if the connection already exists.
  --
  -- Thread safe, exception free.
  initKVDBConnection :: HasCallStack => KVDBConfig -> m (KVDBAnswer KVDBConn)

  -- | Deinits the given KV DB connection.
  -- Does nothing if the connection is not found (might have been closed earlier).
  --
  -- Thread safe, exception free.
  deinitKVDBConnection :: HasCallStack => KVDBConn -> m ()

  -- | Get the existing connection.

  -- Returns an error (Left $ KVDBError KVDBConnectionDoesNotExist)
  -- if the connection does not exits for this config.
  --
  -- Thread safe, exception free.
  getKVDBConnection :: HasCallStack => KVDBConfig -> m (KVDBAnswer KVDBConn)

  -- | Evaluates SQL DB operations outside of any transaction.
  -- It's possible to have a chain of SQL DB calls (within the SqlDB language).
  -- These chains will be executed as a single transaction.
  --
  -- Thread safe, exception free.
  --
  -- The underlying library is beam which allows to access 3 different SQL backends.
  -- See TUTORIAL.md, README.md and QueryExamplesSpec.hs for more info.
  --
  -- > myFlow :: HasCallStack => L.Flow (T.DBResult (Maybe User))
  -- > myFlow = do
  -- >   connection <- L.initSqlDBConnection postgresCfg
  -- >
  -- >   res <- L.runDB connection $ do
  -- >     let predicate1 User {..} = _userFirstName ==. B.val_ "John"
  -- >
  -- >     L.updateRows $ B.update (_users eulerDb)
  -- >       (\User {..} -> mconcat
  -- >         [ _userFirstName <-. B.val_ "Leo"
  -- >         , _userLastName  <-. B.val_ "San"
  -- >         ]
  -- >       )
  -- >       predicate1
  -- >
  -- >     let predicate2 User {..} = _userFirstName ==. B.val_ "Leo"
  -- >     L.findRow
  -- >       $ B.select
  -- >       $ B.limit_ 1
  -- >       $ B.filter_ predicate2
  -- >       $ B.all_ (_users eulerDb)
  -- >
  -- >   L.deinitSqlDBConnection connection
  -- >   pure res
  runDB
    ::
      ( HasCallStack
      , BeamRunner beM
      , BeamRuntime be beM
      )
    => SqlConn beM
    -> SqlDB beM a
    -> m (DBResult a)

  runDBWithRetry
    ::
      ( HasCallStack
      , BeamRunner beM
      , BeamRuntime be beM
      )
    => SqlConn beM
    -> SqlDB beM a
    -> m (DBResult a)

  -- | Like `runDB` but runs inside a SQL transaction.
  runTransaction
    ::
      ( HasCallStack
      , BeamRunner beM
      , BeamRuntime be beM
      )
    => SqlConn beM
    -> SqlDB beM a
    -> m (DBResult a)

  -- | Await for some a result from the flow.
  -- If the timeout is Nothing than the operation is blocking.
  -- If the timeout is set then the internal mechanism tries to do several (10) checks for the result.
  -- Can return earlier if the result became available.
  -- Returns either an Awaiting error or a result.
  --
  -- Warning. There are no guarantees of a proper thread delaying here.
  --
  -- Thread safe, exception free.
  --
  -- | mbMcs == Nothing: infinite awaiting.
  -- | mbMcs == Just (Microseconds n): await for approximately n seconds.
  --     Awaiting may succeed ealier.
  --
  -- > myFlow1 = do
  -- >   logInfoT "myflow1" "logFromMyFlow1"
  -- >   pure 10
  -- >
  -- > myFlow2 = do
  -- >   awaitable <- forkFlow' "myFlow1 fork" myFlow1
  -- >   await Nothing awaitable
  await
    :: HasCallStack
    => Maybe Microseconds
    -> Awaitable (Either Text a)
    -> m (Either AwaitingError a)

  -- | Throw a given exception.
  --
  -- It's possible to catch this exception using runSafeFlow method.
  --
  -- Thread safe. Exception throwing.
  --
  -- > myFlow = do
  -- >   res <- authAction
  -- >   case res of
  -- >     Failure reason -> throwException err403 {errBody = reason}
  -- >     Success -> ...
  throwException :: forall a e. (HasCallStack, Exception e) => e -> m a
  throwException = throwM

  throwExceptionWithoutCallStack :: forall a e. (HasCallStack, Exception e) => e -> m a
  throwExceptionWithoutCallStack = throwM

  -- | Run a flow safely with catching all the exceptions from it.
  -- Returns either a result or the exception turned into a text message.
  --
  -- This includes ususal instances of the Exception type class,
  -- `error` exception and custom user exceptions thrown by the `throwException` method.
  --
  -- Thread safe, exception free.
  --
  -- > myFlow = runSafeFlow $ throwException err403 {errBody = reason}
  --
  -- > myFlow = do
  -- >   eitherContent <- runSafeFlow $ runIO $ readFromFile file
  -- >   case eitherContent of
  -- >     Left err -> ...
  -- >     Right content -> ...
  runSafeFlow :: HasCallStack => Flow a -> m (Either Text a)

  -- | Execute kvdb actions.
  --
  -- Thread safe, exception free.
  --
  -- > myFlow = do
  -- >   kvres <- L.runKVDB $ do
  -- >     set "aaa" "bbb"
  -- >     res <- get "aaa"
  -- >     del ["aaa"]
  -- >     pure res
  runKVDB
    :: HasCallStack
    => Text
    -> KVDB a -- ^ KVDB action
    -> m (KVDBAnswer a)

  ---- Experimental Pub Sub implementation using Redis Pub Sub.

  runPubSub
    :: HasCallStack
    => PubSub a
    -> m a

  -- | Publish payload to channel.
  publish
    :: HasCallStack
    => PSL.Channel                        -- ^ Channel in which payload will be send
    -> PSL.Payload                        -- ^ Payload
    -> m (Either KVDBReply Integer)  -- ^ Number of subscribers received payload

  -- | Subscribe to all channels from list.
  -- Note: Subscription won't be unsubscribed automatically on thread end.
  -- Use canceller explicitly to cancel subscription
  subscribe
    :: HasCallStack
    => [PSL.Channel]    -- ^ List of channels to subscribe
    -> MessageCallback  -- ^ Callback function.
    -> m (Flow ())   -- ^ Inner flow is a canceller of current subscription

  -- | Subscribe to all channels from list. Respects redis pattern syntax.
  -- Note: Subscription won't be unsubscribed automatically on thread end.
  -- Use canceller explicitly to cancel subscription
  psubscribe
    :: HasCallStack
    => [PSL.ChannelPattern] -- ^ List of channels to subscribe (wit respect to patterns supported by redis)
    -> PMessageCallback     -- ^ Callback function
    -> m (Flow ())       -- ^ Inner flow is a canceller of current subscription

  -- | Run a flow with a modified runtime. The runtime will be restored after
  -- the computation finishes.
  --
  -- @since 2.0.3.1
  withModifiedRuntime
    :: HasCallStack
    => (FlowRuntime -> FlowRuntime) -- ^ Temporary modification function for runtime
    -> Flow a -- ^ Computation to run with modified runtime
    -> m a

  fork
    :: HasCallStack
    => m a -> m ()
  
  getSnowflakeID
    :: HasCallStack
    => Word8
    -> Word16
    -> String
    -> m (Either SnowflakeError Snowflake)

  forkAwaitable
    :: HasCallStack
    => Description -> m a
    -> m (Awaitable (Either Text a))
    
  runInDBTransaction :: HasCallStack
    => SqlConn beM
    -> (T.NativeSqlConn -> Flow a)
    -> m (DBResult a)

  runDBWithConn ::
      ( HasCallStack
      , BeamRunner beM
      , BeamRuntime be beM
      )
    => T.NativeSqlConn
    -> SqlDB beM a
    -> m (DBResult a)

instance MonadFlow Flow where
  {-# INLINEABLE callServantAPI #-}
  callServantAPI mgrSel url apiTag errFunc cl = do
    let _callServantApi mgr = ExceptT $ liftFC $ CallServantAPI mgr url apiTag errFunc cl id
    runExceptT $ getMgr mgrSel >>= _callServantApi
  {-# INLINEABLE callAPIUsingManager #-}
  callAPIUsingManager mgr url apiTag errFunc cl = liftFC $ CallServantAPI mgr url apiTag errFunc cl id
  {-# INLINEABLE lookupHTTPManager #-}
  lookupHTTPManager mMgrSel = liftFC $ LookupHTTPManager mMgrSel id
  {-# INLINEABLE getHTTPManager #-}
  getHTTPManager settings = liftFC $ GetHTTPManager settings id
  {-# INLINEABLE callHTTPUsingManager #-}
  callHTTPUsingManager mgr url apiTag errFunc mskReqRespBody mbDigestDetails = liftFC $ CallHTTP url apiTag errFunc mgr mskReqRespBody mbDigestDetails id
  {-# INLINEABLE evalLogger' #-}
  evalLogger' logAct = liftFC $ EvalLogger logAct id
  {-# INLINEABLE runIO' #-}
  runIO' descr ioAct = liftFC $ RunIO descr ioAct id
  {-# INLINEABLE getOption #-}
  getOption :: forall k v. (HasCallStack, OptionEntity k v) => k -> Flow (Maybe v)
  getOption k = liftFC $ GetOption (mkOptionKey @k @v k) id
  {-# INLINEABLE setOption #-}
  setOption :: forall k v. (HasCallStack, OptionEntity k v) => k -> v -> Flow ()
  setOption k v = liftFC $ SetOption (mkOptionKey @k @v k) v id
  {-# INLINEABLE setLoggerContext #-}
  setLoggerContext :: (HasCallStack) => Text -> Text -> Flow ()
  setLoggerContext k v = liftFC $ SetLoggerContext k v id
  {-# INLINEABLE getLoggerContext #-}
  getLoggerContext :: (HasCallStack) => Text -> Flow (Maybe Text)
  getLoggerContext k = liftFC $ GetLoggerContext k id
  {-# INLINEABLE setLoggerContextMap #-}
  setLoggerContextMap :: (HasCallStack) => HashMap Text Text -> Flow ()
  setLoggerContextMap v = liftFC $ SetLoggerContextMap v id
  {-# INLINEABLE getLoggerContextMap #-}
  getLoggerContextMap :: (HasCallStack) => Flow (HashMap Text Text)
  getLoggerContextMap = liftFC $ GetLoggerContextMap id
  {-# INLINEABLE incrementArtCounter #-}
  incrementArtCounter :: (HasCallStack) => Flow Int
  incrementArtCounter = liftFC $ IncrementArtCounter id
  {-# INLINEABLE modifyOption #-}
  modifyOption :: forall k v. (HasCallStack, OptionEntity k v) => k -> (v -> v) -> Flow (Maybe v,Maybe v)
  modifyOption k fn = liftFC $ ModifyOption  (mkOptionKey @k @v k) fn id
  {-# INLINEABLE delOption #-}
  delOption :: forall k v. (HasCallStack, OptionEntity k v) => k -> Flow ()
  delOption k = liftFC $ DelOption (mkOptionKey @k @v k) id
  {-# INLINEABLE getOptionLocal #-}
  getOptionLocal :: forall k v. (HasCallStack, OptionEntity k v) => k -> Flow (Maybe v)
  getOptionLocal k = liftFC $ GetOptionLocal (mkOptionKey @k @v k) id
  {-# INLINEABLE setOptionLocal #-}
  setOptionLocal :: forall k v. (HasCallStack, OptionEntity k v) => k -> v -> Flow ()
  setOptionLocal k v = liftFC $ SetOptionLocal (mkOptionKey @k @v k) v id
  {-# INLINEABLE modifyOptionLocal #-}
  modifyOptionLocal :: forall k v. (HasCallStack, OptionEntity k v) => k -> (v -> v) -> Flow (Maybe v, Maybe v)
  modifyOptionLocal k fn = liftFC $ ModifyOptionLocal  (mkOptionKey @k @v k) fn id
  {-# INLINEABLE delOptionLocal #-}
  delOptionLocal :: forall k v. (HasCallStack, OptionEntity k v) => k -> Flow ()
  delOptionLocal k = liftFC $ DelOptionLocal (mkOptionKey @k @v k) id
  {-# INLINE getFlowRuntime #-}
  getFlowRuntime :: (HasCallStack) => Flow FlowRuntime
  getFlowRuntime = liftFC $ GetFlowRuntime id
  {-# INLINEABLE getRecordingLocal #-}
  getRecordingLocal :: (HasCallStack) => Flow ([RecordingEntry])
  getRecordingLocal = liftFC $ GetRecordingLocal id
  {-# INLINEABLE appendRecordingLocal #-}
  appendRecordingLocal :: (HasCallStack) => RecordingEntry -> Flow ()
  appendRecordingLocal v = liftFC $ AppendRecordingLocal v id
  {-# INLINEABLE delRecordingLocal #-}
  delRecordingLocal :: (HasCallStack) => Flow ()
  delRecordingLocal = liftFC $ DelRecordingLocal id
  {-# INLINEABLE getConfig #-}
  getConfig :: HasCallStack => W8Arr -> QueryType -> Flow (Maybe ConfigEntry)
  getConfig k queryType = liftFC $ GetConfig k queryType id
  {-# INLINEABLE setConfig #-}
  setConfig :: HasCallStack => W8Arr -> ConfigEntry -> QueryType -> Flow ()
  setConfig k v queryType = liftFC $ SetConfig k v queryType id
  {-# INLINEABLE modifyConfig #-}
  modifyConfig :: HasCallStack => W8Arr -> (Maybe ConfigEntry -> ConfigEntry) -> QueryType -> Flow ()
  modifyConfig k modification queryType = liftFC $ ModifyConfig k modification queryType id
  {-# INLINEABLE trySetConfig #-}
  trySetConfig :: HasCallStack => W8Arr -> ConfigEntry -> QueryType -> Flow (Maybe ())
  trySetConfig k v queryType = liftFC $ TrySetConfig k v queryType id
  {-# INLINEABLE delConfig #-}
  delConfig :: HasCallStack => W8Arr -> QueryType -> Flow ()
  delConfig k queryType = liftFC $ DelConfig k queryType id
  {-# INLINEABLE acquireConfigLock #-}
  acquireConfigLock :: HasCallStack => W8Arr -> Flow Bool
  acquireConfigLock k = liftFC $ AcquireConfigLock k id
  {-# INLINEABLE releaseConfigLock #-}
  releaseConfigLock :: HasCallStack => W8Arr -> Flow Bool
  releaseConfigLock k = liftFC $ ReleaseConfigLock k id
  {-# INLINEABLE generateGUID #-}
  generateGUID = liftFC $ UUID.toText <$> GenerateGUID "GenerateGUID::NO_TAG" id
  {-# INLINEABLE generateRawGUID #-}
  generateRawGUID tag = liftFC $ GenerateGUID tag id
  {-# INLINEABLE getCurrentTime #-}
  getCurrentTime = liftFC $ GetCurrentTime id
  {-# INLINEABLE getPOSIXTime #-}
  getPOSIXTime = liftFC $ GetPOSIXTime id
  {-#INLINABLE randomRIO #-}
  randomRIO k p = liftFC $ RandomRIO k p id
  {-#INLINABLE getRandomBytes #-}
  getRandomBytes k p = liftFC $ GetRandomBytes k p id
  {-# INLINEABLE runSysCmd #-}
  runSysCmd cmd = liftFC $ RunSysCmd cmd id
  {-# INLINEABLE initSqlDBConnection #-}
  initSqlDBConnection cfg = liftFC $ InitSqlDBConnection cfg id
  {-# INLINEABLE deinitSqlDBConnection #-}
  deinitSqlDBConnection conn = liftFC $ DeInitSqlDBConnection conn id
  {-# INLINEABLE getSqlDBConnection #-}
  getSqlDBConnection cfg = liftFC $ GetSqlDBConnection cfg id
  {-# INLINEABLE initKVDBConnection #-}
  initKVDBConnection cfg = liftFC $ InitKVDBConnection cfg id
  {-# INLINEABLE deinitKVDBConnection #-}
  deinitKVDBConnection conn = liftFC $ DeInitKVDBConnection conn id
  {-# INLINEABLE getKVDBConnection #-}
  getKVDBConnection cfg = liftFC $ GetKVDBConnection cfg id
  {-# INLINEABLE runDB #-}
  runDB conn dbAct = liftFC $ RunDB conn (SqlDBFlow dbAct False) id
  {-# INLINEABLE runDBWithRetry #-}
  runDBWithRetry conn dbAct = liftFC $ RunDBWithRetry conn (SqlDBFlow dbAct False) id
  {-# INLINEABLE runTransaction #-}
  runTransaction conn dbAct = liftFC $ RunDB conn (SqlDBFlow dbAct True) id
  {-# INLINEABLE await #-}
  await mbMcs awaitable = liftFC $ Await mbMcs awaitable id
  {-# INLINEABLE runSafeFlow #-}
  runSafeFlow flow = do
    safeFlowGUID <- generateGUIDWithTag "runSafeFlow::safeFlowGUID"
    liftFC $ RunSafeFlow safeFlowGUID flow id
  {-# INLINEABLE runKVDB #-}
  runKVDB cName act = do
    res <- liftFC $ RunKVDB cName act id
    case res of
      Left err -> incrementRedisMetric err cName *> pure res
      Right _ -> pure res
  {-# INLINEABLE runPubSub #-}
  runPubSub act = liftFC $ RunPubSub act id
  {-# INLINEABLE publish #-}
  publish channel payload = runPubSub $ PubSub $ const $ PSL.publish channel payload
  {-# INLINEABLE subscribe #-}
  subscribe channels cb = fmap (runIO' "subscribe") $
    runPubSub $ PubSub $ \runFlow -> PSL.subscribe channels (runFlow . cb)
  {-# INLINEABLE psubscribe #-}
  psubscribe channels cb = fmap (runIO' "psubscribe") $
    runPubSub $ PubSub $ \runFlow -> PSL.psubscribe channels (\ch -> runFlow . cb ch)
  {-# INLINEABLE withModifiedRuntime #-}
  withModifiedRuntime f flow = liftFC $ WithModifiedRuntime f flow id
  {-# INLINEABLE fork #-}
  fork flow = do
    forkFlow "test" flow
  {-# INLINEABLE getSnowflakeID #-}
  getSnowflakeID sId pId k = liftFC $ GetSnowflakeID sId pId k id
  {-# INLINEABLE forkAwaitable #-}
  forkAwaitable description flow = do
    forkFlow' description flow
  {-# INLINEABLE runInDBTransaction #-}
  runInDBTransaction conf flow = liftFC $ RunDB conf (TransactionFlow flow) id
  {-# INLINEABLE runDBWithConn #-}
  runDBWithConn conn dbAction = liftFC $ RunDBWithConn conn dbAction id

instance MonadFlow m => MonadFlow (ReaderT r m) where
  {-# INLINEABLE callServantAPI #-}
  callServantAPI mbMgrSel url apiTag errFunc = lift . callServantAPI mbMgrSel url apiTag errFunc
  {-# INLINEABLE callAPIUsingManager #-}
  callAPIUsingManager mgr url apiTag errFunc = lift . callAPIUsingManager mgr url apiTag errFunc
  {-# INLINEABLE lookupHTTPManager #-}
  lookupHTTPManager = lift . lookupHTTPManager
  {-# INLINEABLE getHTTPManager #-}
  getHTTPManager = lift . getHTTPManager
  {-# INLINEABLE callHTTPUsingManager #-}
  callHTTPUsingManager mgr url apiTag errFunc mskReqRespBody mbDigestDetails = lift $ callHTTPUsingManager mgr url apiTag errFunc mskReqRespBody mbDigestDetails
  {-# INLINEABLE evalLogger' #-}
  evalLogger' = lift . evalLogger'
  {-# INLINEABLE runIO' #-}
  runIO' descr = lift . runIO' descr
  {-# INLINEABLE getOption #-}
  getOption = lift . getOption
  {-# INLINEABLE setOption #-}
  setOption k = lift . setOption k
  {-# INLINEABLE setLoggerContext #-}
  setLoggerContext k = lift . setLoggerContext k
  {-# INLINEABLE getLoggerContext #-}
  getLoggerContext = lift . getLoggerContext
  {-# INLINEABLE setLoggerContextMap #-}
  setLoggerContextMap = lift . setLoggerContextMap
  {-# INLINEABLE getLoggerContextMap #-}
  getLoggerContextMap = lift getLoggerContextMap
  {-# INLINEABLE incrementArtCounter #-}
  incrementArtCounter = lift incrementArtCounter
  {-# INLINEABLE modifyOption #-}
  modifyOption k = lift . modifyOption k
  {-# INLINEABLE delOption #-}
  delOption = lift . delOption
  {-# INLINEABLE getOptionLocal #-}
  getOptionLocal = lift . getOptionLocal
  {-# INLINEABLE setOptionLocal #-}
  setOptionLocal k = lift . setOptionLocal k
  {-# INLINEABLE modifyOptionLocal #-}
  modifyOptionLocal k = lift . modifyOptionLocal k
  {-# INLINEABLE delOptionLocal #-}
  delOptionLocal = lift . delOptionLocal
  {-# INLINEABLE getFlowRuntime #-}
  getFlowRuntime = lift getFlowRuntime
  {-# INLINEABLE getRecordingLocal #-}
  getRecordingLocal = lift getRecordingLocal
  {-# INLINEABLE appendRecordingLocal #-}
  appendRecordingLocal = lift . appendRecordingLocal
  {-# INLINEABLE delRecordingLocal #-}
  delRecordingLocal = lift delRecordingLocal
  {-# INLINEABLE getConfig #-}
  getConfig k queryType = lift $ getConfig k queryType
  {-# INLINEABLE setConfig #-}
  setConfig k configEntry queryType = lift $ setConfig k configEntry queryType
  {-# INLINEABLE modifyConfig #-}
  modifyConfig k modification queryType = lift $ modifyConfig k modification queryType
  {-# INLINEABLE trySetConfig #-}
  trySetConfig k v queryType = lift $ trySetConfig k v queryType
  {-# INLINEABLE delConfig #-}
  delConfig k queryType = lift $ delConfig k queryType
  {-# INLINEABLE acquireConfigLock #-}
  acquireConfigLock = lift . acquireConfigLock
  {-# INLINEABLE releaseConfigLock #-}
  releaseConfigLock = lift . releaseConfigLock
  {-# INLINEABLE generateGUID #-}
  generateGUID = lift generateGUID
  {-# INLINEABLE generateRawGUID #-}
  generateRawGUID = lift . generateRawGUID
  {-# INLINABLE getCurrentTime #-}
  getCurrentTime = lift getCurrentTime
  {-# INLINABLE getPOSIXTime #-}
  getPOSIXTime = lift getPOSIXTime
  {-# INLINABLE randomRIO #-}
  randomRIO  k p = lift $ randomRIO k p
  {-# INLINABLE getRandomBytes #-}
  getRandomBytes  k p = lift $ getRandomBytes k p
  {-# INLINEABLE runSysCmd #-}
  runSysCmd = lift . runSysCmd
  {-# INLINEABLE initSqlDBConnection #-}
  initSqlDBConnection = lift . initSqlDBConnection
  {-# INLINEABLE deinitSqlDBConnection #-}
  deinitSqlDBConnection = lift . deinitSqlDBConnection
  {-# INLINEABLE getSqlDBConnection #-}
  getSqlDBConnection = lift . getSqlDBConnection
  {-# INLINEABLE initKVDBConnection #-}
  initKVDBConnection = lift . initKVDBConnection
  {-# INLINEABLE deinitKVDBConnection #-}
  deinitKVDBConnection = lift . deinitKVDBConnection
  {-# INLINEABLE getKVDBConnection #-}
  getKVDBConnection = lift . getKVDBConnection
  {-# INLINEABLE runDB #-}
  runDB conn = lift . runDB conn
  {-# INLINEABLE runDBWithRetry #-}
  runDBWithRetry conn = lift . runDBWithRetry conn
  {-# INLINEABLE runTransaction #-}
  runTransaction conn = lift . runTransaction conn
  {-# INLINEABLE await #-}
  await mbMcs = lift . await mbMcs
  {-# INLINEABLE runSafeFlow #-}
  runSafeFlow = lift . runSafeFlow
  {-# INLINEABLE runKVDB #-}
  runKVDB cName = lift . runKVDB cName
  {-# INLINEABLE runPubSub #-}
  runPubSub = lift . runPubSub
  {-# INLINEABLE publish #-}
  publish channel = lift . publish channel
  {-# INLINEABLE subscribe #-}
  subscribe channels = lift . subscribe channels
  {-# INLINEABLE psubscribe #-}
  psubscribe channels = lift . psubscribe channels
  {-# INLINEABLE withModifiedRuntime #-}
  withModifiedRuntime f = lift . withModifiedRuntime f
  {-# INLINEABLE fork #-}
  fork flow = do
    env <- ask
    lift . fork $ runReaderT flow env
  {-# INLINEABLE getSnowflakeID #-}
  getSnowflakeID sId pId = lift . getSnowflakeID sId pId
  {-# INLINEABLE forkAwaitable #-}
  forkAwaitable description flow = do
    env <- ask
    lift $ forkAwaitable description $ runReaderT flow env
  {-# INLINEABLE runInDBTransaction #-}
  runInDBTransaction conn = lift . runInDBTransaction conn
  {-# INLINEABLE runDBWithConn #-}
  runDBWithConn conn = lift . runDBWithConn conn

instance MonadFlow m => MonadFlow (StateT s m) where
  {-# INLINEABLE callServantAPI #-}
  callServantAPI mbMgrSel url apiTag errFunc = lift . callServantAPI mbMgrSel url apiTag errFunc
  {-# INLINEABLE callAPIUsingManager #-}
  callAPIUsingManager mgr url apiTag errFunc = lift . callAPIUsingManager mgr url apiTag errFunc
  {-# INLINEABLE lookupHTTPManager #-}
  lookupHTTPManager = lift . lookupHTTPManager
  {-# INLINEABLE getHTTPManager #-}
  getHTTPManager = lift . getHTTPManager
  {-# INLINEABLE callHTTPUsingManager #-}
  callHTTPUsingManager mgr url apiTag respFunc mskReqRespBody mbDigestDetails = lift $ callHTTPUsingManager mgr url apiTag respFunc mskReqRespBody mbDigestDetails
  {-# INLINEABLE evalLogger' #-}
  evalLogger' = lift . evalLogger'
  {-# INLINEABLE runIO' #-}
  runIO' descr = lift . runIO' descr
  {-# INLINEABLE getOption #-}
  getOption = lift . getOption
  {-# INLINEABLE setOption #-}
  setOption k = lift . setOption k
  {-# INLINEABLE setLoggerContext #-}
  setLoggerContext k = lift . setLoggerContext k
  {-# INLINEABLE getLoggerContext #-}
  getLoggerContext = lift . getLoggerContext
  {-# INLINEABLE setLoggerContextMap #-}
  setLoggerContextMap = lift . setLoggerContextMap
  {-# INLINEABLE getLoggerContextMap #-}
  getLoggerContextMap = lift getLoggerContextMap
  {-# INLINEABLE incrementArtCounter #-}
  incrementArtCounter = lift incrementArtCounter
  {-# INLINEABLE modifyOption #-}
  modifyOption fn = lift . modifyOption fn
  {-# INLINEABLE delOption #-}
  delOption = lift . delOption
  {-# INLINEABLE getOptionLocal #-}
  getOptionLocal = lift . getOptionLocal
  {-# INLINEABLE setOptionLocal #-}
  setOptionLocal k = lift . setOptionLocal k
  {-# INLINEABLE modifyOptionLocal #-}
  modifyOptionLocal fn = lift . modifyOptionLocal fn
  {-# INLINEABLE delOptionLocal #-}
  delOptionLocal = lift . delOptionLocal
  {-# INLINEABLE getFlowRuntime #-}
  getFlowRuntime = lift getFlowRuntime
  {-# INLINEABLE getRecordingLocal #-}
  getRecordingLocal = lift getRecordingLocal
  {-# INLINEABLE appendRecordingLocal #-}
  appendRecordingLocal = lift . appendRecordingLocal
  {-# INLINEABLE delRecordingLocal #-}
  delRecordingLocal = lift delRecordingLocal
  {-# INLINEABLE getConfig #-}
  getConfig k queryType = lift $ getConfig k queryType 
  {-# INLINEABLE setConfig #-}
  setConfig k configEntry queryType = lift $ setConfig k configEntry queryType
  {-# INLINEABLE modifyConfig #-}
  modifyConfig k modification queryType = lift $ modifyConfig k modification queryType
  {-# INLINEABLE trySetConfig #-}
  trySetConfig k v queryType = lift $ trySetConfig k v queryType
  {-# INLINEABLE delConfig #-}
  delConfig k queryType = lift $ delConfig k queryType
  {-# INLINEABLE acquireConfigLock #-}
  acquireConfigLock = lift . acquireConfigLock
  {-# INLINEABLE releaseConfigLock #-}
  releaseConfigLock = lift . releaseConfigLock
  {-# INLINEABLE generateGUID #-}
  generateGUID = lift generateGUID
  {-# INLINEABLE generateRawGUID #-}
  generateRawGUID = lift . generateRawGUID
  {-# INLINABLE getCurrentTime #-}
  getCurrentTime = lift getCurrentTime
  {-# INLINABLE getPOSIXTime #-}
  getPOSIXTime = lift getPOSIXTime
  {-# INLINABLE randomRIO #-}
  randomRIO k p = lift $ randomRIO k p
  {-# INLINABLE getRandomBytes #-}
  getRandomBytes k p = lift $ getRandomBytes k p
  {-# INLINEABLE runSysCmd #-}
  runSysCmd = lift . runSysCmd
  {-# INLINEABLE initSqlDBConnection #-}
  initSqlDBConnection = lift . initSqlDBConnection
  {-# INLINEABLE deinitSqlDBConnection #-}
  deinitSqlDBConnection = lift . deinitSqlDBConnection
  {-# INLINEABLE getSqlDBConnection #-}
  getSqlDBConnection = lift . getSqlDBConnection
  {-# INLINEABLE initKVDBConnection #-}
  initKVDBConnection = lift . initKVDBConnection
  {-# INLINEABLE deinitKVDBConnection #-}
  deinitKVDBConnection = lift . deinitKVDBConnection
  {-# INLINEABLE getKVDBConnection #-}
  getKVDBConnection = lift . getKVDBConnection
  {-# INLINEABLE runDB #-}
  runDB conn = lift . runDB conn
  {-# INLINEABLE runDBWithRetry #-}
  runDBWithRetry conn = lift . runDBWithRetry conn
  {-# INLINEABLE runTransaction #-}
  runTransaction conn = lift . runTransaction conn
  {-# INLINEABLE await #-}
  await mbMcs = lift . await mbMcs
  {-# INLINEABLE runSafeFlow #-}
  runSafeFlow = lift . runSafeFlow
  {-# INLINEABLE runKVDB #-}
  runKVDB cName = lift . runKVDB cName
  {-# INLINEABLE runPubSub #-}
  runPubSub = lift . runPubSub
  {-# INLINEABLE publish #-}
  publish channel = lift . publish channel
  {-# INLINEABLE subscribe #-}
  subscribe channels = lift . subscribe channels
  {-# INLINEABLE psubscribe #-}
  psubscribe channels = lift . psubscribe channels
  {-# INLINEABLE withModifiedRuntime #-}
  withModifiedRuntime f = lift . withModifiedRuntime f
  {-# INLINEABLE fork #-}
  fork flow = do
    s <- get
    lift . fork $ evalStateT flow s
  {-# INLINEABLE getSnowflakeID #-}
  getSnowflakeID sId pId = lift . getSnowflakeID sId pId
  {-# INLINEABLE forkAwaitable #-}
  forkAwaitable description flow = do
    s <- get
    lift $ forkAwaitable description $ evalStateT flow s
  {-# INLINEABLE runInDBTransaction #-}
  runInDBTransaction conn = lift . runInDBTransaction conn
  {-# INLINEABLE runDBWithConn #-}
  runDBWithConn conn = lift . runDBWithConn conn

instance (MonadFlow m, Monoid w) => MonadFlow (WriterT w m) where
  {-# INLINEABLE callServantAPI #-}
  callServantAPI mbMgrSel url apiTag errFunc = lift . callServantAPI mbMgrSel url apiTag errFunc
  {-# INLINEABLE callAPIUsingManager #-}
  callAPIUsingManager mgr url apiTag errFunc = lift . callAPIUsingManager mgr url apiTag errFunc
  {-# INLINEABLE lookupHTTPManager #-}
  lookupHTTPManager = lift . lookupHTTPManager
  {-# INLINEABLE getHTTPManager #-}
  getHTTPManager = lift . getHTTPManager
  {-# INLINEABLE callHTTPUsingManager #-}
  callHTTPUsingManager mgr url apiTag errFunc mskReqRespBody mbDigestDetails = lift $ callHTTPUsingManager mgr url apiTag errFunc mskReqRespBody mbDigestDetails
  {-# INLINEABLE evalLogger' #-}
  evalLogger' = lift . evalLogger'
  {-# INLINEABLE runIO' #-}
  runIO' descr = lift . runIO' descr
  {-# INLINEABLE getOption #-}
  getOption = lift . getOption
  {-# INLINEABLE setOption #-}
  setOption k = lift . setOption k
  {-# INLINEABLE setLoggerContext #-}
  setLoggerContext k = lift . setLoggerContext k
  {-# INLINEABLE getLoggerContext #-}
  getLoggerContext = lift . getLoggerContext
  {-# INLINEABLE setLoggerContextMap #-}
  setLoggerContextMap = lift . setLoggerContextMap
  {-# INLINEABLE getLoggerContextMap #-}
  getLoggerContextMap = lift getLoggerContextMap
  {-# INLINEABLE incrementArtCounter #-}
  incrementArtCounter = lift incrementArtCounter
  {-# INLINEABLE modifyOption #-}
  modifyOption fn = lift . modifyOption fn
  {-# INLINEABLE delOption #-}
  delOption = lift . delOption
  {-# INLINEABLE getOptionLocal #-}
  getOptionLocal = lift . getOptionLocal
  {-# INLINEABLE setOptionLocal #-}
  setOptionLocal k = lift . setOptionLocal k
  {-# INLINEABLE modifyOptionLocal #-}
  modifyOptionLocal fn = lift . modifyOptionLocal fn
  {-# INLINEABLE delOptionLocal #-}
  delOptionLocal = lift . delOptionLocal
  {-# INLINEABLE getFlowRuntime #-}
  getFlowRuntime = lift getFlowRuntime
  {-# INLINEABLE getRecordingLocal #-}
  getRecordingLocal = lift getRecordingLocal
  {-# INLINEABLE appendRecordingLocal #-}
  appendRecordingLocal = lift . appendRecordingLocal
  {-# INLINEABLE delRecordingLocal #-}
  delRecordingLocal = lift delRecordingLocal
  {-# INLINEABLE getConfig #-}
  getConfig k queryType = lift $ getConfig k queryType
  {-# INLINEABLE setConfig #-}
  setConfig k configEntry queryType = lift $ setConfig k configEntry queryType
  {-# INLINEABLE modifyConfig #-}
  modifyConfig k modification queryType = lift $ modifyConfig k modification queryType
  {-# INLINEABLE trySetConfig #-}
  trySetConfig k v queryType = lift $ trySetConfig k v queryType
  {-# INLINEABLE delConfig #-}
  delConfig k queryType = lift $ delConfig k queryType
  {-# INLINEABLE acquireConfigLock #-}
  acquireConfigLock = lift . acquireConfigLock
  {-# INLINEABLE releaseConfigLock #-}
  releaseConfigLock = lift . releaseConfigLock
  {-# INLINEABLE generateGUID #-}
  generateGUID = lift generateGUID
  {-# INLINEABLE generateRawGUID #-}
  generateRawGUID = lift . generateRawGUID
  {-# INLINEABLE getCurrentTime #-}
  getCurrentTime = lift getCurrentTime
  {-# INLINEABLE getPOSIXTime #-}
  getPOSIXTime = lift getPOSIXTime
  {-# INLINEABLE randomRIO #-}
  randomRIO k p = lift $ randomRIO k p
  {-# INLINEABLE getRandomBytes #-}
  getRandomBytes k p = lift $ getRandomBytes k p
  {-# INLINEABLE runSysCmd #-}
  runSysCmd = lift . runSysCmd
  {-# INLINEABLE initSqlDBConnection #-}
  initSqlDBConnection = lift . initSqlDBConnection
  {-# INLINEABLE deinitSqlDBConnection #-}
  deinitSqlDBConnection = lift . deinitSqlDBConnection
  {-# INLINEABLE getSqlDBConnection #-}
  getSqlDBConnection = lift . getSqlDBConnection
  {-# INLINEABLE initKVDBConnection #-}
  initKVDBConnection = lift . initKVDBConnection
  {-# INLINEABLE deinitKVDBConnection #-}
  deinitKVDBConnection = lift . deinitKVDBConnection
  {-# INLINEABLE getKVDBConnection #-}
  getKVDBConnection = lift . getKVDBConnection
  {-# INLINEABLE runDB #-}
  runDB conn = lift . runDB conn
  {-# INLINEABLE runDBWithRetry #-}
  runDBWithRetry conn = lift . runDBWithRetry conn
  {-# INLINEABLE runTransaction #-}
  runTransaction conn = lift . runTransaction conn
  {-# INLINEABLE await #-}
  await mbMcs = lift . await mbMcs
  {-# INLINEABLE runSafeFlow #-}
  runSafeFlow = lift . runSafeFlow
  {-# INLINEABLE runKVDB #-}
  runKVDB cName = lift . runKVDB cName
  {-# INLINEABLE runPubSub #-}
  runPubSub = lift . runPubSub
  {-# INLINEABLE publish #-}
  publish channel = lift . publish channel
  {-# INLINEABLE subscribe #-}
  subscribe channels = lift . subscribe channels
  {-# INLINEABLE psubscribe #-}
  psubscribe channels = lift . psubscribe channels
  {-# INLINEABLE withModifiedRuntime #-}
  withModifiedRuntime f = lift . withModifiedRuntime f
  {-# INLINEABLE fork #-}
  fork = error "Not implemented"
  {-# INLINEABLE getSnowflakeID #-}
  getSnowflakeID sId pId = lift . getSnowflakeID sId pId
  {-# INLINEABLE forkAwaitable #-}
  forkAwaitable = error "Not implemented"
  {-# INLINEABLE runInDBTransaction #-}
  runInDBTransaction conn = lift . runInDBTransaction conn
  {-# INLINEABLE runDBWithConn #-}
  runDBWithConn conn = lift . runDBWithConn conn

instance MonadFlow m => MonadFlow (ExceptT e m) where
  {-# INLINEABLE callServantAPI #-}
  callServantAPI mbMgrSel url apiTag errFunc = lift . callServantAPI mbMgrSel url apiTag errFunc
  {-# INLINEABLE callAPIUsingManager #-}
  callAPIUsingManager mgr url apiTag errFunc = lift . callAPIUsingManager mgr url apiTag errFunc
  {-# INLINEABLE lookupHTTPManager #-}
  lookupHTTPManager = lift . lookupHTTPManager
  {-# INLINEABLE getHTTPManager #-}
  getHTTPManager = lift . getHTTPManager
  {-# INLINEABLE callHTTPUsingManager #-}
  callHTTPUsingManager mgr url apiTag errFunc mskReqRespBody mbDigestDetails = lift $ callHTTPUsingManager mgr url apiTag errFunc mskReqRespBody mbDigestDetails
  {-# INLINEABLE evalLogger' #-}
  evalLogger' = lift . evalLogger'
  {-# INLINEABLE runIO' #-}
  runIO' descr = lift . runIO' descr
  {-# INLINEABLE getOption #-}
  getOption = lift . getOption
  {-# INLINEABLE setOption #-}
  setOption k = lift . setOption k
  {-# INLINEABLE setLoggerContext #-}
  setLoggerContext k = lift . setLoggerContext k
  {-# INLINEABLE getLoggerContext #-}
  getLoggerContext = lift . getLoggerContext
  {-# INLINEABLE setLoggerContextMap #-}
  setLoggerContextMap = lift . setLoggerContextMap
  {-# INLINEABLE getLoggerContextMap #-}
  getLoggerContextMap = lift getLoggerContextMap
  {-# INLINEABLE incrementArtCounter #-}
  incrementArtCounter = lift incrementArtCounter
  {-# INLINEABLE modifyOption #-}
  modifyOption fn = lift . modifyOption fn
  {-# INLINEABLE delOption #-}
  delOption = lift . delOption
  {-# INLINEABLE getOptionLocal #-}
  getOptionLocal = lift . getOptionLocal
  {-# INLINEABLE setOptionLocal #-}
  setOptionLocal k = lift . setOptionLocal k
  {-# INLINEABLE modifyOptionLocal #-}
  modifyOptionLocal fn = lift . modifyOptionLocal fn
  {-# INLINEABLE delOptionLocal #-}
  delOptionLocal = lift . delOptionLocal
  {-# INLINEABLE getFlowRuntime #-}
  getFlowRuntime = lift getFlowRuntime
  {-# INLINEABLE getRecordingLocal #-}
  getRecordingLocal = lift getRecordingLocal
  {-# INLINEABLE appendRecordingLocal #-}
  appendRecordingLocal = lift . appendRecordingLocal
  {-# INLINEABLE delRecordingLocal #-}
  delRecordingLocal = lift delRecordingLocal
  {-# INLINEABLE getConfig #-}
  getConfig k queryType = lift $ getConfig k queryType
  {-# INLINEABLE setConfig #-}
  setConfig k configEntry queryType = lift $ setConfig k configEntry queryType
  {-# INLINEABLE modifyConfig #-}
  modifyConfig k modification queryType = lift $ modifyConfig k modification queryType
  {-# INLINEABLE trySetConfig #-}
  trySetConfig k v queryType = lift $ trySetConfig k v queryType
  {-# INLINEABLE delConfig #-}
  delConfig k queryType = lift $ delConfig k queryType
  {-# INLINEABLE acquireConfigLock #-}
  acquireConfigLock = lift . acquireConfigLock
  {-# INLINEABLE releaseConfigLock #-}
  releaseConfigLock = lift . releaseConfigLock
  {-# INLINEABLE generateGUID #-}
  generateGUID = lift generateGUID
  {-# INLINEABLE generateRawGUID #-}
  generateRawGUID = lift . generateRawGUID
  {-# INLINEABLE getCurrentTime #-}
  getCurrentTime = lift getCurrentTime
  {-# INLINEABLE getPOSIXTime #-}
  getPOSIXTime = lift getPOSIXTime
  {-# INLINEABLE randomRIO #-}
  randomRIO k p = lift $ randomRIO k p
  {-# INLINEABLE getRandomBytes #-}
  getRandomBytes k p = lift $ getRandomBytes k p
  {-# INLINEABLE runSysCmd #-}
  runSysCmd = lift . runSysCmd
  {-# INLINEABLE initSqlDBConnection #-}
  initSqlDBConnection = lift . initSqlDBConnection
  {-# INLINEABLE deinitSqlDBConnection #-}
  deinitSqlDBConnection = lift . deinitSqlDBConnection
  {-# INLINEABLE getSqlDBConnection #-}
  getSqlDBConnection = lift . getSqlDBConnection
  {-# INLINEABLE initKVDBConnection #-}
  initKVDBConnection = lift . initKVDBConnection
  {-# INLINEABLE deinitKVDBConnection #-}
  deinitKVDBConnection = lift . deinitKVDBConnection
  {-# INLINEABLE getKVDBConnection #-}
  getKVDBConnection = lift . getKVDBConnection
  {-# INLINEABLE runDB #-}
  runDB conn = lift . runDB conn
  {-# INLINEABLE runDBWithRetry #-}
  runDBWithRetry conn = lift . runDBWithRetry conn
  {-# INLINEABLE runTransaction #-}
  runTransaction conn = lift . runTransaction conn
  {-# INLINEABLE await #-}
  await mbMcs = lift . await mbMcs
  {-# INLINEABLE runSafeFlow #-}
  runSafeFlow = lift . runSafeFlow
  {-# INLINEABLE runKVDB #-}
  runKVDB cName = lift . runKVDB cName
  {-# INLINEABLE runPubSub #-}
  runPubSub = lift . runPubSub
  {-# INLINEABLE publish #-}
  publish channel = lift . publish channel
  {-# INLINEABLE subscribe #-}
  subscribe channels = lift . subscribe channels
  {-# INLINEABLE psubscribe #-}
  psubscribe channels = lift . psubscribe channels
  {-# INLINEABLE withModifiedRuntime #-}
  withModifiedRuntime f = lift . withModifiedRuntime f
  {-# INLINEABLE fork #-}
  fork = error "Not implemented"
  {-# INLINEABLE getSnowflakeID #-}
  getSnowflakeID sId pId = lift . getSnowflakeID sId pId
  {-# INLINEABLE forkAwaitable #-}
  forkAwaitable = error "Not implemented"
  {-# INLINEABLE runInDBTransaction #-}
  runInDBTransaction conn = lift . runInDBTransaction conn
  {-# INLINEABLE runDBWithConn #-}
  runDBWithConn conn = lift . runDBWithConn conn

instance (MonadFlow m, Monoid w) => MonadFlow (RWST r w s m) where
  {-# INLINEABLE callServantAPI #-}
  callServantAPI mbMgrSel url apiTag errFunc = lift . callServantAPI mbMgrSel url apiTag errFunc
  {-# INLINEABLE callAPIUsingManager #-}
  callAPIUsingManager mgr url apiTag errFunc = lift . callAPIUsingManager mgr url apiTag errFunc
  {-# INLINEABLE lookupHTTPManager #-}
  lookupHTTPManager = lift . lookupHTTPManager
  {-# INLINEABLE getHTTPManager #-}
  getHTTPManager = lift . getHTTPManager
  {-# INLINEABLE callHTTPUsingManager #-}
  callHTTPUsingManager mgr url apiTag errFunc mskReqRespBody mbDigestDetails = lift $ callHTTPUsingManager mgr url apiTag errFunc mskReqRespBody mbDigestDetails
  {-# INLINEABLE evalLogger' #-}
  evalLogger' = lift . evalLogger'
  {-# INLINEABLE runIO' #-}
  runIO' descr = lift . runIO' descr
  {-# INLINEABLE getOption #-}
  getOption = lift . getOption
  {-# INLINEABLE setOption #-}
  setOption k = lift . setOption k
  {-# INLINEABLE setLoggerContext #-}
  setLoggerContext k = lift . setLoggerContext k
  {-# INLINEABLE getLoggerContext #-}
  getLoggerContext = lift . getLoggerContext
  {-# INLINEABLE setLoggerContextMap #-}
  setLoggerContextMap = lift . setLoggerContextMap
  {-# INLINEABLE getLoggerContextMap #-}
  getLoggerContextMap = lift getLoggerContextMap
  {-# INLINEABLE incrementArtCounter #-}
  incrementArtCounter = lift incrementArtCounter
  {-# INLINEABLE modifyOption #-}
  modifyOption fn = lift . modifyOption fn
  {-# INLINEABLE delOption #-}
  delOption = lift . delOption
  {-# INLINEABLE getOptionLocal #-}
  getOptionLocal = lift . getOptionLocal
  {-# INLINEABLE setOptionLocal #-}
  setOptionLocal k = lift . setOptionLocal k
  {-# INLINEABLE modifyOptionLocal #-}
  modifyOptionLocal fn = lift . modifyOptionLocal fn
  {-# INLINEABLE delOptionLocal #-}
  delOptionLocal = lift . delOptionLocal
  {-# INLINEABLE getFlowRuntime #-}
  getFlowRuntime = lift getFlowRuntime
  {-# INLINEABLE getRecordingLocal #-}
  getRecordingLocal = lift getRecordingLocal
  {-# INLINEABLE appendRecordingLocal #-}
  appendRecordingLocal = lift . appendRecordingLocal
  {-# INLINEABLE delRecordingLocal #-}
  delRecordingLocal = lift delRecordingLocal
  {-# INLINEABLE getConfig #-}
  getConfig k queryType = lift $ getConfig k queryType
  {-# INLINEABLE setConfig #-}
  setConfig k configEntry queryType = lift $ setConfig k configEntry queryType
  {-# INLINEABLE modifyConfig #-}
  modifyConfig k modification queryType = lift $ modifyConfig k modification queryType
  {-# INLINEABLE trySetConfig #-}
  trySetConfig k v queryType = lift $ trySetConfig k v queryType
  {-# INLINEABLE delConfig #-}
  delConfig k queryType = lift $ delConfig k queryType
  {-# INLINEABLE acquireConfigLock #-}
  acquireConfigLock = lift . acquireConfigLock
  {-# INLINEABLE releaseConfigLock #-}
  releaseConfigLock = lift . releaseConfigLock
  {-# INLINEABLE generateGUID #-}
  generateGUID = lift generateGUID
  {-# INLINEABLE generateRawGUID #-}
  generateRawGUID = lift . generateRawGUID
  {-# INLINEABLE getCurrentTime #-}
  getCurrentTime = lift getCurrentTime
  {-# INLINEABLE getPOSIXTime #-}
  getPOSIXTime = lift getPOSIXTime
  {-# INLINEABLE randomRIO #-}
  randomRIO k p = lift $ randomRIO k p
  {-# INLINEABLE getRandomBytes #-}
  getRandomBytes k p = lift $ getRandomBytes k p
  {-# INLINEABLE runSysCmd #-}
  runSysCmd = lift . runSysCmd
  {-# INLINEABLE initSqlDBConnection #-}
  initSqlDBConnection = lift . initSqlDBConnection
  {-# INLINEABLE deinitSqlDBConnection #-}
  deinitSqlDBConnection = lift . deinitSqlDBConnection
  {-# INLINEABLE getSqlDBConnection #-}
  getSqlDBConnection = lift . getSqlDBConnection
  {-# INLINEABLE initKVDBConnection #-}
  initKVDBConnection = lift . initKVDBConnection
  {-# INLINEABLE deinitKVDBConnection #-}
  deinitKVDBConnection = lift . deinitKVDBConnection
  {-# INLINEABLE getKVDBConnection #-}
  getKVDBConnection = lift . getKVDBConnection
  {-# INLINEABLE runDB #-}
  runDB conn = lift . runDB conn
  {-# INLINEABLE runDBWithRetry #-}
  runDBWithRetry conn = lift . runDBWithRetry conn
  {-# INLINEABLE runTransaction #-}
  runTransaction conn = lift . runTransaction conn
  {-# INLINEABLE await #-}
  await mbMcs = lift . await mbMcs
  {-# INLINEABLE runSafeFlow #-}
  runSafeFlow = lift . runSafeFlow
  {-# INLINEABLE runKVDB #-}
  runKVDB cName = lift . runKVDB cName
  {-# INLINEABLE runPubSub #-}
  runPubSub = lift . runPubSub
  {-# INLINEABLE publish #-}
  publish channel = lift . publish channel
  {-# INLINEABLE subscribe #-}
  subscribe channels = lift . subscribe channels
  {-# INLINEABLE psubscribe #-}
  psubscribe channels = lift . psubscribe channels
  {-# INLINEABLE withModifiedRuntime #-}
  withModifiedRuntime f = lift . withModifiedRuntime f
  {-# INLINEABLE fork #-}
  fork = error "Not implemented"
  {-# INLINEABLE getSnowflakeID #-}
  getSnowflakeID sId pId = lift . getSnowflakeID sId pId
  {-# INLINEABLE forkAwaitable #-}
  forkAwaitable = error "Not implemented"
  {-# INLINEABLE runInDBTransaction #-}
  runInDBTransaction conn = lift . runInDBTransaction conn
  {-# INLINEABLE runDBWithConn #-}
  runDBWithConn conn = lift . runDBWithConn conn



--
--
-- Additional actions
--
--




--
-- HTTP managers
--

selectManager :: MonadFlow m => ManagerSelector -> m (Maybe Manager)
selectManager m = lookupHTTPManager $ Just m

getDefaultManager :: MonadFlow m => m Manager
getDefaultManager = fromJust <$> lookupHTTPManager Nothing

getMgr :: MonadFlow m => Maybe ManagerSelector -> ExceptT ClientError m Manager
getMgr mgrSel =
  ExceptT $ case mgrSel of
    Nothing  -> Right <$> getDefaultManager
    Just sel@(ManagerSelector name) -> do
      mmgr <- selectManager sel
      case mmgr of
        Just mgr -> pure $ Right mgr
        Nothing  -> pure $ Left $
          ConnectionError $ toException $ HttpManagerNotFound name

--
-- Untyped HTTP calls
--

-- | Method for calling external HTTP APIs without bothering with types with custom manager.
--
-- Thread safe, exception free.
--
-- Takes remote url, optional custom manager selector and returns either client error or result.
--
-- > myFlow = do
-- >   book <- callHTTPWithManager url mSel

callHTTP'
  :: (HasCallStack, MonadFlow m, Show apiTag)
  => Maybe ManagerSelector              -- ^ Selector
  -> HTTPRequest                        -- ^ remote url 'Text'
  -> apiTag
  -> (HTTPResponse -> Maybe Log.ErrorInfo)
  -> Maybe MaskReqRespBody
  -> Maybe (Text, Text)
  -> m (Either Text.Text HTTPResponse)  -- ^ result
callHTTP' mSel req apiTag errFunc mbMskReqRespBody mbDigestDetails = do
    runExceptT $ withExceptT show (getMgr mSel) >>= (\mngr -> ExceptT $ callHTTPUsingManager mngr req apiTag errFunc mbMskReqRespBody mbDigestDetails)
    
{-# DEPRECATED callHTTPWithManager "Use callHTTP' instead. This method has a confusing name, as it accepts a selector not a manager." #-}
callHTTPWithManager
  :: (HasCallStack, MonadFlow m, Show apiTag)
  => Maybe ManagerSelector              -- ^ Selector
  -> HTTPRequest                        -- ^ remote url 'Text'
  -> apiTag
  -> (HTTPResponse -> Maybe Log.ErrorInfo)
  -> Maybe (Text, Text)
  -> m (Either Text.Text HTTPResponse)  -- ^ result
callHTTPWithManager mSel req apiTag errFunc mbDigestDetails = callHTTP' mSel req apiTag errFunc Nothing mbDigestDetails

-- applies custom masking function while logging outgoing request
callHTTPWithManager'
  :: (HasCallStack, MonadFlow m, Show apiTag)
  => Maybe ManagerSelector              -- ^ Selector
  -> HTTPRequest                        -- ^ remote url 'Text'
  -> apiTag
  -> (HTTPResponse -> Maybe Log.ErrorInfo)
  -> Maybe MaskReqRespBody
  -> Maybe (Text, Text)
  -> m (Either Text.Text HTTPResponse)  -- ^ result
callHTTPWithManager' = callHTTP'

-- | The same as callHTTP' but uses the default HTTP manager.
--
-- Thread safe, exception free.
--
-- Takes remote url and returns either client error or result.
--
-- > myFlow = do
-- >   book <- callHTTP url

callHttpWithDigest :: (HasCallStack, MonadFlow m, Show apiTag) =>
  HTTPRequest -> apiTag -> (HTTPResponse -> Maybe Log.ErrorInfo) -> (Text, Text) -> m (Either Text.Text HTTPResponse)
callHttpWithDigest httpRequest apiTag mbRespToErrInfo digestDetails = callHTTPWithManager Nothing httpRequest apiTag mbRespToErrInfo (Just digestDetails)

callHTTP :: (HasCallStack, MonadFlow m, Show apiTag) =>
  HTTPRequest -> apiTag -> (HTTPResponse -> Maybe Log.ErrorInfo) -> m (Either Text.Text HTTPResponse)
callHTTP httpRequest apiTag mbRespToErrInfo = callHTTPWithManager Nothing httpRequest apiTag mbRespToErrInfo Nothing

{-# DEPRECATED callHTTPWithCert    "Use getHTTPManager/callHTTPUsingManager instead. This method does not allow custom CA store." #-}
callHTTPWithCert :: (MonadFlow m, Show apiTag) => HTTPRequest -> apiTag -> (HTTPResponse -> Maybe Log.ErrorInfo) -> Maybe HTTPCert -> m (Either Text HTTPResponse)
callHTTPWithCert req apiTag errFunc cert  = do
  mgr <- maybe getDefaultManager (getHTTPManager . withClientTls) cert
  callHTTPUsingManager mgr req apiTag errFunc Nothing Nothing

-- applies custom masking function while logging outgoing request
callHTTPWithCert' :: (Show apiTag, MonadFlow m) => HTTPRequest -> Maybe HTTPCert -> apiTag -> (HTTPResponse -> Maybe Log.ErrorInfo) -> Maybe MaskReqRespBody-> m (Either Text HTTPResponse)
callHTTPWithCert' req cert apiTag errFunc mskReqRespBody = do
  mgr <- maybe getDefaultManager (getHTTPManager . withClientTls) cert
  callHTTPUsingManager mgr req apiTag errFunc mskReqRespBody Nothing

-- To extract the MySQL connection
-- Restricted Function, not for external usecase
getMySQLConnection :: (HasCallStack, MonadIO m, MonadFlow m) => DBConfig beM -> m (Either Text MySQL.MySQLConn)
getMySQLConnection dbConf = do
  eConn <- getSqlDBConnection dbConf
  case eConn of
    Right (T.MySQLPool _ pool) -> do
          myConn <- liftIO $ DP.withResource pool pure
          pure $ Right myConn
    Right _ -> pure $ Left "Could not fetch MySQL DB connection"
    Left err -> pure $ Left $ "Error in getting connection: " <> (show err)
--
-- Well-typed HTTP calls
--

-- | Method for calling external HTTP APIs using the facilities of servant-client.
-- Allows to specify what manager should be used. If no manager found,
-- `HttpManagerNotFound` will be returne (as part of `ClientError.ConnectionError`).
--
-- Thread safe, exception free.
--
-- Alias for callServantAPI.
--
-- | Takes remote url, servant client for this endpoint
-- and returns either client error or result.
--
-- > data User = User { firstName :: String, lastName :: String , userGUID :: String}
-- >   deriving (Generic, Show, Eq, ToJSON, FromJSON )
-- >
-- > data Book = Book { author :: String, name :: String }
-- >   deriving (Generic, Show, Eq, ToJSON, FromJSON )
-- >
-- > type API = "user" :> Get '[JSON] User
-- >       :<|> "book" :> Get '[JSON] Book
-- >
-- > api :: HasCallStack => Proxy API
-- > api = Proxy
-- >
-- > getUser :: HasCallStack => EulerClient User
-- > getBook :: HasCallStack => EulerClient Book
-- > (getUser :<|> getBook) = client api
-- >
-- > url = BaseUrl Http "localhost" port ""
-- >
-- >
-- > myFlow = do
-- >   book <- callAPI url getBook
-- >   user <- callAPI url getUser
callAPI' :: (HasCallStack, MonadFlow m, Show apiTag) =>
  Maybe ManagerSelector -> BaseUrl -> apiTag -> (LBS.ByteString -> Maybe Log.ErrorInfo) -> EulerClient a -> m (Either ClientError a)
callAPI' = callServantAPI

-- | The same as `callAPI'` but with default manager to be used.
callAPI :: (HasCallStack, MonadFlow m, Show apiTag) =>
  BaseUrl -> apiTag -> (LBS.ByteString -> Maybe Log.ErrorInfo) -> EulerClient a -> m (Either ClientError a)
callAPI = callServantAPI Nothing


-- TODO: save a builder in some state for using `hPutBuilder`?
--
-- Doubts:
-- Is it the right place to put it?
-- Should the type be more generic than IO ()?
logCallStack :: (HasCallStack, MonadFlow m) => m ()
logCallStack = logDebug ("CALLSTACK" :: Text) $ Text.pack $ prettyCallStack callStack

-- customPrettyCallStack :: Int -> CallStack -> String
-- customPrettyCallStack numLines stack =
--   let stackLines = prettyCallStackLines stack
--       lastNumLines = takeEnd numLines stackLines
--    in "CallStack: " ++ intercalate "; " lastNumLines

logExceptionCallStack :: (HasCallStack, Exception e, MonadFlow m) => e -> m ()
logExceptionCallStack ex =
  let errorReason = Text.pack $ displayException ex
    in logErrorWithCategory ("EXCEPTION" :: Text) errorReason $ ErrorL Nothing "EXCEPTION" errorReason

foldFlow :: (Monad m) => (forall b . FlowMethod b -> m b) -> Flow a -> m a
foldFlow f (Flow comp) = foldF f comp

type ReaderFlow r = ReaderT r Flow

newtype PubSub a = PubSub {
  unpackLanguagePubSub :: HasCallStack => (forall b . Flow b -> IO b) -> PSL.PubSub a
  }

type MessageCallback
    =  ByteString  -- ^ Message payload
    -> Flow ()

type PMessageCallback
    =  ByteString  -- ^ Channel name
    -> ByteString  -- ^ Message payload
    -> Flow ()

-- | MonadBaseControl/UnliftIO-like interface for flow.
--
-- > withSomeResourceFromIO :: (SomeRes -> IO a) -> IO a
-- > someFlowAction :: SomeRes -> Flow Result
-- >
-- > example :: Flow Result
-- > example = do
-- >   withRunFlow \runFlow -> do
-- >     withSomeResourceFromIO \res -> do
-- >       runFlow (someFlowAction res)
withRunFlow :: ((forall x. Flow x -> IO x) -> IO a) -> Flow a
withRunFlow ioAct = liftFC $ WithRunFlow ioAct

-- | Fork a unit-returning flow.
--
-- __Note__: to fork a flow which yields a value use 'forkFlow\'' instead.
--
-- __Warning__: With forked flows, race coniditions and dead / live blocking become possible.
-- All the rules applied to forked threads in Haskell can be applied to forked flows.
--
-- Generally, the method is thread safe. Doesn't do anything to bookkeep the threads.
-- There is no possibility to kill a thread at the moment.
--
-- Thread safe, exception free.
--
-- > myFlow1 = do
-- >   logInfoT "myflow1" "logFromMyFlow1"
-- >   someAction
-- >
-- > myFlow2 = do
-- >   _ <- runIO someAction
-- >   forkFlow "myFlow1 fork" myFlow1
-- >   pure ()
--

forkFlow :: HasCallStack => Description -> Flow a -> Flow ()
forkFlow description flow = do
  flowGUID <- generateGUIDWithTag $ "forkFlow::flowGUID_" <> description
  void $ forkFlow'' description flowGUID $ do
    eitherResult <- runSafeFlow flow
    case eitherResult of
      Left msg -> logErrorWithCategory ("forkFlow" :: Text) msg $ ErrorL Nothing "forkFlow" msg
      Right _  -> pure ()

-- | Same as 'forkFlow', but takes @Flow a@ and returns an 'T.Awaitable' which can be used
-- to reap results from the flow being forked.
--
-- > myFlow1 = do
-- >   logInfoT "myflow1" "logFromMyFlow1"
-- >   pure 10
-- >
-- > myFlow2 = do
-- >   awaitable <- forkFlow' "myFlow1 fork" myFlow1
-- >   await Nothing awaitable
--
forkFlow'' :: HasCallStack =>
  Description -> ForkGUID -> Flow a -> Flow (Awaitable (Either Text a))
forkFlow'' description flowGUID flow = do
    logDebug ("ForkFlow" :: Text) $ case Text.uncons description of
      Nothing ->
        "Flow forked. Description: " +| description |+ " GUID: " +| flowGUID |+ ""
      Just _  -> "Flow forked. GUID: " +| flowGUID |+ ""
    liftFC $ Fork description flowGUID flow id

forkFlow' :: HasCallStack =>
  Description -> Flow a -> Flow (Awaitable (Either Text a))
forkFlow' description flow = do
    flowGUID <- generateGUIDWithTag $ "forkFlow'::flowGUID_" <> description
    logDebug ("ForkFlow" :: Text) $ case Text.uncons description of
      Just _ ->
        "Flow forked. Description: " +| description |+ " GUID: " +| flowGUID |+ ""
      Nothing  -> "Flow forked. GUID: " +| flowGUID |+ ""
    liftFC $ Fork description flowGUID flow id

class Loggable a where
  toLogValue :: a -> A.Value

instance {-# OVERLAPPABLE #-} (ToJSON a) => Loggable a where
  toLogValue = toJSON

logM :: forall (tag :: Type) (m :: Type -> Type) msg val .
  (HasCallStack, MonadFlow m, Show tag, Typeable tag, Loggable msg, Loggable val) => LogLevel -> tag -> msg -> val -> Maybe ErrorL -> m ()
logM logLvl tag m v err
  | logLvl == Error = 
    let jsonVal = toLogValue v
        e = err <|> (Just $ ErrorL Nothing "DOMAIN_ERROR" $ show jsonVal)
      in evalLogger' $ masterLogger logLvl tag "ERROR" Nothing Nothing Nothing Nothing e Nothing Nothing Nothing Nothing Nothing Nothing (Message (Just $ toLogValue m) (Just jsonVal)) Nothing
  | otherwise = evalLogger' $ masterLogger logLvl tag "DOMAIN" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Message (Just $ toLogValue m) (Just $ toLogValue v)) Nothing

log :: forall (tag :: Type) (m :: Type -> Type) .
  (HasCallStack, MonadFlow m, Show tag, Typeable tag) => LogLevel -> tag -> Text -> Maybe ErrorL -> m ()
log logLvl tag msg err
  | logLvl == Error =
    let e = err <|> (Just $ ErrorL Nothing "DOMAIN_ERROR" msg)
      in evalLogger' $ masterLogger logLvl tag "ERROR" Nothing Nothing Nothing Nothing e Nothing Nothing Nothing Nothing Nothing Nothing (Message (Just $ A.toJSON msg) Nothing) Nothing
  | otherwise = evalLogger' $ masterLogger logLvl tag "DOMAIN" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Message (Just $ A.toJSON msg) Nothing) Nothing

logV :: forall (tag :: Type) (m :: Type -> Type) val .
  (HasCallStack, MonadFlow m, Show tag, Typeable tag, Loggable val) => LogLevel -> tag -> val -> Maybe ErrorL -> m ()
logV logLvl tag v err
  | logLvl == Error =
    let jsonVal = toLogValue v
        e = err <|> (Just $ ErrorL Nothing "DOMAIN_ERROR" $ show jsonVal)
      in evalLogger' $ masterLogger logLvl tag "ERROR" Nothing Nothing Nothing Nothing e Nothing Nothing Nothing Nothing Nothing Nothing (Message Nothing (Just jsonVal)) Nothing
  | otherwise = evalLogger' $ masterLogger logLvl tag "DOMAIN" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Message Nothing (Just $ toLogValue v)) Nothing

encryptValue :: (HasCallStack, MonadFlow m) => Text -> m Text
encryptValue v = do
  mKey <- getOption LogEncryptionKey
  mIV <- getOption LogEncryptionIV
  let encV = 
        case (mKey, mIV) of
          (Just key, Just iv) -> either id id $ EE.aesEncryptText key iv v
          (Just _, _) -> "Couldn't find LogEncryptionIV"
          (_, Just _) -> "Couldn't find LogEncryptionKey"
          (_, _) -> "Couldn't find both LogEncryptionKey and LogEncryptionIV"
  pure encV

-- | Log message with Info level.
--
-- Thread safe.

logInfoM :: forall (tag :: Type) (m :: Type -> Type) msg val .
  (HasCallStack, MonadFlow m, Show tag, Typeable tag, Loggable msg, Loggable val) => tag -> msg -> val -> m ()
logInfoM t m v = logM Info t m v Nothing

logInfo :: forall (tag :: Type) (m :: Type -> Type) .
  (HasCallStack, MonadFlow m, Show tag, Typeable tag) => tag -> Text -> m ()
logInfo t v = log Info t v Nothing

logEncryptedInfo :: forall (tag :: Type) (m :: Type -> Type) .
  (HasCallStack, MonadFlow m, Show tag, Typeable tag) => tag -> Text -> m ()
logEncryptedInfo t v = do
  encV <- encryptValue v
  log Info t encV Nothing

logInfoV :: forall (tag :: Type) (m :: Type -> Type) val .
  (HasCallStack, MonadFlow m, Show tag, Typeable tag, Loggable val) => tag -> val -> m ()
logInfoV t v = logV Info t v Nothing


-- | Log message with Error level.
--
-- Thread safe.
logErrorM :: forall (tag :: Type) (m :: Type -> Type) msg val .
  (HasCallStack, MonadFlow m, Show tag, Typeable tag, Loggable msg, Loggable val) => tag -> msg -> val -> m ()
logErrorM t m v = logM Error t m v Nothing

logError :: forall (tag :: Type) (m :: Type -> Type) .
  (HasCallStack, MonadFlow m, Show tag, Typeable tag) => tag -> Text -> m ()
logError t v = log Error t v Nothing

logEncryptedError :: forall (tag :: Type) (m :: Type -> Type) .
  (HasCallStack, MonadFlow m, Show tag, Typeable tag) => tag -> Text -> m ()
logEncryptedError t v = do
  encV <- encryptValue v
  log Error t encV Nothing

logErrorV :: forall (tag :: Type) (m :: Type -> Type) val .
  (HasCallStack, MonadFlow m, Show tag, Typeable tag, Loggable val) => tag -> val -> m ()
logErrorV t v = logV Error t v Nothing

logErrorWithCategoryV :: forall (tag :: Type) (m :: Type -> Type) val .
  (HasCallStack, MonadFlow m, Show tag, Typeable tag, Loggable val) => tag -> val -> ErrorL -> m ()
logErrorWithCategoryV tag v (ErrorL mbErrCode errCategory errReason) = logV Error ("ERROR_TRACKING" :: Text) v $ Just (ErrorL (mbErrCode <|> Just textTag) errCategory errReason)
  where
    textTag :: Text
    textTag
      | Just HRefl <- eqTypeRep (typeRep @tag) (typeRep @Text  ) = tag
      | Just HRefl <- eqTypeRep (typeRep @tag) (typeRep @String) = toText tag
      | otherwise = show tag

logErrorWithCategory :: forall (tag :: Type) (m :: Type -> Type) .
  (HasCallStack, MonadFlow m, Show tag, Typeable tag) => tag -> Text -> ErrorL -> m ()
logErrorWithCategory tag v (ErrorL mbErrCode errCategory errReason) = log Error ("ERROR_TRACKING" :: Text) v $ Just (ErrorL (mbErrCode <|> Just textTag) errCategory errReason)
  where
    textTag :: Text
    textTag
      | Just HRefl <- eqTypeRep (typeRep @tag) (typeRep @Text  ) = tag
      | Just HRefl <- eqTypeRep (typeRep @tag) (typeRep @String) = toText tag
      | otherwise = show tag
-- | Log message with Debug level.
--
-- Thread safe.
logDebugM :: forall (tag :: Type) (m :: Type -> Type) msg val .
  (HasCallStack, MonadFlow m, Show tag, Typeable tag, Loggable msg, Loggable val) => tag -> msg -> val -> m ()
logDebugM t m v = logM Debug t m v Nothing

logDebug :: forall (tag :: Type) (m :: Type -> Type) .
  (HasCallStack, MonadFlow m, Show tag, Typeable tag) => tag -> Text -> m ()
logDebug t m = log Debug t m Nothing

logDebugV :: forall (tag :: Type) (m :: Type -> Type) val .
  (HasCallStack, MonadFlow m, Show tag, Typeable tag, Loggable val) => tag -> val -> m ()
logDebugV t v = logV Debug t v Nothing

-- | Log message with Warning level.
--
-- Thread safe.
logWarningM :: forall (tag :: Type) (m :: Type -> Type) msg val .
  (HasCallStack, MonadFlow m, Show tag, Typeable tag, Loggable msg, Loggable val) => tag -> msg -> val -> m ()
logWarningM t m v = logM Warning t m v Nothing

logWarning :: forall (tag :: Type) (m :: Type -> Type) .
  (HasCallStack, MonadFlow m, Show tag, Typeable tag) => tag -> Text -> m ()
logWarning t v = log Warning t v Nothing

logWarningV :: forall (tag :: Type) (m :: Type -> Type) val .
  (HasCallStack, MonadFlow m, Show tag, Typeable tag, Loggable val) => tag -> val -> m ()
logWarningV t v = logV Warning t v Nothing

logException :: (HasCallStack, MonadFlow m) => SomeException -> m ()
logException exception =
  let exceptionLogEntry = getErrorLogFromException exception
    in logErrorWithCategoryV ("ERROR_TRACKING" :: Text) exceptionLogEntry $ getErrorLog exceptionLogEntry
  where 
    getErrorLog e = ErrorL (Just $ jp_error_code e) (error_code e) (Text.pack $ error_message e)

-- | Run some IO operation, result should have 'ToJSONEx' instance (extended 'ToJSON'),
-- because we have to collect it in recordings for ART system.
--
-- Warning. This method is dangerous and should be used wisely.
--
-- > myFlow = do
-- >   content <- runIO $ readFromFile file
-- >   logDebugT "content id" $ extractContentId content
-- >   pure content
{-# DEPRECATED runIO "Use runIOWithART instead." #-}
runIO :: (HasCallStack, MonadFlow m) => IO a -> m a
runIO = runIO' ""

generateGUIDWithTag :: (MonadFlow m) => Text -> m Text
generateGUIDWithTag tag = UUID.toText <$> generateRawGUID tag

-------------------------------------------------------
incrementDbAndRedisMetric :: MonadFlow m => DBAndRedisMetricHandler -> DBAndRedisMetric -> Text -> Text -> m ()
incrementDbAndRedisMetric handle metric dbName hostName = do
  runIO $ ((dBAndRedisCounter handle) (metric, dbName, hostName))


mkDBAndRedisMetricHandler :: IO DBAndRedisMetricHandler
mkDBAndRedisMetricHandler = do
  metrics <- register collectionLock
  pure $ DBAndRedisMetricHandler $ \case
    (ConnectionLost, dbName, hostName)   ->
      inc (metrics </> #connection_lost) dbName hostName
    (ConnectionFailed, dbName, hostName)    ->
      inc (metrics </> #connection_failed) dbName hostName
    (ConnectionDoesNotExist, dbName, hostName)    ->
      inc (metrics </> #connection_doesnot_exist) dbName hostName
    (ConnectionAlreadyExists, dbName, hostName)    ->
      inc (metrics </> #connection_already_exists) dbName hostName
    (TransactionRollbacked, dbName, hostName)    ->
      inc (metrics </> #transaction_rollbacked) dbName hostName
    -- (SQLQueryError,dbName, hostName)    ->
    --   inc (metrics </> #sql_query_error) dbName hostName
    (UnrecognizedDBError, dbName, hostName)    ->
      inc (metrics </> #unrecognized_db_error) dbName hostName
    (UnexpectedDBResult, dbName, hostName)    ->
      inc (metrics </> #unexpected_db_result) dbName hostName
    (RedisExceptionMessage, dbName, hostName)    ->
      inc (metrics </> #redis_exception_msg) dbName hostName

connection_lost = counter #connection_lost
      .& lbl @"db_name" @Text
      .& lbl @"host_name" @Text
      .& build

connection_failed = counter #connection_failed
      .& lbl @"db_name" @Text
      .& lbl @"host_name" @Text
      .& build

connection_doesnot_exist = counter #connection_doesnot_exist
      .& lbl @"db_name" @Text
      .& lbl @"host_name" @Text
      .& build

connection_already_exists = counter #connection_already_exists
      .& lbl @"db_name" @Text
      .& lbl @"host_name" @Text
      .& build

-- sql_query_error = counter #sql_query_error
--       .& lbl @"db_name" @Text
--       .& lbl @"host_name" @Text
--       .& build

transaction_rollbacked = counter #transaction_rollbacked
      .& lbl @"db_name" @Text
      .& lbl @"host_name" @Text
      .& build

unrecognized_db_error = counter #unrecognized_db_error
      .& lbl @"db_name" @Text
      .& lbl @"host_name" @Text
      .& build


unexpected_db_result = counter #unexpected_db_result
      .& lbl @"db_name" @Text
      .& lbl @"host_name" @Text
      .& build

redis_exception_msg = counter #redis_exception_msg
      .& lbl @"db_name" @Text
      .& lbl @"host_name" @Text
      .& build

collectionLock =
     connection_lost
  .> connection_failed
  .> connection_doesnot_exist
  .> connection_already_exists
--   .> sql_query_error
  .> transaction_rollbacked
  .> unrecognized_db_error
  .> unexpected_db_result
  .> redis_exception_msg
  .> MNil
