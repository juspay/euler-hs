
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module KV.FlowHelper where
import qualified Data.Map as Map
import qualified EulerHS.Interpreters as I
import qualified EulerHS.Language as L
import           EulerHS.Prelude
import qualified EulerHS.Runtime as R
import qualified EulerHS.Types as T
import qualified Database.Beam.MySQL as BM
import qualified Data.Text as Text
import           Test.Hspec
import           Database.Beam.MySQL (MySQLM)
import           System.Environment (getEnvironment)
import           System.IO.Unsafe (unsafePerformIO)
import qualified Data.Time as Time
import qualified Data.Aeson as A
import Data.Aeson
import           Data.HashMap.Strict (lookup)
import qualified EulerHS.Extra.EulerDB as Extra
import qualified System.Logger as Log
import qualified Data.ByteString.Builder as BB
import           System.Directory(getCurrentDirectory)

type FlowSpec = SpecWith R.FlowRuntime

itFlow :: [Char] -> L.Flow () -> FlowSpec
itFlow description flow =
    it description (`I.runFlow` flow)

xitFlow :: [Char] -> L.Flow () -> FlowSpec
xitFlow description flow =
    xit description (`I.runFlow` flow)

-- NOTE
-- This is a the flowruntime for the KV tests
-- Log to file is enabled and log to console is disabled
-- so that the test report are more readable and don't get
-- entangled with the application logs
-- The file to which logs are added is present in the _logFilePath
-- field below

flowSpec :: FlowSpec -> Spec
flowSpec = do
    aroundAll $ \tests -> do
      dir <- getCurrentDirectory
      R.withFlowRuntime (Just $ logger dir) $ \rt -> do
        I.runFlow rt preparePSqlConnection
        tests rt

  where
    logToConsole = False -- Set this to true if you want to see application logs in the console
    logToFile = True
    logSql = T.UnsafeLogSQL_DO_NOT_USE_IN_PRODUCTION -- This enabled logging raw SQL queries for debugging
    loggerCfg dir =
        T.LoggerConfig
          { T._logToFile = logToFile,
            T._logFilePath = dir <> "/testlog.json",
            T._isAsync = False,
            T._logLevel = T.Debug,
            T._logToConsole = logToConsole,
            T._maxQueueSize = 1000,
            T._logRawSql = logSql,
            T._logMaskingConfig = Nothing,
            T._logAPI = True
          }
    logger dir = R.createLoggerRuntime' Nothing (Just defaultRenderer) 4096 psMimicFlowFormatter Nothing $ loggerCfg dir

defaultRenderer :: ByteString -> p1 -> p2 -> [Log.Element] -> BB.Builder
defaultRenderer s _ _ (_:es) = Log.renderDefault s es
defaultRenderer s _ _ []     = Log.renderDefault s []

asserting :: Expectation -> L.Flow ()
asserting = L.runIO

preparePSqlConnection :: (HasCallStack, L.MonadFlow m) => m ()
preparePSqlConnection = do
  psqlDbCfg <- L.runIO mysqlDBC
  epool <- L.initSqlDBConnection psqlDbCfg
  kv <- L.initKVDBConnection kvdbClusterConfig
  L.throwOnFailedWithLog kv L.KVDBConnectionFailedException "Failed to connect to Redis Cluster DB."
  L.throwOnFailedWithLog epool L.SqlDBConnectionFailedException "Failed to connect to SQLite DB."
  L.setOption Extra.EulerDbCfg psqlDbCfg

mysqlDBC :: IO (T.DBConfig MySQLM)
mysqlDBC = do
  mySqlConfig <- getMySQLCfg
  pure $ T.mkMySQLPoolConfig (Text.pack devMysqlConnectionName) mySqlConfig mySqlPoolConfig

kvdbClusterConfig :: T.KVDBConfig
kvdbClusterConfig = T.mkKVDBClusterConfig kvRedis redisClusterConfig

mySqlPoolConfig :: T.PoolConfig
mySqlPoolConfig = T.PoolConfig
    { stripes = devMysqlPoolStripes
    , keepAlive = fromInteger devMysqlPoolKeepAlive
    , resourcesPerStripe = devMysqlPoolResourcesPerStripe
    }

kvRedis :: Text
kvRedis = "KVRedis"

getMySQLCfg :: IO T.MySQLConfig
getMySQLCfg =
    pure T.MySQLConfig
          { connectHost     = devMysqlConnectHost
          , connectPort     = devMysqlConnectPort
          , connectUser     = devMysqlConnectUser
          , connectPassword = devMysqlConnectPassword
          , connectDatabase = devMysqlConnectDatabase
          , connectOptions  = []
          , connectPath     = devMysqlConnectPath
          , connectSSL      = Nothing
          , connectCharset  = T.Latin1
          }

redisClusterConfig :: T.RedisConfig
redisClusterConfig =
  T.RedisConfig
    { connectHost           = devRedisClusterHost
    , connectPort           = devRedisClusterPort
    , connectAuth           = Nothing
    , connectDatabase       = devRedisClusterDatabase
    , connectReadOnly       = False
    , connectMaxConnections = devRedisClusterMaxConnections
    , connectMaxIdleTime    = fromInteger devRedisClusterMaxIdleTime
    , connectTimeout        = Nothing
    }

devMysqlConnectionName :: String
devMysqlConnectionName = fromMaybe "eulerMysqlDB" $ lookupEnv "DEV_MYSQL_CONNECTION_NAME"

devMysqlConnectHost :: String
devMysqlConnectHost = fromMaybe "localhost" $ lookupEnv "DEV_MYSQL_CONNECT_HOST"

devMysqlConnectPort :: Word16
devMysqlConnectPort = fromMaybe 3306 $ readMaybe =<< lookupEnv "DEV_MYSQL_CONNECT_PORT"

devMysqlConnectUser :: String
devMysqlConnectUser = fromMaybe "cloud" $ lookupEnv "DEV_MYSQL_CONNECT_USER"

devMysqlConnectPassword :: String
devMysqlConnectPassword = fromMaybe "scape" $ lookupEnv "DEV_MYSQL_CONNECT_PASSWORD"

devMysqlConnectDatabase :: String
devMysqlConnectDatabase = fromMaybe "jdb" $ lookupEnv "DEV_MYSQL_CONNECT_DATABASE"

devMysqlConnectPath :: String
devMysqlConnectPath = fromMaybe "" $ lookupEnv "DEV_MYSQL_CONNECT_PATH"

devMysqlPoolStripes :: Int
devMysqlPoolStripes = fromMaybe 1 $ readMaybe =<< lookupEnv "DEV_MYSQL_POOL_STRIPES"

devMysqlPoolKeepAlive :: Integer
devMysqlPoolKeepAlive = fromMaybe 10 $ readMaybe =<< lookupEnv "DEV_MYSQL_POOL_KEEP_ALIVE"

devMysqlPoolResourcesPerStripe :: Int
devMysqlPoolResourcesPerStripe = fromMaybe 50 $ readMaybe =<< lookupEnv "DEV_MYSQL_POOL_RESOURCES_PER_STRIPE"

devRedisClusterHost :: String
devRedisClusterHost = fromMaybe "localhost" $ lookupEnv "DEV_REDIS_CLUSTER_CONNECT_HOST"

devRedisClusterPort :: Word16
devRedisClusterPort = fromMaybe 30001 $ readMaybe =<< lookupEnv "DEV_REDIS_CLUSTER_CONNECT_PORT"

devRedisClusterDatabase :: Integer
devRedisClusterDatabase = fromMaybe 0 $ readMaybe =<< lookupEnv "DEV_REDIS_CLUSTER_CONNECT_DATABASE"

devRedisClusterMaxConnections :: Int
devRedisClusterMaxConnections = fromMaybe 50 $ readMaybe =<< lookupEnv "DEV_REDIS_CLUSTER_CONNECT_MAX_CONNECTIONS"

devRedisClusterMaxIdleTime :: Integer
devRedisClusterMaxIdleTime = fromMaybe 30 $ readMaybe =<< lookupEnv "DEV_REDIS_CLUSTER_CONNECT_MAX_IDLE_TIME"

{-# NOINLINE environmentVars #-}
environmentVars :: Map String String
environmentVars = Map.fromList $ unsafePerformIO getEnvironment

lookupEnv :: String -> Maybe String
lookupEnv k = Map.lookup k environmentVars

data OfferEngineCfg = OfferEngineCfg
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance T.OptionEntity OfferEngineCfg (T.DBConfig BM.MySQLM)

psMimicFlowFormatter :: Maybe T.FlowGUID -> IO (T.PendingMsg -> T.MessageBuilder)   -- T.FlowFormatter
psMimicFlowFormatter _ = do
  currTime <- Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S%3Q" <$> Time.getCurrentTime
  pure $! aesonPSMimicFormatterText (Text.pack currTime)

aesonPSMimicFormatterText
  :: Text       -- timestamp
  -> T.MessageFormatter
aesonPSMimicFormatterText timestamp (T.V1 _mbFlowGuid lvl tag msg msgNum logContext) = res
  where
    logEntry = PSLogEntry
          { _timestamp                = timestamp
          , _app_framework            = appFramework
          , _hostname                 = hostname
          , _source_commit            = "sourceCommit"
          , _env                      = env
          , _level                    = show lvl
          , _merchant_id              = fromMaybe "null" $ lookup "merchantId" logContext
          , _message_number           = show msgNum
          , _x_request_id             = fromMaybe "null" $ lookup "x-request-id" logContext
          , _x_global_request_id      = fromMaybe "null" $ lookup "x-global-request-id" logContext
          , _action                   = if isOutgoingAPICall tag then outgoingApiTag else "null"
          , _label                    = if isOutgoingAPICall tag then "UPSTREAM" else "null"
          , _message                  = msg.msgMessage
          , _message_type             = if isJust msg.msgValue then "json" else "string"
          , _value                    = msg.msgValue
          , _tag                      = tag
          , _notification_id          = fromMaybe "null" $ lookup "notification_id" logContext
          }
    res = T.SimpleLBS $ A.encode logEntry
aesonPSMimicFormatterText timestamp (T.V2 _mbFlowGuid lvl category action entity errorL latency respcode message msgnumber logContext) = res
  where
    logEntry = V2LogEntry
          { _timestamp = timestamp
          , _level  = show lvl
          , _env  = env
          , _app_framework = appFramework
          , _schema_version = "V2"
          , _hostname = "hostname"
          , _category = category
          , _action  = action
          , _entity  = entity
          , _latency = show <$> latency
          , _resp_code = respcode
          , _request_id = fromMaybe "null" $ lookup "x-request-id" logContext
          , _merchant_id = lookup "merchantId" logContext
          , _error_code = getErrorCode
          , _error_category = getErrorCategory
          , _error_reason = getErrorReason
          , _message = message.msgMessage
          , _message_number = show msgnumber
          }
    getErrorCode  = (\ (T.ErrorL e  _ _) -> e) =<< errorL
    getErrorCategory = (\ (T.ErrorL _ c _) -> c) <$> errorL
    getErrorReason = (\ (T.ErrorL _ _ r) -> r) <$> errorL

    res = T.SimpleLBS $ A.encode logEntry

data V2LogEntry strType = V2LogEntry
  { _timestamp :: strType
  , _level :: strType
  , _env :: strType
  , _app_framework :: strType
  , _schema_version :: strType
  , _hostname :: strType
  , _category :: strType
  , _action :: Maybe strType
  , _entity :: Maybe strType
  , _latency :: Maybe strType
  , _resp_code :: Maybe Int
  , _request_id :: strType
  , _merchant_id :: Maybe strType
  , _error_code :: Maybe strType
  , _error_category :: Maybe strType
  , _error_reason :: Maybe strType
  , _message :: Maybe A.Value
  , _message_number :: strType
  }

instance A.ToJSON (V2LogEntry Text) where
  toJSON V2LogEntry {..} =
    A.object [ "timestamp" A..= _timestamp
             , "level" A..= _level
             , "env" A..= _env
             , "app_framework" A..= _app_framework
             , "schema_version" A..= _schema_version
             , "hostname" A..= _hostname
             , "category" A..= _category
             , "action" A..= _action
             , "entity" A..= _entity
             , "latency" A..= _latency
             , "resp_code" A..= _resp_code
             , "request_id" A..= _request_id
             , "merchant_id" A..= _merchant_id
             , "error_code" A..= _error_code
             , "error_category" A..= _error_category
             , "error_reason" A..= _error_reason
             , "message" A..= _message
             , "message_number" A..= _message_number
             ]

data PSLogEntry strType = PSLogEntry
  { _timestamp                :: strType
  , _app_framework            :: strType
  , _hostname                 :: strType
  , _source_commit            :: strType
  , _env                      :: strType
  , _level                    :: strType
  , _action                   :: strType
  , _label                    :: strType
  , _merchant_id              :: strType
  , _message_number           :: strType
  , _x_request_id             :: strType
  , _x_global_request_id      :: strType
  , _message                  :: Maybe A.Value
  , _message_type             :: strType
  , _value                    :: Maybe A.Value
  , _tag                      :: strType
  , _notification_id          :: strType
  }

instance A.ToJSON (PSLogEntry Text) where
  toJSON PSLogEntry {..} =
    A.object [ "timestamp"                A..= _timestamp
             , "app_framework"            A..= _app_framework
             , "hostname"                 A..= _hostname
             , "source_commit"            A..= _source_commit
             , "env"                      A..= _env
             , "level"                    A..= _level
             , "merchant_id"              A..= _merchant_id
             , "message_number"           A..= _message_number
             , "action"                   A..= _action
             , "label"                    A..= _label
             , "x-request-id"             A..= _x_request_id
             , "x-global-request-id"      A..= _x_global_request_id
             , "tag"                      A..= _tag
             , "message"                  A..= _message
             , "message_type"             A..= _message_type
             , "value"                    A..= _value
             , "notification_id"          A..= _notification_id
             ]

  toEncoding PSLogEntry{..} = A.pairs $ mconcat [
    "timestamp"                  .= _timestamp,
    "hostname"                   .= _hostname,
    "app_framework"              .= _app_framework,
    "env"                        .= _env,
    "level"                      .= _level,
    "x-request-id"               .= _x_request_id ,
    "x-global-request-id"        .= _x_global_request_id ,
    "tag"                        .= _tag,
    maybe mempty (\v -> ("message" .= v)) _message,
    "message_type"               .= _message_type,
    "action"                     .= _action,
    "label"                      .= _label,
    maybe mempty (\v -> ("value" .= v)) _value,
    "source_commit"              .= _source_commit,
    "message_number"             .= _message_number,
    "notification_id"            .= _notification_id
    ]

isOutgoingAPICall :: Text -> Bool
isOutgoingAPICall tag =
           tag == "OUTGOING_REQUEST"
        || tag == "CallServantAPI impl"
        || tag == "\"CallServantAPI impl\""
        || tag == "callHTTP"
        || tag == "\"callHTTP\""

outgoingApiTag :: Text
outgoingApiTag = "OUTGOING_REQUEST"

appFramework :: Text
appFramework = "euler-hs-application"

hostname :: Text
hostname = "euler-hs"

env :: Text
env = "test"

getEulerDbConf :: (L.MonadFlow m) => m (T.DBConfig MySQLM)
getEulerDbConf = Extra.getEulerDbConf internalError

withEulerDB :: (L.MonadFlow m) => L.SqlDB MySQLM a -> m a
withEulerDB = Extra.withEulerDB internalError

data ErrorResponse = ErrorResponse
   { code     :: Int
   , response :: ErrorPayload
   }
  deriving (Eq, Show, Generic)

instance Exception ErrorResponse

data ErrorPayload = ErrorPayload
  { error1        :: Bool
  , errorMessage :: Text
  , userMessage  :: Text
  }
  deriving (Eq, Show, Generic)

internalError :: ErrorResponse
internalError = ErrorResponse
  { code = 500
  , response = ErrorPayload
      { errorMessage = "Internal Server Error"
      , userMessage = "Internal Server Error"
      , error1 = True
      }
  }
