{- |
Module      :  EulerHS.Framework.Runtime
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
This module contains functions and types to work with `FlowRuntime`.
This is an internal module. Import EulerHS.Runtime instead.
-}

{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}

module EulerHS.Framework.Runtime
  (
    -- * Framework Runtime
    FlowRuntime(..)
  , ConfigEntry(..)
  , mkConfigEntry
  , createFlowRuntime
  , createFlowRuntime'
  , withFlowRuntime
  , kvDisconnect
  , runPubSubWorker
  , shouldFlowLogRawSql
  , CertificateRegistrationError(..)
  , withSelfSignedFlowRuntime
  , forkFlowWithNewRecordingLocal
  ) where

import qualified Control.Concurrent.Map as CMap
import           Control.Monad.Trans.Except (throwE)
import qualified Data.Cache.LRU as SimpleLRU
import qualified Data.LruCache as LRU
import qualified Data.Map as Map (empty)
import qualified Data.Pool as DP (destroyAllResources)
import           Data.Time.Clock.POSIX
import           Data.X509.CertificateStore (readCertificateStore)
import qualified Database.Redis as RD
import           EulerHS.HttpAPI
import           EulerHS.KVDB.Types (NativeKVDBConn (NativeKVDB))
import qualified EulerHS.Logger.Runtime as R
import           EulerHS.Prelude
import           EulerHS.SqlDB.Types (ConnTag,
                                      NativeSqlPool (NativeMySQLPool, NativePGPool, NativeSQLitePool))
import           GHC.Conc (labelThread)
import           EulerHS.Extra.Snowflakes.Types (SnowflakeGenerator)
import           EulerHS.Extra.Snowflakes.Flow (getSnowflakeGenerator)
import           Juspay.Extra.Config (lookupEnvT)
import           Network.Connection (TLSSettings (TLSSettings))
import           Network.HTTP.Client (Manager, newManager)
import           Network.HTTP.Client.TLS (mkManagerSettings)
import           Network.TLS (ClientParams (clientShared, clientSupported),
                              defaultParamsClient, sharedCAStore,
                              supportedCiphers)
import           Network.TLS.Extra.Cipher (ciphersuite_default)
import           System.IO.Unsafe (unsafePerformIO)
import qualified System.Mem as SYSM (performGC)
import           Unsafe.Coerce (unsafeCoerce)
import EulerHS.ART.Types (RecordingEntry(..))


-- | FlowRuntime state and options.
data FlowRuntime = FlowRuntime
  { _coreRuntime              :: R.CoreRuntime
  -- ^ Contains logger settings
  , _defaultHttpClientManager :: Manager
  -- ^ Http default manager, used for external api calls
  , _httpClientManagers       :: HashMap Text Manager
  -- ^ Http managers, used for external api calls
  , _dynHttpClientManagers    :: MVar (LRU.LruCache HTTPClientSettings Manager)
  -- ^ LRU cache of Managers.
  , _options                  :: MVar (Map Text Any)
  -- ^ Typed key-value storage
  , _optionsLocal             :: MVar (Map Text Any)
  -- ^ Typed key-value storage - New Ref for every api call
  , _kvdbConnections          :: MVar (Map Text NativeKVDBConn)
  -- ^ Connections for key-value databases
  , _sqldbConnections         :: MVar (Map ConnTag NativeSqlPool)
  -- ^ Connections for SQL databases
  , _pubSubController         :: RD.PubSubController
  -- ^ Subscribe controller
  , _pubSubConnection         :: Maybe RD.Connection
  -- ^ Connection being used for Publish
  , _configCache              :: IORef (SimpleLRU.LRU Text ConfigEntry)

  , _configCacheLock          :: MVar (CMap.Map Text ())

  , _snowflakeGenerator        :: SnowflakeGenerator
  , _recordingLocal          :: MVar ([RecordingEntry])
  
  }

data ConfigEntry =   ConfigEntry
  {
      ttl   :: POSIXTime
    , entry :: Maybe Any
  }

deriving instance Show ConfigEntry

configCacheSize :: Integer
configCacheSize =
  let
    mbSize :: Maybe Integer
    mbSize = readMaybe =<< lookupEnvT "CONFIG_CACHE_SIZE"

  in fromMaybe 4096 mbSize

mkConfigEntry :: POSIXTime -> Maybe a -> ConfigEntry
mkConfigEntry valTtl mbVal = ConfigEntry valTtl $ (unsafeCoerce @_ @Any) <$> mbVal
-- | Possible issues that can arise when registering certificates.
--
-- @since 2.0.4.3
newtype CertificateRegistrationError = NoCertificatesAtPath FilePath
  deriving (Eq) via FilePath
  deriving stock (Show)

instance Exception CertificateRegistrationError

-- | Works identically to 'withFlowRuntime', but takes an extra parameter. This
-- parameter is a map of textual identifiers to paths where custom CA
-- certificates can be found.
--
-- You can then use 'callAPI', providing 'Just' the textual identifier to use
-- the self-signed certificate(s) provided.
--
-- The handler is provided an 'Either' to allow for graceful recovery; if you
-- have nothing you can do with a 'CertificateRegistrationError', you can throw
-- it.
--
-- @since 2.0.4.3

{-# DEPRECATED withSelfSignedFlowRuntime "use manager builders instead, see HttpAPI.hs" #-}
withSelfSignedFlowRuntime ::
  HashMap Text FilePath ->
  Maybe (IO R.LoggerRuntime) ->
  (Either CertificateRegistrationError FlowRuntime -> IO a) ->
  IO a
withSelfSignedFlowRuntime certPathMap mRTF handler = do
  res <- runExceptT . traverse go $ certPathMap
  case res of
    Left err         -> handler . Left $ err
    Right managerMap ->
      bracket (fromMaybe R.createVoidLoggerRuntime mRTF) R.clearLoggerRuntime $
        \loggerRT -> bracket (R.createCoreRuntime loggerRT) R.clearCoreRuntime $
          \coreRT -> bracket (mkFlowRT coreRT managerMap) clearFlowRuntime $
            handler . Right
  where
    go ::
      FilePath ->
      ExceptT CertificateRegistrationError IO Manager
    go certPath = do
      mCertStore <- lift . readCertificateStore $ certPath
      case mCertStore of
        Nothing    -> throwE . NoCertificatesAtPath $ certPath
        Just store -> do
          let defs = defaultParamsClient "localhost" ""
          let clientParams = defs {
            clientShared = (clientShared defs) { sharedCAStore = store },
            clientSupported = (clientSupported defs) { supportedCiphers = ciphersuite_default }}
          lift . newManager . mkManagerSettings (TLSSettings clientParams) $ Nothing
    mkFlowRT :: R.CoreRuntime -> HashMap Text Manager -> IO FlowRuntime
    mkFlowRT coreRT managers = do
      frt <- createFlowRuntime coreRT
      pure frt { _httpClientManagers = managers }

-- | Create default FlowRuntime.
createFlowRuntime :: R.CoreRuntime -> IO FlowRuntime
createFlowRuntime coreRt = do
  defaultManagerVar     <- newManager $ buildSettings mempty
  optionsVar            <- newMVar mempty
  optionsLocalVar       <- newMVar mempty
  configCacheVar        <- newIORef $ SimpleLRU.newLRU $ Just configCacheSize
  configCacheLockVar    <- newMVar =<< CMap.empty
  snowFlakeGenerator    <- getSnowflakeGenerator
  kvdbConnections       <- newMVar Map.empty
  sqldbConnections      <- newMVar Map.empty
  dynHttpClientManagers <- newMVar $ LRU.empty 100
  recordingLocal        <- newMVar $ []
  pubSubController  <- RD.newPubSubController [] []
  pure $ FlowRuntime
    { _coreRuntime              = coreRt
    , _defaultHttpClientManager = defaultManagerVar
    , _httpClientManagers       = mempty
    , _options                  = optionsVar
    , _optionsLocal             = optionsLocalVar
    , _configCache              = configCacheVar
    , _configCacheLock          = configCacheLockVar
    , _kvdbConnections          = kvdbConnections
    -- , _runMode                  = T.RegularMode
    , _sqldbConnections         = sqldbConnections
    , _pubSubController         = pubSubController
    , _pubSubConnection         = Nothing
    , _dynHttpClientManagers    = dynHttpClientManagers
    , _snowflakeGenerator       = snowFlakeGenerator
    , _recordingLocal           = recordingLocal
    }

createFlowRuntime' :: Maybe (IO R.LoggerRuntime) -> IO FlowRuntime
createFlowRuntime' Nothing = R.createVoidLoggerRuntime >>= R.createCoreRuntime >>= createFlowRuntime
createFlowRuntime' (Just loggerRtCreator) = loggerRtCreator >>= R.createCoreRuntime >>= createFlowRuntime

-- | Clear resources in given 'FlowRuntime'
clearFlowRuntime :: FlowRuntime  -> IO ()
clearFlowRuntime FlowRuntime{..} = do
  _ <- takeMVar _options
  putMVar _options mempty
  _ <- takeMVar _optionsLocal
  putMVar _optionsLocal mempty
  kvConns <- takeMVar _kvdbConnections
  putMVar _kvdbConnections mempty
  traverse_ kvDisconnect kvConns
  sqlConns <- takeMVar _sqldbConnections
  putMVar _sqldbConnections mempty
  traverse_ sqlDisconnect sqlConns
  -- The Manager will be shut down automatically via garbage collection.
  SYSM.performGC

shouldFlowLogRawSql :: FlowRuntime -> Bool
shouldFlowLogRawSql = R.shouldLogRawSql . R._loggerRuntime . _coreRuntime

sqlDisconnect :: NativeSqlPool -> IO ()
sqlDisconnect = \case
  NativePGPool connPool     -> DP.destroyAllResources connPool
  NativeMySQLPool connPool  -> DP.destroyAllResources connPool
  NativeSQLitePool connPool -> DP.destroyAllResources connPool

kvDisconnect :: NativeKVDBConn -> IO ()
kvDisconnect (NativeKVDB conn) = RD.disconnect conn

-- | Run flow with given logger runtime creation function.
withFlowRuntime :: Maybe (IO R.LoggerRuntime) -> (FlowRuntime -> IO a) -> IO a
withFlowRuntime Nothing actionF =
  bracket R.createVoidLoggerRuntime R.clearLoggerRuntime $ \loggerRt ->
  bracket (R.createCoreRuntime loggerRt) R.clearCoreRuntime $ \coreRt ->
  bracket (createFlowRuntime coreRt) clearFlowRuntime actionF
withFlowRuntime (Just loggerRuntimeCreator) actionF =
  bracket loggerRuntimeCreator R.clearLoggerRuntime $ \loggerRt ->
  bracket (R.createCoreRuntime loggerRt) R.clearCoreRuntime $ \coreRt ->
  bracket (createFlowRuntime coreRt) clearFlowRuntime actionF

forkFlowWithNewRecordingLocal :: FlowRuntime -> MVar ([RecordingEntry]) -> IO FlowRuntime
forkFlowWithNewRecordingLocal FlowRuntime {..} recordingLocal = do
  pure $ FlowRuntime
    { _coreRuntime              
    , _defaultHttpClientManager 
    , _httpClientManagers       
    , _options                  
    , _optionsLocal             
    , _configCache              
    , _configCacheLock          
    , _kvdbConnections 
    , _sqldbConnections         
    , _pubSubController         
    , _pubSubConnection         
    , _dynHttpClientManagers
    , _snowflakeGenerator  
    , _recordingLocal           = recordingLocal
    }

-- Use {-# NOINLINE foo #-} as a pragma on any function foo that calls unsafePerformIO.
-- If the call is inlined, the I/O may be performed more than once.
{-# NOINLINE pubSubWorkerLock #-}
pubSubWorkerLock :: MVar ()
pubSubWorkerLock = unsafePerformIO $ newMVar ()

runPubSubWorker :: FlowRuntime -> (Text -> IO ()) -> IO (IO ())
runPubSubWorker rt log = do
    let tsecond = 10 ^ (6 :: Int)

    lock <- tryTakeMVar pubSubWorkerLock
    case lock of
      Nothing -> error "Unable to run Publish/Subscribe worker: Only one worker allowed"
      Just _  -> pure ()

    let mconnection = _pubSubConnection rt

    delayRef <- newIORef tsecond

    threadId <- case mconnection of
      Nothing   -> do
        putMVar pubSubWorkerLock ()
        error "Unable to run Publish/Subscribe worker: No connection to Redis provided"

      Just conn -> do
        tid <- forkIO $ forever $ do
            res <- try @_ @SomeException $ RD.pubSubForever conn (_pubSubController rt) $ do
              writeIORef delayRef tsecond
              log "Publish/Subscribe worker: Run successfuly"

            case res of
              Left e -> do
                  delay <- readIORef delayRef

                  log $ "Publish/Subscribe worker: Got error: " <> show e
                  log $ "Publish/Subscribe worker: Restart in " <> show (delay `div` tsecond) <> " sec"

                  modifyIORef' delayRef (\d -> d + d `div` 2) -- (* 1.5)
                  threadDelay delay
              Right _ -> pure ()
        labelThread tid "euler-runPubSubWorker"
        return tid

    pure $ do
      killThread threadId
      putMVar pubSubWorkerLock ()
      log "Publish/Subscribe worker: Killed"
