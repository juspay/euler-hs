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

module EulerHS.Framework.Runtime
  (
    -- * Framework Runtime
    FlowRuntime(..)
  , createFlowRuntime
  , createFlowRuntime'
  , withFlowRuntime
  , kvDisconnect
  , runPubSubWorker
  , shouldFlowLogRawSql
  ) where

import           EulerHS.Prelude

import           Network.HTTP.Client (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import qualified Data.Map as Map (empty)
import qualified Data.Pool as DP (destroyAllResources)
import qualified Database.Redis as RD
import qualified System.Mem as SYSM (performGC)

import           System.IO.Unsafe (unsafePerformIO)

import qualified EulerHS.Core.Runtime as R
import qualified EulerHS.Core.Types as T


{- | FlowRuntime state and options.

`FlowRuntime` is a structure that stores operational data of the framework,
such as native connections, internal state, threads, and other things
needed to run the framework.

@
import qualified EulerHS.Types as T
import qualified EulerHS.Language as L
import qualified EulerHS.Runtime as R
import qualified EulerHS.Interpreters as R

myFlow :: L.Flow ()
myFlow = L.runIO $ putStrLn @String "Hello there!"

runApp :: IO ()
runApp = do
  let mkLoggerRt = R.createLoggerRuntime T.defaultFlowFormatter T.defaultLoggerConfig
  R.withFlowRuntime (Just mkLoggerRt)
    $ \flowRt -> R.runFlow flowRt myFlow
@

Typically, you need only one instance of `FlowRuntime` in your project.
You can run your flows with this instance in parallel, it should be thread-safe.

It's okay to pass `FlowRuntime` here and there, but avoid changing its data.
Only the framework has a right to update those fields.

Mutating any of its data from the outside will lead to an undefined behavior.
-}
data FlowRuntime = FlowRuntime
  { _coreRuntime              :: R.CoreRuntime
  -- ^ Contains logger settings
  , _defaultHttpClientManager :: Manager
  -- ^ Http default manager, used for external api calls
  , _httpClientManagers       :: Map String Manager
  -- ^ Http managers, used for external api calls
  , _options                  :: MVar (Map Text Any)
  -- ^ Typed key-value storage
  , _kvdbConnections          :: MVar (Map Text T.NativeKVDBConn)
  -- ^ Connections for key-value databases
  , _sqldbConnections         :: MVar (Map T.ConnTag T.NativeSqlPool)
  -- ^ Connections for SQL databases
  , _pubSubController         :: RD.PubSubController
  -- ^ Subscribe controller
  , _pubSubConnection         :: Maybe RD.Connection
  -- ^ Connection being used for Publish
  }

-- | Create default FlowRuntime.
createFlowRuntime :: R.CoreRuntime -> IO FlowRuntime
createFlowRuntime coreRt = do
  defaultManagerVar <- newManager tlsManagerSettings
  optionsVar        <- newMVar mempty
  kvdbConnections   <- newMVar Map.empty
  sqldbConnections  <- newMVar Map.empty
  pubSubController  <- RD.newPubSubController [] []

  pure $ FlowRuntime
    { _coreRuntime              = coreRt
    , _defaultHttpClientManager = defaultManagerVar
    , _httpClientManagers       = Map.empty
    , _options                  = optionsVar
    , _kvdbConnections          = kvdbConnections
    -- , _runMode                  = T.RegularMode
    , _sqldbConnections         = sqldbConnections
    , _pubSubController         = pubSubController
    , _pubSubConnection         = Nothing
    }

createFlowRuntime' :: Maybe (IO R.LoggerRuntime) -> IO FlowRuntime
createFlowRuntime' Nothing = R.createVoidLoggerRuntime >>= R.createCoreRuntime >>= createFlowRuntime
createFlowRuntime' (Just loggerRtCreator) = loggerRtCreator >>= R.createCoreRuntime >>= createFlowRuntime

-- | Clear resources in given 'FlowRuntime'
clearFlowRuntime :: FlowRuntime  -> IO ()
clearFlowRuntime FlowRuntime{..} = do
  _ <- takeMVar _options
  putMVar _options mempty
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

sqlDisconnect :: T.NativeSqlPool -> IO ()
sqlDisconnect = \case
  T.NativePGPool connPool -> DP.destroyAllResources connPool
  T.NativeMySQLPool connPool -> DP.destroyAllResources connPool
  T.NativeSQLitePool connPool -> DP.destroyAllResources connPool
  T.NativeMockedPool -> pure ()

kvDisconnect :: T.NativeKVDBConn -> IO ()
kvDisconnect = \case
  T.NativeKVDBMockedConn -> pure ()
  T.NativeKVDB conn -> RD.disconnect conn

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

      Just conn -> forkIO $ forever $ do
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

    pure $ do
      killThread threadId
      putMVar pubSubWorkerLock ()
      log $ "Publish/Subscribe worker: Killed"
