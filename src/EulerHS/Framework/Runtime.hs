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
{-
Creates a new 'FlowRuntime' given a 'CoreRuntime'.

This function initializes various components within the 'FlowRuntime', such as HTTP client managers,
options, key-value database connections, SQL database connections, and a pub-sub controller.

Parameters:
  - 'coreRt': The 'CoreRuntime' to associate with the new 'FlowRuntime'.

Returns:
  - An 'IO' action producing the initialized 'FlowRuntime'.
-}
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

{-
Creates a new 'FlowRuntime' given an optional logger runtime creator.

This function allows flexibility in providing a custom logger runtime through the 'Maybe' type.
If the logger runtime creator is 'Nothing', it uses the default logger runtime created by 'createVoidLoggerRuntime'.

Parameters:
  - 'loggerRtCreator': An optional action that creates a 'LoggerRuntime'.

Returns:
  - An 'IO' action producing the initialized 'FlowRuntime'.
-}
createFlowRuntime' :: Maybe (IO R.LoggerRuntime) -> IO FlowRuntime
createFlowRuntime' Nothing = R.createVoidLoggerRuntime >>= R.createCoreRuntime >>= createFlowRuntime
createFlowRuntime' (Just loggerRtCreator) = loggerRtCreator >>= R.createCoreRuntime >>= createFlowRuntime

{-
Clears resources associated with a 'FlowRuntime'.

This function releases and cleans up various resources within a 'FlowRuntime', such as options,
key-value database connections, SQL database connections, and performs garbage collection for the HTTP manager.
-}
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

{-
Determines whether raw SQL logs should be enabled for the given 'FlowRuntime'.

This function extracts the information from the underlying 'LoggerRuntime' in the 'FlowRuntime'.
-}
shouldFlowLogRawSql :: FlowRuntime -> Bool
shouldFlowLogRawSql = R.shouldLogRawSql . R._loggerRuntime . _coreRuntime


{-
Disconnects and releases resources associated with a native SQL connection pool.

This function destroys all resources in the provided 'T.NativeSqlPool', which may be specific to
PostgreSQL, MySQL, SQLite, or a mocked connection pool.
-}
sqlDisconnect :: T.NativeSqlPool -> IO ()
sqlDisconnect = \case
  T.NativePGPool connPool -> DP.destroyAllResources connPool
  T.NativeMySQLPool connPool -> DP.destroyAllResources connPool
  T.NativeSQLitePool connPool -> DP.destroyAllResources connPool
  T.NativeMockedPool -> pure ()

{-
Disconnects and releases resources associated with a native key-value database connection.

This function disconnects the provided 'T.NativeKVDBConn', which may be a native KVDB connection or a mocked connection.

Parameters:
  - 'kvdbConn': The 'T.NativeKVDBConn' to be disconnected.

Returns:
  - An 'IO' action performing the disconnection.

Usage Example:
  @
  redisConn <- RD.connect someRedisConfig
  kvDisconnect (T.NativeKVDB redisConn)
  @
-}
kvDisconnect :: T.NativeKVDBConn -> IO ()
kvDisconnect = \case
  T.NativeKVDBMockedConn -> pure ()
  T.NativeKVDB conn -> RD.disconnect conn

{-
withFlowRuntime: Run flow with given logger runtime creation function.

This function provides a way to manage the lifecycle of a FlowRuntime by acquiring and releasing the necessary resources, with the flexibility to use a custom logger runtime creator if provided.

Manages the lifecycle of a FlowRuntime, allowing the use of a custom logger runtime creator if provided.
If the logger runtime creator is Nothing, default logger and core runtimes are created.
The provided action function is executed with the FlowRuntime, ensuring proper cleanup.
Usage:
  
  withFlowRuntime (Just (createLoggerRuntime formatter Nothing loggerCfg)) $ \flowRuntime -> do
            -- Your code using the flowRuntime
            -- Cleanup is handled automatically.
  Example -
  @
  loggerConfig :: T.LoggerConfig
  loggerConfig = T.LoggerConfig
      { T._isAsync = False
      , T._logLevel = T.Debug
      , T._logFilePath = "/tmp/logs/myFlow.log"
      , T._logToConsole = True
      , T._logToFile = False
      , T._maxQueueSize = 1000
      , T._logRawSql = False
      }

  main :: IO ()
  main = do
    let mkLoggerRt = createLoggerRuntime defaultFlowFormatter Nothing loggerConfig

    R.withFlowRuntime (Just mkLoggerRt) $ \flowRt ->
      runEchoServer  "This is an easter egg." flowRt 8080
  @
-}
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
