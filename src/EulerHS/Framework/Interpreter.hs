{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EulerHS.Framework.Interpreter
  ( -- * Flow Interpreter
    runFlow
  , runFlow'
  ) where

import           Control.Concurrent.MVar (modifyMVar)
import           Control.Exception (throwIO)
import qualified Control.Exception as Exception
import qualified Control.Concurrent.Map as CMap
import qualified Data.Aeson as A
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.CaseInsensitive as CI
import qualified Data.DList as DL
import           Data.Either.Extra (mapLeft)
import qualified Data.HashMap.Strict as HM
import qualified Data.LruCache as LRU
import qualified Data.Map as Map
import qualified Data.Pool as DP
import           Data.Profunctor (dimap)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import           Data.Time.Clock (diffTimeToPicoseconds)
import           Data.Time.Clock.System (getSystemTime, systemToTAITime)
import           Data.Time.Clock.TAI (diffAbsoluteTime)
import qualified Data.UUID as UUID (toText)
import qualified Data.UUID.V4 as UUID (nextRandom)
import           EulerHS.Api (runEulerClient)
import           EulerHS.BinaryString (LBinaryString (LBinaryString),
                                       getLBinaryString)
import           EulerHS.Common (Awaitable (Awaitable), FlowGUID,
                                 ManagerSelector (ManagerSelector),
                                 Microseconds (Microseconds))
import qualified EulerHS.Framework.Language as L
import qualified EulerHS.Framework.Runtime as R
import           EulerHS.HttpAPI (HTTPIOException (HTTPIOException),
                                  HTTPMethod (Connect, Delete, Get, Head, Options, Patch, Post, Put, Trace),
                                  HTTPRequest(..), HTTPRequestMasked,
                                  HTTPResponse (HTTPResponse), buildSettings,
                                  defaultTimeout, getRequestBody,
                                  getRequestHeaders, getRequestMethod,
                                  getRequestRedirects, getRequestTimeout,
                                  getRequestURL, getResponseBody,
                                  getResponseCode, getResponseHeaders,
                                  getResponseStatus, maskHTTPRequest,
                                  maskHTTPResponse, mkHttpApiCallLogEntry)
import           EulerHS.KVDB.Interpreter (runKVDB)
import           EulerHS.KVDB.Types (KVDBAnswer,
                                     KVDBConfig (KVDBClusterConfig, KVDBConfig),
                                     KVDBConn (Redis),
                                     KVDBError (KVDBConnectionAlreadyExists, KVDBConnectionDoesNotExist, KVDBConnectionFailed),
                                     KVDBReplyF (KVDBError), kvdbToNative,
                                     mkRedisConn, nativeToKVDB)
import           EulerHS.Logger.Interpreter (runLogger)
import qualified EulerHS.Logger.Language as L
import qualified EulerHS.Logger.Runtime as R
import           EulerHS.Logger.Types (LogLevel (Debug, Error),
                                       Message (Message))
import           EulerHS.Prelude
import           EulerHS.PubSub.Interpreter (runPubSub)
import           EulerHS.SqlDB.Interpreter (runSqlDB)
import           EulerHS.SqlDB.Types (ConnTag,
                                      DBConfig (MySQLPoolConf, PostgresPoolConf, SQLitePoolConf),
                                      DBError (DBError),
                                      DBErrorType (ConnectionAlreadyExists, ConnectionDoesNotExist, ConnectionFailed, UnrecognizedError),
                                      DBResult,
                                      NativeSqlConn (NativeMySQLConn, NativePGConn, NativeSQLiteConn),
                                      SqlConn (MySQLPool, PostgresPool, SQLitePool),
                                      bemToNative, mkSqlConn,
                                      mysqlErrorToDbError, nativeToBem,
                                      postgresErrorToDbError,
                                      sqliteErrorToDbError)
import           GHC.Conc (labelThread)
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Client.Internal
import qualified Network.HTTP.Types as HTTP
import qualified Servant.Client as S
import           System.Process (readCreateProcess, shell)
import           Unsafe.Coerce (unsafeCoerce)

connect :: DBConfig be -> IO (DBResult (SqlConn be))
connect cfg = do
  eConn <- try $ mkSqlConn cfg
  case eConn of
    Left (e :: SomeException) -> pure $ Left $ DBError ConnectionFailed $ show e
    Right conn                -> pure $ Right conn

connectRedis :: KVDBConfig -> IO (KVDBAnswer KVDBConn)
connectRedis cfg = do
  eConn <- try $ mkRedisConn cfg
  case eConn of
    Left (e :: SomeException) -> pure $ Left $ KVDBError KVDBConnectionFailed $ show e
    Right conn                -> pure $ Right conn

disconnect :: SqlConn beM ->   IO ()
disconnect (PostgresPool _ pool) = DP.destroyAllResources pool
disconnect (MySQLPool _ pool)    = DP.destroyAllResources pool
disconnect (SQLitePool _ pool)   = DP.destroyAllResources pool

suppressErrors :: IO a -> IO ()
suppressErrors = void . try @_ @SomeException

awaitMVarWithTimeout :: MVar (Either Text a) -> Int -> IO (Either L.AwaitingError a)
awaitMVarWithTimeout mvar mcs | mcs <= 0  = go 0
                              | otherwise = go mcs
  where
    portion = (mcs `div` 10) + 1
    go rest
      | rest <= 0 = do
        mValue <- tryReadMVar mvar
        pure $ case mValue of
          Nothing          -> Left L.AwaitingTimeout
          Just (Right val) -> Right val
          Just (Left err)  -> Left $ L.ForkedFlowError err
      | otherwise = do
          tryReadMVar mvar >>= \case
            Just (Right val) -> pure $ Right val
            Just (Left err)  -> pure $ Left $ L.ForkedFlowError err
            Nothing          -> threadDelay portion >> go (rest - portion)

-- | Utility function to convert HttpApi HTTPRequests to http-client HTTP
-- requests
getHttpLibRequest :: MonadThrow m => HTTPRequest -> m HTTP.Request
getHttpLibRequest request = do
  let url = Text.unpack $ getRequestURL request
  httpLibRequest <- HTTP.parseRequest url
  let
    requestMethod = case getRequestMethod request of
      Get     -> "GET"
      Put     -> "PUT"
      Post    -> "POST"
      Delete  -> "DELETE"
      Head    -> "HEAD"
      Trace   -> "TRACE"
      Connect -> "CONNECT"
      Options -> "OPTIONS"
      Patch   -> "PATCH"
  let
    setBody = case getRequestBody request of
      Just body ->
        let body' = getLBinaryString body
        in  \req -> req { HTTP.requestBody = HTTP.RequestBodyLBS body' }
      Nothing   -> id

  -- TODO: Respect "Content-Transfer-Encoding" header
  let
    headers :: HTTP.RequestHeaders = getRequestHeaders request
      & Map.toList
      & map (\(x, y) -> (CI.mk (Encoding.encodeUtf8 x), Encoding.encodeUtf8 y))

  let
    setTimeout = case getRequestTimeout request of
      Just x  -> setRequestTimeout x
      Nothing -> setRequestTimeout defaultTimeout

  let
    setRedirects = case getRequestRedirects request of
      Just x  -> \req -> req {HTTP.redirectCount = x}
      Nothing -> id

  pure $ setRedirects . setTimeout . setBody $
      httpLibRequest
        { HTTP.method         = requestMethod
        , HTTP.requestHeaders = headers
        }

-- | Set timeout in microseconds
setRequestTimeout :: Int -> HTTP.Request -> HTTP.Request
setRequestTimeout x req = req {HTTP.responseTimeout = HTTP.responseTimeoutMicro x}


-- | Utility function to translate http-client HTTP responses back to HttpAPI
-- responses
translateHttpResponse :: HTTP.Response Lazy.ByteString -> Either Text HTTPResponse
translateHttpResponse response = do
  headers <- translateResponseHeaders $ HTTP.responseHeaders response
  status <-  translateResponseStatusMessage . HTTP.statusMessage . HTTP.responseStatus $ response
  pure $ HTTPResponse
    { getResponseBody    = LBinaryString $ HTTP.responseBody response
    , getResponseCode    = HTTP.statusCode $ HTTP.responseStatus response
    , getResponseHeaders = headers
    , getResponseStatus  = status
    }

translateResponseHeaders
  :: [(CI.CI Strict.ByteString, Strict.ByteString)]
  -> Either Text (Map.Map Text.Text Text.Text)
translateResponseHeaders httpLibHeaders = do
  let
    result = do
      headerNames <- mapM  (Encoding.decodeUtf8' . CI.original . fst) httpLibHeaders
      headerValues <- mapM (Encoding.decodeUtf8' . snd) httpLibHeaders
      return $ zip (map Text.toLower headerNames) headerValues

  -- TODO: Look up encoding and use some thread-safe unicode package to decode
  --       headers
  -- let encoding
  --   = List.findIndex (\name -> name == "content-transfer-encoding") headerNames
  headers <- displayEitherException "Error decoding HTTP response headers: " result
  pure $ Map.fromList headers

translateResponseStatusMessage :: Strict.ByteString -> Either Text Text
translateResponseStatusMessage = displayEitherException "Error decoding HTTP response status message: " . Encoding.decodeUtf8'

displayEitherException :: Exception e => Text -> Either e a -> Either Text a
displayEitherException prefix = either (Left . (prefix <>) . Text.pack . Exception.displayException) Right

-- translateHeaderName :: CI.CI Strict.ByteString -> Text.Text
-- translateHeaderName = Encoding.decodeUtf8' . CI.original

interpretFlowMethod :: Maybe FlowGUID -> R.FlowRuntime -> L.FlowMethod a -> IO a
interpretFlowMethod _ R.FlowRuntime {_httpClientManagers, _defaultHttpClientManager} (L.LookupHTTPManager mbMgrSel next) =
    pure $ next $ case mbMgrSel of
      Just (ManagerSelector mngrName) -> HM.lookup mngrName _httpClientManagers
      Nothing                         -> Just _defaultHttpClientManager

interpretFlowMethod mbFlowGuid flowRt (L.CallServantAPI mngr bUrl clientAct next) =
    fmap next $ do
          let S.ClientEnv manager baseUrl cookieJar makeClientRequest = S.mkClientEnv mngr bUrl
          let setR req = if HTTP.responseTimeout req == HTTP.responseTimeoutNone
                            then setRequestTimeout defaultTimeout req
                            else req {HTTP.responseTimeout = mResponseTimeout mngr}
          eitherResult <- tryRunClient $! S.runClientM (runEulerClient (if shouldLogAPI
                                                                          then dbgLogger Debug
                                                                          else const $ return ()
                                                                      ) getLoggerMaskConfig bUrl clientAct) $
            S.ClientEnv manager baseUrl cookieJar (\url -> setR . makeClientRequest url)
          case eitherResult of
            Left err -> do
              dbgLogger Error $ show @Text err
              pure $ Left err
            Right response ->
              pure $ Right response
  where
    dbgLogger :: forall msg . A.ToJSON msg => LogLevel -> msg -> IO ()
    dbgLogger debugLevel msg =
      runLogger mbFlowGuid (R._loggerRuntime . R._coreRuntime $ flowRt)
        . L.logMessage' debugLevel ("CallServantAPI impl" :: String)
        $ Message Nothing (Just $ A.toJSON msg)
    shouldLogAPI =
      R.shouldLogAPI . R._loggerRuntime . R._coreRuntime $ flowRt
    getLoggerMaskConfig =
      R.getLogMaskingConfig . R._loggerRuntime . R._coreRuntime $ flowRt
    tryRunClient :: IO (Either S.ClientError a) -> IO (Either S.ClientError a)
    tryRunClient act = do
      res :: Either S.ClientError (Either S.ClientError a) <- try act
      pure $ join res

interpretFlowMethod _ R.FlowRuntime {..} (L.GetHTTPManager settings next) =
  fmap next $ do
    modifyMVar _dynHttpClientManagers $ \_cache -> do
      let mCacheMgr = LRU.lookup settings _cache
      case mCacheMgr of
        Just (mgr, cache) -> pure (cache, mgr)
        Nothing  -> do
          mgr <- HTTP.newManager $ buildSettings settings
          pure (LRU.insert settings mgr _cache, mgr)


interpretFlowMethod _ flowRt (L.CallHTTP request manager next) =
    fmap next $ do
      httpLibRequest <- getHttpLibRequest request
      start <- systemToTAITime <$> getSystemTime
      eResponse <- try $! HTTP.httpLbs httpLibRequest manager
      end <- liftIO $ systemToTAITime <$> getSystemTime
      let lat = div (diffTimeToPicoseconds $ diffAbsoluteTime end start) picoMilliDiff
      case eResponse of
        Left (err :: SomeException) -> do
          let errMsg = Text.pack $ displayException err
          logJsonError errMsg (maskHTTPRequest getLoggerMaskConfig request)
          pure $ Left errMsg
        Right httpResponse -> do
          case translateHttpResponse httpResponse of
            Left errMsg -> do
              logJsonError errMsg (maskHTTPRequest getLoggerMaskConfig request)
              pure $ Left errMsg
            Right response -> do
              when shouldLogAPI $ do
                let logEntry = mkHttpApiCallLogEntry lat
                                (maskHTTPRequest getLoggerMaskConfig request)
                                (maskHTTPResponse getLoggerMaskConfig response)
                logJson Debug logEntry
              pure $ Right response
  where
    picoMilliDiff :: Integer
    picoMilliDiff = 1000000000
    logJsonError :: Text -> HTTPRequestMasked -> IO ()
    logJsonError err = logJson Error . HTTPIOException err
    logJson :: ToJSON a => LogLevel -> a -> IO ()
    logJson debugLevel msg =
      runLogger (Just "API CALL:") (R._loggerRuntime . R._coreRuntime $ flowRt)
        . L.logMessage' debugLevel ("callHTTP" :: String)
        $ Message Nothing (Just $ A.toJSON msg)

    shouldLogAPI =
      R.shouldLogAPI . R._loggerRuntime . R._coreRuntime $ flowRt
    getLoggerMaskConfig =
      R.getLogMaskingConfig . R._loggerRuntime . R._coreRuntime $ flowRt

interpretFlowMethod mbFlowGuid R.FlowRuntime {..} (L.EvalLogger loggerAct next) =
  next <$> runLogger mbFlowGuid (R._loggerRuntime _coreRuntime) loggerAct

interpretFlowMethod _ _ (L.RunIO _ ioAct next) =
  next <$> ioAct

interpretFlowMethod _ flowRt (L.WithRunFlow ioAct) =
  ioAct (runFlow flowRt)

interpretFlowMethod _ R.FlowRuntime {..} (L.GetOption k next) =
  fmap next $ do
    m <- readMVar _options
    pure $ do
      valAny <- Map.lookup k m
      pure $ unsafeCoerce valAny

interpretFlowMethod _ R.FlowRuntime {..} (L.SetOption k v next) =
  fmap next $ do
    m <- takeMVar _options
    let newMap = Map.insert k (unsafeCoerce @_ @Any v) m
    putMVar _options newMap

interpretFlowMethod _ R.FlowRuntime {..} (L.DelOption k next) =
  fmap next $ do
    m <- takeMVar _options
    let newMap = Map.delete k m
    putMVar _options newMap

interpretFlowMethod _ R.FlowRuntime {..} (L.GetConfig k next) =
  fmap next $ do
    m <- readMVar _configCache
    pure $ do
      Map.lookup k m

interpretFlowMethod _ R.FlowRuntime {..} (L.SetConfig k v next) =
  fmap next $ do
    m <- takeMVar _configCache
    let newMap = Map.insert k v m
    putMVar _configCache newMap

interpretFlowMethod _ R.FlowRuntime {..} (L.DelConfig k next) =
  fmap next $ do
    m <- takeMVar _configCache
    let newMap = Map.delete k m
    putMVar _configCache newMap

interpretFlowMethod _ R.FlowRuntime {..} (L.TrySetConfig k v next) =
  fmap next $ do
    mbM <- tryTakeMVar _configCache
    case mbM of
      Nothing -> pure Nothing
      Just m -> do
        let newMap = Map.insert k v m
        Just <$> putMVar _configCache newMap

interpretFlowMethod _ R.FlowRuntime {..} (L.AcquireConfigLock k next) =
  fmap next $ do
    m <- takeMVar _configCacheLock
    didAcquire <- CMap.insertIfAbsent k () m
    putMVar _configCacheLock m
    return didAcquire

interpretFlowMethod _ R.FlowRuntime {..} (L.ReleaseConfigLock k next) =
  fmap next $ do
    m <- takeMVar _configCacheLock
    didDelete <- CMap.delete k m
    putMVar _configCacheLock m
    return didDelete

interpretFlowMethod _ _ (L.GenerateGUID next) = do
  next <$> (UUID.toText <$> UUID.nextRandom)

interpretFlowMethod _ _ (L.RunSysCmd cmd next) =
  next <$> readCreateProcess (shell cmd) ""

----------------------------------------------------------------------
interpretFlowMethod mbFlowGuid rt (L.Fork desc _newFlowGUID flow next) = do
  awaitableMVar <- newEmptyMVar
  tid <- forkIO (suppressErrors (runFlow' mbFlowGuid rt (L.runSafeFlow flow) >>= putMVar awaitableMVar))
  labelThread tid $ "euler-Fork:" ++ Text.unpack desc
  pure $ next $ Awaitable awaitableMVar

----------------------------------------------------------------------

interpretFlowMethod _ _ (L.Await mbMcs (Awaitable awaitableMVar) next) = do
  let act = case mbMcs of
        Nothing -> do
          val <- readMVar awaitableMVar
          case val of
            Left err  -> pure $ Left $ L.ForkedFlowError err
            Right res -> pure $ Right res
        Just (Microseconds mcs) -> awaitMVarWithTimeout awaitableMVar $ fromIntegral mcs
  next <$> act

interpretFlowMethod _ _ (L.ThrowException ex _) = do
  throwIO ex

interpretFlowMethod mbFlowGuid rt (L.CatchException comp handler cont) =
  cont <$> catch (runFlow' mbFlowGuid rt comp) (runFlow' mbFlowGuid rt . handler)

-- Lack of impredicative polymorphism in GHC makes me sad. - Koz
interpretFlowMethod mbFlowGuid rt (L.Mask cb cont) =
  cont <$> mask (\cb' -> runFlow' mbFlowGuid rt (cb (dimap (runFlow' mbFlowGuid rt) (L.runIO' "Mask") cb')))

interpretFlowMethod mbFlowGuid rt (L.UninterruptibleMask cb cont) =
  cont <$> uninterruptibleMask
    (\cb' -> runFlow' mbFlowGuid rt (cb (dimap (runFlow' mbFlowGuid rt) (L.runIO' "UninterruptibleMask") cb')))

interpretFlowMethod mbFlowGuid rt (L.GeneralBracket acquire release use' cont) =
  cont <$> generalBracket
    (runFlow' mbFlowGuid rt acquire)
    (\x -> runFlow' mbFlowGuid rt . release x)
    (runFlow' mbFlowGuid rt . use')

interpretFlowMethod mbFlowGuid rt (L.RunSafeFlow _ flow next) = fmap next $ do
  fl <- try @_ @SomeException $ runFlow' mbFlowGuid rt flow
  pure $ mapLeft show fl

----------------------------------------------------------------------

interpretFlowMethod _ R.FlowRuntime {..} (L.InitSqlDBConnection cfg next) =
  fmap next $ do
    let connTag = dbConfigToTag cfg
    connMap <- takeMVar _sqldbConnections
    res <- case Map.lookup connTag connMap of
      Just _ -> pure $ Left $ DBError ConnectionAlreadyExists $ "Connection for " <> connTag <> " already created."
      Nothing -> connect cfg
    case res of
      Right conn -> putMVar _sqldbConnections $ Map.insert connTag (bemToNative conn) connMap
      Left _     -> putMVar _sqldbConnections connMap
    pure res

interpretFlowMethod _ R.FlowRuntime {..} (L.DeInitSqlDBConnection conn next) =
  fmap next $ do
    let connTag = sqlConnToTag conn
    connMap <- takeMVar _sqldbConnections
    case Map.lookup connTag connMap of
      Nothing -> putMVar _sqldbConnections connMap
      Just _ -> do
        disconnect conn
        putMVar _sqldbConnections $ Map.delete connTag connMap

interpretFlowMethod _ R.FlowRuntime {..} (L.GetSqlDBConnection cfg next) =
  fmap next $ do
    let connTag = dbConfigToTag cfg
    connMap <- readMVar _sqldbConnections
    pure $ case Map.lookup connTag connMap of
      Just conn -> Right $ nativeToBem connTag conn
      Nothing   -> Left $ DBError ConnectionDoesNotExist $ "Connection for " <> connTag <> " does not exists."

interpretFlowMethod _ R.FlowRuntime {..} (L.InitKVDBConnection cfg next) =
  fmap next $ do
    let connTag = kvdbConfigToTag cfg
    connections <- takeMVar _kvdbConnections
    res <- case Map.lookup connTag connections of
      Just _  -> pure $ Left $ KVDBError KVDBConnectionAlreadyExists $ "Connection for " +|| connTag ||+ " already created."
      Nothing -> connectRedis cfg
    case res of
      Left _  -> putMVar _kvdbConnections connections
      Right conn -> putMVar _kvdbConnections
        $ Map.insert connTag (kvdbToNative conn) connections
    pure res

interpretFlowMethod _ R.FlowRuntime {..} (L.DeInitKVDBConnection conn next) =
  fmap next $ do
    let connTag = kvdbConnToTag conn
    connections <- takeMVar _kvdbConnections
    case Map.lookup connTag connections of
      Nothing -> putMVar _kvdbConnections connections
      Just _ -> do
        R.kvDisconnect $ kvdbToNative conn
        putMVar _kvdbConnections $ Map.delete connTag connections

interpretFlowMethod _ R.FlowRuntime {..} (L.GetKVDBConnection cfg next) =
  fmap next $ do
    let connTag = kvdbConfigToTag cfg
    connMap <- readMVar _kvdbConnections
    pure $ case Map.lookup connTag connMap of
      Just conn -> Right $ nativeToKVDB connTag conn
      Nothing   -> Left $ KVDBError KVDBConnectionDoesNotExist $ "Connection for " +|| connTag ||+ " does not exists."

interpretFlowMethod mbFlowGuid flowRt (L.RunDB conn sqlDbMethod runInTransaction next) = do
    let dbgLogger msg =
          if R.shouldFlowLogRawSql flowRt
          then runLogger mbFlowGuid (R._loggerRuntime . R._coreRuntime $ flowRt)
               . L.logMessage' Debug ("RunDB Impl" :: String) $ Message (Just $ A.toJSON msg) Nothing
          else pure ()
    rawSqlTVar <- newTVarIO mempty
    -- This function would be used inside beam and write raw sql, generated by beam backend, in TVar.
    let dbgLogAction = \rawSqlStr -> atomically (modifyTVar' rawSqlTVar (`DL.snoc` rawSqlStr)) *> dbgLogger rawSqlStr
    fmap (next . fst . connPoolExceptionWrapper) $ tryAny $ if runInTransaction
      then do
        eRes <- R.withTransaction conn $ \nativeConn -> runSqlDB nativeConn dbgLogAction sqlDbMethod
        eRes' <- case eRes of
                  Left exception -> Left <$> wrapException exception
                  Right x        -> pure $ Right x
        rawSql <- DL.toList <$> readTVarIO rawSqlTVar
        pure (eRes', rawSql)
      else do
        eRes <- try @_ @SomeException $
          case conn of
            PostgresPool _ pool ->
              DP.withResource pool $ \conn' ->
                runSqlDB (NativePGConn conn') dbgLogAction $ sqlDbMethod
            MySQLPool _ pool ->
              DP.withResource pool $ \conn' ->
                runSqlDB (NativeMySQLConn conn') dbgLogAction $ sqlDbMethod
            SQLitePool _ pool ->
              DP.withResource pool $ \conn' ->
                runSqlDB (NativeSQLiteConn conn') dbgLogAction $ sqlDbMethod
        wrapAndSend rawSqlTVar eRes
  where
      wrapAndSend rawSqlLoc eResult = do
        rawSql <- DL.toList <$> readTVarIO rawSqlLoc
        eResult' <- case eResult of
          Left exception -> Left <$> wrapException exception
          Right x        -> pure $ Right x
        pure (eResult', rawSql)

      wrapException :: HasCallStack => SomeException -> IO DBError
      wrapException exception = do
        runLogger mbFlowGuid (R._loggerRuntime . R._coreRuntime $ flowRt)
               . L.logMessage' Debug ("CALLSTACK" :: String) $ Message (Just $ A.toJSON $ Text.pack $ prettyCallStack callStack) Nothing
        pure (wrapException' exception)

      wrapException' :: SomeException -> DBError
      wrapException' e = fromMaybe (DBError UnrecognizedError $ show e)
        (sqliteErrorToDbError   (show e) <$> fromException e <|>
          mysqlErrorToDbError    (show e) <$> fromException  e <|>
            postgresErrorToDbError (show e) <$> fromException e)

      connPoolExceptionWrapper :: Either SomeException (Either DBError _a1, [Text]) -> (Either DBError _a1, [Text])
      connPoolExceptionWrapper (Left e) = (Left $ DBError ConnectionFailed $ show e, [])
      connPoolExceptionWrapper (Right r) = r

interpretFlowMethod _ R.FlowRuntime {..} (L.RunKVDB cName act next) =
    next <$> runKVDB cName _kvdbConnections act

interpretFlowMethod mbFlowGuid rt@R.FlowRuntime {_pubSubController, _pubSubConnection} (L.RunPubSub act next) =
    case _pubSubConnection of
      Nothing -> go $ error "Connection to pubSub is not set in FlowRuntime"
      Just cn -> go cn
  where
    go conn = next <$> runPubSub _pubSubController conn
      (L.unpackLanguagePubSub act $ runFlow' mbFlowGuid rt)

interpretFlowMethod _ rt (L.WithModifiedRuntime f flow next) = next <$> runFlow (f rt) flow

runFlow' :: Maybe FlowGUID -> R.FlowRuntime -> L.Flow a -> IO a
runFlow' mbFlowGuid flowRt (L.Flow comp) = foldF (interpretFlowMethod mbFlowGuid flowRt) comp

runFlow :: R.FlowRuntime -> L.Flow a -> IO a
runFlow = runFlow' Nothing

-- Helpers

dbConfigToTag :: DBConfig beM -> ConnTag
dbConfigToTag = \case
  PostgresPoolConf t _ _ -> t
  MySQLPoolConf t _ _    -> t
  SQLitePoolConf t _ _   -> t

sqlConnToTag :: SqlConn beM -> ConnTag
sqlConnToTag = \case
  PostgresPool t _ -> t
  MySQLPool t _    -> t
  SQLitePool t _   -> t

kvdbConfigToTag :: KVDBConfig -> Text
kvdbConfigToTag = \case
  KVDBConfig t _        -> t
  KVDBClusterConfig t _ -> t

kvdbConnToTag :: KVDBConn -> Text
kvdbConnToTag (Redis t _) = t
