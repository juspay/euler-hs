
module EulerHS.Framework.Flow.Interpreter
  ( -- * Flow Interpreter
    runFlow
  , runFlow'
  ) where

import           Control.Exception (throwIO)
import qualified Control.Exception as Exception
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.CaseInsensitive as CI
import qualified Data.DList as DL
import           Data.Default (def)
import           Data.Either.Extra (mapLeft)
import           Data.Generics.Product.Positions (getPosition)
import qualified Data.Map as Map
import qualified Data.Pool as DP
import           Data.Profunctor (dimap)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.UUID as UUID (toText)
import qualified Data.UUID.V4 as UUID (nextRandom)
import qualified EulerHS.Core.Interpreters as R
import qualified EulerHS.Core.Logger.Language as L
import qualified EulerHS.Core.Runtime as R
import qualified EulerHS.Core.Types as T
import           EulerHS.Core.Types.KVDB
import qualified EulerHS.Framework.Flow.Language as L
import qualified EulerHS.Framework.Runtime as R
import           EulerHS.Prelude
import qualified Network.Connection as Conn
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import           Network.HTTP.Client.Internal
import qualified Network.HTTP.Types as HTTP
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS
import qualified Servant.Client as S
import           System.Process (readCreateProcess, shell)
import           Unsafe.Coerce (unsafeCoerce)

connect :: T.DBConfig be -> IO (T.DBResult (T.SqlConn be))
connect cfg = do
  eConn <- try $ T.mkSqlConn cfg
  case eConn of
    Left (e :: SomeException) -> pure $ Left $ T.DBError T.ConnectionFailed $ show e
    Right conn                -> pure $ Right conn

connectRedis :: T.KVDBConfig -> IO (T.KVDBAnswer T.KVDBConn)
connectRedis cfg = do
  eConn <- try $ T.mkRedisConn cfg
  case eConn of
    Left (e :: SomeException) -> pure $ Left $ T.KVDBError T.KVDBConnectionFailed $ show e
    Right conn                -> pure $ Right conn

disconnect :: T.SqlConn beM ->   IO ()
disconnect (T.MockedPool _)        = pure ()
disconnect (T.PostgresPool _ pool) = DP.destroyAllResources pool
disconnect (T.MySQLPool _ pool)    = DP.destroyAllResources pool
disconnect (T.SQLitePool _ pool)   = DP.destroyAllResources pool

suppressErrors :: IO a -> IO ()
suppressErrors = void . try @_ @SomeException

awaitMVarWithTimeout :: MVar (Either Text a) -> Int -> IO (Either T.AwaitingError a)
awaitMVarWithTimeout mvar mcs | mcs <= 0  = go 0
                              | otherwise = go mcs
  where
    portion = (mcs `div` 10) + 1
    go rest
      | rest <= 0 = do
        mValue <- tryReadMVar mvar
        pure $ case mValue of
          Nothing          -> Left T.AwaitingTimeout
          Just (Right val) -> Right val
          Just (Left err)  -> Left $ T.ForkedFlowError err
      | otherwise = do
          tryReadMVar mvar >>= \case
            Just (Right val) -> pure $ Right val
            Just (Left err)  -> pure $ Left $ T.ForkedFlowError err
            Nothing          -> threadDelay portion >> go (rest - portion)

-- | Utility function to convert HttpApi HTTPRequests to http-client HTTP
-- requests
getHttpLibRequest :: MonadThrow m => T.HTTPRequest -> m HTTP.Request
getHttpLibRequest request = do
  let url = Text.unpack $ T.getRequestURL request
  httpLibRequest <- HTTP.parseRequest url
  let
    requestMethod = case T.getRequestMethod request of
      T.Get    -> "GET"
      T.Put    -> "PUT"
      T.Post   -> "POST"
      T.Delete -> "DELETE"
      T.Head   -> "HEAD"
  let
    setBody = case T.getRequestBody request of
      Just body ->
        let body' = T.getLBinaryString body
        in  \req -> req { HTTP.requestBody = HTTP.RequestBodyLBS body' }
      Nothing   -> id

  -- TODO: Respect "Content-Transfer-Encoding" header
  let
    headers :: HTTP.RequestHeaders = T.getRequestHeaders request
      & Map.toList
      & map (\(x, y) -> (CI.mk (Encoding.encodeUtf8 x), Encoding.encodeUtf8 y))

  let
    setTimeout = case T.getRequestTimeout request of
      Just x  -> setRequestTimeout x
      Nothing -> setRequestTimeout T.defaultTimeout

  let
    setRedirects = case T.getRequestRedirects request of
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
translateHttpResponse :: HTTP.Response Lazy.ByteString -> Either Text T.HTTPResponse
translateHttpResponse response = do
  headers <- translateResponseHeaders $ HTTP.responseHeaders response
  status <-  translateResponseStatusMessage . HTTP.statusMessage . HTTP.responseStatus $ response
  pure $ T.HTTPResponse
    { getResponseBody    = T.LBinaryString $ HTTP.responseBody response
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

-- | Utility function to create a manager from certificate data
mkManagerFromCert :: T.HTTPCert -> IO (Either String HTTP.Manager)
mkManagerFromCert T.HTTPCert {..} = do
  case TLS.credentialLoadX509ChainFromMemory getCert getCertChain getCertKey of
    Right creds -> do
      let hooks = def { TLS.onCertificateRequest = \_ -> return $ Just creds }
      let clientParams = (TLS.defaultParamsClient getCertHost "")
                         { TLS.clientHooks = hooks
                         , TLS.clientSupported = def { TLS.supportedCiphers = TLS.ciphersuite_default }
                         }
      let tlsSettings = Conn.TLSSettings clientParams
      fmap Right $ HTTP.newManager $ TLS.mkManagerSettings tlsSettings Nothing
    Left err -> pure $ Left err

-- translateHeaderName :: CI.CI Strict.ByteString -> Text.Text
-- translateHeaderName = Encoding.decodeUtf8' . CI.original

{-|
Interprets the 'CallServantAPI' constructor of the 'FlowMethod' within the context of a specific 'FlowRuntime'.

Parameters:
  - 'mbFlowGuid': Maybe a 'T.FlowGUID' representing the flow GUID.
  - 'flowRt': The 'FlowRuntime' within which the 'CallServantAPI' action is being interpreted.
  - 'mbMgrSel': Maybe a 'T.ManagerSelector' indicating the HTTP client manager to use.
  - 'bUrl': The base URL for the API call.
  - 'clientAct': The servant client action to execute.
  - 'next': The continuation function to execute after interpreting the 'CallServantAPI' action.

Returns:
  - The result of the API call.

Notes:
  - This function is part of the interpreter for 'FlowMethod' actions.
  - It handles servant API calls, client manager selection, and logging.
-}
interpretFlowMethod :: HasCallStack => Maybe T.FlowGUID -> R.FlowRuntime -> L.FlowMethod a -> IO a
interpretFlowMethod mbFlowGuid flowRt@R.FlowRuntime {..} (L.CallServantAPI mbMgrSel bUrl clientAct next) =
    fmap next $ do
      let mbClientMngr = case mbMgrSel of
            Nothing       -> Right _defaultHttpClientManager
            Just mngrName -> maybeToRight mngrName $ Map.lookup mngrName _httpClientManagers
      case mbClientMngr of
        Right mngr -> do
          let S.ClientEnv manager baseUrl cookieJar makeClientRequest = S.mkClientEnv mngr bUrl
          let setR req = if HTTP.responseTimeout req == HTTP.responseTimeoutNone
                            then setRequestTimeout T.defaultTimeout req
                            else req {HTTP.responseTimeout = mResponseTimeout mngr}
          eitherResult <- S.runClientM (T.runEulerClient (dbgLogger T.Debug) getLoggerMaskConfig bUrl clientAct) $
            S.ClientEnv manager baseUrl cookieJar (\url -> setR . makeClientRequest url)
          case eitherResult of
            Left err -> do
              dbgLogger T.Error $ show err
              pure $ Left err
            Right response ->
              pure $ Right response
        Left name -> do
          let err = S.ConnectionError $ toException $ T.HttpManagerNotFound name
          dbgLogger T.Error (show err)
          pure $ Left err
  where
    dbgLogger debugLevel =
      R.runLogger mbFlowGuid (R._loggerRuntime . R._coreRuntime $ flowRt)
        . L.logMessage' debugLevel ("CallServantAPI impl" :: String)
        . show
    getLoggerMaskConfig =
      R.getLogMaskingConfig . R._loggerRuntime . R._coreRuntime $ flowRt

{-|
Interprets the 'CallHTTP' constructor of the 'FlowMethod' within the context of a specific 'FlowRuntime'.

Parameters:
  - 'mbFlowGuid': Maybe a 'T.FlowGUID' representing the flow GUID.
  - 'flowRt': The 'FlowRuntime' within which the 'CallHTTP' action is being interpreted.
  - 'request': The 'T.HTTPRequest' to be executed.
  - 'cert': Maybe a 'T.HTTPCert' representing the TLS certificate data.
  - 'next': The continuation function to execute after interpreting the 'CallHTTP' action.

Returns:
  - The result of the HTTP call.

Notes:
  - This function is part of the interpreter for 'FlowMethod' actions.
  - It handles HTTP calls, TLS certificate verification, and logging.
-}
interpretFlowMethod _ flowRt@R.FlowRuntime {..} (L.CallHTTP request cert next) =
    fmap next $ do
      httpLibRequest <- getHttpLibRequest request
      _manager <- maybe (pure $ Right _defaultHttpClientManager) mkManagerFromCert cert
      -- TODO: Refactor
      case _manager of
        Left err -> do
          let errMsg = "Certificate failure: " <> Text.pack err
          pure $ Left errMsg
        Right manager -> do
          eResponse <- try $ HTTP.httpLbs httpLibRequest manager
          case eResponse of
            Left (err :: SomeException) -> do
              let errMsg = Text.pack $ displayException err
              pure $ Left errMsg
            Right httpResponse -> do
              case translateHttpResponse httpResponse of
                Left errMsg -> do
                  logJsonError errMsg (T.maskHTTPRequest getLoggerMaskConfig request)
                  pure $ Left errMsg
                Right response -> do
                  logJson T.Debug 
                    $ T.HTTPRequestResponse 
                      (T.maskHTTPRequest getLoggerMaskConfig request) 
                      (T.maskHTTPResponse getLoggerMaskConfig response)
                  pure $ Right response
  where
    logJsonError :: Text -> T.HTTPRequest -> IO ()
    logJsonError err = logJson T.Error . T.HTTPIOException err
    logJson :: ToJSON a => T.LogLevel -> a -> IO ()
    logJson debugLevel =
      R.runLogger (Just "API CALL:") (R._loggerRuntime . R._coreRuntime $ flowRt)
        . L.logMessage' debugLevel ("callHTTP" :: String)
        . encodeJSON
    
    getLoggerMaskConfig = 
      R.getLogMaskingConfig . R._loggerRuntime . R._coreRuntime $ flowRt

interpretFlowMethod mbFlowGuid R.FlowRuntime {..} (L.EvalLogger loggerAct next) =
  next <$> R.runLogger mbFlowGuid (R._loggerRuntime _coreRuntime) loggerAct

interpretFlowMethod _ _ (L.RunIO _ ioAct next) =
  next <$> ioAct

{-
Interpret 'GetOption', 'SetOption', and 'DelOption' operations in the Flow runtime.

unsafeCoerce is a function in Haskell that allows you to change the type of a value without any type checking by the Haskell compiler. It essentially tells the compiler to treat the expression as if it had the desired type, even if the actual type doesn't match.

NOTE : While unsafeCoerce can be powerful and flexible, it comes with significant risks. If misused, it can lead to runtime errors or undefined behavior. It bypasses the type system, so there's no guarantee that the resulting program will be type-safe. It's generally recommended to avoid using unsafeCoerce unless you have a deep understanding of its implications and are confident in its safe use.
-}
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

interpretFlowMethod _ _ (L.GenerateGUID next) = do
  next <$> (UUID.toText <$> UUID.nextRandom)

interpretFlowMethod _ _ (L.RunSysCmd cmd next) =
  next <$> readCreateProcess (shell cmd) ""

----------------------------------------------------------------------
interpretFlowMethod mbFlowGuid rt (L.Fork _desc _newFlowGUID flow next) = do
  awaitableMVar <- newEmptyMVar
  void $ forkIO (suppressErrors (runFlow' mbFlowGuid rt (L.runSafeFlow flow) >>= putMVar awaitableMVar))
  pure $ next $ T.Awaitable awaitableMVar

----------------------------------------------------------------------

interpretFlowMethod _ _ (L.Await mbMcs (T.Awaitable awaitableMVar) next) = do
  let act = case mbMcs of
        Nothing -> do
          val <- readMVar awaitableMVar
          case val of
            Left err  -> pure $ Left $ T.ForkedFlowError err
            Right res -> pure $ Right res
        Just (T.Microseconds mcs) -> awaitMVarWithTimeout awaitableMVar $ fromIntegral mcs
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
    let connTag = getPosition @1 cfg
    connMap <- takeMVar _sqldbConnections
    res <- case Map.lookup connTag connMap of
      Just _ -> pure $ Left $ T.DBError T.ConnectionAlreadyExists $ "Connection for " <> connTag <> " already created."
      Nothing -> connect cfg
    case res of
      Right conn -> putMVar _sqldbConnections $ Map.insert connTag (T.bemToNative conn) connMap
      Left _     -> putMVar _sqldbConnections connMap
    pure res

interpretFlowMethod _ R.FlowRuntime {..} (L.DeInitSqlDBConnection conn next) =
  fmap next $ do
    let connTag = getPosition @1 conn
    connMap <- takeMVar _sqldbConnections
    case Map.lookup connTag connMap of
      Nothing -> putMVar _sqldbConnections connMap
      Just _ -> do
        disconnect conn
        putMVar _sqldbConnections $ Map.delete connTag connMap

interpretFlowMethod _ R.FlowRuntime {..} (L.GetSqlDBConnection cfg next) =
  fmap next $ do
    let connTag = getPosition @1 cfg
    connMap <- readMVar _sqldbConnections
    pure $ case Map.lookup connTag connMap of
      Just conn -> Right $ T.nativeToBem connTag conn
      Nothing   -> Left $ T.DBError T.ConnectionDoesNotExist $ "Connection for " <> connTag <> " does not exists."

interpretFlowMethod _ R.FlowRuntime {..} (L.InitKVDBConnection cfg next) =
  fmap next $ do
    let connTag = getPosition @1 cfg
    connections <- takeMVar _kvdbConnections
    res <- case Map.lookup connTag connections of
      Just _  -> pure $ Left $ T.KVDBError T.KVDBConnectionAlreadyExists $ "Connection for " +|| connTag ||+ " already created."
      Nothing -> connectRedis cfg
    case res of
      Left _  -> putMVar _kvdbConnections connections
      Right conn -> putMVar _kvdbConnections
        $ Map.insert connTag (kvdbToNative conn) connections
    pure res

interpretFlowMethod _ R.FlowRuntime {..} (L.DeInitKVDBConnection conn next) =
  fmap next $ do
    let connTag = getPosition @1 conn
    connections <- takeMVar _kvdbConnections
    case Map.lookup connTag connections of
      Nothing -> putMVar _kvdbConnections connections
      Just _ -> do
        R.kvDisconnect $ kvdbToNative conn
        putMVar _kvdbConnections $ Map.delete connTag connections

interpretFlowMethod _ R.FlowRuntime {..} (L.GetKVDBConnection cfg next) =
  fmap next $ do
    let connTag = getPosition @1 cfg
    connMap <- readMVar _kvdbConnections
    pure $ case Map.lookup connTag connMap of
      Just conn -> Right $ T.nativeToKVDB connTag conn
      Nothing   -> Left $ KVDBError KVDBConnectionDoesNotExist $ "Connection for " +|| connTag ||+ " does not exists."

{-|
Interprets the 'RunDB' constructor of the 'FlowMethod' within the context of a specific 'FlowRuntime'.

Parameters:
  - 'mbFlowGuid': Maybe a 'Text' representing the flow GUID.
  - 'flowRt': The 'FlowRuntime' within which the 'RunDB' action is being interpreted.
  - 'conn': The database connection information.
  - 'sqlDbMethod': The SQL database method to execute.
  - 'runInTransaction': A flag indicating whether the SQL operation should run within a transaction.
  - 'next': The continuation function to execute after interpreting the 'RunDB' action.

Returns:
  - A tuple containing the result of the SQL operation and a list of raw SQL statements executed.

Notes:
  - This function is part of the interpreter for 'FlowMethod' actions.
  - It handles transaction management, logging, and exception wrapping for SQL operations.
-}
interpretFlowMethod mbFlowGuid flowRt (L.RunDB conn sqlDbMethod runInTransaction next) = do
    let dbgLogger =
          if R.shouldFlowLogRawSql flowRt
          then R.runLogger mbFlowGuid (R._loggerRuntime . R._coreRuntime $ flowRt)
               . L.logMessage' T.Debug ("RunDB Impl" :: String)
          else const $ pure ()
    rawSqlTVar <- newTVarIO mempty
    -- This function would be used inside beam and write raw sql, generated by beam backend, in TVar.
    let dbgLogAction = \rawSqlStr -> atomically (modifyTVar' rawSqlTVar (`DL.snoc` rawSqlStr)) *> dbgLogger rawSqlStr
    -- TODO: unify the below two branches
    fmap (next . fst) $ fmap connPoolExceptionWrapper $ tryAny $ case runInTransaction of
      True ->
        case conn of
          (T.MockedPool _) -> error "Mocked Pool not implemented"
          _ -> do
            eRes <- R.withTransaction conn $ \nativeConn ->
              R.runSqlDB nativeConn dbgLogAction sqlDbMethod
            eRes' <- case eRes of
                       Left exception -> Left <$> wrapException exception
                       Right x        -> pure $ Right x
            rawSql <- DL.toList <$> readTVarIO rawSqlTVar
            pure (eRes', rawSql)
      False ->
        case conn of
          (T.MockedPool _)        -> error "Mocked Pool not implemented"
          (T.PostgresPool _ pool) -> do
            eRes <- try @_ @SomeException . DP.withResource pool $ \conn' -> 
                        R.runSqlDB (T.NativePGConn conn') dbgLogAction $ sqlDbMethod
            wrapAndSend rawSqlTVar eRes
          (T.MySQLPool _ pool)    -> do
            eRes <- try @_ @SomeException . DP.withResource pool $ \conn' -> 
                        R.runSqlDB (T.NativeMySQLConn conn') dbgLogAction $ sqlDbMethod
            wrapAndSend rawSqlTVar eRes
          (T.SQLitePool _ pool)   -> do
            eRes <- try @_ @SomeException . DP.withResource pool $ \conn' -> 
                        R.runSqlDB (T.NativeSQLiteConn conn') dbgLogAction $ sqlDbMethod
            wrapAndSend rawSqlTVar eRes
  where
      wrapAndSend rawSqlLoc eResult = do
        rawSql <- DL.toList <$> readTVarIO rawSqlLoc
        eResult' <- case eResult of
          Left exception -> Left <$> wrapException exception
          Right x        -> pure $ Right x
        pure (eResult', rawSql)

      wrapException :: HasCallStack => SomeException -> IO T.DBError
      wrapException exception = do
        R.runLogger mbFlowGuid (R._loggerRuntime . R._coreRuntime $ flowRt)
               . L.logMessage' T.Debug ("CALLSTACK" :: String) $ Text.pack $ prettyCallStack callStack
        pure (wrapException' exception)

      wrapException' :: SomeException -> T.DBError
      wrapException' e = fromMaybe (T.DBError T.UnrecognizedError $ show e)
        (T.sqliteErrorToDbError   (show e) <$> fromException e <|>
          T.mysqlErrorToDbError    (show e) <$> fromException e <|>
            T.postgresErrorToDbError (show e) <$> fromException e)

      connPoolExceptionWrapper :: Either SomeException (Either T.DBError _a1, [Text]) -> (Either T.DBError _a1, [Text])
      connPoolExceptionWrapper (Left e) = (Left $ T.DBError T.ConnectionFailed $ show e, [])
      connPoolExceptionWrapper (Right r) = r

interpretFlowMethod _ R.FlowRuntime {..} (L.RunKVDB cName act next) =
    next <$> R.runKVDB cName _kvdbConnections act

interpretFlowMethod mbFlowGuid rt@R.FlowRuntime {_pubSubController, _pubSubConnection} (L.RunPubSub act next) =
    case _pubSubConnection of
      Nothing -> go $ error "Connection to pubSub is not set in FlowRuntime"
      Just cn -> go cn
  where
    go conn = next <$> R.runPubSub _pubSubController conn
      (L.unpackLanguagePubSub act $ runFlow' mbFlowGuid rt)

interpretFlowMethod _ rt (L.WithModifiedRuntime f flow next) = next <$> runFlow (f rt) flow

runFlow' :: HasCallStack => Maybe T.FlowGUID -> R.FlowRuntime -> L.Flow a -> IO a
runFlow' mbFlowGuid flowRt (L.Flow comp) = foldF (interpretFlowMethod mbFlowGuid flowRt) comp

runFlow :: HasCallStack => R.FlowRuntime -> L.Flow a -> IO a
runFlow = runFlow' Nothing
