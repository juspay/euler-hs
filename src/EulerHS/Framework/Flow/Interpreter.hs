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
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.UUID as UUID (toText)
import qualified Data.UUID.V4 as UUID (nextRandom)
import qualified Data.Vector as V
import qualified EulerHS.Core.Interpreters as R
import qualified EulerHS.Core.Logger.Language as L
import qualified EulerHS.Core.Playback.Entries as P
import qualified EulerHS.Core.Playback.Machine as P
import qualified EulerHS.Core.Runtime as R
import qualified EulerHS.Core.Types as T
import           EulerHS.Core.Types.KVDB
import qualified EulerHS.Framework.Flow.Language as L
import qualified EulerHS.Framework.Runtime as R
import           EulerHS.Prelude
import qualified Network.Connection as Conn
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
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

-- TODO: update the code with timeouts when servant 0.18.1 is released
-- (see the code in the private downstream repo)

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
      Just x  -> \req -> req {HTTP.responseTimeout = HTTP.responseTimeoutMicro x}
      Nothing -> id

  let
    setRedirects = case T.getRequestRedirects request of
      Just x  -> \req -> req {HTTP.redirectCount = x}
      Nothing -> id

  pure $ setRedirects . setTimeout . setBody $
      httpLibRequest
        { HTTP.method         = requestMethod
        , HTTP.requestHeaders = headers
        }

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

interpretFlowMethod :: Maybe T.FlowGUID -> R.FlowRuntime -> L.FlowMethod a -> IO a
interpretFlowMethod mbFlowGuid flowRt@R.FlowRuntime {..} (L.CallServantAPI mbMgrSel bUrl clientAct next) =
    fmap next $ P.withRunMode _runMode (P.mkCallServantAPIEntry bUrl) $ do
      let mbClientMngr = case mbMgrSel of
            Nothing       -> Right _defaultHttpClientManager
            Just mngrName -> maybeToRight mngrName $ Map.lookup mngrName _httpClientManagers
      case mbClientMngr of
        Right mngr -> do
          eitherResult <- S.runClientM (T.runEulerClient (dbgLogger T.Debug) bUrl clientAct) (S.mkClientEnv mngr bUrl)
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
      R.runLogger mbFlowGuid T.RegularMode (R._loggerRuntime . R._coreRuntime $ flowRt)
        . L.logMessage' debugLevel ("CallServantAPI impl" :: String)
        . show

interpretFlowMethod mbFlowGuid flowRt@R.FlowRuntime {..} (L.CallHTTP request cert next) =
    fmap next $ P.withRunMode _runMode (P.mkCallHttpAPIEntry request) $ do
      httpLibRequest <- getHttpLibRequest request
      _manager <- maybe (pure $ Right _defaultHttpClientManager) mkManagerFromCert cert
      -- TODO: Refactor
      case _manager of
        Left err -> do
          let errMsg = "Certificate failure: " <> Text.pack err
          logJsonError errMsg request
          pure $ Left errMsg
        Right manager -> do
          eResponse <- try $ HTTP.httpLbs httpLibRequest manager
          case eResponse of
            Left (err :: SomeException) -> do
              let errMsg = Text.pack $ displayException err
              logJsonError errMsg request
              pure $ Left errMsg
            Right httpResponse -> do
              case translateHttpResponse httpResponse of
                Left errMsg -> do
                  logJsonError errMsg request
                  pure $ Left errMsg
                Right response -> do
                  logJson T.Debug $ T.HTTPRequestResponse request response
                  pure $ Right response
  where
    logJsonError :: Text -> T.HTTPRequest -> IO ()
    logJsonError err = logJson T.Error . T.HTTPIOException err

    logJson :: ToJSON a => T.LogLevel -> a -> IO ()
    logJson debugLevel =
      R.runLogger mbFlowGuid T.RegularMode (R._loggerRuntime . R._coreRuntime $ flowRt)
        . L.logMessage' debugLevel ("callHTTP failure" :: String)
        . encodeJSON

interpretFlowMethod mbFlowGuid R.FlowRuntime {..} (L.EvalLogger loggerAct next) =
  next <$> R.runLogger mbFlowGuid _runMode (R._loggerRuntime _coreRuntime) loggerAct

interpretFlowMethod _ R.FlowRuntime {..} (L.RunIO descr ioAct next) =
  next <$> P.withRunMode _runMode (P.mkRunIOEntry descr) ioAct

interpretFlowMethod _ R.FlowRuntime {..} (L.RunUntracedIO descr ioAct next) =
  case _runMode of
    (T.RecordingMode recorderRt) ->
      next <$> P.record recorderRt (P.mkRunUntracedIOEntry descr) ioAct
    _ ->
      next <$> ioAct

interpretFlowMethod _ R.FlowRuntime {..} (L.GetOption k next) =
  fmap next $ P.withRunMode _runMode (P.mkGetOptionEntry k) $ do
    m <- readMVar _options
    pure $ do
      valAny <- Map.lookup k m
      pure $ unsafeCoerce valAny

interpretFlowMethod _ R.FlowRuntime {..} (L.SetOption k v next) =
  fmap next $ P.withRunMode _runMode (P.mkSetOptionEntry k v) $ do
    m <- takeMVar _options
    let newMap = Map.insert k (unsafeCoerce @_ @Any v) m
    putMVar _options newMap

interpretFlowMethod _ R.FlowRuntime {..} (L.DelOption k next) =
  fmap next $ P.withRunMode _runMode (P.mkDelOptionEntry k) $ do
    m <- takeMVar _options
    let newMap = Map.delete k m
    putMVar _options newMap

interpretFlowMethod _ R.FlowRuntime {_runMode} (L.GenerateGUID next) = do
  next <$> P.withRunMode _runMode P.mkGenerateGUIDEntry
    (UUID.toText <$> UUID.nextRandom)

interpretFlowMethod _ R.FlowRuntime {_runMode} (L.RunSysCmd cmd next) =
  next <$> P.withRunMode _runMode
    (P.mkRunSysCmdEntry cmd)
    (readCreateProcess (shell cmd) "")

----------------------------------------------------------------------
interpretFlowMethod mbFlowGuid rt (L.Fork desc newFlowGUID flow next) = do
  awaitableMVar <- newEmptyMVar
  case R._runMode rt of
    T.RegularMode -> void $ forkIO (suppressErrors (runFlow' mbFlowGuid rt (L.runSafeFlow flow) >>= putMVar awaitableMVar))

    T.RecordingMode T.RecorderRuntime{recording = T.Recording{..}, ..} -> do
      finalRecordingMVar       <- newEmptyMVar
      finalSafeRecordingVar    <- newEmptyMVar
      finalForkedRecordingsVar <- newEmptyMVar

      forkRecordingMVar        <- newMVar V.empty
      forkSafeRecordingVar     <- newMVar Map.empty
      forkForkedRecordingsVar  <- newMVar Map.empty

      let freshRecording = T.Recording forkRecordingMVar forkSafeRecordingVar forkForkedRecordingsVar
      let emptyRecording = T.Recording finalRecordingMVar finalSafeRecordingVar finalForkedRecordingsVar

      let forkRuntime = T.RecorderRuntime
            { flowGUID  = newFlowGUID
            , recording = freshRecording
            , ..
            }

      forkedRecs <- takeMVar forkedRecordingsVar
      putMVar forkedRecordingsVar $
        Map.insert newFlowGUID emptyRecording forkedRecs

      let newRt = rt {R._runMode = T.RecordingMode forkRuntime}

      void $ forkIO $ do
        suppressErrors (runFlow' mbFlowGuid newRt (L.runSafeFlow flow) >>= putMVar awaitableMVar)
        putMVar finalRecordingMVar       =<< readMVar forkRecordingMVar
        putMVar finalSafeRecordingVar    =<< readMVar forkSafeRecordingVar
        putMVar finalForkedRecordingsVar =<< readMVar forkForkedRecordingsVar

----------------------------------------------------------------------

    T.ReplayingMode playerRt -> do
      let
        T.PlayerRuntime
          { rerror       = T.ReplayErrors   {..}
          , resRecording = T.ResultRecording{ forkedRecordings }
          , ..
          } = playerRt

      case Map.lookup newFlowGUID forkedRecordings of
        Nothing -> do
          let
            err =
              T.PlaybackError
                { errorType    = T.ForkedFlowRecordingsMissed
                , errorMessage = "No recordings found for forked flow: " <> Text.unpack newFlowGUID
                , errorFlowGUID = flowGUID }

          takeMVar errorMVar *> putMVar errorMVar (Just err)
          throwIO $ T.ReplayingException err

        Just recording -> do
          stepVar           <- newMVar 0

          finalErrorMVar          <- newEmptyMVar
          finalSafeFlowErrorVar   <- newEmptyMVar
          finalForkedFlowErrorVar <- newEmptyMVar

          forkErrorMVar           <- newMVar Nothing
          forkSafeFlowErrorVar    <- newMVar Map.empty
          forkForkedFlowErrorVar  <- newMVar Map.empty

          let freshReplayErrors = T.ReplayErrors forkErrorMVar forkSafeFlowErrorVar forkForkedFlowErrorVar
          let finalReplayErrors = T.ReplayErrors finalErrorMVar finalSafeFlowErrorVar finalForkedFlowErrorVar

          let forkRuntime = T.PlayerRuntime
                { flowGUID     = newFlowGUID
                , stepMVar     = stepVar
                , resRecording = recording
                , rerror       = freshReplayErrors
                , ..
                }

          forkedFlowErrs <- takeMVar forkedFlowErrorsVar

          putMVar forkedFlowErrorsVar $
            Map.insert newFlowGUID finalReplayErrors forkedFlowErrs

          let newRt = rt {R._runMode = T.ReplayingMode forkRuntime}
          void $ forkIO $ do
            suppressErrors (runFlow' mbFlowGuid newRt (L.runSafeFlow flow) >>= putMVar awaitableMVar)
            putMVar finalErrorMVar          =<< readMVar forkErrorMVar
            putMVar finalSafeFlowErrorVar   =<< readMVar forkSafeFlowErrorVar
            putMVar finalForkedFlowErrorVar =<< readMVar forkForkedFlowErrorVar

----------------------------------------------------------------------
----------------------------------------------------------------------

  void $ P.withRunMode (R._runMode rt) (P.mkForkEntry desc newFlowGUID) (pure ())
  pure $ next $ T.Awaitable awaitableMVar

----------------------------------------------------------------------

interpretFlowMethod _ R.FlowRuntime {..} (L.Await mbMcs (T.Awaitable awaitableMVar) next) = do
  let act = case mbMcs of
        Nothing -> do
          val <- readMVar awaitableMVar
          case val of
            Left err  -> pure $ Left $ T.ForkedFlowError err
            Right res -> pure $ Right res
        Just (T.Microseconds mcs) -> awaitMVarWithTimeout awaitableMVar $ fromIntegral mcs
  next <$> P.withRunMode _runMode (P.mkAwaitEntry mbMcs) act

interpretFlowMethod _ R.FlowRuntime {_runMode} (L.ThrowException ex _) = do
  void $ P.withRunMode _runMode (P.mkThrowExceptionEntry ex) (pure ())
  throwIO ex

interpretFlowMethod mbFlowGuid rt@R.FlowRuntime {_runMode} (L.RunSafeFlow newFlowGUID flow next) = fmap next $ do
  fl <- case R._runMode rt of
    T.RegularMode -> do
      fl <- try @_ @SomeException $ runFlow' mbFlowGuid rt flow
      pure $ mapLeft show fl

    T.RecordingMode T.RecorderRuntime{recording = T.Recording{..}, ..} -> do
      freshRecordingMVar       <- newMVar V.empty

      let freshRecording = T.Recording freshRecordingMVar safeRecordingsVar forkedRecordingsVar

      let safeRuntime = T.RecorderRuntime
            { flowGUID  = newFlowGUID
            , recording = freshRecording
            , ..
            }

      let newRt = rt {R._runMode = T.RecordingMode safeRuntime}

      fl <- try @_ @SomeException $ runFlow' mbFlowGuid newRt flow

      freshRec <- readMVar freshRecordingMVar

      safeRecs <- takeMVar safeRecordingsVar

      putMVar safeRecordingsVar $
        Map.insert newFlowGUID freshRec safeRecs

      pure $ mapLeft show fl

----------------------------------------------------------------------

    T.ReplayingMode playerRt -> do
      let
        T.PlayerRuntime
          { rerror       = T.ReplayErrors {..}
          , resRecording
          , ..
          } = playerRt

        T.ResultRecording{ safeRecordings } = resRecording

      case Map.lookup newFlowGUID safeRecordings of
        Nothing -> do
          let
            err =
              T.PlaybackError
                { errorType    = T.SafeFlowRecordingsMissed
                , errorMessage = "No recordings found for safe flow " <> Text.unpack newFlowGUID
                , errorFlowGUID = flowGUID }

          takeMVar errorMVar *> putMVar errorMVar (Just err)
          throwIO $ T.ReplayingException err

        Just (newrecording :: T.RecordingEntries) -> do
          stepVar           <- newMVar 0
          freshErrorMVar    <- newMVar Nothing

          let freshReplayErrors = T.ReplayErrors freshErrorMVar safeFlowErrorsVar forkedFlowErrorsVar

          let forkRuntime = T.PlayerRuntime
                { flowGUID     = newFlowGUID
                , stepMVar     = stepVar
                , resRecording = resRecording { T.recording = newrecording }
                , rerror       = freshReplayErrors
                , ..
                }

          let newRt = rt {R._runMode = T.ReplayingMode forkRuntime}
          fl <- try @_ @SomeException $ runFlow' mbFlowGuid newRt flow

          safeFlowErrs <- takeMVar safeFlowErrorsVar
          freshError   <- takeMVar freshErrorMVar

          putMVar safeFlowErrorsVar $
            case freshError of
              Just err -> Map.insert newFlowGUID err safeFlowErrs
              Nothing  -> safeFlowErrs

          pure $ mapLeft show fl

----------------------------------------------------------------------

  P.withRunMode (R._runMode rt) (P.mkRunSafeFlowEntry newFlowGUID) (pure fl)


interpretFlowMethod _ R.FlowRuntime {..} (L.InitSqlDBConnection cfg next) =
  fmap next $ P.withRunMode _runMode (P.mkInitSqlDBConnectionEntry cfg) $ do
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
  fmap next $ P.withRunMode _runMode (P.mkDeInitSqlDBConnectionEntry conn) $ do
    let connTag = getPosition @1 conn
    connMap <- takeMVar _sqldbConnections
    case Map.lookup connTag connMap of
      Nothing -> putMVar _sqldbConnections connMap
      Just _ -> do
        disconnect conn
        putMVar _sqldbConnections $ Map.delete connTag connMap

interpretFlowMethod _ R.FlowRuntime {..} (L.GetSqlDBConnection cfg next) =
  fmap next $ P.withRunMode _runMode (P.mkGetSqlDBConnectionEntry cfg) $ do
    let connTag = getPosition @1 cfg
    connMap <- readMVar _sqldbConnections
    pure $ case Map.lookup connTag connMap of
      Just conn -> Right $ T.nativeToBem connTag conn
      Nothing   -> Left $ T.DBError T.ConnectionDoesNotExist $ "Connection for " <> connTag <> " does not exists."

interpretFlowMethod _ R.FlowRuntime {..} (L.InitKVDBConnection cfg next) =
  fmap next $ P.withRunMode _runMode (P.mkInitKVDBConnectionEntry cfg) $ do
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
  fmap next $ P.withRunMode _runMode (P.mkDeInitKVDBConnectionEntry conn) $ do
    let connTag = getPosition @1 conn
    connections <- takeMVar _kvdbConnections
    case Map.lookup connTag connections of
      Nothing -> putMVar _kvdbConnections connections
      Just _ -> do
        R.kvDisconnect $ kvdbToNative conn
        putMVar _kvdbConnections $ Map.delete connTag connections

interpretFlowMethod _ R.FlowRuntime {..} (L.GetKVDBConnection cfg next) =
  fmap next $ P.withRunMode _runMode (P.mkGetKVDBConnectionEntry cfg) $ do
    let connTag = getPosition @1 cfg
    connMap <- readMVar _kvdbConnections
    pure $ case Map.lookup connTag connMap of
      Just conn -> Right $ T.nativeToKVDB connTag conn
      Nothing   -> Left $ KVDBError KVDBConnectionDoesNotExist $ "Connection for " +|| connTag ||+ " does not exists."

interpretFlowMethod mbFlowGuid flowRt (L.RunDB conn sqlDbMethod runInTransaction next) = do
    let runMode   = R._runMode flowRt
    let dbgLogger =
          if R.shouldFlowLogRawSql flowRt
          then R.runLogger mbFlowGuid T.RegularMode (R._loggerRuntime . R._coreRuntime $ flowRt)
               . L.logMessage' T.Debug ("RunDB Impl" :: String)
          else const $ pure ()
    rawSqlTVar <- newTVarIO mempty
    -- This function would be used inside beam and write raw sql, generated by beam backend, in TVar.
    let dbgLogAction = \rawSqlStr -> atomically (modifyTVar' rawSqlTVar (`DL.snoc` rawSqlStr)) *> dbgLogger rawSqlStr
    -- TODO: unify the below two branches
    fmap (next . fst)  $ P.withRunMode runMode P.mkRunDBEntry $ case runInTransaction of
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
          (T.PostgresPool _ pool) -> DP.withResource pool $ \conn' -> do
            eRes <- try @_ @SomeException . R.runSqlDB (T.NativePGConn conn') dbgLogAction $ sqlDbMethod
            wrapAndSend rawSqlTVar eRes
          (T.MySQLPool _ pool)    -> DP.withResource pool $ \conn' -> do
            eRes <- try @_ @SomeException . R.runSqlDB (T.NativeMySQLConn conn') dbgLogAction $ sqlDbMethod
            wrapAndSend rawSqlTVar eRes
          (T.SQLitePool _ pool)   -> DP.withResource pool $ \conn' -> do
            eRes <- try @_ @SomeException . R.runSqlDB (T.NativeSQLiteConn conn') dbgLogAction $ sqlDbMethod
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
        R.runLogger mbFlowGuid T.RegularMode (R._loggerRuntime . R._coreRuntime $ flowRt)
               . L.logMessage' T.Debug ("CALLSTACK" :: String) $ Text.pack $ prettyCallStack callStack
        pure (wrapException' exception)

      wrapException' :: SomeException -> T.DBError
      wrapException' e = fromMaybe (T.DBError T.UnrecognizedError $ show e)
        (T.sqliteErrorToDbError   (show e) <$> fromException e <|>
          T.mysqlErrorToDbError    (show e) <$> fromException e <|>
            T.postgresErrorToDbError (show e) <$> fromException e)


interpretFlowMethod _ R.FlowRuntime {..} (L.RunKVDB cName act next) =
    next <$> R.runKVDB cName _runMode _kvdbConnections act


interpretFlowMethod mbFlowGuid rt@R.FlowRuntime {_runMode, _pubSubController, _pubSubConnection} (L.RunPubSub act next) =
    case (_pubSubConnection, _runMode) of
      (Nothing, T.ReplayingMode _) -> go $ error "Connection mock. Shold not ever be evaluated"
      (Just cn, _                ) -> go cn
      _                            -> error "RunPubSub method called, while proper Redis connection has not been provided"
  where
    go conn = next <$> R.runPubSub _runMode _pubSubController conn
      (L.unpackLanguagePubSub act $ runFlow' mbFlowGuid $ rt { R._runMode = T.RegularMode })

runFlow' :: Maybe T.FlowGUID -> R.FlowRuntime -> L.Flow a -> IO a
runFlow' mbFlowGuid flowRt (L.Flow comp) = foldF (interpretFlowMethod mbFlowGuid flowRt) comp

runFlow :: R.FlowRuntime -> L.Flow a -> IO a
runFlow = runFlow' Nothing
