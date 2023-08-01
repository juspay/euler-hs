{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EulerHS.Extra.Language
  ( getOrInitSqlConn
  , getOrInitKVDBConn
  , rExpire
  , rExpireB
  , rDel
  , rDelB
  , rExists
  , rExistsB
  , rExistsT -- alias for rExists (back compat)
  , rHget
  , rHgetB
  , rHset
  , rHsetB
  , rIncr
  , rIncrB
  , rSet
  , rSetT  -- alias for rSet (back compat)
  , rSetB
  , rGet
  , rGetB
  , rGetT  -- alias for rGet (back compat)
  , rSetex
  , rSetexB
  , rSetexT  -- alias for rSetex (back compat)
  , rXreadB
  , rXreadT
  , rXrevrangeT
  , rXrevrangeB
  , rSetexBulk
  , rSetexBulkB
  , rSetOpts
  , rSetOptsB
  , rSetOptsT
  , keyToSlot
  , rSadd
  , rSismember
  , sRemB
  , rZAdd
  , rZRangeByScore
  , rZRangeByScoreWithLimit
  , rZRem
  , rZRemRangeByScore
  , rZCard
  -- * Logging
  , AppException(..)
  , throwOnFailedWithLog
  , checkFailedWithLog
  , updateLoggerContext
  -- , withLoggerContext
  , logInfoT
  , logWarningT
  , logErrorT
  , logDebugT
  -- * Time and date
  , getCurrentTimeUTC
  , getCurrentDateInSeconds
  , getCurrentDateInMillis
  , getCurrentDateStringWithSecOffset
  -- * SQL database
  , withDB
  , withDBTransaction
  , insertRow
  , unsafeInsertRow
  , insertRowMySQL
  , unsafeInsertRowMySQL
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as BSL
import           Data.Either.Extra (fromEither, mapLeft)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import           Data.Time (LocalTime, addUTCTime, defaultTimeLocale,
                            formatTime, getCurrentTime, utc, utcToZonedTime,
                            zonedTimeToLocalTime)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Database.Beam (Beamable, FromBackendRow, SqlInsert)
import           Database.Beam.MySQL (MySQL, MySQLM)
import           Database.Redis (keyToSlot)
import           EulerHS.Extra.Aeson (obfuscate)
import qualified EulerHS.Framework.Language as L
import qualified EulerHS.KVDB.Language as L
import           EulerHS.KVDB.Types (KVDBAnswer, KVDBConfig, KVDBConn,
                                     KVDBReply, KVDBReplyF (..), KVDBError(..),
                                     KVDBStatus)
import           EulerHS.Logger.Types (LogContext)
import           EulerHS.Prelude hiding (get, id)
import           EulerHS.Runtime ( FlowRuntime (..))
import           EulerHS.Logger.Runtime ( LoggerRuntime (..), CoreRuntime(..))
import           EulerHS.SqlDB.Language (SqlDB, insertRowReturningMySQL,
                                         insertRowsReturningList)
import qualified EulerHS.SqlDB.Types as T
import           Servant (err500)

type RedisName = Text
type TextKey = Text
type TextField = Text
type ByteKey = ByteString
type ByteField = ByteString
type ByteValue = ByteString

-- | Retrieves the current UTC time, but as a 'LocalTime'.
--
-- @since 2.1.0.1
getCurrentTimeUTC :: (L.MonadFlow m) => m LocalTime
getCurrentTimeUTC = L.runIO' "getCurrentTimeUTC" go
  where
    go :: IO LocalTime
    go = zonedTimeToLocalTime . utcToZonedTime utc <$> getCurrentTime

-- | Retrieves the current POSIX time, rounded to seconds.
--
-- @since 2.1.0.1
getCurrentDateInSeconds :: (L.MonadFlow m) => m Int
getCurrentDateInSeconds = L.runIO' "getCurrentDateInSeconds" (floor <$> getPOSIXTime)

-- | Retrieves the current POSIX time, rounded to milliseconds.
--
-- @since 2.1.0.1
getCurrentDateInMillis :: (L.MonadFlow m) => m Int
getCurrentDateInMillis = L.runIO' "getCurrentDateInMillis" $ do
  t <- (* 1000) <$> getPOSIXTime
  pure . floor $ t

-- | Given a number of seconds as an offset, return a date string, in the format
-- YYYY-MM-ddTHH:MM:SSZ, representing the current time, offset by the specified
-- number of seconds.
--
-- @since 2.1.0.1
getCurrentDateStringWithSecOffset :: (L.MonadFlow m) => Int -> m Text
getCurrentDateStringWithSecOffset secs = do
  now <- L.runIO' "getCurrentDateStringWithSecOffset" getCurrentTime
  let offset = addUTCTime (realToFrac secs) now
  pure . Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" $ offset

-- | An app-specific exception.
--
-- @since 2.1.0.1
data AppException =
  SqlDBConnectionFailedException Text |
  KVDBConnectionFailedException Text
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Exception AppException

-- | Transforms a 'Left' result into an exception, logging this outcome. Does
-- nothing on a 'Right'.
--
-- @since 2.1.0.1
throwOnFailedWithLog :: (HasCallStack, Show e, L.MonadFlow m) =>
  Either e a -> (Text -> AppException) -> Text -> m ()
throwOnFailedWithLog res mkException msg = case res of
  Left err -> do
    let errMsg = msg <> " " <> show err
    L.logError @Text "" errMsg
    L.throwException . mkException $ errMsg
  Right _  -> pure ()

checkFailedWithLog :: (HasCallStack, Show e, L.MonadFlow m) => Either e a -> Text -> m ()
checkFailedWithLog (Left err) msg = L.logError @Text "" $ msg <> " " <> show err <> ""
checkFailedWithLog _ _ = pure ()

-- | As 'logInfo', but specialized for logging 'Text' tags.
--
-- @since 2.1.0.1
logInfoT :: forall (m :: Type -> Type) .
  (HasCallStack, L.MonadFlow m) => Text -> Text -> m ()
logInfoT = L.logInfo @Text

-- | As 'logError', but specialized for logging 'Text' tags.
--
-- @since 2.1.0.1
logErrorT :: forall (m :: Type -> Type) .
  (HasCallStack, L.MonadFlow m) => Text -> Text -> m ()
logErrorT = L.logError @Text

-- | As 'logDebug', but specialized for logging 'Text' tags.
--
-- @since 2.1.0.1
logDebugT :: forall (m :: Type -> Type) .
  (HasCallStack, L.MonadFlow m) => Text -> Text -> m ()
logDebugT = L.logDebug @Text

-- | As 'logWarning', but specialized for logging 'Text' tags.
--
-- @since 2.1.0.1
logWarningT :: forall (m :: Type -> Type) .
  (HasCallStack, L.MonadFlow m) => Text -> Text -> m ()
logWarningT = L.logWarning @Text

-- | Creates a connection and runs a DB operation. Throws on connection failure
-- or if the operation fails; this will log if either of these things happens.
--
-- NOTE: This does /not/ run inside a transaction.
--
-- @since 2.1.0.1
withDB :: (HasCallStack, L.MonadFlow m, T.BeamRunner beM, T.BeamRuntime be beM) =>
  T.DBConfig beM -> SqlDB beM a -> m a
withDB = withDB' L.runDB

-- | As 'withDB', but runs inside a transaction.
--
-- @since 2.1.0.1
withDBTransaction :: (HasCallStack, L.MonadFlow m, T.BeamRunner beM, T.BeamRuntime be beM) =>
  T.DBConfig beM -> SqlDB beM a -> m a
withDBTransaction = withDB' L.runTransaction

-- Internal helper
withDB' :: (HasCallStack, L.MonadFlow m) =>
  (T.SqlConn beM -> SqlDB beM a -> m (T.DBResult a)) ->
  T.DBConfig beM ->
  SqlDB beM a ->
  m a
withDB' run conf act = do
  mConn <- L.getSqlDBConnection conf
  case mConn of
    Left err   -> do
      L.logError @Text "SqlDB connect" . show $ err
      L.throwException err500
    Right conn -> do
      res <- run conn act
      case res of
        Left err  -> do
          L.incrementDbMetric err conf
          L.logError @Text "SqlDB interaction" . show $ err
          L.throwException err500
        Right val -> pure val

-- | Inserts several rows, returning the first successful inserted result. Use
-- this function with care: if your insert ends up inserting nothing
-- successfully, this will return a 'Left'.
--
-- @since 2.1.0.1
insertRow ::
  (HasCallStack,
    L.MonadFlow m,
    T.BeamRunner beM,
    T.BeamRuntime be beM,
    Beamable table,
    FromBackendRow be (table Identity)) =>
  T.DBConfig beM -> SqlInsert be table -> m (Either Text (table Identity))
insertRow conf ins = do
  results <- withDBTransaction conf . insertRowsReturningList $ ins
  pure $ case results of
    []      -> Left "Unexpected empty result."
    (x : _) -> Right x

-- | As 'insertRow', but instead throws the provided exception on failure. Will
-- also log in such a case.
--
-- @since 2.1.0.1
unsafeInsertRow ::
  (HasCallStack,
    L.MonadFlow m,
    T.BeamRunner beM,
    T.BeamRuntime be beM,
    Beamable table,
    FromBackendRow be (table Identity),
    Exception e) =>
  e -> T.DBConfig beM -> SqlInsert be table -> m (table Identity)
unsafeInsertRow err conf ins = do
  res <- insertRow conf ins
  case res of
    Left err' -> do
      L.logError @Text "unsafeInsertRow" err'
      L.throwException err
    Right x -> pure x

-- | MySQL-specific version of 'insertRow'.
--
-- @since 2.1.0.1
insertRowMySQL ::
  (HasCallStack,
    L.MonadFlow m,
    FromBackendRow MySQL (table Identity)) =>
  T.DBConfig MySQLM -> SqlInsert MySQL table -> m (Either Text (table Identity))
insertRowMySQL conf ins = do
  results <- withDBTransaction conf . insertRowReturningMySQL $ ins
  pure $ case results of
    Nothing -> Left "Unexpected empty result."
    Just x  -> Right x

-- | MySQL-specific version of 'unsafeInsertRow'.
--
-- @since 2.1.0.1
unsafeInsertRowMySQL ::
  (HasCallStack,
    L.MonadFlow m,
    FromBackendRow MySQL (table Identity),
    Exception e) =>
  e -> T.DBConfig MySQLM -> SqlInsert MySQL table -> m (table Identity)
unsafeInsertRowMySQL err conf ins = do
  res <- insertRowMySQL conf ins
  case res of
    Left err' -> do
      L.logError @Text "unsafeInsertRowMySQL" err'
      L.throwException err
    Right x -> pure x

-- | Get existing SQL connection, or init a new connection.
getOrInitSqlConn :: (HasCallStack, L.MonadFlow m) =>
  T.DBConfig beM -> m (T.DBResult (T.SqlConn beM))
getOrInitSqlConn cfg = do
  eConn <- L.getSqlDBConnection cfg
  case eConn of
    Left err -> do
      L.incrementDbMetric err cfg
      newCon <- L.initSqlDBConnection cfg
      case newCon of
        Left err' -> L.incrementDbMetric err' cfg *> pure newCon
        val -> pure val
    res                                         -> pure res

-- | Get existing Redis connection, or init a new connection.
getOrInitKVDBConn :: (HasCallStack, L.MonadFlow m) => KVDBConfig -> m (KVDBAnswer KVDBConn)
getOrInitKVDBConn cfg = do
  conn <- L.getKVDBConnection cfg
  case conn of
    Left (KVDBError KVDBConnectionDoesNotExist _) -> L.initKVDBConnection cfg
    res                                           -> pure res

-- KVDB convenient functions

-- ----------------------------------------------------------------------------

rExpire :: (HasCallStack, Integral t, L.MonadFlow m) =>
  RedisName -> TextKey -> t -> m (Either KVDBReply Bool)
rExpire cName k = rExpireB cName (TE.encodeUtf8 k)

rExpireB :: (HasCallStack, Integral t, L.MonadFlow m) =>
  RedisName -> ByteKey -> t -> m (Either KVDBReply Bool)
rExpireB cName k t = do
  res <- L.runKVDB cName $ L.expire k $ toInteger t
  case res of
    Right _ -> do
      -- L.logInfo @Text "Redis expire" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis expire" $ show err
      pure res

-- ----------------------------------------------------------------------------

rDel :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> [TextKey] -> m (Either KVDBReply Integer)
rDel cName ks = rDelB cName (TE.encodeUtf8 <$> ks)

rDelB :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> [ByteKey] -> m (Either KVDBReply Integer)
rDelB cName ks = do
  res <- L.runKVDB cName $ L.del ks
  case res of
    Right _ -> do
      -- L.logInfo @Text "Redis del" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis del" $ show err
      pure res

-- ----------------------------------------------------------------------------

rExists :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> TextKey -> m (Either KVDBReply Bool)
rExists cName k = rExistsB cName $ TE.encodeUtf8 k

rExistsB :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> ByteKey -> m (Either KVDBReply Bool)
rExistsB cName k = do
  res <- L.runKVDB cName $ L.exists k
  case res of
    Right _ -> do
      -- L.logInfo @Text "Redis exists" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis exists" $ show err
      pure res

rExistsT :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> TextKey -> m (Either KVDBReply Bool)
rExistsT = rExists

-- ----------------------------------------------------------------------------

rHget :: (HasCallStack, FromJSON v, L.MonadFlow m)
  => RedisName -> TextKey -> TextField -> m (Maybe v)
rHget cName k f = do
  let k' = TE.encodeUtf8 k
  let f' = TE.encodeUtf8 f
  r <- L.runKVDB cName $ L.hget k' f'
  case r of
    Right (Just val) -> do
      let v = A.eitherDecode $ BSL.fromStrict val
      case v of
        Left err -> do
          L.logError @Text "Decoding error: " $ show err
          pure Nothing
        Right v' -> do
          -- L.logDebug @Text "Decoded value" $ show v'
          pure $ Just v'
    Right Nothing -> pure Nothing
    Left err -> do
      L.logError @Text "Redis rHget" $ show err
      pure Nothing

rHgetB :: (HasCallStack, L.MonadFlow m) =>
  Text -> ByteKey -> ByteField -> m (Maybe ByteValue)
rHgetB cName k f = do
  res <- L.runKVDB cName $ L.hget k f
  case res of
    Right (Just val) -> pure $ Just val
    Right Nothing -> pure Nothing
    Left err -> do
      L.logError @Text "Redis hget" $ show err
      pure Nothing

-- ----------------------------------------------------------------------------

rHset :: (HasCallStack, ToJSON v, L.MonadFlow m)
  => RedisName -> TextKey -> TextField -> v -> m (Either KVDBReply Bool)
rHset cName k f v = rHsetB cName k' f' v'
  where
    k' = TE.encodeUtf8 k
    f' = TE.encodeUtf8 f
    v' = BSL.toStrict $ A.encode v

rHsetB :: (HasCallStack, L.MonadFlow m)
  => RedisName -> ByteKey -> ByteField -> ByteValue -> m (Either KVDBReply Bool)
rHsetB cName k f v = do
  res <- L.runKVDB cName $
    L.hset k f v
  case res of
    Right _ -> do
      -- L.logInfo @Text "Redis hset" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis hset" $ show err
      pure res

-- ----------------------------------------------------------------------------

rIncr :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> TextKey -> m (Either KVDBReply Integer)
rIncr cName k = rIncrB cName (TE.encodeUtf8 k)

rIncrB :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> ByteKey -> m (Either KVDBReply Integer)
rIncrB cName k = do
  res <- L.runKVDB cName $ L.incr k
  case res of
    Right _ -> do
      -- L.logInfo @Text "Redis incr" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis incr" $ show err
      pure res

-- ----------------------------------------------------------------------------

rSet :: (HasCallStack, ToJSON v, L.MonadFlow m) =>
  RedisName -> TextKey -> v -> m (Either KVDBReply KVDBStatus)
rSet cName k v = rSetB cName k' v'
  where
    k' = TE.encodeUtf8 k
    v' = BSL.toStrict $ A.encode v

rSetB :: (HasCallStack, L.MonadFlow m) =>
  Text -> ByteKey -> ByteValue -> m (Either KVDBReply KVDBStatus)
rSetB cName k v = do
  res <- L.runKVDB cName $ L.set k v
  case res of
    Right _ -> do
      -- L.logInfo @Text "Redis set" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis set" $ show err
      pure res

rSetT :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> TextKey -> Text -> m (Either KVDBReply KVDBStatus)
rSetT cName k v = rSetB cName k' v'
  where
    k' = TE.encodeUtf8 k
    v' = TE.encodeUtf8 v

-- ----------------------------------------------------------------------------

rGetB :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> ByteKey -> m (Maybe ByteValue) -- Binary.decode?
rGetB cName k = do
  mv <- L.runKVDB cName $ L.get k
  case mv of
    Right mval -> pure mval
    Left err -> do
      L.logError @Text "Redis get" $ show err
      pure Nothing

rGet :: (HasCallStack, FromJSON v, L.MonadFlow m) =>
  RedisName -> TextKey -> m (Maybe v)
rGet cName k = do
  -- L.logDebug @Text "rGet" $ "looking up key: " <> k <> " in redis: " <> cName
  mv <- rGetB cName (TE.encodeUtf8 k)
  case mv of
    Just val -> case A.eitherDecode' @A.Value $ BSL.fromStrict val of
      Left err -> do
        L.logError @Text "rGet value is not a valid JSON" $ "error: '" <> toText err
                                  <> "' while decoding value: "
                                  <> (fromEither $ mapLeft (toText . displayException) $ TE.decodeUtf8' val)
        pure Nothing
      Right value -> do
        case (A.parseEither A.parseJSON value) of
          Left err -> do
            L.logError @Text "rGet value cannot be decoded to target type" $ "error: '" <> toText err
                                      <> "' while decoding value: "
                                      <> (TE.decodeUtf8 . BSL.toStrict . A.encode . obfuscate) value
            pure Nothing
          Right v -> pure $ Just v
    Nothing -> pure Nothing

rGetT :: (HasCallStack, L.MonadFlow m) =>
  Text -> Text -> m (Maybe Text)
rGetT cName k = do
  mv <- rGetB cName (TE.encodeUtf8 k)
  case mv of
    Just val ->
      case TE.decodeUtf8' val of
        Left err -> do
          L.logError @Text "Redis rGetT unicode decode error" (show err)
          pure Nothing
        Right x ->
          pure $ Just x
    Nothing -> pure Nothing

-- ----------------------------------------------------------------------------

rSetex :: (HasCallStack, ToJSON v, Integral t, L.MonadFlow m) =>
  RedisName -> TextKey -> v -> t -> m (Either KVDBReply KVDBStatus)
rSetex cName k v = rSetexB cName k' v'
  where
    k' = TE.encodeUtf8 k
    v' = BSL.toStrict $ A.encode v

rSetexB :: (HasCallStack, Integral t, L.MonadFlow m) =>
  RedisName -> ByteKey -> ByteValue -> t -> m (Either KVDBReply KVDBStatus)
rSetexB cName k v t = do
  res <- L.runKVDB cName $ L.setex k (toInteger t) v
  case res of
    Right _ -> do
      -- L.logInfo @Text "Redis setex" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis setex" $ show err
      pure res

rSetexT :: (HasCallStack, ToJSON v, Integral t, L.MonadFlow m) =>
  RedisName -> TextKey -> v -> t -> m (Either KVDBReply KVDBStatus)
rSetexT = rSetex

rSetexBulk :: (HasCallStack, ToJSON v, Integral t, L.MonadFlow m) =>
  RedisName -> Map TextKey v -> t -> m (Either KVDBReply ())
rSetexBulk cName kvMap = rSetexBulkB cName kvMap'
  where
    encodeKey = TE.encodeUtf8
    encodeVal = BSL.toStrict . A.encode
    kvMap' =
      Map.fromList . map (\(k, v) -> (encodeKey k, encodeVal v)) $ Map.toList kvMap

rSetexBulkB :: (HasCallStack, Integral t, L.MonadFlow m) =>
  RedisName -> Map ByteKey ByteValue -> t -> m (Either KVDBReply ())
rSetexBulkB cName kvMap t = do
  let t' = toInteger t
  res <- L.runKVDB cName $ forM_ (Map.toList kvMap) $ \(k, v) -> L.setex k t' v
  case res of
    Right _ -> do
      -- L.logInfo @Text "Redis setexBulk" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis setexBulk" $ show err
      pure res

-- ----------------------------------------------------------------------------

rSetOpts
  :: (HasCallStack, ToJSON v, L.MonadFlow m)
  => RedisName
  -> TextKey
  -> v
  -> L.KVDBSetTTLOption
  -> L.KVDBSetConditionOption
  -> m (Either KVDBReply Bool)
rSetOpts cName k v = rSetOptsB cName k' v'
  where
    k' = TE.encodeUtf8 k
    v' = BSL.toStrict $ A.encode v

rSetOptsB
  :: (HasCallStack, L.MonadFlow m)
  => RedisName
  -> ByteKey
  -> ByteValue
  -> L.KVDBSetTTLOption
  -> L.KVDBSetConditionOption
  -> m (Either KVDBReply Bool)
rSetOptsB cName k v ttl cond = do
  res <- L.runKVDB cName $ L.setOpts k v ttl cond
  case res of
    Right _ -> pure res
    Left err -> do
      L.logError @Text "Redis setOpts" $ show err
      pure res

rSetOptsT
  :: (HasCallStack, L.MonadFlow m)
  => RedisName
  -> TextKey
  -> Text
  -> L.KVDBSetTTLOption
  -> L.KVDBSetConditionOption
  -> m (Either KVDBReply Bool)
rSetOptsT cName k v = rSetOptsB cName k' v'
  where
    k' = TE.encodeUtf8 k
    v' = TE.encodeUtf8 v

rXreadT
  :: (HasCallStack, L.MonadFlow m)
  => RedisName
  -> Text
  ->  Text
  -> m (Either KVDBReply (Maybe [L.KVDBStreamReadResponse]))
rXreadT cName k v = rXreadB cName k' v'
  where
    k' = TE.encodeUtf8 k
    v' = TE.encodeUtf8 v

rXreadB :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> L.KVDBStream -> L.RecordID -> m (Either KVDBReply (Maybe [L.KVDBStreamReadResponse]))
rXreadB cName strm entryId = do
  res <- L.runKVDB cName $ L.xread strm entryId
  _ <-  case res of
    Left err ->
      L.logError @Text "Redis xread" $ show err
    Right _ -> pure ()
  pure res

rXrevrangeT :: (HasCallStack,L.MonadFlow m) =>
  RedisName -> Text -> Text -> Text -> Maybe Integer -> m (Either KVDBReply ([L.KVDBStreamReadResponseRecord]))
rXrevrangeT cName strm send sstart count = rXrevrangeB cName s' se' ss' count
  where
    s' = TE.encodeUtf8 strm
    se' = TE.encodeUtf8 send
    ss' = TE.encodeUtf8 sstart

rXrevrangeB :: (HasCallStack,L.MonadFlow m) =>
  RedisName -> L.KVDBStream -> L.KVDBStreamEnd -> L.KVDBStreamStart -> Maybe Integer -> m (Either KVDBReply ([L.KVDBStreamReadResponseRecord]))
rXrevrangeB cName strm send sstart count = do
  res <- L.runKVDB cName $ L.xrevrange strm send sstart count
  _ <- case res of
    Left err ->
      L.logError @Text "Redis xrevrange" $ show err
    Right _ -> pure ()
  pure res
-- ------------------------------------------------------------------------------

rSadd :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> L.KVDBKey -> [L.KVDBValue] -> m (Either KVDBReply Integer)
rSadd cName k v = do
  res <- L.runKVDB cName $ L.sadd k v
  case res of
    Right _ -> pure res
    Left err -> do
      L.logError @Text "Redis sadd" $ show err
      pure res

rSismember :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> L.KVDBKey -> L.KVDBValue -> m (Either KVDBReply Bool)
rSismember cName k v = do
  res <- L.runKVDB cName $ L.sismember k v
  case res of
    Right _ -> pure res
    Left err -> do
      L.logError @Text "Redis sismember" $ show err
      pure res

-- withLoggerContext :: (HasCallStack, L.MonadFlow m) => (LogContext -> LogContext) -> L.Flow a -> m a
-- withLoggerContext updateLCtx = L.withModifiedRuntime (updateLoggerContext updateLCtx)

updateLoggerContext :: (IORef LogContext -> IO (IORef LogContext)) -> FlowRuntime -> IO (FlowRuntime)
updateLoggerContext updateLCtx rt@FlowRuntime{..} = do
  newLrt <- newLrtIO
  pure $ rt { _coreRuntime = _coreRuntime {_loggerRuntime = newLrt} }
  where
    newLrtIO :: IO LoggerRuntime
    newLrtIO = case _loggerRuntime _coreRuntime of
              MemoryLoggerRuntime a lc b c d -> do
                newCtx <- updateLCtx lc
                pure $ MemoryLoggerRuntime a newCtx b c d
              -- the next line is courtesy to Kyrylo Havryliuk ;-)
              LoggerRuntime{_logContext, ..} -> do
                newCtx <- updateLCtx _logContext
                pure $ LoggerRuntime {_logContext = newCtx, ..}

rZAdd :: (HasCallStack, L.MonadFlow m) =>
  RedisName
  -> L.KVDBKey
  -> [(Double,ByteValue)]
  -> m (Either KVDBReply Integer)
rZAdd cName k v = do
  res <- L.runKVDB cName $ L.zadd k v
  case res of
    Right _ -> pure res
    Left err -> do
      L.logError @Text "Redis setOpts" $ show err
      pure res

rZRangeByScore :: (HasCallStack, L.MonadFlow m) =>
  RedisName
  -> L.KVDBKey
  -> Double
  -> Double
  -> m (Either KVDBReply [L.KVDBValue])
rZRangeByScore cName k minScore maxScore = do
  res <- L.runKVDB cName $ L.zrangebyscore k minScore maxScore
  case res of
    Right _ -> pure res
    Left err -> do
      L.logError @Text "Redis rZRangeByScore" $ show err
      pure res

rZRangeByScoreWithLimit :: (HasCallStack, L.MonadFlow m) =>
  RedisName
  -> L.KVDBKey
  -> Double
  -> Double
  -> Integer
  -> Integer
  -> m (Either KVDBReply [L.KVDBValue])
rZRangeByScoreWithLimit cName k minScore maxScore offset count = do
  res <- L.runKVDB cName $ L.zrangebyscorewithlimit k minScore maxScore offset count
  case res of
    Right _ -> pure res
    Left err -> do
      L.logError @Text "Redis rZRangeByScoreWithLimit" $ show err
      pure res

rZRem :: (HasCallStack, L.MonadFlow m) =>
  RedisName
  -> L.KVDBKey
  -> [L.KVDBValue]
  -> m (Either KVDBReply Integer)
rZRem cName k v = do
  res <- L.runKVDB cName $ L.zrem k v
  case res of
    Right _ -> pure res
    Left err -> do
      L.logError @Text "Redis rZRem" $ show err
      pure res

sRemB :: (HasCallStack, L.MonadFlow m) => RedisName -> L.KVDBKey -> [L.KVDBValue] -> m (KVDBAnswer Integer)
sRemB redisName oldSKey pKeyList = sRemB'  
  where 
    sRemB' :: (HasCallStack, L.MonadFlow m) => m (KVDBAnswer Integer)
    sRemB' = do
      res <- L.runKVDB redisName $ L.srem oldSKey pKeyList
      case res of
        Right _ -> pure res
        Left err -> do
          L.logErrorWithCategory @Text "Redis sRemB" (show err)
          pure res

rZRemRangeByScore :: (HasCallStack, L.MonadFlow m) =>
  RedisName
  -> L.KVDBKey
  -> Double
  -> Double
  -> m (Either KVDBReply Integer)
rZRemRangeByScore cName k minScore maxScore = do
  res <- L.runKVDB cName $ L.zremrangebyscore k minScore maxScore
  case res of
    Right _ -> pure res
    Left err -> do
      L.logError @Text "Redis rZRemRangeByScore" $ show err
      pure res

rZCard :: (HasCallStack, L.MonadFlow m) =>
  RedisName
  -> L.KVDBKey
  -> m (Either KVDBReply Integer)
rZCard cName k = do
  res <- L.runKVDB cName $ L.zcard k
  case res of
    Right _ -> pure res
    Left err -> do
      L.logError @Text "Redis rZCard" $ show err
      pure res