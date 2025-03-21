{-# LANGUAGE DeriveAnyClass, CPP #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EulerHS.Extra.Language
  ( getOrInitSqlConn
  , getOrInitKVDBConn
  , keyToSlot
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
  , generateSnowflake
#if defined(REDIS_CORE_EXPORT)
  , module EulerHS.Extra.Redis
#endif
  , isArtRecEnabled
  , shouldARTRecord
  , shouldARTV2Record
  , isArtV2RecEnabledForTable
  , isArtV2ReplayEnabledForTable
  , withArtRecOptionsForKVDB
  ) where

import qualified Data.Text as Text
import           Data.Time (LocalTime, addUTCTime, defaultTimeLocale,
                            formatTime, utc, utcToZonedTime,
                            zonedTimeToLocalTime)
import qualified EulerHS.Framework.Language as L
import           Database.Beam (Beamable, FromBackendRow, SqlInsert)
import           Database.Beam.MySQL (MySQL, MySQLM)
import           Database.Redis (keyToSlot)
import           EulerHS.Extra.Snowflakes.Types (StackID (..), PodID (..), Snowflake, SnowflakeError(Fatal))
import           EulerHS.KVDB.Types (KVDBAnswer, KVDBConfig, KVDBConn,
                                     KVDBReplyF (..), KVDBError(..))
import           EulerHS.Logger.Types (LogContext,ErrorL(..))
import           EulerHS.Prelude hiding (get, id)
#if defined(REDIS_CORE_EXPORT)
import           EulerHS.Extra.Redis
#else
import           EulerHS.Extra.Redis (withArtRecOptionsForKVDB)
#endif
import           EulerHS.Runtime ( FlowRuntime (..))
import           EulerHS.Logger.Runtime ( LoggerRuntime (..), CoreRuntime(..))
import           EulerHS.SqlDB.Language (SqlDB, insertRowReturningMySQL,
                                         insertRowsReturningList)
import qualified EulerHS.SqlDB.Types as T
import           Servant (err500)
import           EulerHS.ART.Utils (isArtRecEnabled, shouldARTRecord)
import           EulerHS.ART.V2.Utils (shouldARTV2Record, isArtV2RecEnabledForTable, isArtV2ReplayEnabledForTable)


-- | Retrieves the current UTC time, but as a 'LocalTime'.
--
-- @since 2.1.0.1
getCurrentTimeUTC :: (L.MonadFlow m) => m LocalTime
getCurrentTimeUTC = go
  where
    go :: (L.MonadFlow m) => m LocalTime
    go = zonedTimeToLocalTime . utcToZonedTime utc <$> L.getCurrentTime

-- | Retrieves the current POSIX time, rounded to seconds.
--
-- @since 2.1.0.1
getCurrentDateInSeconds :: (L.MonadFlow m) => m Int
getCurrentDateInSeconds = (floor <$> L.getPOSIXTime)

-- | Retrieves the current POSIX time, rounded to milliseconds.
--
-- @since 2.1.0.1
getCurrentDateInMillis :: (L.MonadFlow m) => m Int
getCurrentDateInMillis = do
  t <- (* 1000) <$> L.getPOSIXTime
  pure . floor $ t

-- | Given a number of seconds as an offset, return a date string, in the format
-- YYYY-MM-ddTHH:MM:SSZ, representing the current time, offset by the specified
-- number of seconds.
--
-- @since 2.1.0.1
getCurrentDateStringWithSecOffset :: (L.MonadFlow m) => Int -> m Text
getCurrentDateStringWithSecOffset secs = do
  now <- L.getCurrentTime
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
    L.logErrorWithCategory @Text "" errMsg $ ErrorL Nothing "" errMsg
    L.throwException . mkException $ errMsg
  Right _  -> pure ()

checkFailedWithLog :: (HasCallStack, ToJSON e, Show e, L.MonadFlow m) => Either e a -> Text -> m ()
checkFailedWithLog (Left err) msg = L.logErrorWithCategoryV @Text "" (msg, err) $ ErrorL Nothing "" (msg <> " " <> show err <> "")
checkFailedWithLog _ _ = pure ()

-- | As 'logInfo', but specialized for logging 'Text' tags.
--
-- @since 2.1.0.1
logInfoT :: forall (m :: Type -> Type) .
  (HasCallStack, L.MonadFlow m) => Text -> Text -> m ()
logInfoT = L.logInfo @Text

-- | As 'L.logError', but specialized for logging 'Text' tags.
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
      let errorReason = show err
      L.logErrorWithCategory @Text "SqlDB connect" errorReason $ ErrorL Nothing "MYSQL_EXCEPTION" errorReason
      L.throwException err500
    Right conn -> do
      res <- run conn act
      case res of
        Left err  -> do
          L.incrementDbMetric err conf
          L.logErrorWithCategoryV @Text "SqlDB interaction" (err) $ ErrorL Nothing "MYSQL_EXCEPTION" (show err)
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
      L.logErrorWithCategory @Text "unsafeInsertRow" err' $ ErrorL Nothing "DB_EXCEPTION" err'
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
      L.logErrorWithCategory @Text "unsafeInsertRowMySQL" err' $ ErrorL Nothing "MYSQL_EXCEPTION" err'
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
    
generateSnowflake :: (L.MonadFlow m) => String -> m (Either SnowflakeError Snowflake)
generateSnowflake key = do
  mbStackID <- L.getOption StackID
  mbPodID <- L.getOption PodID
  case (mbStackID, mbPodID) of
    (Just stackId, Just podId) -> L.getSnowflakeID stackId podId key
    (Nothing, Just _) ->  return . Left . Fatal $ "StackID not set in options"
    (Just _, Nothing) -> return . Left . Fatal $ "PodID not set in options"
    _ -> return . Left . Fatal $ "PodID and StackID not set in options"