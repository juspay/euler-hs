{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module EulerHS.ART.DBReplay where

import qualified Data.Aeson as A
import           Data.Either.Extra (mapLeft)
import qualified EulerHS.Language as L
import           EulerHS.Prelude
import qualified EulerHS.Types as T
import           Sequelize (Model, ModelMeta, modelTableName, Set (..), Where)
import qualified Servant as S
import           EulerHS.ART.FlowUtils (addRecToState)
import           EulerHS.ART.V2.FlowUtils (producePayload)
import           EulerHS.Extra.KafkaClient.Utils (ValueType(..))
import qualified EulerHS.ART.EnvVars as Env
import qualified EulerHS.ART.Utils as ARTUtils
import qualified EulerHS.ART.V2.Utils as ARTV2Utils
import           EulerHS.KVConnector.Types (MeshResult, MeshMeta(..), KVEntry(..))
import           EulerHS.ART.Types (RunDBEntry(..), RecordingEntry(..))
import           EulerHS.ART.V2.Types (ShouldRecordJoin(..))
import EulerHS.KVConnector.Utils
import           Data.Time (getCurrentTime, utc, utcToZonedTime, zonedTimeToLocalTime)
import           EulerHS.KVConnector.DBSync (whereClauseToJson)
import           EulerHS.SqlDB.Types (ModelDBConfig)
import           Database.Beam as B
import qualified EulerHS.ART.ReplayFunctions as ER
import EulerHS.KVDB.Types (MeshError(..))
import qualified Data.ByteString.Lazy as BS
import qualified GHC.Stack as GHC
import EulerHS.ART.IOReplay (runIOWithART)

getCurrentDateInMillis :: (L.MonadFlow m) => m Int
getCurrentDateInMillis = do
   t <- (* 1000) <$> L.getPOSIXTime
   pure . floor $ t

getLatencyInMicroSeconds :: Integer -> Integer
getLatencyInMicroSeconds execTime = execTime `div` 1000000

parseDataReplayList ::(FromJSON b,L.MonadFlow m,HasCallStack) => BS.ByteString -> m (Either T.DBError [b]) -- TODO might have to handle in future
parseDataReplayList res = do
  let eReply = A.eitherDecode res :: (FromJSON b) => Either String (Either T.DBError [b])
  case eReply of
    Left err -> do
      let errorMessage = "Failed to decode response: " <> encodeUtf8 err
      when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_DB" $ GHC.prettyCallStack $ GHC.callStack
      L.throwException $ S.err400 {S.errBody = errorMessage}
    Right reply -> pure reply

parseDataReplay ::(FromJSON b, L.MonadFlow m,HasCallStack, FromJSON err) => BS.ByteString -> m (Either err b)
parseDataReplay res = do
  let eReply = A.eitherDecode res :: (FromJSON b, FromJSON err) => Either String (Either err b)
  case eReply of
    Left err -> do
      let errorMessage = "Failed to decode response: " <> encodeUtf8 err
      when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_DB" $ GHC.prettyCallStack $ GHC.callStack
      L.throwException $ S.err400 {S.errBody = errorMessage}
    Right reply -> pure reply

runWithArtFindSqlSelectWithJoin ::
  forall be beM table m.
    (Model be table
    , FromJSON (table Identity)
    , ToJSON (table Identity)
    , L.MonadFlow m
    ) =>
  ModelDBConfig beM ->
  (Text -> B.SqlSelect be (table Identity)) ->
  ShouldRecordJoin ->
  Text ->
  m (Either T.DBError [KVEntry table]) ->
  m (Either T.DBError [KVEntry table])
runWithArtFindSqlSelectWithJoin _dbConf _findSqlSelect recordJoin method hsDbFunc = do
    isArtV2ReplayEnabledWithSessId <- ARTV2Utils.shouldReplayForDB @table
    let isArtV2ReplayEnabledForJoin = Env.isArtV2ReplayEnabled && Env.shouldReplayAllJoins && ARTV2Utils.shouldRecordDBTable @table && recordJoin == RECORD_JOIN -- Replay join if it was recorded & all joins replay is enabled
    if Env.isArtReplayEnabled || isArtV2ReplayEnabledWithSessId || isArtV2ReplayEnabledForJoin
      then do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.ART.DBReplay::runWithArtFindSqlSelectWithJoin::getCurrentTime" getCurrentTime)
        msessionId <- L.getLoggerContext "x-request-id"
        resp <- ER.callBrahmaReplayDB (RunDBEntryT (RunDBEntry method A.Null A.Null (modelTableName @table) (A.Null) recTimestamp)) msessionId
        parseDataReplayList resp
      else do
        tmp_res <- hsDbFunc
        whenM ARTUtils.isArtRecEnabled $ do
          recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.ART.DBReplay::runWithArtFindSqlSelectWithJoin::getCurrentTime" getCurrentTime)
          addRecToState $ RunDBEntryT (RunDBEntry method A.Null A.Null (modelTableName @table) (toJSON tmp_res) recTimestamp)
        whenM (ARTV2Utils.isArtV2RecEnabledForTable @table) $ do
          recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.ART.DBReplay::runWithArtFindSqlSelectWithJoin::getCurrentTime" getCurrentTime)
          producePayload (bool DB DB_JOINS $ recordJoin == RECORD_JOIN) . BS.toStrict . A.encode $ RunDBEntryT (RunDBEntry method A.Null A.Null (modelTableName @table) (toJSON tmp_res) recTimestamp)
        pure tmp_res


runWithArtFindALL ::
  forall be beM table m.
    (Model be table
    , FromJSON (table Identity)
    , ToJSON (table Identity)
    , MeshMeta be table
    , L.MonadFlow m
    ) =>
  ModelDBConfig beM->
  Where be table ->
  Text ->
  m (Either T.DBError [KVEntry table]) ->
  m (Either T.DBError [KVEntry table])
runWithArtFindALL _dbConf whereClause method hsDbFunc = do
    isArtV2ReplayEnabledWithSessId <- ARTV2Utils.shouldReplayForDB @table
    if Env.isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
      then do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.ART.DBReplay::runWithArtFindALL::getCurrentTime" getCurrentTime)
        msessionId <- L.getLoggerContext "x-request-id"
        resp <- ER.callBrahmaReplayDB (RunDBEntryT (RunDBEntry method A.Null (whereClauseToJson whereClause) (modelTableName @table) (A.Null) recTimestamp)) msessionId
        parseDataReplayList resp
      else do
        tmp_res <- hsDbFunc
        whenM ARTUtils.isArtRecEnabled $ do
          recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.ART.DBReplay::runWithArtFindALL::getCurrentTime" getCurrentTime)
          addRecToState $ RunDBEntryT (RunDBEntry method A.Null (whereClauseToJson whereClause) (modelTableName @table) (toJSON tmp_res) recTimestamp)
        whenM (ARTV2Utils.isArtV2RecEnabledForTable @table) $ do
          recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.ART.DBReplay::runWithArtFindALL::getCurrentTime" getCurrentTime)
          producePayload DB . BS.toStrict . A.encode $ RunDBEntryT (RunDBEntry method A.Null (whereClauseToJson whereClause) (modelTableName @table) (toJSON tmp_res) recTimestamp)
        pure tmp_res

runWithArtCount ::
  forall be beM table m.
    (Model be table
    , MeshMeta be table
    , L.MonadFlow m
    ) =>
  ModelDBConfig beM->
  Where be table ->
  Text ->
  m (Either T.DBError Int) ->
  m (Either T.DBError Int)
runWithArtCount _dbConf whereClause method hsDbFunc = do
    isArtV2ReplayEnabledWithSessId <- ARTV2Utils.shouldARTV2Replay
    let isArtV2ReplayEnabledForAggregate = isArtV2ReplayEnabledWithSessId && ARTV2Utils.shouldRecordDBTable @table -- Replay aggregate query if it was recorded
    if Env.isArtReplayEnabled || isArtV2ReplayEnabledForAggregate
      then do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.ART.DBReplay::runWithArtCount::getCurrentTime" getCurrentTime)
        msessionId <- L.getLoggerContext "x-request-id"
        resp <- ER.callBrahmaReplayDB (RunDBEntryT (RunDBEntry method A.Null (whereClauseToJson whereClause) (modelTableName @table) (A.Null) recTimestamp)) msessionId
        parseDataReplay resp
      else do
        tmp_res <- hsDbFunc
        whenM ARTUtils.isArtRecEnabled $ do
          recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.ART.DBReplay::runWithArtCount::getCurrentTime" getCurrentTime)
          addRecToState $ RunDBEntryT (RunDBEntry method A.Null (whereClauseToJson whereClause) (modelTableName @table) (toJSON tmp_res) recTimestamp)
        whenM (ARTV2Utils.isArtV2RecEnabledForTable @table) $ do 
          recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.ART.DBReplay::runWithArtCount::getCurrentTime" getCurrentTime)
          producePayload DB_AGGREGATE . BS.toStrict . A.encode $ RunDBEntryT (RunDBEntry method A.Null (whereClauseToJson whereClause) (modelTableName @table) (toJSON tmp_res) recTimestamp)
        pure tmp_res

runWithArtFindAllExtended ::
  forall be beM table m.
    (Model be table
    , FromJSON (table Identity)
    , ToJSON (table Identity)
    , MeshMeta be table
    , L.MonadFlow m
    ) =>
  ModelDBConfig beM->
  Where be table ->
  Text ->
  m (Either T.DBError [KVEntry table]) ->
  m (Either T.DBError [KVEntry table])
runWithArtFindAllExtended _dbConf whereClause method hsDbFunc = do
    isArtV2ReplayEnabledWithSessId <- ARTV2Utils.shouldReplayForDB @table
    if Env.isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
      then do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.ART.DBReplay::runWithArtFindAllExtended::getCurrentTime" getCurrentTime)
        msessionId <- L.getLoggerContext "x-request-id"
        resp <- ER.callBrahmaReplayDB (RunDBEntryT (RunDBEntry method A.Null (whereClauseToJson whereClause) (modelTableName @table) (A.Null) recTimestamp)) msessionId
        parseDataReplayList resp
      else do
        tmp_res <- hsDbFunc
        whenM ARTUtils.isArtRecEnabled $ do 
          recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.ART.DBReplay::runWithArtFindAllExtended::getCurrentTime" getCurrentTime)
          addRecToState $ RunDBEntryT (RunDBEntry method A.Null (whereClauseToJson whereClause) (modelTableName @table) (toJSON tmp_res) recTimestamp)
        whenM (ARTV2Utils.isArtV2RecEnabledForTable @table) $ do 
          recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.ART.DBReplay::runWithArtFindAllExtended::getCurrentTime" getCurrentTime)
          producePayload DB . BS.toStrict . A.encode $ RunDBEntryT (RunDBEntry method A.Null (whereClauseToJson whereClause) (modelTableName @table) (toJSON tmp_res) recTimestamp)
        pure tmp_res

runWithArtFind ::
  forall be beM table m.
    (Model be table
    , FromJSON (table Identity)
    , ToJSON (table Identity)
    , MeshMeta be table
    , L.MonadFlow m
    ) =>
  ModelDBConfig beM->
  Where be table ->
  Text ->
  m (Either T.DBError (Maybe (KVEntry table))) ->
  m (MeshResult (Maybe (KVEntry table)))
runWithArtFind _dbConf whereClause method hsDbFunc = do
    isArtV2ReplayEnabledWithSessId <- ARTV2Utils.shouldReplayForDB @table
    if Env.isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
      then do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.ART.DBReplay::runWithArtFind::getCurrentTime" getCurrentTime)
        msessionId <- L.getLoggerContext "x-request-id"
        resp <- ER.callBrahmaReplayDB (RunDBEntryT (RunDBEntry method A.Null (whereClauseToJson whereClause) (modelTableName @table) (A.Null) recTimestamp)) msessionId
        pure $
          case A.decode resp of
            Just val -> val
            Nothing -> Right Nothing
      else do
        res <- hsDbFunc
        whenM ARTUtils.isArtRecEnabled $ do 
          recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.ART.DBReplay::runWithArtFind::getCurrentTime" getCurrentTime)
          addRecToState $ RunDBEntryT (RunDBEntry method A.Null (whereClauseToJson whereClause) (modelTableName @table) (toJSON res) recTimestamp)
        whenM (ARTV2Utils.isArtV2RecEnabledForTable @table) $ do 
          recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.ART.DBReplay::runWithArtFind::getCurrentTime" getCurrentTime)
          producePayload DB . BS.toStrict . A.encode $ RunDBEntryT (RunDBEntry method A.Null (whereClauseToJson whereClause) (modelTableName @table) (toJSON res) recTimestamp)
        pure $ mapLeft MDBError $ res

runWithArtUpdate ::
  forall be beM a table m.
    (Model be table
    , FromJSON a
    , ToJSON a
    , MeshMeta be table
    , L.MonadFlow m
    ) =>
  ModelDBConfig beM->
  [Set be table] ->
  Where be table ->
  Text ->
  m (T.DBResult a) ->
  m (MeshResult a)
runWithArtUpdate _ setClause whereClause method hsDbFunc = do
    isArtV2ReplayEnabledWithSessId <- ARTV2Utils.shouldReplayForDB @table
    if Env.isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
      then do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.ART.DBReplay::runWithArtUpdate::getCurrentTime" getCurrentTime)
        msessionId <- L.getLoggerContext "x-request-id"
        resp <- ER.callBrahmaReplayDB (RunDBEntryT (RunDBEntry method (toJSON (jsonKeyValueUpdates setClause)) (whereClauseToJson whereClause) (modelTableName @table) (A.Null) recTimestamp)) msessionId
        parseDataReplay resp
      else do
        tmp_res <- hsDbFunc
        whenM ARTUtils.isArtRecEnabled $ do 
          recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.ART.DBReplay::runWithArtUpdate::getCurrentTime" getCurrentTime)
          addRecToState $ RunDBEntryT (RunDBEntry method (toJSON (jsonKeyValueUpdates setClause)) (whereClauseToJson whereClause) (modelTableName @table) (toJSON tmp_res) recTimestamp)
        whenM (ARTV2Utils.isArtV2RecEnabledForTable @table) $ do 
          recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.ART.DBReplay::runWithArtUpdate::getCurrentTime" getCurrentTime)
          producePayload DB . BS.toStrict . A.encode $ RunDBEntryT (RunDBEntry method (toJSON (jsonKeyValueUpdates setClause)) (whereClauseToJson whereClause) (modelTableName @table) (toJSON tmp_res) recTimestamp)
        pure $ mapLeft MDBError $ tmp_res

runWithArtCreatemSQl ::
  forall beM a table m.
    ( ToJSON (table Identity)
    , FromJSON a
    , ToJSON a
    , ModelMeta table
    , L.MonadFlow m
    ) =>
  ModelDBConfig beM->
  table Identity ->
  Text ->
  m (T.DBResult a) ->
  m (MeshResult a)
runWithArtCreatemSQl _ value method hsDbFunc = do
    isArtV2ReplayEnabledWithSessId <- ARTV2Utils.shouldReplayForDB @table
    if Env.isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
      then do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.ART.DBReplay::runWithArtCreatemSQl::getCurrentTime" getCurrentTime)
        msessionId <- L.getLoggerContext "x-request-id"
        resp <- ER.callBrahmaReplayDB (RunDBEntryT (RunDBEntry method (toJSON value) A.Null (modelTableName @table) (A.Null) recTimestamp)) msessionId
        parseDataReplay resp
      else do
        tmp_res <- hsDbFunc
        whenM ARTUtils.isArtRecEnabled $ do
          recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.ART.DBReplay::runWithArtCreatemSQl::getCurrentTime" getCurrentTime)
          addRecToState $ RunDBEntryT (RunDBEntry method (toJSON value) A.Null (modelTableName @table) (toJSON tmp_res) recTimestamp)
        whenM (ARTV2Utils.isArtV2RecEnabledForTable @table) $ do
          recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.ART.DBReplay::runWithArtCreatemSQl::getCurrentTime" getCurrentTime)
          producePayload DB . BS.toStrict . A.encode $ RunDBEntryT (RunDBEntry method (toJSON value) A.Null (modelTableName @table) (toJSON tmp_res) recTimestamp)
        pure $ mapLeft MDBError $ tmp_res

runWithArtDelete ::
  forall be beM a table m.
    (Model be table
    , FromJSON a
    , ToJSON a
    , MeshMeta be table
    , L.MonadFlow m
    ) =>
  ModelDBConfig beM->
  Where be table ->
  Text ->
  m (T.DBResult a) ->
  m (MeshResult a)
runWithArtDelete _ whereClause method hsDbFunc = do
    isArtV2ReplayEnabledWithSessId <- ARTV2Utils.shouldReplayForDB @table
    if Env.isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
      then do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.ART.DBReplay::runWithArtDelete::getCurrentTime" getCurrentTime)
        msessionId <- L.getLoggerContext "x-request-id"
        resp <- ER.callBrahmaReplayDB (RunDBEntryT (RunDBEntry method A.Null (whereClauseToJson whereClause) (modelTableName @table) (A.Null) recTimestamp)) msessionId
        parseDataReplay resp
      else do
        tmp_res <- hsDbFunc
        whenM ARTUtils.isArtRecEnabled $ do
          recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.ART.DBReplay::runWithArtDelete::getCurrentTime" getCurrentTime)
          addRecToState $ RunDBEntryT (RunDBEntry method A.Null (whereClauseToJson whereClause) (modelTableName @table) (toJSON tmp_res) recTimestamp)
        whenM (ARTV2Utils.isArtV2RecEnabledForTable @table) $ do
          recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (runIOWithART "EulerHS.ART.DBReplay::runWithArtDelete::getCurrentTime" getCurrentTime)
          producePayload DB . BS.toStrict . A.encode $ RunDBEntryT (RunDBEntry method A.Null (whereClauseToJson whereClause) (modelTableName @table) (toJSON tmp_res) recTimestamp)
        pure $ mapLeft MDBError $ tmp_res
