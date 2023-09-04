{- |
Module      :  EulerHS.ART.DBReplay
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable

This module contains interpreters and methods for running `Flow` scenarios.
-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module EulerHS.ART.DBReplay where

import qualified Data.Aeson as A
import           Data.Either.Extra (mapLeft)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import           EulerHS.Prelude
import qualified EulerHS.SqlDB.Language as DB
import           EulerHS.Types (DBConfig)
import qualified EulerHS.Types as T
import           EulerHS.KVConnector.InMemConfig.Flow (searchInMemoryCache)
import           Sequelize (Model, Set (..), Where)
import qualified Servant as S
import qualified Data.Serialize as Serialize
import           EulerHS.ART.FlowUtils (addRecToState)
import qualified EulerHS.ART.EnvVars as Env
import           EulerHS.KVConnector.Types (KVConnector(..), MeshResult, MeshMeta(..), tableName, Source(..))
import           EulerHS.ART.Types (RunDBEntry(..), RecordingEntry(..),RunInMemEntry(..))
import EulerHS.KVConnector.Utils
import           EulerHS.KVConnector.DBSync (whereClauseToJson)
import           EulerHS.SqlDB.Types (BeamRunner, BeamRuntime)
import qualified EulerHS.ART.ReplayFunctions as ER
import EulerHS.KVDB.Types (MeshError(..))
import EulerHS.PIIEncryption (PII(..))
import qualified Data.ByteString.Lazy as BS

getCurrentDateInMillis :: (L.MonadFlow m) => m Int
getCurrentDateInMillis = L.runIO $ do
   t <- (* 1000) <$> getPOSIXTime
   pure . floor $ t

getLatencyInMicroSeconds :: Integer -> Integer
getLatencyInMicroSeconds execTime = execTime `div` 1000000

parseDataReplayList ::(FromJSON b,L.MonadFlow m) => BS.ByteString -> m (Either T.DBError [b]) 
parseDataReplayList res = do
  let eReply = A.eitherDecode res :: (FromJSON b) => Either String (Either T.DBError [b])
  case eReply of
    Left err -> do
      let errorMessage = "Failed to decode response: " <> encodeUtf8 err
      L.throwException $ S.err400 {S.errBody = errorMessage}
    Right reply -> pure reply

parseDataReplay ::(FromJSON b, L.MonadFlow m) => BS.ByteString -> m (Either MeshError b)
parseDataReplay res = do
  let eReply = A.eitherDecode res :: (FromJSON b) => Either String (Either MeshError b)
  case eReply of
    Left err -> do
      let errorMessage = "Failed to decode response: " <> encodeUtf8 err
      L.throwException $ S.err400 {S.errBody = errorMessage}
    Right reply -> pure reply

runWithArtFindALL ::
  forall be beM table m.
    (Model be table
    , FromJSON (table Identity)
    , ToJSON (table Identity)
    , KVConnector (table Identity)
    , MeshMeta be table
    , L.MonadFlow m
    ) =>
  DBConfig beM ->
  Where be table ->
  Text ->
  m (Either T.DBError [table Identity]) ->
  m (Either T.DBError [table Identity])
runWithArtFindALL _dbConf whereClause method hsDbFunc = do
  do
    if Env.isArtReplayEnabled
      then do
        recTimestamp <- L.getCurrentTimeUTC
        msessionId <- L.getLoggerContext "x-request-id"
        resp <- L.runIO $ ER.callBrahmaReplayDB (RunDBEntryT (RunDBEntry method A.Null (whereClauseToJson whereClause) (tableName @(table Identity)) (A.Null) recTimestamp)) msessionId
        parseDataReplayList resp
      else do
        tmp_res <- hsDbFunc
        when Env.isArtRecEnabled $ do
          recTimestamp <- L.getCurrentTimeUTC
          addRecToState $ RunDBEntryT (RunDBEntry method A.Null (whereClauseToJson whereClause) (tableName @(table Identity)) (toJSON tmp_res) recTimestamp)
        pure tmp_res

runWithArtFindAllExtended ::
  forall be beM table m.
    (Model be table
    , FromJSON (table Identity)
    , ToJSON (table Identity)
    , KVConnector (table Identity)
    , MeshMeta be table
    , L.MonadFlow m
    ) =>
  DBConfig beM ->
  DB.SqlDB beM [table Identity] ->
  Where be table ->
  Text ->
  m (Either T.DBError [table Identity]) ->
  m (Either T.DBError [table Identity])
runWithArtFindAllExtended _dbConf _query whereClause method hsDbFunc = do
  do
    if Env.isArtReplayEnabled
      then do
        recTimestamp <- L.getCurrentTimeUTC
        msessionId <- L.getLoggerContext "x-request-id"
        resp <- L.runIO $ ER.callBrahmaReplayDB (RunDBEntryT (RunDBEntry method A.Null (whereClauseToJson whereClause) (tableName @(table Identity)) (A.Null) recTimestamp)) msessionId
        parseDataReplayList resp
      else do
        tmp_res <- hsDbFunc
        when Env.isArtRecEnabled $ do 
          recTimestamp <- L.getCurrentTimeUTC
          addRecToState $ RunDBEntryT (RunDBEntry method A.Null (whereClauseToJson whereClause) (tableName @(table Identity)) (toJSON tmp_res) recTimestamp)
        pure tmp_res

runWithArtFind ::
  forall be beM table m.
    (Model be table
    , KVConnector (table Identity)
    , FromJSON (table Identity)
    , ToJSON (table Identity)
    , MeshMeta be table
    , L.MonadFlow m
    ) =>
  DBConfig beM ->
  Where be table ->
  Text ->
  m (Either T.DBError (Maybe (table Identity))) ->
  m (MeshResult (Maybe (table Identity)))
runWithArtFind _dbConf whereClause method hsDbFunc = do
  do
    if Env.isArtReplayEnabled
      then do
        recTimestamp <- L.getCurrentTimeUTC
        msessionId <- L.getLoggerContext "x-request-id"
        resp <- L.runIO $ ER.callBrahmaReplayDB (RunDBEntryT (RunDBEntry method A.Null (whereClauseToJson whereClause) (tableName @(table Identity)) (A.Null) recTimestamp)) msessionId
        pure $
          case A.decode resp of
            Just val -> val
            Nothing -> Right Nothing
      else do
        res <- hsDbFunc
        when Env.isArtRecEnabled $ do 
          recTimestamp <- L.getCurrentTimeUTC
          addRecToState $ RunDBEntryT (RunDBEntry method A.Null (whereClauseToJson whereClause) (tableName @(table Identity)) (toJSON res) recTimestamp)
        pure $ mapLeft MDBError $ res

runWithArtUpdate ::
  forall be beM a table m.
    (Model be table
    , FromJSON a
    , ToJSON a
    , KVConnector (table Identity)
    , MeshMeta be table
    , L.MonadFlow m
    ) =>
  DBConfig beM ->
  [Set be table] ->
  Where be table ->
  Text ->
  m (T.DBResult a) ->
  m (MeshResult a)
runWithArtUpdate _ setClause whereClause method hsDbFunc = do
  do
    if Env.isArtReplayEnabled
      then do
        recTimestamp <- L.getCurrentTimeUTC
        msessionId <- L.getLoggerContext "x-request-id"
        resp <- L.runIO $ ER.callBrahmaReplayDB (RunDBEntryT (RunDBEntry method (toJSON (jsonKeyValueUpdates setClause)) (whereClauseToJson whereClause) (tableName @(table Identity)) (A.Null) recTimestamp)) msessionId
        parseDataReplay resp
      else do
        tmp_res <- hsDbFunc
        when Env.isArtRecEnabled $ do 
          recTimestamp <- L.getCurrentTimeUTC
          addRecToState $ RunDBEntryT (RunDBEntry method (toJSON (jsonKeyValueUpdates setClause)) (whereClauseToJson whereClause) (tableName @(table Identity)) (toJSON tmp_res) recTimestamp)
        pure $ mapLeft MDBError $ tmp_res

runWithArtCreatemSQl ::
  forall beM a table m.
    ( ToJSON (table Identity)
    , FromJSON a
    , ToJSON a
    , KVConnector (table Identity)
    , L.MonadFlow m
    ) =>
  DBConfig beM ->
  table Identity ->
  Text ->
  m (T.DBResult a) ->
  m (MeshResult a)
runWithArtCreatemSQl _ value method hsDbFunc = do
  do
    if Env.isArtReplayEnabled
      then do
        recTimestamp <- L.getCurrentTimeUTC
        msessionId <- L.getLoggerContext "x-request-id"
        resp <- L.runIO $ ER.callBrahmaReplayDB (RunDBEntryT (RunDBEntry method (toJSON value) A.Null (tableName @(table Identity)) (A.Null) recTimestamp)) msessionId
        parseDataReplay resp
      else do
        tmp_res <- hsDbFunc
        when Env.isArtRecEnabled $ do
          recTimestamp <- L.getCurrentTimeUTC
          addRecToState $ RunDBEntryT (RunDBEntry method (toJSON value) A.Null (tableName @(table Identity)) (toJSON tmp_res) recTimestamp)
        pure $ mapLeft MDBError $ tmp_res

runWithArtDelete ::
  forall be beM a table m.
    (Model be table
    , FromJSON a
    , ToJSON a
    , KVConnector (table Identity)
    , MeshMeta be table
    , L.MonadFlow m
    ) =>
  DBConfig beM ->
  Where be table ->
  Text ->
  m (T.DBResult a) ->
  m (MeshResult a)
runWithArtDelete _ whereClause method hsDbFunc = do
  do
    if Env.isArtReplayEnabled
      then do
        recTimestamp <- L.getCurrentTimeUTC
        msessionId <- L.getLoggerContext "x-request-id"
        resp <- L.runIO $ ER.callBrahmaReplayDB (RunDBEntryT (RunDBEntry method A.Null (whereClauseToJson whereClause) (tableName @(table Identity)) (A.Null) recTimestamp)) msessionId
        parseDataReplay resp
      else do
        tmp_res <- hsDbFunc
        when Env.isArtRecEnabled $ do
          recTimestamp <- L.getCurrentTimeUTC
          addRecToState $ RunDBEntryT (RunDBEntry method A.Null (whereClauseToJson whereClause) (tableName @(table Identity)) (toJSON tmp_res) recTimestamp)
        pure $ mapLeft MDBError $ tmp_res

searchInMemoryCacheRecRepWrapper :: forall be beM table m.
  (
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    HasCallStack,
    KVConnector (table Identity),
    ToJSON (table Identity),
    Show (table Identity),
    Serialize.Serialize (table Identity),
    FromJSON (table Identity),
    Model be table,
    MeshMeta be table,
    PII table,
    L.MonadFlow m
  ) => Text ->
        DBConfig beM ->
        Where be table ->
        m (Source, MeshResult (Maybe (table Identity)))
searchInMemoryCacheRecRepWrapper method dbConf whereClause = do
    if Env.isArtReplayEnabled
      then do
        recTimestamp <- L.getCurrentTimeUTC
        let recInmem = RunInMemEntryT (RunInMemEntry method A.Null (whereClauseToJson whereClause) (toJSON $ tableName @(table Identity)) (Left A.Null) recTimestamp)
        msessionId <- L.getLoggerContext "x-request-id"
        resp <- L.runIO $ ER.callBrahmaReplayDB recInmem msessionId
        meshRes <- parseDataReplay resp
        pure (IN_MEM,meshRes)
      else do
        (src,meshResult) <- searchInMemoryCache dbConf whereClause
        when Env.isArtRecEnabled $ do
          recTimestamp <- L.getCurrentTimeUTC 
          addRecToState $ RunInMemEntryT (RunInMemEntry method A.Null (whereClauseToJson whereClause) (toJSON $ tableName @(table Identity)) (either (Left . toJSON) (Right . toJSON) meshResult) recTimestamp)
        pure (src,meshResult)
