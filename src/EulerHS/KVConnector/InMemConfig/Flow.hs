{- |
Module      :  EulerHS.KVConnector.InMemConfig.Flow
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module EulerHS.KVConnector.InMemConfig.Flow

    where

import           Control.Monad.Catch (bracket)
import qualified Data.Aeson as A
import qualified Database.Beam as B
import           EulerHS.Prelude hiding (bracket)
import qualified EulerHS.SqlDB.Language as DB
import           EulerHS.SqlDB.Types (BeamRunner, BeamRuntime, DBConfig)
-- import           Control.Monad.Extra (notM)
import qualified Data.HashSet as HS
import qualified Data.Text as T
import           EulerHS.CachedSqlDBQuery (findOne)
import qualified EulerHS.Language as L
import           EulerHS.KVConnector.InMemConfig.Types
import           EulerHS.KVConnector.Types (KVConnector(..), MeshConfig, tableName, MeshResult, MeshMeta(..), Source(..))
import           Unsafe.Coerce (unsafeCoerce)
import           Data.Either.Extra (mapLeft)
import           EulerHS.CachedSqlDBQuery (findAllExtended')
import           EulerHS.Runtime (mkConfigEntry)
import           EulerHS.KVConnector.Utils
import           Sequelize (Model, Where, Clause(..), sqlSelect)
import           Named (defaults, (!))
import qualified Data.Serialize as Serialize
import           EulerHS.KVConnector.DBSync (whereClauseToJson)
import EulerHS.ART.Types 
import           EulerHS.ART.FlowUtils (addRecToState)
import           EulerHS.Logger.Types (ErrorL(..))
import qualified EulerHS.ART.ReplayFunctions as ER
import qualified EulerHS.ART.EnvVars as Env
import qualified Data.ByteString.Lazy as BS
import qualified Servant as S
import           EulerHS.PIIEncryption hiding (PIIEncryptionKey(..))
import           EulerHS.KVDB.Types (MeshError(MDBError, UnexpectedError))
import           Data.Time.Clock.POSIX

checkAndStartLooper :: forall table m beM.
    (
    HasCallStack,
    KVConnector (table Identity),
    L.MonadFlow m) => DBConfig beM -> MeshConfig -> (ByteString -> Either String (ImcStreamValue (table Identity))) -> m ()
checkAndStartLooper dbConf meshCfg decodeTable = do
    hasLooperStarted <- L.getOption $ (LooperStarted (tableName @(table Identity)))
    case hasLooperStarted of
        _hasLooperStarted
            | _hasLooperStarted == Just True ->  pure ()
            | otherwise ->  do
                strmName <- getRandomStream 
                when shouldLogFindDBCallLogs $ L.logDebug @Text "checkAndStartLooper" $ "Connecting with Stream <" <> strmName <> "> for table " <> tableName @(table Identity)
                L.setOption (LooperStarted (tableName @(table Identity))) True
                L.fork $ looperForRedisStream dbConf decodeTable meshCfg.kvRedis strmName

looperForRedisStream :: forall table m beM.(
    HasCallStack,
    KVConnector (table Identity),
    L.MonadFlow m
    ) =>
    DBConfig beM ->
    (ByteString -> Either String (ImcStreamValue (table Identity))) -> Text -> Text -> m ()
looperForRedisStream dbConf decodeTable redisName' strmName = bracket 
    (pure ())
    (\ _ -> do
      L.logInfoT "looperForRedisStream failed" ("Setting LooperStarted option as False for table " <> tableName @(table Identity))
      L.setOption (LooperStarted (tableName @(table Identity))) False)
    (\ _ -> forever $ do
      let tName = tableName @(table Identity)
      maybeRId <- L.getOption (RecordId tName)
      case maybeRId of
          Nothing -> do
              rId <- T.pack . show <$> L.getCurrentDateInMillis
              initRecords <- getRecordsFromStream redisName' strmName rId tName
              case initRecords of 
                  Nothing -> do
                      L.setOption (RecordId tName) rId
                      return ()
                  Just (latestId, rs) -> do
                      L.setOption (RecordId tName) latestId
                      mapM_ (updateInMemCache dbConf tName decodeTable) rs
          Just rId -> do
              newRecords <- getRecordsFromStream redisName' strmName rId tName
              case newRecords of
                  Nothing ->
                      return ()
                  Just (latestId, rs) -> do
                      L.setOption (RecordId tName) latestId
                      mapM_ (updateInMemCache dbConf tName decodeTable) rs
      void $ looperDelayInSec)


looperDelayInSec :: (L.MonadFlow m) => m ()
looperDelayInSec = L.runIO $ threadDelay $ getConfigStreamLooperDelayInSec * 1000000

updateInMemCache :: forall table m beM.(
    HasCallStack,
    KVConnector(table Identity),
    L.MonadFlow m
    ) =>
    DBConfig beM ->
    Text ->
    (ByteString -> Either String (ImcStreamValue (table Identity))) ->
    RecordKeyValues -> m ()
updateInMemCache dbConf tName decodeTable (k,val) = do
  when (tName == k) $                             -- decode only when entry is for the looper's table
    case decodeTable val of
        Left e-> do
          L.logErrorWithCategory ("setInMemCache" :: Text) ("Unable to decode ImcStreamValue for the table <" <> k) $ ErrorL Nothing "MEM_CACHE_ERROR" $ show e
          return ()
        Right strmVal -> do
          let
            pKeyText = getLookupKeyByPKey (isMySQLConfig dbConf) strmVal.tableRow
          invalidateDataCache pKeyText (tableName @(table Identity))

extractRecordsFromStreamResponse :: [L.KVDBStreamReadResponseRecord] -> [RecordKeyValues]
extractRecordsFromStreamResponse  = foldMap (fmap (bimap decodeUtf8 id) . L.records)

getRecordsFromStream :: Text -> Text -> LatestRecordId -> Text -> (L.MonadFlow m) => m (Maybe (LatestRecordId, [RecordKeyValues]))
getRecordsFromStream redisName' strmName lastRecordId tName = do
    eitherReadResponse <- L.rXreadT redisName' strmName lastRecordId
    case eitherReadResponse of
        Left err -> do
            L.delOption (RecordId tName)    
            L.logErrorWithCategory ("getRecordsFromStream recorded 1" :: Text) ("Error getting initial records from stream <" <> strmName <> ">" <> show err) $ ErrorL Nothing "STREAM_ERROR" (show err)
            return Nothing
        Right maybeRs -> case maybeRs of
            Nothing -> return Nothing -- Maybe stream doesn't exist or Maybe no new records
            Just [] -> return Nothing
            Just rs -> case filter (\rec-> (decodeUtf8 rec.streamName) == strmName) rs of
                [] -> return Nothing
                (rss : _) -> do
                    case uncons . reverse . L.response $ rss of
                        Nothing -> return Nothing
                        Just (latestRecord, _) -> do
                                L.logDebugT ("getRecordsFromStream for " <> tName) $ (show . length . L.response $ rss) <> " new records in stream <" <> strmName <> ">"
                                return . Just . bimap (decodeUtf8 . L.recordId) (extractRecordsFromStreamResponse . L.response ) $ (latestRecord, rss)

{-# INLINE searchInMemoryCache #-}
searchInMemoryCache :: forall be beM table m.
  (
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    HasCallStack,
    KVConnector (table Identity),
    ToJSON (table Identity),
    Show (table Identity),
    Serialize.Serialize (table Identity),
    PII table,
    FromJSON (table Identity),
    Model be table,
    MeshMeta be table,
    L.MonadFlow m
  ) =>  DBConfig beM ->
        Where be table ->
        m (Source, MeshResult (Maybe (table Identity)))
searchInMemoryCache dbConf whereClause = do
  mbVal <- L.getConfig inMemCacheKey
  case mbVal of
    Just val -> do
      currentTime <- L.runIO getPOSIXTime
      if (val.ttl < currentTime)
        then getFromDBAndCache
        else
          case val.entry of
            Nothing -> do
              when shouldLogFindDBCallLogs $ L.logDebugT "IMC_EMPTY_RESULT" $ (tableName @(table Identity))
              return (IN_MEM, Right Nothing)         
            Just item -> do
              let parsedVal = (unsafeCoerce @_ @(table Identity) item)
              when shouldLogFindDBCallLogs $ L.logDebugV ("IMC_RESULT_FOUND for " <> tableName @(table Identity) ::Text) parsedVal
              return . (IN_MEM,) . Right . Just $ parsedVal
    Nothing -> getFromDBAndCache
  where
    inMemCacheKey = getInMemCacheKeyFromWhereClause dbConf (inMemPrefix @(table Identity) <> (tableName @(table Identity)) <>"_key_") meshModelTableEntityDescriptor (And whereClause)
    {-# INLINE getFromDBAndCache #-}
    getFromDBAndCache = do
      eiVal <- mapLeft MDBError <$> findOne dbConf Nothing whereClause
      when (isRight eiVal) $ do
        let mbVal = fromRight Nothing eiVal
        case mbVal of
          Just val -> do
            newTtl <- getConfigEntryNewTtl
            setInMemCache newTtl val
          Nothing -> do
            when shouldLogFindDBCallLogs $ L.logDebugT ("IMC_KEY_NOT_FOUND_IN_DB - ") ((tableName @(table Identity)))
            newTtl <- getConfigNullEntryNewTtl
            L.setConfig inMemCacheKey $ mkConfigEntry newTtl Nothing
      pure (SQL, eiVal)

    setInMemCache newTtl val = do
      L.setConfig inMemCacheKey $ mkConfigEntry newTtl $ Just val
      let pKeyText = getLookupKeyByPKey (isMySQLConfig dbConf) val
      addToInMemTracker inMemCacheKey pKeyText (tableName @(table Identity))

{-# INLINABLE addToInMemTracker #-}
addToInMemTracker :: L.MonadFlow m => Text -> Text -> Text -> m ()
addToInMemTracker inMemCacheKey pkVal tName = do
  newTtl <- getConfigEntryNewTtl
  let f = (\cfgEntry -> do
              let newEntry = case cfgEntry of
                    Just val -> do
                      case val.entry of
                        Just item -> HS.insert inMemCacheKey (unsafeCoerce @_ @(HashSet Text) item)
                        Nothing -> HS.singleton inMemCacheKey
                    Nothing -> HS.singleton inMemCacheKey
              mkConfigEntry newTtl $ Just newEntry)
  L.modifyConfig (mkTrackerKey pkVal tName) f

{-# INLINABLE invalidateDataCache #-}
invalidateDataCache :: L.MonadFlow m => Text -> Text -> m ()
invalidateDataCache pkVal tName = do
  cfgEntry <- L.getConfig $ mkTrackerKey pkVal tName
  case cfgEntry of
    Just val -> do
      case val.entry of
        Just item -> do
          let trackerValues = (unsafeCoerce @_ @(HashSet Text) item)
          mapM_ L.delConfig trackerValues
        Nothing -> pure ()
    Nothing -> pure ()

pushToConfigStream :: (L.MonadFlow m ) => Text -> Text -> Text -> Text -> m ()
pushToConfigStream redisName' k v strmName = 
  void $ L.rXaddB redisName' (encodeUtf8 strmName) (encodeUtf8 k) (encodeUtf8 v)

pushToInMemConfigStream :: forall table m.
  ( KVConnector (table Identity),
    ToJSON (table Identity),
    L.MonadFlow m
  ) => MeshConfig -> ImcStreamCommand -> table Identity -> m ()
pushToInMemConfigStream meshCfg imcCommand alteredModel = do
  let
    strmValue = ImcStreamValue {
      command = imcCommand,
      tableRow = Just alteredModel
    }
    strmValueT = decodeUtf8 . A.encode $ strmValue
  mapM_ (pushToConfigStream meshCfg.kvRedis (tableName @(table Identity)) strmValueT) getConfigStreamNames
  pure ()

parseDataReplay ::(FromJSON b,L.MonadFlow m) => BS.ByteString -> m (Either MeshError b)
parseDataReplay res = do
  let eReply = A.eitherDecode res :: (FromJSON b) => Either String (Either MeshError b)
  case eReply of
    Left err -> do
      let errorMsg = "Failed to decode response: " <> (encodeUtf8 err)
      L.throwException $ S.err400 {S.errBody = errorMsg}
    Right reply -> pure reply


fetchRowFromDBAndAlterImc :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    MeshMeta be table,
    ToJSON (table Identity),
    PII table,
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  MeshConfig ->
  Where be table ->
  ImcStreamCommand ->
  m (MeshResult ())
fetchRowFromDBAndAlterImc dbConf meshCfg whereClause imcCommand = do
  let findQuery = DB.findRows (sqlSelect ! #where_ whereClause ! defaults)
  if Env.isArtReplayEnabled
    then do
      recTimestamp <- L.getCurrentTimeUTC
      msessionId <- L.getLoggerContext "x-request-id"
      let recDBFind = RunDBEntryT (RunDBEntry "fetchRowFromDBAndAlterImc" A.Null (whereClauseToJson whereClause) (tableName @(table Identity)) A.Null recTimestamp)
      resp <- L.runIO $ ER.callBrahmaReplayDB recDBFind msessionId
      parseDataReplay resp
    else do
      dbRes <- findAllExtended' dbConf findQuery
      when Env.isArtRecEnabled $ do
        recTimestamp <- L.getCurrentTimeUTC
        addRecToState $ RunDBEntryT (RunDBEntry "fetchRowFromDBAndAlterImc" A.Null (whereClauseToJson whereClause) (tableName @(table Identity)) (toJSON dbRes) recTimestamp)
      case dbRes of
        Right [x] -> do
          when meshCfg.memcacheEnabled $ pushToInMemConfigStream meshCfg imcCommand x
          return $ Right ()
        Right [] -> return $ Right ()
        Right xs -> do
          let message = "DB returned \"" <> show (length xs) <> "\" rows after update for table: " <> show (tableName @(table Identity))
          L.logErrorWithCategory @Text "updateWoReturningWithKVConnector" message $ ErrorL Nothing "DB_ERROR" message
          return $ Left $ UnexpectedError message
        Left e -> return $ Left (MDBError e)
