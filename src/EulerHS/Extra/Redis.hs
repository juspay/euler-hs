{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module EulerHS.Extra.Redis
  ( rExpire
  , rExpireWithART
  , rExpireB
  , rExpireBWithART
  , rExpireAt
  , rExpireAtB
  , rDel
  , rDelWithART
  , rDelB
  , rDelBWithART
  , rExists
  , rExistsB
  , rExistsT -- alias for rExists (back compat)
  , rHget
  , rHgetB
  , rHset
  , rHsetB
  , rHsetNx
  , rHmset
  , rHmsetB
  , rHGetAll
  , rHDelB
  , rHLenB
  , rIncr
  , rIncrWithART
  , rIncrB
  , rIncrBWithART
  , rIncrBy
  , rIncrByB
  , rIncrByFloat
  , rIncrByFloatB
  , rDecr
  , rDecrB
  , rDecrBy
  , rDecrByB
  , rSet
  , rSetT  -- alias for rSet (back compat)
  , rSetB
  , rSetBWithART
  , rGet
  , rGetWithART
  , rGetB
  , rGetBWithART
  , rGetT  -- alias for rGet (back compat)
  , rSetex
  , rSetexWithART
  , rSetexB
  , rSetexBWithART
  , rSetexT  -- alias for rSetex (back compat)
  , rXreadB
  , rXreadBWithART
  , rXreadT
  , rXreadTWithART
  , rXrangeT
  , rXrangeB
  , rXrevrangeT
  , rXrevrangeB
  , rSetexBulk
  , rSetexBulkB
  , rSetOpts
  , rSetOptsB
  , rSetOptsT
  , rSadd
  , rSaddWithART
  , rSismember
  , rZAdd
  , rZAddWithART
  , rZRangeByScore
  , rZRangeWithScores
  , rZRangeByScoreWithScores
  , rZRangeByScoreWithLimit
  , rZRem
  , rZRemRangeByScore
  , rZCard
  , rXaddB
  , rXaddBWithART
  , rGetBEither
  , rGetBEitherWithART
  , rSmembersB
  , rSmembersBWithART
  , sRemB
  , sRemBWithART
  , rMultiExec
  , rLLenB
  , rLRangeB
  , rPingRequest
  , rXLenB
  , rXDelB
  , rXgroupCreate
  , rXreadGroup
  , rXreadOpts
  , rMultiExecWithHash
  , rMultiExecWithHashWithART
  , RedisName
  , type TextKey
  , TextField
  , ByteKey
  , ByteField
  , ByteValue
  , withArtRecOptionsForKVDB
  , producePayload
  , rHincrBy
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as BSL
import           Data.Either.Extra (fromEither, mapLeft)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           EulerHS.Extra.Aeson (obfuscate)
import           Data.Time (getCurrentTime, utc, utcToZonedTime, zonedTimeToLocalTime)
import qualified EulerHS.Framework.Language as L
import qualified EulerHS.KVDB.Language as L
import           EulerHS.KVDB.Types (KVDBAnswer, KVDBReply, KVDBReplyF (..), KVDBStatus,TxResult (..))
import           EulerHS.Logger.Types (ErrorL(..))
import           EulerHS.Prelude hiding (get, id)
import           EulerHS.ART.Types
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Database.Redis as R
import           EulerHS.ART.EnvVars as Env (isArtReplayEnabled, shouldLogCallStackART)
import qualified EulerHS.ART.ReplayFunctions as ER
import qualified Servant as S
import           EulerHS.ART.Utils (isArtRecEnabled)
import           EulerHS.ART.V2.FlowUtils (producePayload)
import           EulerHS.ART.V2.Utils (isArtV2RecEnabled, isArtV2RecEnabledForKVDB, withDefaultArtRecOptions, shouldRecordDBTable, shouldReplayDBTable, shouldReplayForRedis, shouldReplayForKVDB, shouldARTV2Replay)
import           EulerHS.ART.V2.Types (HasArtRecOptions, ArtRecOptions(..))
import           Sequelize (ModelMeta)
import           EulerHS.Prelude (id)
import qualified Data.ByteString.Lazy as BS
import           EulerHS.Extra.KafkaClient.Utils
import qualified GHC.Stack as GHC
import qualified EulerHS.ART.IOReplay as ART

type RedisName = Text
type TextKey = Text
type TextField = Text
type ByteKey = ByteString
type ByteField = ByteString
type ByteValue = ByteString

-- KVDB convenient functions

-- ----------------------------------------------------------------------------

withArtRecOptionsForKVDB :: forall table m a. (ModelMeta table) => (HasArtRecOptions => m a) -> m a
withArtRecOptionsForKVDB = withDefaultArtRecOptions updateArtRecOptionsForDB
  where
    updateArtRecOptionsForDB :: ArtRecOptions -> ArtRecOptions
    updateArtRecOptionsForDB currentArtRecOptions = currentArtRecOptions {
          shouldRecordForART = shouldRecordDBTable @table,
          shouldReplayForART = shouldReplayDBTable @table
        }

rExpire :: (HasCallStack, Integral t, L.MonadFlow m) =>
  RedisName -> TextKey -> t -> m (Either KVDBReply Bool)
rExpire cName k = rExpireB cName (TE.encodeUtf8 k)

rExpireWithART :: (HasCallStack, HasArtRecOptions, Integral t, L.MonadFlow m) =>
  RedisName -> TextKey -> t -> m (Either KVDBReply Bool)
rExpireWithART cName k = rExpireBWithART cName (TE.encodeUtf8 k)

rExpireB :: (HasCallStack, Integral t, L.MonadFlow m) =>
  RedisName -> ByteKey -> t -> m (Either KVDBReply Bool)
rExpireB = withDefaultArtRecOptions id rExpireBWithART

rExpireBWithART :: (HasCallStack, Integral t, L.MonadFlow m, HasArtRecOptions) =>
  RedisName -> ByteKey -> t -> m (Either KVDBReply Bool)
rExpireBWithART cName k t = do
  shouldReplayForKVDBWithSessId <- shouldReplayForKVDB cName
  if isArtReplayEnabled || shouldReplayForKVDBWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rExpireBWithART::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RExpireBT $ (RExpireB (k) (toInteger t) (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply Bool) -> pure reply
    else do
      res <-rExpireB'
      whenM isArtRecEnabled $ do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rExpireBWithART::getCurrentTime" getCurrentTime)
        L.appendRecordingLocal $ RunKVDBEntryT $ RExpireBT $ (RExpireB (k) (toInteger t) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
      whenM isArtV2RecEnabledForKVDB $ do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rExpireBWithART::getCurrentTime" getCurrentTime)
        producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RExpireBT $ (RExpireB (k) (toInteger t) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
      pure res
      
  where 
    rExpireB' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply Bool)
    rExpireB' = do
      res <- L.runKVDB cName $ L.expire k $ toInteger t
      case res of
        Right _ -> do
          -- L.logInfo @Text "Redis expire" $ show r
          pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis expire" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

rExpireAt :: (HasCallStack, Integral t, L.MonadFlow m) =>
  RedisName -> TextKey -> t -> m (Either KVDBReply Bool)
rExpireAt cName k = rExpireAtB cName (TE.encodeUtf8 k)

rExpireAtB :: (HasCallStack, Integral t, L.MonadFlow m) =>
  RedisName -> ByteKey -> t -> m (Either KVDBReply Bool)
rExpireAtB cName k t = do
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rExpireAtB::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RExpireAtBT $ (RExpireAtB (k) (toInteger t) (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply Bool) -> pure reply
    else do
      res <-rExpireAtB'
      whenM isArtRecEnabled $ do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rExpireAtB::getCurrentTime" getCurrentTime)
        L.appendRecordingLocal $ RunKVDBEntryT $ RExpireAtBT $ (RExpireAtB (k) (toInteger t) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
      whenM isArtV2RecEnabled $ do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rExpireAtB::getCurrentTime" getCurrentTime)
        producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RExpireAtBT $ (RExpireAtB (k) (toInteger t) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
      pure res
      
  where 
    rExpireAtB' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply Bool)
    rExpireAtB' = do
      res <- L.runKVDB cName $ L.expire k $ toInteger t
      case res of
        Right _ -> pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis expireAt" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res
-- ----------------------------------------------------------------------------

rDel :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> [TextKey] -> m (Either KVDBReply Integer)
rDel cName ks = rDelB cName (TE.encodeUtf8 <$> ks)

rDelWithART :: (HasCallStack, HasArtRecOptions, L.MonadFlow m) =>
  RedisName -> [TextKey] -> m (Either KVDBReply Integer)
rDelWithART cName ks = rDelBWithART cName (TE.encodeUtf8 <$> ks)

rDelB :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> [ByteKey] -> m (Either KVDBReply Integer)
rDelB = withDefaultArtRecOptions id rDelBWithART

rDelBWithART :: (HasCallStack, L.MonadFlow m, HasArtRecOptions) =>
  RedisName -> [ByteKey] -> m (Either KVDBReply Integer)
rDelBWithART cName ks = do
  
  shouldReplayForKVDBWithSessId <- shouldReplayForKVDB cName
  if isArtReplayEnabled || shouldReplayForKVDBWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rDelBWithART::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RDelBT $ (RDelB (ks) (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply Integer) -> pure reply
  else do
    
    res <- rDelB'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rDelBWithART::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RDelBT $ (RDelB (ks) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName) 
    whenM isArtV2RecEnabledForKVDB $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rDelBWithART::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RDelBT $ (RDelB (ks) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName) 
    pure res
  where
    rDelB' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply Integer)
    rDelB' = do
      res <- L.runKVDB cName $ L.del ks
      case res of
        Right _ -> do
          -- L.logInfo @Text "Redis del" $ show r
          pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis del" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

-- ----------------------------------------------------------------------------

rExists :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> TextKey -> m (Either KVDBReply Bool)
rExists cName k = rExistsB cName $ TE.encodeUtf8 k

rExistsB :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> ByteKey -> m (Either KVDBReply Bool)
rExistsB cName k = do
    
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rExistsB::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RExistsBT $ (RExistsB (k) (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply Bool) -> pure reply
  else do  
    
    res <- rExistsB'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rExistsB::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RExistsBT $ (RExistsB (k) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName) 
    whenM isArtV2RecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rExistsB::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RExistsBT $ (RExistsB (k) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName) 
    pure res
  where
  rExistsB' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply Bool)
  rExistsB' = do
    res <- L.runKVDB cName $ L.exists k
    case res of
      Right _ -> do
        -- L.logInfo @Text "Redis exists" $ show r
        pure res
      Left err -> do
        L.logErrorWithCategoryV @Text "Redis exists" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
        pure res

rExistsT :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> TextKey -> m (Either KVDBReply Bool)
rExistsT = rExists

-- ----------------------------------------------------------------------------

rHget :: (HasCallStack, ToJSON v, FromJSON v, L.MonadFlow m)
  => RedisName -> TextKey -> TextField -> m (Maybe v)
rHget cName k f = do
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rHget::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RHGetT $ (RHGet (encodeUtf8 k) (toJSON f) (Nothing) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = encodeUtf8 $ err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          L.throwException $ S.err400 {S.errBody = errorMessage}
        Right (reply :: Maybe v) -> pure reply
    else rHGetWithART
  where
    rHGetWithART :: (HasCallStack, ToJSON v, FromJSON v, L.MonadFlow m) => m (Maybe v)
    rHGetWithART = do
      let k' = TE.encodeUtf8 k
      let f' = TE.encodeUtf8 f
      r <- L.runKVDB cName $ L.hget k' f'
      res <- case r of
        Right (Just val) -> do
          let v = A.eitherDecode $ BSL.fromStrict val
          case v of
            Left err -> do
              L.logErrorWithCategory @Text "Decoding error: " (T.pack err) $ ErrorL Nothing "REDIS_EXCEPTION" (T.pack err)
              pure Nothing
            Right v' -> do
              -- L.logDebug @Text "Decoded value" $ show v'
              pure $ Just v'
        Right Nothing -> pure Nothing
        Left err -> do
          L.logErrorWithCategoryV @Text "Decoding error: " (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure Nothing
      
      whenM isArtRecEnabled $ do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rHget::rHGetWithART::getCurrentTime" getCurrentTime  )
        L.appendRecordingLocal $ RunKVDBEntryT $ RHGetT $ (RHGet (encodeUtf8 k) (toJSON f) (maybe (Nothing) (Just . toJSON) res) recTimestamp cName) 
      whenM isArtV2RecEnabled $ do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rHget::rHGetWithART::getCurrentTime" getCurrentTime  )
        producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RHGetT $ (RHGet (encodeUtf8 k) (toJSON f) (maybe (Nothing) (Just . toJSON) res) recTimestamp cName) 
      pure res

rHgetB :: (HasCallStack, L.MonadFlow m) =>
  Text -> ByteKey -> ByteField -> m (Maybe ByteValue)
rHgetB cName k f = do  
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rHgetB::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RHGetT $ (RHGet (k) (toJSON f) (Nothing) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = encodeUtf8 $ err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          L.throwException $ S.err400 {S.errBody = errorMessage}
        Right (reply :: Maybe ByteValue) -> pure reply
    else rHgetBWithART
  where
    rHgetBWithART :: (HasCallStack, L.MonadFlow m) => m (Maybe ByteValue)
    rHgetBWithART = do
      r <- L.runKVDB cName $ L.hget k f
      res <- case r of
        Right (Just val) -> pure $ Just val
        Right Nothing -> pure $ Nothing
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis hget" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure $ Nothing
      
      whenM isArtRecEnabled $ do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rHgetB::rHgetBWithART::getCurrentTime" getCurrentTime)
        L.appendRecordingLocal $ RunKVDBEntryT $ RHGetT $ (RHGet (k) (toJSON f) (maybe (Nothing) (Just . toJSON) res) recTimestamp cName)
      whenM isArtV2RecEnabled $ do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rHgetB::rHgetBWithART::getCurrentTime" getCurrentTime)
        producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RHGetT $ (RHGet (k) (toJSON f) (maybe (Nothing) (Just . toJSON) res) recTimestamp cName)
      pure res
    
rHGetAll :: (HasCallStack, L.MonadFlow m) =>
  RedisName
  -> L.KVDBKey
  -> m (Either KVDBReply [(ByteString, ByteString)])
rHGetAll cName k = do  
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rHGetAll::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RHGetAllT $ (RHGetAll k (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  A.eitherDecode <$> ER.callBrahmaReplayR replayKVDBEntry msessionId
      case resp of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply [(ByteString, ByteString)]) -> pure reply
  else do
    res <- rHGetAll'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rHGetAll::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RHGetAllT $ (RHGetAll k (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
    whenM isArtV2RecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rHGetAll::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RHGetAllT $ (RHGetAll k (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
    pure res
    where
      rHGetAll' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply [(ByteString, ByteString)])
      rHGetAll' = do 
        res <- L.runKVDB cName $ L.hgetAll k
        case res of
          Right _ -> pure res
          Left err -> do
            L.logErrorWithCategoryV @Text "Redis rHGetAllB" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
            pure res 

rHDelB :: (HasCallStack, L.MonadFlow m) =>
  RedisName
  -> L.KVDBKey
  -> [L.KVDBField]
  -> m (Either KVDBReply Integer)
rHDelB cName k f = do  
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rHDelB::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RHDelBT $ (RHDel k f (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  A.eitherDecode <$> ER.callBrahmaReplayR replayKVDBEntry msessionId
      case resp of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply Integer) -> pure reply
  else do
    res <- rHDel'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rHDelB::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RHDelBT $ (RHDel k f (either (Left . toJSON) (Right) res) recTimestamp cName)
    whenM isArtV2RecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rHDelB::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RHDelBT $ (RHDel k f (either (Left . toJSON) (Right) res) recTimestamp cName)
    pure res
    where
      rHDel' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply Integer)
      rHDel' = do 
        res <- L.runKVDB cName $ L.hdel k f
        case res of
          Right _ -> pure res
          Left err -> do
            L.logErrorWithCategoryV @Text "Redis rHDelB" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
            pure res

-- ----------------------------------------------------------------------------
rHincrBy :: (HasCallStack, L.MonadFlow m)
  => RedisName -> TextKey -> TextField -> Integer -> m (Either KVDBReply Integer)
rHincrBy cName k f val = rHincrByB cName k' f' val
  where
    k' = TE.encodeUtf8 k
    f' = TE.encodeUtf8 f

rHincrByB :: (HasCallStack, L.MonadFlow m)
  => RedisName -> ByteKey -> ByteField -> Integer -> m (Either KVDBReply Integer)
rHincrByB cName k f val = do
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rHincrByB::getCurrentTime" getCurrentTime  )
      let replayKVDBEntry = RunKVDBEntryT $ RHIncrByBT $ (RHIncrByB (k) (toJSON f) val (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply Integer) -> pure reply
    else do
      
      res <- rHincrByB'
      whenM isArtRecEnabled $ do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rHincrByB::getCurrentTime" getCurrentTime )
        L.appendRecordingLocal $ RunKVDBEntryT $ RHIncrByBT $ (RHIncrByB (k) (toJSON f) val (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
      whenM isArtV2RecEnabled $ do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rHincrByB::getCurrentTime" getCurrentTime )
        producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RHIncrByBT $ (RHIncrByB (k) (toJSON f) val (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
      pure res
    where
    rHincrByB' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply Integer)
    rHincrByB' = do
      res <- L.runKVDB cName $ L.hincrBy k f val
      case res of
        Right _ -> do
          pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis hincrBy" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

rHset :: (HasCallStack, ToJSON v, L.MonadFlow m)
  => RedisName -> TextKey -> TextField -> v -> m (Either KVDBReply Integer)
rHset cName k f v = rHsetB cName k' f' v'
  where
    k' = TE.encodeUtf8 k
    f' = TE.encodeUtf8 f
    v' = BSL.toStrict $ A.encode v

rHsetB :: (HasCallStack, L.MonadFlow m)
  => RedisName -> ByteKey -> ByteField -> ByteValue -> m (Either KVDBReply Integer)
rHsetB cName k f v = do
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rHsetB::getCurrentTime" getCurrentTime )
      let replayKVDBEntry = RunKVDBEntryT $ RHSetBT $ (RHSetB (k) (toJSON f) (toJSON v) (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply Integer) -> pure reply
    else do
      
      res <- rHsetB'
      whenM isArtRecEnabled $ do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rHsetB::getCurrentTime" getCurrentTime)
        L.appendRecordingLocal $ RunKVDBEntryT $ RHSetBT $ (RHSetB (k) (toJSON f) (toJSON v) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
      whenM isArtV2RecEnabled $ do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rHsetB::getCurrentTime" getCurrentTime)
        producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RHSetBT $ (RHSetB (k) (toJSON f) (toJSON v) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
      pure res
    where
    rHsetB' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply Integer)
    rHsetB' = do
      res <- L.runKVDB cName $ L.hset k f v
      case res of
        Right _ -> do
          -- L.logInfo @Text "Redis hset" $ show r
          pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis hset" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

rHsetNx :: (HasCallStack, L.MonadFlow m)
  => RedisName -> ByteKey -> ByteField -> ByteValue -> m (Either KVDBReply Bool)
rHsetNx cName k f v = do
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rHsetNx::getCurrentTime" getCurrentTime )
      let replayKVDBEntry = RunKVDBEntryT $ RHSetNxBT $ (RHSetNxB (k) (toJSON f) (toJSON v) (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply Bool) -> pure reply
    else do
      res <- rHsetNx'
      whenM isArtRecEnabled $ do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rHsetNx::getCurrentTime" getCurrentTime)
        L.appendRecordingLocal $ RunKVDBEntryT $ RHSetNxBT $ (RHSetNxB (k) (toJSON f) (toJSON v) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
      whenM isArtV2RecEnabled $ do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rHsetNx::getCurrentTime" getCurrentTime)
        producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RHSetNxBT $ (RHSetNxB (k) (toJSON f) (toJSON v) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
      pure res
    where
    rHsetNx' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply Bool)
    rHsetNx' = do
      res <- L.runKVDB cName $ L.hsetNx k f v
      case res of
        Right _ -> do
          -- L.logInfo @Text "Redis hset" $ show r
          pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis hset" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

rHmset :: (HasCallStack, ToJSON v, L.MonadFlow m)
  => RedisName -> TextKey -> [(TextField, v)] -> m (Either KVDBReply KVDBStatus)
rHmset cName k v = rHmsetB cName k' $ map (\(f, val) -> (TE.encodeUtf8 f, BSL.toStrict $ A.encode val)) v
  where
    k' = TE.encodeUtf8 k

rHmsetB :: (HasCallStack, L.MonadFlow m)
  => RedisName -> ByteKey -> [(ByteField, ByteValue)] -> m (Either KVDBReply KVDBStatus)
rHmsetB cName k v = do
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rHmsetB::getCurrentTime" getCurrentTime )
      let replayKVDBEntry = RunKVDBEntryT $ RHmSetBT $ (RHmSetB (k)  v (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply KVDBStatus) -> pure reply
    else do
      
      res <- rHmsetB'
      whenM isArtRecEnabled $ do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rHmsetB::getCurrentTime" getCurrentTime)
        L.appendRecordingLocal $ RunKVDBEntryT $ RHmSetBT $ (RHmSetB (k) v (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
      whenM isArtV2RecEnabled $ do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rHmsetB::getCurrentTime" getCurrentTime)
        producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RHmSetBT $ (RHmSetB (k) v (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
      pure res
    where
    rHmsetB' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply KVDBStatus)
    rHmsetB' = do
      res <- L.runKVDB cName $ L.hmset k v
      case res of
        Right _ -> do
          pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis hmset" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

rHLenB :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> ByteKey -> m (Either KVDBReply Integer)
rHLenB cName key = do
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rHLenB::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RHLenBT $ (RHLenB key (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply Integer) -> pure reply
  else do
    res <- rHLenB'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rHLenB::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RHLenBT $ (RHLenB key (either (Left . toJSON) (Right) res) recTimestamp cName)
    whenM isArtV2RecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rHLenB::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RHLenBT $ (RHLenB key (either (Left . toJSON) (Right) res) recTimestamp cName)
    pure res
  where
    rHLenB' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply Integer)
    rHLenB' = do
      res <- L.runKVDB cName $ L.hlen key
      case res of
        Right _ -> pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis hlen" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

-- ----------------------------------------------------------------------------

rIncr :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> TextKey -> m (Either KVDBReply Integer)
rIncr cName k = rIncrB cName (TE.encodeUtf8 k)

rIncrWithART :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> TextKey -> m (Either KVDBReply Integer)
rIncrWithART cName k = rIncrBWithART cName (TE.encodeUtf8 k)

rIncrB :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> ByteKey -> m (Either KVDBReply Integer)
rIncrB = withDefaultArtRecOptions id rIncrBWithART

rIncrBWithART :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> ByteKey -> m (Either KVDBReply Integer)
rIncrBWithART cName k = do
    
  -- shouldReplayForKVDBWithSessId <- shouldReplayForKVDB cName
  -- skipping the above check for incr operation to record auto increment ids
  shouldReplayArtV2 <- shouldARTV2Replay 
  if isArtReplayEnabled || shouldReplayArtV2
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rIncrBWithART::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RIncrBT $ (RIncrB (k) (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply Integer) -> pure reply
  else do
    
    res <- rIncrB'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rIncrBWithART::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RIncrBT $ (RIncrB (k) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
    whenM isArtV2RecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rIncrBWithART::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RIncrBT $ (RIncrB (k) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
    pure res
  where
    rIncrB' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply Integer)
    rIncrB' = do 
      res <- L.runKVDB cName $ L.incr k
      case res of
        Right _ -> do
          -- L.logInfo @Text "Redis incr" $ show r
          pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis incr" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

rIncrBy :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> TextKey -> Integer -> m (Either KVDBReply Integer)
rIncrBy cName k v = rIncrByB cName (TE.encodeUtf8 k) v

rIncrByB :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> ByteKey -> Integer -> m (Either KVDBReply Integer)
rIncrByB cName k v = do
    
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rIncrByB::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RIncrByBT $ (RIncrByB (k) (v) (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply Integer) -> pure reply
  else do
    
    res <- rIncrByB'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rIncrByB::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RIncrByBT $ (RIncrByB (k) (v) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
    whenM isArtV2RecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rIncrByB::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RIncrByBT $ (RIncrByB (k) (v) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
    pure res
  where
    rIncrByB' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply Integer)
    rIncrByB' = do 
      res <- L.runKVDB cName $ L.incrBy k v
      case res of
        Right _ -> pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis incrBy" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

rIncrByFloat :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> TextKey -> Double -> m (Either KVDBReply Double)
rIncrByFloat cName k v = rIncrByFloatB cName (TE.encodeUtf8 k) v

rIncrByFloatB :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> ByteKey -> Double -> m (Either KVDBReply Double)
rIncrByFloatB cName k v = do
    
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rIncrByFloatB::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RIncrByFloatBT $ (RIncrByFloatB (k) (v) (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply Double) -> pure reply
  else do
    
    res <- rIncrByFloatB'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rIncrByFloatB::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RIncrByFloatBT $ (RIncrByFloatB (k) (v) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
    whenM isArtV2RecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rIncrByFloatB::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RIncrByFloatBT $ (RIncrByFloatB (k) (v) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
    pure res
  where
    rIncrByFloatB' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply Double)
    rIncrByFloatB' = do 
      res <- L.runKVDB cName $ L.incrByFloat k v
      case res of
        Right _ -> pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis incrByFloat" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

-- ----------------------------------------------------------------------------

-- ----------------------------------------------------------------------------

rDecr :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> TextKey -> m (Either KVDBReply Integer)
rDecr cName k = rDecrB cName (TE.encodeUtf8 k)

rDecrB :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> ByteKey -> m (Either KVDBReply Integer)
rDecrB cName k = do
    
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rDecrB::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RDecrBT $ (RDecrB (k) (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <- ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply Integer) -> pure reply
  else do
    
    res <- rDecrB'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rDecrB::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RDecrBT $ (RDecrB (k) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
    whenM isArtV2RecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rDecrB::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RDecrBT $ (RDecrB (k) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
    pure res
  where
    rDecrB' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply Integer)
    rDecrB' = do 
      res <- L.runKVDB cName $ L.decr k
      case res of
        Right _ -> do
          -- L.logInfo @Text "Redis decr" $ show r
          pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis decr" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

rDecrBy :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> TextKey -> Integer -> m (Either KVDBReply Integer)
rDecrBy cName k v = rDecrByB cName (TE.encodeUtf8 k) v

rDecrByB :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> ByteKey -> Integer -> m (Either KVDBReply Integer)
rDecrByB cName k v = do
    
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rDecrByB::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RDecrByBT $ (RDecrByB (k) (v) (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <- ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply Integer) -> pure reply
  else do
    
    res <- rDecrByB'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rDecrByB::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RDecrByBT $ (RDecrByB (k) (v) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
    whenM isArtV2RecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rDecrByB::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RDecrByBT $ (RDecrByB (k) (v) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
    pure res
  where
    rDecrByB' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply Integer)
    rDecrByB' = do 
      res <- L.runKVDB cName $ L.decrBy k v
      case res of
        Right _ -> pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis decrBy" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
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
rSetB = withDefaultArtRecOptions id rSetBWithART

rSetBWithART :: (HasCallStack, L.MonadFlow m, HasArtRecOptions) =>
  Text -> ByteKey -> ByteValue -> m (Either KVDBReply KVDBStatus)
rSetBWithART cName k v = do 
    
  shouldReplayForKVDBWithSessId <- shouldReplayForKVDB cName
  if isArtReplayEnabled || shouldReplayForKVDBWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rSetBWithART::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RSetBT $ (RSetB (k) (toJSON v) (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply KVDBStatus) -> pure reply
  else do
    
    res <- rSetB'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rSetBWithART::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RSetBT $ (RSetB (k) (toJSON v) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
    whenM isArtV2RecEnabledForKVDB $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rSetBWithART::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RSetBT $ (RSetB (k) (toJSON v) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
    pure res
  where 
    rSetB' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply KVDBStatus)
    rSetB' = do
      res <- L.runKVDB cName $ L.set k v
      case res of
        Right _ -> do
          -- L.logInfo @Text "Redis set" $ show r
          pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis set" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
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
rGetB = withDefaultArtRecOptions id rGetBWithART

rGetBWithART :: (HasCallStack, L.MonadFlow m, HasArtRecOptions) =>
  RedisName -> ByteKey -> m (Maybe ByteValue) -- Binary.decode?
rGetBWithART cName k = do
  shouldReplayForKVDBWithSessId <- shouldReplayForKVDB cName
  if isArtReplayEnabled || shouldReplayForKVDBWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rGetBWithART::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RGetBT $ (RGetB (k) (Nothing) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp :: Either String (Maybe ByteValue)
      case maybeReply of
        Left err -> do
          let errorMessage = encodeUtf8 $ err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          L.throwException $ S.err400 {S.errBody = errorMessage}
        Right (reply :: Maybe ByteValue) -> pure reply
  else do
    res <- rGetB'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rGetBWithART::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RGetBT $ (RGetB (k) (maybe (Nothing) (Just . toJSON) res) recTimestamp cName)
    whenM isArtV2RecEnabledForKVDB $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rGetBWithART::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RGetBT $ (RGetB (k) (maybe (Nothing) (Just . toJSON) res) recTimestamp cName)
    pure res
  where
    rGetB' :: (HasCallStack, L.MonadFlow m) => m (Maybe ByteValue)
    rGetB' = do
      mv <- L.runKVDB cName $ L.get k
      case mv of
        Right mval -> pure mval
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis get" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure Nothing

rGetBEither :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> ByteKey -> m (Either KVDBReply (Maybe ByteValue)) -- Binary.decode?
rGetBEither = withDefaultArtRecOptions id rGetBEitherWithART

rGetBEitherWithART :: (HasCallStack, L.MonadFlow m, HasArtRecOptions) =>
  RedisName -> ByteKey -> m (Either KVDBReply (Maybe ByteValue))
rGetBEitherWithART cName k = do
  shouldReplayForKVDBWithSessId <- shouldReplayForKVDB cName
  if isArtReplayEnabled || shouldReplayForKVDBWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rGetBEitherWithART::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RGetBT $ (RGetB (k) (Nothing) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp :: Either String (Maybe ByteValue)
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right reply -> pure $ Right reply
  else do
    res <- rGetB'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rGetBEitherWithART::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RGetBT $ (RGetB (k) (either (\_ -> Nothing) (maybe (Nothing) (Just . toJSON)) res) recTimestamp cName)
    whenM isArtV2RecEnabledForKVDB $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rGetBEitherWithART::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RGetBT $ (RGetB (k) (either (\_ -> Nothing) (maybe (Nothing) (Just . toJSON)) res) recTimestamp cName)
    pure res
  where
    rGetB' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply (Maybe ByteValue))
    rGetB' = do
      mv <- L.runKVDB cName $ L.get k
      case mv of
        Right _ -> pure mv
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis get" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure mv
--doubt
rGet :: (HasCallStack, FromJSON v, L.MonadFlow m) =>
  RedisName -> TextKey -> m (Maybe v)
rGet = withDefaultArtRecOptions id rGetWithART

rGetWithART :: (HasCallStack, FromJSON v, L.MonadFlow m, HasArtRecOptions) =>
  RedisName -> TextKey -> m (Maybe v)
rGetWithART cName k = do
  -- L.logDebug @Text "rGet" $ "looking up key: " <> k <> " in redis: " <> cName
  mv <- rGetBWithART cName (TE.encodeUtf8 k)
  case mv of
    Just val -> case A.eitherDecode' @A.Value $ BSL.fromStrict val of
      Left err -> do
        let errReason = "error: '" <> toText err
                                  <> "' while decoding key: "
                                  <> k <>
                                  " with value: "
                                  <> (fromEither $ mapLeft (toText . displayException) $ TE.decodeUtf8' val)
        L.logErrorWithCategory @Text "rGet value is not a valid JSON" errReason $ ErrorL Nothing "REDIS_EXCEPTION" errReason
        pure Nothing
      Right value -> do
        case (A.parseEither A.parseJSON value) of
          Left err -> do
            let errReason = "error: '" <> toText err
                                      <> "' while decoding key: "
                                      <> k <>
                                      " with value: "
                                      <> (TE.decodeUtf8 . BSL.toStrict . A.encode . obfuscate) value
            L.logErrorWithCategory @Text "rGet value cannot be decoded to target type" errReason $ ErrorL Nothing "REDIS_EXCEPTION" errReason
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
          L.logErrorWithCategoryV @Text "Redis rGetT unicode decode error" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
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

rSetexWithART :: (HasArtRecOptions, HasCallStack, ToJSON v, Integral t, L.MonadFlow m) =>
  RedisName -> TextKey -> v -> t -> m (Either KVDBReply KVDBStatus)
rSetexWithART cName k v = rSetexBWithART cName k' v'
  where
    k' = TE.encodeUtf8 k
    v' = BSL.toStrict $ A.encode v

rSetexB :: (HasCallStack, Integral t, L.MonadFlow m) =>
  RedisName -> ByteKey -> ByteValue -> t -> m (Either KVDBReply KVDBStatus)
rSetexB cName k v t = withDefaultArtRecOptions id $ rSetexBWithART cName k v t

rSetexBWithART :: (HasArtRecOptions, HasCallStack, Integral t, L.MonadFlow m) =>
  RedisName -> ByteKey -> ByteValue -> t -> m (Either KVDBReply KVDBStatus)
rSetexBWithART cName k v t = do
  shouldReplayForKVDBWithSessId <- shouldReplayForKVDB cName
  if isArtReplayEnabled || shouldReplayForKVDBWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rSetexBWithART::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RSetexBT $ (RSetexB (k) (toInteger t) (toJSON v) (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply KVDBStatus) -> pure reply
  else do
    res <- rSetexB'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rSetexBWithART::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RSetexBT $ (RSetexB (k) (toInteger t) (toJSON v) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
    whenM isArtV2RecEnabledForKVDB $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rSetexBWithART::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RSetexBT $ (RSetexB (k) (toInteger t) (toJSON v) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
    pure res
  where
    rSetexB' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply KVDBStatus)
    rSetexB' = do
      res <- L.runKVDB cName $ L.setex k (toInteger t) v
      case res of
        Right _ -> do
          -- L.logInfo @Text "Redis setex" $ show r
          pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis setex" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
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
  let kvMap' = A.Object $ KM.fromList $ foldl' (\acc (k,v) -> acc <> ([(AK.fromText $ decodeUtf8 k,A.String $ decodeUtf8 v)])) [] $ Map.toList kvMap
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rSetexBulkB::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RSetexBulkBT $ (RSetexBulkB (kvMap') (toInteger t) (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply ()) -> pure reply
  else do 
    res <- rSetexBulkB'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rSetexBulkB::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RSetexBulkBT $ (RSetexBulkB (kvMap') (toInteger t) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
    whenM isArtV2RecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rSetexBulkB::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RSetexBulkBT $ (RSetexBulkB (kvMap') (toInteger t) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
    pure res
    where
      rSetexBulkB' :: (HasCallStack, L.MonadFlow m) =>  m (Either KVDBReply ())
      rSetexBulkB' = do
        let t' = toInteger t
        res <- L.runKVDB cName $ forM_ (Map.toList kvMap) $ \(k, v) -> L.setex k t' v
        case res of
          Right _ -> do
            -- L.logInfo @Text "Redis setexBulk" $ show r
            pure res
          Left err -> do
            L.logErrorWithCategoryV @Text "Redis setexBulk" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
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
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rSetOptsB::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RSetOptsBT $ (RSetOptsB (k) (toJSON v) (toJSON ttl) (toJSON cond) (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply Bool) -> pure reply
    else do
      res <- rSetOptsB'
      whenM isArtRecEnabled $ do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rSetOptsB::getCurrentTime" getCurrentTime)
        L.appendRecordingLocal $ RunKVDBEntryT $ RSetOptsBT $ (RSetOptsB (k) (toJSON v) (toJSON ttl) (toJSON cond) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
      whenM isArtV2RecEnabled $ do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rSetOptsB::getCurrentTime" getCurrentTime)
        producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RSetOptsBT $ (RSetOptsB (k) (toJSON v) (toJSON ttl) (toJSON cond) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
      pure res
  where
    rSetOptsB' :: (HasCallStack, L.MonadFlow m) =>  m (Either KVDBReply Bool)
    rSetOptsB' = do
      res <- L.runKVDB cName $ L.setOpts k v ttl cond
      case res of
        Right _ -> pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis setOpts" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
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
  -> Text
  -> m (Either KVDBReply (Maybe [L.KVDBStreamReadResponse]))
rXreadT cName k v = rXreadB cName k' v'
  where
    k' = TE.encodeUtf8 k
    v' = TE.encodeUtf8 v

rXreadTWithART
  :: (HasCallStack, L.MonadFlow m, HasArtRecOptions)
  => RedisName
  -> Text
  -> Text
  -> m (Either KVDBReply (Maybe [L.KVDBStreamReadResponse]))
rXreadTWithART cName k v = rXreadBWithART cName k' v'
  where
    k' = TE.encodeUtf8 k
    v' = TE.encodeUtf8 v

rXreadB :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> L.KVDBStream -> L.RecordID -> m (Either KVDBReply (Maybe [L.KVDBStreamReadResponse]))
rXreadB = withDefaultArtRecOptions id rXreadBWithART

rXreadBWithART :: (HasCallStack, L.MonadFlow m, HasArtRecOptions) =>
  RedisName -> L.KVDBStream -> L.RecordID -> m (Either KVDBReply (Maybe [L.KVDBStreamReadResponse]))
rXreadBWithART cName strm entryId = do
  shouldReplayForKVDBWithSessId <- shouldReplayForKVDB cName
  if isArtReplayEnabled || shouldReplayForKVDBWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rXreadBWithART::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RXreadBT $ (RXreadB (strm) (entryId) (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply (Maybe [L.KVDBStreamReadResponse])) -> pure reply
    else do
      res <- rXreadB'
      whenM isArtRecEnabled $ do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rXreadBWithART::getCurrentTime" getCurrentTime)
        L.appendRecordingLocal $ RunKVDBEntryT $ RXreadBT $ (RXreadB (strm) (entryId) (either (Left . toJSON) (Right . maybe (Nothing) (Just . toJSON)) res) recTimestamp cName)
      whenM isArtV2RecEnabledForKVDB $ do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rXreadBWithART::getCurrentTime" getCurrentTime)
        producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RXreadBT $ (RXreadB (strm) (entryId) (either (Left . toJSON) (Right . maybe (Nothing) (Just . toJSON)) res) recTimestamp cName)
      pure res
  where
    rXreadB' ::  (HasCallStack, L.MonadFlow m) => m (Either KVDBReply (Maybe [L.KVDBStreamReadResponse]))
    rXreadB' = do
      res <- L.runKVDB cName $ L.xread strm entryId
      _ <-  case res of
        Left err ->
          L.logErrorWithCategoryV @Text "Redis xread" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
        Right _ -> pure ()
      pure res


rXrangeT ::
  (HasCallStack, L.MonadFlow m) =>
  RedisName ->
  Text ->
  Text ->
  Text ->
  Maybe Integer ->
  m (Either KVDBReply ([L.KVDBStreamReadResponseRecord]))
rXrangeT cName strm sstart send count = rXrangeB cName s' ss' se' count
  where
    s' = TE.encodeUtf8 strm
    se' = TE.encodeUtf8 send
    ss' = TE.encodeUtf8 sstart

rXrangeB ::
  (HasCallStack, L.MonadFlow m) =>
  RedisName ->
  L.KVDBStream ->
  L.KVDBStreamStart ->
  L.KVDBStreamEnd ->
  Maybe Integer ->
  m (Either KVDBReply ([L.KVDBStreamReadResponseRecord]))
rXrangeB cName strm sstart send count = do
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rXrangeB::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RXrangeBT $ (RXrangeB (strm) (sstart) (send)  (count) (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <- ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply ([L.KVDBStreamReadResponseRecord])) -> pure reply
    else do
      res <- rXrangeB'
      whenM isArtRecEnabled $ do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rXrangeB::getCurrentTime" getCurrentTime)
        L.appendRecordingLocal $ RunKVDBEntryT $ RXrangeBT $ (RXrangeB (strm) (sstart) (send) (count) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
      whenM isArtV2RecEnabled $ do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rXrangeB::getCurrentTime" getCurrentTime)
        producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RXrangeBT $ (RXrangeB (strm) (sstart) (send) (count) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
      pure res
  where
    rXrangeB' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply ([L.KVDBStreamReadResponseRecord]))
    rXrangeB' = do
      res <- L.runKVDB cName $ L.xrange strm sstart send  count
      _ <- case res of
        Left err ->
          L.logErrorWithCategoryV @Text "Redis xrange" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
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
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rXrevrangeB::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RXrevrangeBT $ (RXrevrangeB (strm) (send) (sstart) (count) (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply ([L.KVDBStreamReadResponseRecord])) -> pure reply
  else do
    res <- rXrevrangeB'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rXrevrangeB::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RXrevrangeBT $ (RXrevrangeB (strm) (send) (sstart) (count) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
    whenM isArtV2RecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rXrevrangeB::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RXrevrangeBT $ (RXrevrangeB (strm) (send) (sstart) (count) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
    pure res
    where
      rXrevrangeB' :: (HasCallStack,L.MonadFlow m) => m (Either KVDBReply ([L.KVDBStreamReadResponseRecord]))
      rXrevrangeB' = do
        res <- L.runKVDB cName $ L.xrevrange strm send sstart count
        _ <- case res of
          Left err ->
            L.logErrorWithCategoryV @Text "Redis xrevrange" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          Right _ -> pure ()
        pure res
-- ------------------------------------------------------------------------------

rSadd :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> L.KVDBKey -> [L.KVDBValue] -> m (Either KVDBReply Integer)
rSadd = withDefaultArtRecOptions id rSaddWithART

rSaddWithART :: (HasCallStack, L.MonadFlow m, HasArtRecOptions) =>
  RedisName -> L.KVDBKey -> [L.KVDBValue] -> m (Either KVDBReply Integer)
rSaddWithART cName k v = do
  shouldReplayForKVDBWithSessId <- shouldReplayForKVDB cName
  if isArtReplayEnabled || shouldReplayForKVDBWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rSaddWithART::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RSaddT $ (RSadd k v (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp :: Either String Integer
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right reply -> pure $ Right reply
  else do
    res <- rSadd'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rSaddWithART::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RSaddT $ (RSadd k v (either (Left . toJSON) (Right) res) recTimestamp cName)
    whenM isArtV2RecEnabledForKVDB $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rSaddWithART::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RSaddT $ (RSadd k v (either (Left . toJSON) (Right) res) recTimestamp cName)
    pure res
  where
    rSadd' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply Integer)
    rSadd' = do
      res <- L.runKVDB cName $ L.sadd k v
      case res of
        Right _ -> pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis sadd" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

rSismember :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> L.KVDBKey -> L.KVDBValue -> m (Either KVDBReply Bool)
rSismember cName k v = do
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rSismember::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RSismemberT $ (RSismember k v (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply Bool) -> pure reply
    else do
      res <- rSismember'
      whenM isArtRecEnabled $ do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rSismember::getCurrentTime" getCurrentTime)
        L.appendRecordingLocal $ RunKVDBEntryT $ RSismemberT $ (RSismember k v (either (Left . toJSON) (Right) res) recTimestamp cName)
      whenM isArtV2RecEnabled $ do
        recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rSismember::getCurrentTime" getCurrentTime)
        producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RSismemberT $ (RSismember k v (either (Left . toJSON) (Right) res) recTimestamp cName)
      pure res
  where
    rSismember' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply Bool)
    rSismember' = do
      res <- L.runKVDB cName $ L.sismember k v
      case res of
        Right _ -> pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis sismember" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

rZAdd :: (HasCallStack, L.MonadFlow m) =>
  RedisName
  -> L.KVDBKey
  -> [(Double,ByteValue)]
  -> m (Either KVDBReply Integer)
rZAdd = withDefaultArtRecOptions id rZAddWithART

rZAddWithART :: (HasCallStack, L.MonadFlow m, HasArtRecOptions) =>
  RedisName
  -> L.KVDBKey
  -> [(Double,ByteValue)]
  -> m (Either KVDBReply Integer)
rZAddWithART cName k v = do
  shouldReplayForKVDBWithSessId <- shouldReplayForKVDB cName
  if isArtReplayEnabled || shouldReplayForKVDBWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rZAddWithART::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RZAddT $ (RZAdd k v (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply Integer) -> pure reply
  else do
    res <- rZAdd'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rZAddWithART::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RZAddT $ (RZAdd k v (either (Left . toJSON) (Right) res) recTimestamp cName)
    whenM isArtV2RecEnabledForKVDB $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rZAddWithART::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RZAddT $ (RZAdd k v (either (Left . toJSON) (Right) res) recTimestamp cName)
    pure res
  where
    rZAdd' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply Integer)
    rZAdd' = do
      res <- L.runKVDB cName $ L.zadd k v
      case res of
        Right _ -> pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis setOpts" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

rZRangeByScore :: (HasCallStack, L.MonadFlow m) =>
  RedisName
  -> L.KVDBKey
  -> Double
  -> Double
  -> m (Either KVDBReply [L.KVDBValue])
rZRangeByScore cName k minScore maxScore = do
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rZRangeByScore::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RZRangeByScoreT $ (RZRangeByScore k minScore maxScore (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply [L.KVDBValue]) -> pure reply
  else do
    res <- rZRangeByScore'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rZRangeByScore::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RZRangeByScoreT $ (RZRangeByScore k minScore maxScore (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
    whenM isArtV2RecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rZRangeByScore::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RZRangeByScoreT $ (RZRangeByScore k minScore maxScore (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
    pure res
  where
    rZRangeByScore' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply [L.KVDBValue])
    rZRangeByScore' = do
      res <- L.runKVDB cName $ L.zrangebyscore k minScore maxScore
      case res of
        Right _ -> pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis rZRangeByScore" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

rZRangeWithScores :: (HasCallStack, L.MonadFlow m) =>
  RedisName
  -> L.KVDBKey
  -> Integer
  -> Integer
  -> m (Either KVDBReply [(L.KVDBValue,Double)])
rZRangeWithScores cName k minScore maxScore = do
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rZRangeWithScores::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RZRangeWithScoresBT $ (RZRangeWithScoresB k minScore maxScore (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply [(L.KVDBValue,Double)]) -> pure reply
  else do
    res <- L.runKVDB cName $ L.zrangewithscores k minScore maxScore
    whenM isArtV2RecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rZRangeWithScores::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RZRangeWithScoresBT $ RZRangeWithScoresB (k) minScore maxScore (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName
    case res of
      Right _ -> pure res
      Left err -> do
        L.logErrorWithCategoryV @Text "Redis rZRangeWithScores" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
        pure res

rZRangeByScoreWithScores :: (HasCallStack, L.MonadFlow m) =>
  RedisName
  -> L.KVDBKey
  -> Double
  -> Double
  -> m (Either KVDBReply [(L.KVDBValue,Double)])
rZRangeByScoreWithScores cName k minScore maxScore = do
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rZRangeByScoreWithScores::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RZRangeByScoreWithScoreBT $ (RZRangeByScoreWithScoreB k minScore maxScore (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply [(L.KVDBValue,Double)]) -> pure reply
  else do
    res <- L.runKVDB cName $ L.zrangebyscorewithscore k minScore maxScore
    whenM isArtV2RecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rZRangeByScoreWithScores::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RZRangeByScoreWithScoresBT $ RZRangeByScoreWithScoresB (k) minScore maxScore res recTimestamp cName
    case res of
      Right _ -> pure res
      Left err -> do
        L.logErrorWithCategoryV @Text "Redis rZRangeByScoreWithScore" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
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
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rZRangeByScoreWithLimit::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RZRangeByScoreWithLimitT $ (RZRangeByScoreWithLimit k minScore maxScore offset count (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply [L.KVDBValue]) -> pure reply
  else do
    res <- rZRangeByScoreWithLimit'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rZRangeByScoreWithLimit::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RZRangeByScoreWithLimitT $ (RZRangeByScoreWithLimit k minScore maxScore offset count (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
    whenM isArtV2RecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rZRangeByScoreWithLimit::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RZRangeByScoreWithLimitT $ (RZRangeByScoreWithLimit k minScore maxScore offset count (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
    pure res
   where
    rZRangeByScoreWithLimit' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply [L.KVDBValue]) 
    rZRangeByScoreWithLimit' = do
      res <- L.runKVDB cName $ L.zrangebyscorewithlimit k minScore maxScore offset count
      case res of
        Right _ -> pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis rZRangeByScoreWithLimit" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

rZRem :: (HasCallStack, L.MonadFlow m) =>
  RedisName
  -> L.KVDBKey
  -> [L.KVDBValue]
  -> m (Either KVDBReply Integer)
rZRem cName k v = do  
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rZRem::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RZRemT $ (RZRem k v (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply Integer) -> pure reply
  else do
    res <- rZRem'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rZRem::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RZRemT $ (RZRem k v (either (Left . toJSON) (Right) res) recTimestamp cName)
    whenM isArtV2RecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rZRem::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RZRemT $ (RZRem k v (either (Left . toJSON) (Right) res) recTimestamp cName)
    pure res
    where
      rZRem' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply Integer)
      rZRem' = do 
        res <- L.runKVDB cName $ L.zrem k v
        case res of
          Right _ -> pure res
          Left err -> do
            L.logErrorWithCategoryV @Text "Redis rZRem" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
            pure res

rZRemRangeByScore :: (HasCallStack, L.MonadFlow m) =>
  RedisName
  -> L.KVDBKey
  -> Double
  -> Double
  -> m (Either KVDBReply Integer)
rZRemRangeByScore cName k minScore maxScore = do
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rZRemRangeByScore::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RZRemRangeByScoreT $ (RZRemRangeByScore k minScore maxScore (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply Integer) -> pure reply
  else do
    res <- rZRemRangeByScore'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rZRemRangeByScore::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RZRemRangeByScoreT $ (RZRemRangeByScore k minScore maxScore (either (Left . toJSON) (Right) res) recTimestamp cName)
    whenM isArtV2RecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rZRemRangeByScore::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RZRemRangeByScoreT $ (RZRemRangeByScore k minScore maxScore (either (Left . toJSON) (Right) res) recTimestamp cName)
    pure res
    where
      rZRemRangeByScore' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply Integer)
      rZRemRangeByScore' = do
        res <- L.runKVDB cName $ L.zremrangebyscore k minScore maxScore
        case res of
          Right _ -> pure res
          Left err -> do
            L.logErrorWithCategoryV @Text "Redis rZRemRangeByScore" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
            pure res

rZCard :: (HasCallStack, L.MonadFlow m) =>
  RedisName
  -> L.KVDBKey
  -> m (Either KVDBReply Integer)
rZCard cName k = do
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rZCard::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RZCardT $ (RZCard k (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply Integer) -> pure reply
  else do
    res <- rZCard'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rZCard::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RZCardT $ (RZCard k (either (Left . toJSON) (Right) res) recTimestamp cName)
    whenM isArtV2RecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rZCard::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RZCardT $ (RZCard k (either (Left . toJSON) (Right) res) recTimestamp cName)
    pure res
  where
    rZCard' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply Integer)
    rZCard' = do 
      res <- L.runKVDB cName $ L.zcard k
      case res of
        Right _ -> pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis rZCard" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

rXaddB :: (HasCallStack, L.MonadFlow m) => RedisName -> L.KVDBStream -> [L.KVDBStreamItem] -> L.KVDBStreamEntryIDInput -> m (KVDBAnswer L.KVDBStreamEntryID)
rXaddB = withDefaultArtRecOptions id rXaddBWithART

rXaddBWithART :: (HasCallStack, L.MonadFlow m, HasArtRecOptions) => RedisName -> L.KVDBStream -> [L.KVDBStreamItem] -> L.KVDBStreamEntryIDInput -> m (KVDBAnswer L.KVDBStreamEntryID)
rXaddBWithART redisName streamName streamItems entryID = do
  shouldReplayForKVDBWithSessId <- shouldReplayForKVDB redisName
  if isArtReplayEnabled || shouldReplayForKVDBWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rXaddBWithART::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RXaddBT $ (RXaddB streamName (makeStreamEntryId entryID) (toJSON streamItems) (Left A.Null) recTimestamp redisName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: KVDBAnswer L.KVDBStreamEntryID) -> pure reply
  else do
    res <- rXaddB'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rXaddBWithART::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RXaddBT $ (RXaddB streamName (makeStreamEntryId entryID) (toJSON streamItems) (either (Left . toJSON) (Right . toJSON) res) recTimestamp redisName)
    whenM isArtV2RecEnabledForKVDB $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rXaddBWithART::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RXaddBT $ (RXaddB streamName (makeStreamEntryId entryID) (toJSON streamItems) (either (Left . toJSON) (Right . toJSON) res) recTimestamp redisName)
    pure res
  where 
    makeStreamEntryId (L.EntryID (L.KVDBStreamEntryID ms sq)) = show ms <> "-" <> show sq
    makeStreamEntryId L.AutoID = "*"

    rXaddB' :: (HasCallStack, L.MonadFlow m) => m (KVDBAnswer L.KVDBStreamEntryID)
    rXaddB' = do
      res <- L.runKVDB redisName $ L.xadd streamName entryID streamItems
      case res of
        Right _ -> pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis rXaddB" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

rSmembersB :: (HasCallStack, L.MonadFlow m) => RedisName -> ByteString -> m (KVDBAnswer [ByteString])
rSmembersB = withDefaultArtRecOptions id rSmembersBWithART

rSmembersBWithART :: (HasCallStack, L.MonadFlow m, HasArtRecOptions) => RedisName -> ByteString -> m (KVDBAnswer [ByteString])
rSmembersBWithART redisName k = do 
  shouldReplayForKVDBWithSessId <- shouldReplayForKVDB redisName
  if isArtReplayEnabled || shouldReplayForKVDBWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rSmembersBWithART::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RSmembersBT $ (RSmembersB k (Left A.Null) recTimestamp redisName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp :: Either String (KVDBAnswer [ByteString])
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: KVDBAnswer [ByteString]) -> pure $ reply
  else do
    res <- rSmembersB'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rSmembersBWithART::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RSmembersBT $ (RSmembersB k (either (Left . toJSON) (Right . toJSON) res) recTimestamp redisName)
    whenM isArtV2RecEnabledForKVDB $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rSmembersBWithART::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RSmembersBT $ (RSmembersB k (either (Left . toJSON) (Right . toJSON) res) recTimestamp redisName)
    pure res
  where 
    rSmembersB' :: (HasCallStack, L.MonadFlow m) => m (KVDBAnswer [ByteString])
    rSmembersB' = do
      res <- L.runKVDB redisName $ L.smembers k
      case res of
        Right _ -> pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis rSmembersB" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

sRemB :: (HasCallStack, L.MonadFlow m) => RedisName -> L.KVDBKey -> [L.KVDBValue] -> m (KVDBAnswer Integer)
sRemB = withDefaultArtRecOptions id sRemBWithART

sRemBWithART :: (HasCallStack, L.MonadFlow m, HasArtRecOptions) => RedisName -> L.KVDBKey -> [L.KVDBValue] -> m (KVDBAnswer Integer)
sRemBWithART redisName oldSKey pKeyList = do 
  shouldReplayForKVDBWithSessId <- shouldReplayForKVDB redisName
  if isArtReplayEnabled || shouldReplayForKVDBWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::sRemBWithART::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RSRemBT $ (RSRemB oldSKey pKeyList (Left A.Null) recTimestamp redisName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: KVDBAnswer Integer) -> pure reply
  else do
    res <- sRemB'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::sRemBWithART::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RSRemBT $ (RSRemB oldSKey pKeyList (either (Left . toJSON) (Right) res) recTimestamp redisName)
    whenM isArtV2RecEnabledForKVDB $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::sRemBWithART::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RSRemBT $ (RSRemB oldSKey pKeyList (either (Left . toJSON) (Right) res) recTimestamp redisName)
    pure res
  where 
    sRemB' :: (HasCallStack, L.MonadFlow m) => m (KVDBAnswer Integer)
    sRemB' = do
      res <- L.runKVDB redisName $ L.srem oldSKey pKeyList
      case res of
        Right _ -> pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis sRemB" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

rMultiExec :: (HasCallStack, L.MonadFlow m, ToJSON a ,FromJSON a) => RedisName -> L.KVDBTx (R.Queued a) -> m (KVDBAnswer (TxResult a))
rMultiExec redisName tx = do
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis redisName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rMultiExec::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RMultiExecT $ (RMultiExec (Left A.Null) recTimestamp redisName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage err)
        Right (reply :: KVDBAnswer (TxResult a)) -> pure reply
  else do
    
    res <- multiExecWithHash'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rMultiExec::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RMultiExecT $ (RMultiExec (either (Left . toJSON) (Right . toJSON) res) recTimestamp redisName)
    whenM isArtV2RecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rMultiExec::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RMultiExecT $ (RMultiExec (either (Left . toJSON) (Right . toJSON) res) recTimestamp redisName)
    pure res
  where 
    multiExecWithHash' = do
      res <- L.runKVDB redisName $ L.multiExec tx
      case res of
        Right _ -> pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis multiExec" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

rLLenB :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> ByteKey -> m (Either KVDBReply Integer)
rLLenB cName key = do
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rLLenB::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RLLenBT $ (RLLenB key (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply Integer) -> pure reply
  else do
    res <- rLLenB'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rLLenB::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RLLenBT $ (RLLenB key (either (Left . toJSON) (Right) res) recTimestamp cName)
    whenM isArtV2RecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rLLenB::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RLLenBT $ (RLLenB key (either (Left . toJSON) (Right) res) recTimestamp cName)
    pure res
  where
    rLLenB' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply Integer)
    rLLenB' = do
      res <- L.runKVDB cName $ L.llen key
      case res of
        Right _ -> pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis llen" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

rLRangeB :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> ByteKey -> Integer -> Integer -> m (Either KVDBReply [ByteString])
rLRangeB cName key start stop = do
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rLRangeB::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RLRangeBT $ (RLRangeB key start stop (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply [ByteString]) -> pure reply
  else do
    res <- rLRangeB'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rLRangeB::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RLRangeBT $ (RLRangeB key start stop (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName)
    pure res
  where
    rLRangeB' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply [ByteString])
    rLRangeB' = do
      res <- L.runKVDB cName $ L.lrange key start stop
      case res of
        Right _ -> pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis lrange" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

rPingRequest :: (HasCallStack, L.MonadFlow m) => RedisName -> m (KVDBAnswer R.Status)
rPingRequest cName = do
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rPingRequest::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RPingBT $ RPingB (Left A.Null) recTimestamp cName
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: KVDBAnswer R.Status) -> pure reply
  else do
    res <- rPingRequest'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rPingRequest::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RPingBT $ RPingB (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName
    whenM isArtV2RecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rPingRequest::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RPingBT $ RPingB (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName
    pure res
  where
    rPingRequest' :: (HasCallStack, L.MonadFlow m) => m (KVDBAnswer R.Status)
    rPingRequest' = do
      res <- L.runKVDB cName L.pingRequest
      case res of
        Right _ -> pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis pingRequest" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

rXLenB :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> ByteKey -> m (Either KVDBReply Integer)
rXLenB cName key = do
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rXLenB::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RLLenBT $ (RLLenB key (Left A.Null) recTimestamp cName)
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply Integer) -> pure reply
  else do
    res <- rXLenB'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rXLenB::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RLLenBT $ (RLLenB key (either (Left . toJSON) (Right) res) recTimestamp cName)
    whenM isArtV2RecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rXLenB::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RLLenBT $ (RLLenB key (either (Left . toJSON) (Right) res) recTimestamp cName)
    pure res
  where
    rXLenB' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply Integer)
    rXLenB' = do
      res <- L.runKVDB cName $ L.xlen key
      case res of
        Right _ -> pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis xlen" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

rXDelB :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> ByteKey -> [L.KVDBStreamEntryID] -> m (Either KVDBReply Integer)
rXDelB cName key ids = do
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rXDelB::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RXDelBT $ RXDelB key (toJSON ids) (Left A.Null) recTimestamp cName
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: Either KVDBReply Integer) -> pure reply
  else do
    res <- rXDelB'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rXDelB::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RXDelBT $ RXDelB key (toJSON ids) (either (Left . toJSON) (Right) res) recTimestamp cName
    whenM isArtV2RecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rXDelB::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RXDelBT $ RXDelB key (toJSON ids) (either (Left . toJSON) (Right) res) recTimestamp cName
    pure res
  where
    rXDelB' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply Integer)
    rXDelB' = do
      res <- L.runKVDB cName $ L.xdel key ids
      case res of
        Right _ -> pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis xdel" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

rXgroupCreate :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> ByteKey -> L.KVDBGroupName -> L.RecordID -> m (KVDBAnswer R.Status)
rXgroupCreate cName key groupName startId = do
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rXgroupCreate::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RXGroupCreateBT $ RXGroupCreateB key (toJSON groupName) (toJSON startId) (Left A.Null) recTimestamp cName
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: KVDBAnswer R.Status) -> pure reply
  else do
    res <- rXgroupCreate'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rXgroupCreate::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RXGroupCreateBT $ RXGroupCreateB key (toJSON groupName) (toJSON startId) ((either (Left . toJSON) (Right . toJSON) res)) recTimestamp cName
    whenM isArtV2RecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rXgroupCreate::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RXGroupCreateBT $ RXGroupCreateB key (toJSON groupName) (toJSON startId) ((either (Left . toJSON) (Right . toJSON) res)) recTimestamp cName
    pure res
  where
    rXgroupCreate' :: (HasCallStack, L.MonadFlow m) => m (KVDBAnswer R.Status)
    rXgroupCreate' = do
      res <- L.runKVDB cName $ L.xgroupCreate key groupName startId
      case res of
        Right _ -> pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis xgroupCreate" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

rXreadGroup :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> L.KVDBGroupName -> ByteKey -> [(L.KVDBStream, L.RecordID)] -> Maybe Integer -> Maybe Integer -> Bool -> m (KVDBAnswer (Maybe [L.KVDBStreamReadResponse]))
rXreadGroup cName groupName key streamsAndIds mBlock mCount noack = do
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rXreadGroup::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RXGroupCreateBT $ RXGroupCreateB key (toJSON groupName) (toJSON streamsAndIds) (Left A.Null) recTimestamp cName
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: (KVDBAnswer (Maybe [L.KVDBStreamReadResponse]))) -> pure reply
  else do
    res <- rXreadGroup'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rXreadGroup::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RXGroupCreateBT $ RXGroupCreateB key (toJSON groupName) (toJSON streamsAndIds) ((either (Left . toJSON) (Right . toJSON) res)) recTimestamp cName
    whenM isArtV2RecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rXreadGroup::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RXGroupCreateBT $ RXGroupCreateB key (toJSON groupName) (toJSON streamsAndIds) ((either (Left . toJSON) (Right . toJSON) res)) recTimestamp cName
    pure res
  where
    rXreadGroup' :: (HasCallStack, L.MonadFlow m) => m (KVDBAnswer (Maybe [L.KVDBStreamReadResponse]))
    rXreadGroup' = do
      res <- L.runKVDB cName $ L.xreadGroup groupName key streamsAndIds mBlock mCount noack
      case res of
        Right _ -> pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis xreadGroup" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

rXreadOpts :: (HasCallStack, L.MonadFlow m) =>
  RedisName -> [(ByteKey, L.KVDBStreamEntryIDInput)] -> R.XReadOpts -> m (Either KVDBReply (Maybe [R.XReadResponse]))
rXreadOpts cName stPair readOpts = do
  isArtV2ReplayEnabledWithSessId <- shouldReplayForRedis cName
  if isArtReplayEnabled || isArtV2ReplayEnabledWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rXreadOpts::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RXReadOptsBT $ RXReadOptsB (toJSON stPair) (toJSON readOpts) (Left A.Null) recTimestamp cName
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          let errorMessage = err
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage errorMessage)
        Right (reply :: (Either KVDBReply (Maybe [R.XReadResponse]))) -> pure reply
  else do
    res <- rXreadOpts'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rXreadOpts::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RXReadOptsBT $ RXReadOptsB (toJSON stPair) (toJSON readOpts) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName
    whenM isArtV2RecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rXreadOpts::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RXReadOptsBT $ RXReadOptsB (toJSON stPair) (toJSON readOpts) (either (Left . toJSON) (Right . toJSON) res) recTimestamp cName
    pure res
  where
    rXreadOpts' :: (HasCallStack, L.MonadFlow m) => m (Either KVDBReply (Maybe [R.XReadResponse]))
    rXreadOpts' = do
      res <- L.runKVDB cName $ L.xreadOpts stPair readOpts
      case res of
        Right _ -> pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis xreadOpts" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res

rMultiExecWithHash :: (HasCallStack, L.MonadFlow m, ToJSON a ,FromJSON a) => RedisName -> ByteString -> L.KVDBTx (R.Queued a) -> m (KVDBAnswer (TxResult a))
rMultiExecWithHash = withDefaultArtRecOptions id rMultiExecWithHashWithART

rMultiExecWithHashWithART :: (HasCallStack, L.MonadFlow m, ToJSON a ,FromJSON a, HasArtRecOptions) => RedisName -> ByteString -> L.KVDBTx (R.Queued a) -> m (KVDBAnswer (TxResult a))
rMultiExecWithHashWithART redisName key tx = do
  shouldReplayForKVDBWithSessId <- shouldReplayForKVDB redisName
  if isArtReplayEnabled || shouldReplayForKVDBWithSessId
    then do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rMultiExecWithHashWithART::getCurrentTime" getCurrentTime)
      let replayKVDBEntry = RunKVDBEntryT $ RMultiExecWithHashT $ RMultiExecWithHash key (Left A.Null) recTimestamp redisName
      msessionId <- L.getLoggerContext "x-request-id"
      resp <-  ER.callBrahmaReplayR replayKVDBEntry msessionId
      let maybeReply = A.eitherDecode resp
      case maybeReply of
        Left err -> do
          when Env.shouldLogCallStackART $ L.logErrorV @Text "CALLSTACK_ART_ERROR_REDIS" $ GHC.prettyCallStack $ GHC.callStack
          pure $ Left (ExceptionMessage err)
        Right (reply :: KVDBAnswer (TxResult a)) -> pure reply
  else do
    res <- multiExecWithHash'
    whenM isArtRecEnabled $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rMultiExecWithHashWithART::getCurrentTime" getCurrentTime)
      L.appendRecordingLocal $ RunKVDBEntryT $ RMultiExecWithHashT $ (RMultiExecWithHash key (either (Left . toJSON) (Right . toJSON) res) recTimestamp redisName)
    whenM isArtV2RecEnabledForKVDB $ do
      recTimestamp <- zonedTimeToLocalTime . utcToZonedTime utc <$> (ART.runIOWithART "EulerHS.Extra.Redis::rMultiExecWithHashWithART::getCurrentTime" getCurrentTime)
      producePayload REDIS . BS.toStrict . A.encode $ RunKVDBEntryT $ RMultiExecWithHashT $ (RMultiExecWithHash key (either (Left . toJSON) (Right . toJSON) res) recTimestamp redisName)
    pure res
  where 
    multiExecWithHash' = do
      res <- L.runKVDB redisName $ L.multiExecWithHash key tx
      case res of
        Right _ -> pure res
        Left err -> do
          L.logErrorWithCategoryV @Text "Redis multiExecWithHash" (err) $ ErrorL Nothing "REDIS_EXCEPTION" (show err)
          pure res