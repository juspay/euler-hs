{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

module EulerHS.ART.V2.Utils where

import qualified Data.Map as Map
import           EulerHS.ART.EnvVars as Env
import           EulerHS.ART.V2.Types
import           EulerHS.ART.V2.Option 
import qualified EulerHS.Framework.Language as L
import           EulerHS.Options (OptionKey, mkOptionKey)
import           EulerHS.Prelude
import           Sequelize (ModelMeta(..), TableType(..), modelTableType)
import           Unsafe.Coerce (unsafeCoerce)
import qualified Data.List as List
import qualified Data.Text as T

isArtV2RecEnabled :: L.MonadFlow m => m Bool
isArtV2RecEnabled = do
  mShouldRecord <- L.getOptionLocal ArtRecordingEnabled
  pure $ fromMaybe False mShouldRecord

shouldARTV2Record :: MVar (Map OptionKey Any) -> IO Bool
shouldARTV2Record optionLocalMVar = do
    m <- readMVar optionLocalMVar
    maybeValue <- pure $ do
      valAny <- Map.lookup (mkOptionKey ArtRecordingEnabled) m
      pure $ unsafeCoerce valAny
    return $ fromMaybe False maybeValue

getArtReplayDBPrefix :: L.MonadFlow m => m Text
getArtReplayDBPrefix = do
  mbArtReplayPrefix <- L.getOptionLocal ArtReplayDBPrefix
  pure $ maybe "" (<> "_") mbArtReplayPrefix

getArtReplayRedisPrefix :: L.MonadFlow m => m Text
getArtReplayRedisPrefix = do
  mbArtReplayPrefix <- L.getOptionLocal ArtReplayRedisPrefix
  pure $ maybe "" (<> ":::") mbArtReplayPrefix

shouldARTV2Replay :: L.MonadFlow m => m Bool
shouldARTV2Replay = do
  mbSessionId <- L.getLoggerContext "x-request-id"
  pure $ Env.isArtV2ReplayEnabled && isJust mbSessionId

shouldRecordDBTable :: forall table. (ModelMeta table) => Bool
shouldRecordDBTable = case modelTableType @table of
    Just TRACKER -> Env.shouldRecordTrackerTables
    _ -> True

shouldReplayDBTable :: forall table. (ModelMeta table) => Bool
shouldReplayDBTable = case modelTableType @table of
  Just CONFIG        -> Env.shouldReplayConfigTables
  Just COMMON_CONFIG -> Env.shouldReplayCommonConfigTables
  _                  -> False

isArtV2RecEnabledForTable :: forall table m. (L.MonadFlow m, ModelMeta table) => m Bool
isArtV2RecEnabledForTable = bool (pure False) isArtV2RecEnabled $ shouldRecordDBTable @table

isArtV2ReplayEnabledForTable :: forall table. (ModelMeta table) => Bool
isArtV2ReplayEnabledForTable = Env.isArtV2ReplayEnabled && shouldReplayDBTable @table

withDefaultArtRecOptions :: (ArtRecOptions -> ArtRecOptions) -> (HasArtRecOptions => m a) -> m a
withDefaultArtRecOptions modifier flow = 
  let ?artRecOptions = modifier defaultArtRecOptions
  in flow

withArtRecOptions :: HasArtRecOptions => (ArtRecOptions -> ArtRecOptions) -> (HasArtRecOptions => m a) -> m a
withArtRecOptions modifier flow = 
  let currArtRecOpts = ?artRecOptions
  in let ?artRecOptions = modifier currArtRecOpts in flow

isArtV2RecEnabledForKVDB :: (HasArtRecOptions, L.MonadFlow m) => m Bool
isArtV2RecEnabledForKVDB = 
  let artRecOptions = ?artRecOptions
  in bool (pure False) isArtV2RecEnabled $ shouldRecordForART artRecOptions

isArtV2ReplayEnabledForKVDB :: (HasArtRecOptions) => Bool
isArtV2ReplayEnabledForKVDB = 
  let artRecOptions = ?artRecOptions
  in Env.isArtV2ReplayEnabled && shouldReplayForART artRecOptions

isKVRedis :: Text -> Bool
isKVRedis redisName = redisName `elem` ["KVRedis", "M1KVRedis", "KVRedis2"]

shouldMockAPICall :: Maybe Text -> Bool
shouldMockAPICall = maybe True (\hostname -> not $ List.any (\x -> T.isInfixOf x hostname) httpReplayAPIList)

shouldRecordGlobalOption :: forall v. (ArtRecordable v) => OptionKey -> Bool
shouldRecordGlobalOption (optKeyTy, _) = isRecordable @v &&
  if shouldBlacklistTagsForRecordingGlobalOptions
    then show optKeyTy `notElem` (defaultBlacklistedTagsForRecordingGlobalOptions <> recordGlobalOptionsTagsList)
    else show optKeyTy `elem` recordGlobalOptionsTagsList

shouldReplayGlobalOption :: forall v. (ArtRecordable v) => OptionKey -> Bool
shouldReplayGlobalOption (optKeyTy, _) = isRecordable @v &&
  if shouldBlacklistTagsForRecordingGlobalOptions
    then show optKeyTy `notElem` (defaultBlacklistedTagsForRecordingGlobalOptions <> recordGlobalOptionsTagsList)
    else show optKeyTy `elem` recordGlobalOptionsTagsList

defaultBlacklistedTagsForRecordingGlobalOptions :: [String]
defaultBlacklistedTagsForRecordingGlobalOptions = ["RecordId", "LooperStarted"]

shouldReplayForRedis :: L.MonadFlow m => Text -> m Bool
shouldReplayForRedis cName = do
  mbSessionId <- L.getLoggerContext "x-request-id"
  pure $ isArtV2ReplayEnabled && (not (isKVRedis cName) && Env.shouldReplayNonKVRedis) && isJust mbSessionId

shouldReplayForKVDB :: (HasArtRecOptions, L.MonadFlow m) => Text -> m Bool
shouldReplayForKVDB redisName = do
  mbSessionId <- L.getLoggerContext "x-request-id"
  pure $ isArtV2ReplayEnabledForKVDB && (not (isKVRedis redisName) && Env.shouldReplayNonKVRedis) && isJust mbSessionId

shouldReplayForDB :: forall table m. (ModelMeta table, L.MonadFlow m) => m Bool
shouldReplayForDB = do
  mbSessionId <- L.getLoggerContext "x-request-id"
  pure $ isArtV2ReplayEnabledForTable @table && isJust mbSessionId