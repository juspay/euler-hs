{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies, TypeOperators, ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-error=unused-imports #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}


module EulerHS.ART.IOReplay where

import Data.Aeson
import EulerHS.Prelude
import qualified Prelude as P
import qualified EulerHS.Framework.Language as L
import EulerHS.ART.V2.Types
import EulerHS.ART.Types
import qualified EulerHS.ART.V2.Utils as ARTUtils
import qualified EulerHS.ART.ReplayFunctions as ER
import qualified GHC.Stack as GHC
import qualified EulerHS.Extra.KafkaClient.Utils as KUtils
import qualified Data.ByteString.Lazy as Lazy
import qualified EulerHS.ART.EnvVars as Env
import qualified EulerHS.ART.V2.FlowUtils as FlowUtil
import qualified Data.Text as T

class RunIOWithART f where
  type Final f (m :: Type -> Type) :: Type
  runIOWithART :: L.MonadFlow m => Text -> f -> Final f m

instance (ArtRecordable r) => RunIOWithART (IO r) where
  type Final (IO r) m = m r
  runIOWithART uniqueTag f = do
    isArtV2ReplayEnabledWithSessId <- ARTUtils.shouldARTV2Replay
    if isArtV2ReplayEnabledWithSessId
      then
        if shouldReplayIO uniqueTag
          then do
            let input = []
            mOutput <- ioReplayHelper input uniqueTag
            case mOutput of
              Just output -> pure output
              Nothing -> L.runIO $ f
          else L.runIO $ f
      else do
        output <- L.runIO $ f
        shoudRecord <- shouldRecordIO uniqueTag
        when (shoudRecord) $ do
          let input = []
          void $ ioRecordHelper input output uniqueTag
        pure output

instance (ArtRecordable r, ArtRecordable a) => RunIOWithART (a -> IO r) where
  type Final (a -> IO r) m = a -> m r
  runIOWithART uniqueTag f = \x -> do
    isArtV2ReplayEnabledWithSessId <- ARTUtils.shouldARTV2Replay
    if isArtV2ReplayEnabledWithSessId
      then
        if shouldReplayIO uniqueTag
          then do
            let input = [toArtRecordingValue x]
            mOutput <- ioReplayHelper input uniqueTag
            case mOutput of
              Just output -> pure output
              Nothing -> L.runIO $ f x
          else L.runIO $ f x
      else do
        output <- L.runIO $ f x
        shoudRecord <- shouldRecordIO uniqueTag
        when (shoudRecord) $ do
          let input = [toArtRecordingValue x]
          void $ ioRecordHelper input output uniqueTag
        pure output

instance (ArtRecordable a, ArtRecordable b, ArtRecordable r) => RunIOWithART (a -> b -> IO r) where
  type Final (a -> b -> IO r) m = a -> b -> m r
  runIOWithART uniqueTag f = \x y -> do
    isArtV2ReplayEnabledWithSessId <- ARTUtils.shouldARTV2Replay
    if isArtV2ReplayEnabledWithSessId
      then
        if shouldReplayIO uniqueTag
          then do
            let input = [toArtRecordingValue x, toArtRecordingValue y]
            mOutput <- ioReplayHelper input uniqueTag
            case mOutput of
              Just output -> pure output
              Nothing -> L.runIO $ f x y
          else L.runIO $ f x y
      else do
        output <- L.runIO $ f x y
        shoudRecord <- shouldRecordIO uniqueTag
        when (shoudRecord) $ do
          let input = [toArtRecordingValue x, toArtRecordingValue y]
          void $ ioRecordHelper input output uniqueTag
        pure output

instance (ArtRecordable a, ArtRecordable b, ArtRecordable c, ArtRecordable r) => RunIOWithART (a -> b -> c -> IO r) where
  type Final (a -> b -> c -> IO r) m = a -> b -> c -> m r
  runIOWithART uniqueTag f = \x y z -> do
    isArtV2ReplayEnabledWithSessId <- ARTUtils.shouldARTV2Replay
    if isArtV2ReplayEnabledWithSessId
      then
        if shouldReplayIO uniqueTag
          then do
            let input = [toArtRecordingValue x, toArtRecordingValue y, toArtRecordingValue z]
            mOutput <- ioReplayHelper input uniqueTag
            case mOutput of
              Just output -> pure output
              Nothing -> L.runIO $ f x y z
          else L.runIO $ f x y z
      else do
        output <- L.runIO $ f x y z
        shoudRecord <- shouldRecordIO uniqueTag
        when (shoudRecord) $ do
          let input = [toArtRecordingValue x, toArtRecordingValue y, toArtRecordingValue z]
          void $ ioRecordHelper input output uniqueTag
        pure output

instance (ArtRecordable a, ArtRecordable b, ArtRecordable c, ArtRecordable d, ArtRecordable r) => RunIOWithART (a -> b -> c -> d -> IO r) where
  type Final (a -> b -> c -> d -> IO r) m = a -> b -> c -> d -> m r
  runIOWithART uniqueTag f = \w q r s -> do
    isArtV2ReplayEnabledWithSessId <- ARTUtils.shouldARTV2Replay
    if isArtV2ReplayEnabledWithSessId
      then
        if shouldReplayIO uniqueTag
          then do
            let input = [toArtRecordingValue w, toArtRecordingValue q, toArtRecordingValue r, toArtRecordingValue s]
            mOutput <- ioReplayHelper input uniqueTag
            case mOutput of
              Just output -> pure output
              Nothing -> L.runIO $ f w q r s
          else L.runIO $ f w q r s
      else do
        output <- L.runIO $ f w q r s
        shoudRecord <- shouldRecordIO uniqueTag
        when (shoudRecord) $ do
          let input = [toArtRecordingValue w, toArtRecordingValue q, toArtRecordingValue r, toArtRecordingValue s]
          void $ ioRecordHelper input output uniqueTag
        pure output

instance (ArtRecordable a, ArtRecordable b, ArtRecordable c, ArtRecordable d, ArtRecordable e, ArtRecordable r) => RunIOWithART (a -> b -> c -> d -> e -> IO r) where
  type Final (a -> b -> c -> d -> e -> IO r) m = a -> b -> c -> d -> e -> m r
  runIOWithART uniqueTag f = \w q r s t -> do
    isArtV2ReplayEnabledWithSessId <- ARTUtils.shouldARTV2Replay
    if isArtV2ReplayEnabledWithSessId
      then
        if shouldReplayIO uniqueTag
          then do
            let input = [toArtRecordingValue w, toArtRecordingValue q, toArtRecordingValue r, toArtRecordingValue s, toArtRecordingValue t]
            mOutput <- ioReplayHelper input uniqueTag
            case mOutput of
              Just output -> pure output
              Nothing -> L.runIO $ f w q r s t
          else L.runIO $ f w q r s t
      else do
        output <- L.runIO $ f w q r s t
        shoudRecord <- shouldRecordIO uniqueTag
        when (shoudRecord) $ do
          let input = [toArtRecordingValue w, toArtRecordingValue q, toArtRecordingValue r, toArtRecordingValue s, toArtRecordingValue t]
          void $ ioRecordHelper input output uniqueTag
        pure output

instance ( ArtRecordable a, ArtRecordable b, ArtRecordable c
         , ArtRecordable d, ArtRecordable e, ArtRecordable f
         , ArtRecordable r
         ) => RunIOWithART (a -> b -> c -> d -> e -> f -> IO r) where
  type Final (a -> b -> c -> d -> e -> f -> IO r) m = a -> b -> c -> d -> e -> f -> m r
  runIOWithART uniqueTag f = \w q r s t u -> do
    isArtV2ReplayEnabledWithSessId <- ARTUtils.shouldARTV2Replay
    if isArtV2ReplayEnabledWithSessId
      then
        if shouldReplayIO uniqueTag
          then do
            let input = [ toArtRecordingValue w, toArtRecordingValue q
                        , toArtRecordingValue r, toArtRecordingValue s
                        , toArtRecordingValue t, toArtRecordingValue u
                        ]
            mOutput <- ioReplayHelper input uniqueTag
            case mOutput of
              Just output -> pure output
              Nothing     -> L.runIO $ f w q r s t u
          else L.runIO $ f w q r s t u
      else do
        output <- L.runIO $ f w q r s t u
        shoudRecord <- shouldRecordIO uniqueTag
        when shoudRecord $ do
          let input = [ toArtRecordingValue w, toArtRecordingValue q
                      , toArtRecordingValue r, toArtRecordingValue s
                      , toArtRecordingValue t, toArtRecordingValue u
                      ]
          void $ ioRecordHelper input output uniqueTag
        pure output

instance ( ArtRecordable a, ArtRecordable b, ArtRecordable c
         , ArtRecordable d, ArtRecordable e, ArtRecordable f
         , ArtRecordable g, ArtRecordable r
         ) => RunIOWithART (a -> b -> c -> d -> e -> f -> g -> IO r) where
  type Final (a -> b -> c -> d -> e -> f -> g -> IO r) m = a -> b -> c -> d -> e -> f -> g -> m r
  runIOWithART uniqueTag f = \w q r s t u v -> do
    isArtV2ReplayEnabledWithSessId <- ARTUtils.shouldARTV2Replay
    if isArtV2ReplayEnabledWithSessId
      then
        if shouldReplayIO uniqueTag
          then do
            let input = [ toArtRecordingValue w, toArtRecordingValue q
                        , toArtRecordingValue r, toArtRecordingValue s
                        , toArtRecordingValue t, toArtRecordingValue u
                        , toArtRecordingValue v
                        ]
            mOutput <- ioReplayHelper input uniqueTag
            case mOutput of
              Just output -> pure output
              Nothing     -> L.runIO $ f w q r s t u v
          else L.runIO $ f w q r s t u v
      else do
        output <- L.runIO $ f w q r s t u v
        shoudRecord <- shouldRecordIO uniqueTag
        when shoudRecord $ do
          let input = [ toArtRecordingValue w, toArtRecordingValue q
                      , toArtRecordingValue r, toArtRecordingValue s
                      , toArtRecordingValue t, toArtRecordingValue u
                      , toArtRecordingValue v
                      ]
          void $ ioRecordHelper input output uniqueTag
        pure output

instance ( ArtRecordable a, ArtRecordable b, ArtRecordable c
         , ArtRecordable d, ArtRecordable e, ArtRecordable f
         , ArtRecordable g, ArtRecordable h, ArtRecordable r
         ) => RunIOWithART (a -> b -> c -> d -> e -> f -> g -> h -> IO r) where
  type Final (a -> b -> c -> d -> e -> f -> g -> h -> IO r) m = a -> b -> c -> d -> e -> f -> g -> h -> m r
  runIOWithART uniqueTag f = \w q r s t u v x -> do
    isArtV2ReplayEnabledWithSessId <- ARTUtils.shouldARTV2Replay
    if isArtV2ReplayEnabledWithSessId
      then
        if shouldReplayIO uniqueTag
          then do
            let input = [ toArtRecordingValue w, toArtRecordingValue q
                        , toArtRecordingValue r, toArtRecordingValue s
                        , toArtRecordingValue t, toArtRecordingValue u
                        , toArtRecordingValue v, toArtRecordingValue x
                        ]
            mOutput <- ioReplayHelper input uniqueTag
            case mOutput of
              Just output -> pure output
              Nothing     -> L.runIO $ f w q r s t u v x
          else L.runIO $ f w q r s t u v x
      else do
        output <- L.runIO $ f w q r s t u v x
        shoudRecord <- shouldRecordIO uniqueTag
        when shoudRecord $ do
          let input = [ toArtRecordingValue w, toArtRecordingValue q
                      , toArtRecordingValue r, toArtRecordingValue s
                      , toArtRecordingValue t, toArtRecordingValue u
                      , toArtRecordingValue v, toArtRecordingValue x
                      ]
          void $ ioRecordHelper input output uniqueTag
        pure output

instance ( ArtRecordable a, ArtRecordable b, ArtRecordable c
         , ArtRecordable d, ArtRecordable e, ArtRecordable f
         , ArtRecordable g, ArtRecordable h, ArtRecordable i
         , ArtRecordable r
         ) => RunIOWithART (a -> b -> c -> d -> e -> f -> g -> h -> i -> IO r) where
  type Final (a -> b -> c -> d -> e -> f -> g -> h -> i -> IO r) m = a -> b -> c -> d -> e -> f -> g -> h -> i -> m r
  runIOWithART uniqueTag f = \w q r s t u v x y -> do
    isArtV2ReplayEnabledWithSessId <- ARTUtils.shouldARTV2Replay
    if isArtV2ReplayEnabledWithSessId
      then
        if shouldReplayIO uniqueTag
          then do
            let input = [ toArtRecordingValue w, toArtRecordingValue q
                        , toArtRecordingValue r, toArtRecordingValue s
                        , toArtRecordingValue t, toArtRecordingValue u
                        , toArtRecordingValue v, toArtRecordingValue x
                        , toArtRecordingValue y
                        ]
            mOutput <- ioReplayHelper input uniqueTag
            case mOutput of
              Just output -> pure output
              Nothing     -> L.runIO $ f w q r s t u v x y
          else L.runIO $ f w q r s t u v x y
      else do
        output <- L.runIO $ f w q r s t u v x y
        shoudRecord <- shouldRecordIO uniqueTag
        when shoudRecord $ do
          let input = [ toArtRecordingValue w, toArtRecordingValue q
                      , toArtRecordingValue r, toArtRecordingValue s
                      , toArtRecordingValue t, toArtRecordingValue u
                      , toArtRecordingValue v, toArtRecordingValue x
                      , toArtRecordingValue y
                      ]
          void $ ioRecordHelper input output uniqueTag
        pure output

instance ( ArtRecordable a, ArtRecordable b, ArtRecordable c
         , ArtRecordable d, ArtRecordable e, ArtRecordable f
         , ArtRecordable g, ArtRecordable h, ArtRecordable i
         , ArtRecordable j, ArtRecordable r
         ) => RunIOWithART (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> IO r) where
  type Final (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> IO r) m = a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> m r
  runIOWithART uniqueTag f = \w q r s t u v x y z -> do
    isArtV2ReplayEnabledWithSessId <- ARTUtils.shouldARTV2Replay
    if isArtV2ReplayEnabledWithSessId
      then
        if shouldReplayIO uniqueTag
          then do
            let input = [ toArtRecordingValue w, toArtRecordingValue q
                        , toArtRecordingValue r, toArtRecordingValue s
                        , toArtRecordingValue t, toArtRecordingValue u
                        , toArtRecordingValue v, toArtRecordingValue x
                        , toArtRecordingValue y, toArtRecordingValue z
                        ]
            mOutput <- ioReplayHelper input uniqueTag
            case mOutput of
              Just output -> pure output
              Nothing     -> L.runIO $ f w q r s t u v x y z
          else L.runIO $ f w q r s t u v x y z
      else do
        output <- L.runIO $ f w q r s t u v x y z
        shoudRecord <- shouldRecordIO uniqueTag
        when shoudRecord $ do
          let input = [ toArtRecordingValue w, toArtRecordingValue q
                      , toArtRecordingValue r, toArtRecordingValue s
                      , toArtRecordingValue t, toArtRecordingValue u
                      , toArtRecordingValue v, toArtRecordingValue x
                      , toArtRecordingValue y, toArtRecordingValue z
                      ]
          void $ ioRecordHelper input output uniqueTag
        pure output

processRecordedValues :: [Either String Value] -> Either Text [Value]
processRecordedValues arr =
  let (errors, values) = partitionEithers arr
  in if null errors
     then Right values
     else Left $ T.pack (intercalate ", " errors)

shouldRecordIO :: L.MonadFlow m => Text -> m Bool
shouldRecordIO tag = do
  rt <- L.getFlowRuntime
  shouldRecordV2 <- L.runIO $ ARTUtils.shouldARTV2Record rt._optionsLocal
  let splitArray = T.splitOn "::" tag
      (funcName, moduleName) = if length splitArray >= 1 then (P.last splitArray, P.head splitArray) else (tag, tag)
  pure $ shouldRecordV2 &&
      T.unpack funcName `elem` Env.whitelistedRunIORecordingFunctions &&
      T.unpack moduleName `notElem` Env.blacklistedModuleForRunIORecording &&
      T.unpack tag `notElem` Env.blacklistedTagsForRunIORecording

shouldReplayIO :: Text -> Bool
shouldReplayIO tag =
  let splitArray = T.splitOn "::" tag
      (funcName, moduleName) = if length splitArray >= 1 then (P.last splitArray, P.head splitArray) else (tag, tag)
  in (T.unpack funcName `elem` Env.whitelistedRunIOReplayFunctions &&
      T.unpack moduleName `notElem` Env.blacklistedModuleForRunIOReplay &&
      T.unpack tag `notElem` Env.blacklistedTagsForRunIOReplay)

ioRecordHelper :: (ArtRecordable a, L.MonadFlow m) => [Either String Value] -> a -> Text -> m ()
ioRecordHelper recordedInput recordedResult uniqueTag = do
  case processRecordedValues recordedInput of
    Right recordedVals -> 
      case toArtRecordingValue recordedResult of
        Right val -> do
          let replayRunIOWithArt = RunIOWithArtEntryT $ RunIOWithArtEntry {
                              functionName = "runIOWithART",
                              input = recordedVals,
                              value = val,
                              tag = uniqueTag
                            }
          FlowUtil.producePayload KUtils.RUN_IO (Lazy.toStrict $ encode replayRunIOWithArt)
        Left err -> L.logErrorV @Text "CALLSTACK_ART_ERROR_RUN_IO : toArtRecordingValue failed for output : " err
    Left err -> L.logErrorV @Text "CALLSTACK_ART_ERROR_RUN_IO : toArtRecordingValue failed for input : " err

ioReplayHelper :: (ArtRecordable b, L.MonadFlow m) => [Either String Value] -> Text -> m (Maybe b)
ioReplayHelper recordedInput uniqueTag = do
  case processRecordedValues recordedInput of
    Right recordedVals -> do
      let replayRunIOWithArt = RunIOWithArtEntryT $ RunIOWithArtEntry {
          functionName = "runIOWithART",
          input = recordedVals,
          value = Null,
          tag = uniqueTag
        }
      mSessId <- L.getLoggerContext "x-request-id"
      resp <- ER.callBrahmaReplayWithRespCodeCheck replayRunIOWithArt mSessId
      let maybeReply = either (Left) eitherDecode resp
      case maybeReply of
        Left err -> do
          when Env.shouldLogCallStackART $ do
            L.logErrorV @Text "CALLSTACK_ART_ERROR_RUN_IO" $ GHC.prettyCallStack $ GHC.callStack
            L.logErrorV @Text "CALLTIME_ERROR" $ err
          pure Nothing
        Right reply -> case fromArtRecordedValue reply of
                          Left err -> do
                            L.logErrorV @Text "CALLSTACK_ART_ERROR_RUN_IO : fromArtRecordedValue " err
                            pure Nothing
                          Right val -> pure $ Just val
    Left err -> do
      when Env.shouldLogCallStackART $ do
        L.logErrorV @Text "CALLSTACK_ART_ERROR_RUN_IO" $ GHC.prettyCallStack $ GHC.callStack
        L.logErrorV @Text "toArtRecordingValue failed for input: " $ err
      pure Nothing
