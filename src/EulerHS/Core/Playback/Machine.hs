{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{- |
Module      :  EulerHS.Core.Playback.Machine
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2021
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable

Automatic Regression Testing (ART) system.

You typically don't need to import this module.
-}

module EulerHS.Core.Playback.Machine
  (
    -- * Playback Machine
    record,
    withRunMode
  ) where

import           Control.Exception (throwIO)
import           Data.Vector as V ((!?))
import qualified Data.Vector as V
import           EulerHS.Prelude hiding (note)
import           EulerHS.Types


showInfo :: String -> String -> String
showInfo flowStep recordingEntry =
  "\n>>>>Recording entry: \n" ++ recordingEntry
  ++ "\n>>>>Flow step: \n" ++ flowStep

unexpectedRecordingEnd :: Text -> String -> PlaybackError
unexpectedRecordingEnd errFlowGuid flowStep
  = PlaybackError UnexpectedRecordingEnd
  ("\n>>>>Flow step: " ++ flowStep)
  errFlowGuid

unknownRRItem :: Text -> String -> String -> PlaybackError
unknownRRItem errFlowGuid flowStep recordingEntry
  = PlaybackError UnknownRRItem
  (showInfo flowStep recordingEntry)
  errFlowGuid

mockDecodingFailed :: Text -> String -> String -> PlaybackError
mockDecodingFailed errFlowGuid flowStep recordingEntry
  = PlaybackError MockDecodingFailed
  (showInfo flowStep recordingEntry)
  errFlowGuid

itemMismatch :: Text -> String -> String -> PlaybackError
itemMismatch errFlowGuid flowStep recordingEntry
  = PlaybackError ItemMismatch
  (showInfo flowStep recordingEntry)
  errFlowGuid

setReplayingError :: MonadIO m => PlayerRuntime -> PlaybackError -> m e
setReplayingError playerRt err = do
  let PlayerRuntime{rerror = ReplayErrors{errorMVar}} = playerRt

  void $ takeMVar errorMVar
  putMVar errorMVar $ Just err
  liftIO $ throwIO $ ReplayingException err

pushRecordingEntry
  :: MonadIO m
  => RecorderRuntime
  -> RecordingEntry
  -> m ()
pushRecordingEntry RecorderRuntime{recording} (RecordingEntry _ mode n p) = do
  let recMVar = recordingMVar recording
  entries <- takeMVar recMVar
  let idx = V.length entries
  let re  = RecordingEntry idx mode n p

  putMVar recMVar $ V.snoc entries re

popNextRecordingEntry :: MonadIO m => PlayerRuntime -> m (Maybe RecordingEntry)
popNextRecordingEntry PlayerRuntime{resRecording = ResultRecording{..}, ..} = do
  cur <- takeMVar stepMVar
  let mbItem = recording !? cur
  when (isJust mbItem) $ putMVar stepMVar (cur + 1)
  pure mbItem

popNextRRItem
  :: forall rrItem m
   . MonadIO m
  => Show rrItem
  => RRItem rrItem
  => PlayerRuntime
  -> m (Either PlaybackError (RecordingEntry, rrItem))
popNextRRItem playerRt@PlayerRuntime{..}  = do
  mbRecordingEntry <- popNextRecordingEntry playerRt
  let flowStep = getTag $ Proxy @rrItem
  pure $ do
    recordingEntry <- note (unexpectedRecordingEnd flowGUID flowStep) mbRecordingEntry
    let unknownErr = unknownRRItem flowGUID flowStep $ showRecEntry @rrItem recordingEntry -- show recordingEntry
    rrItem <- note unknownErr $ fromRecordingEntry recordingEntry
    pure (recordingEntry, rrItem)

popNextRRItemAndResult
  :: forall rrItem native m
   . MonadIO m
  => Show rrItem
  => MockedResult rrItem native
  => PlayerRuntime
  -> m (Either PlaybackError (RecordingEntry, rrItem, native))
popNextRRItemAndResult playerRt@PlayerRuntime{..}  = do
  let flowStep = getTag $ Proxy @rrItem
  eNextRRItem <- popNextRRItem playerRt
  pure $ do
    (recordingEntry, rrItem) <- eNextRRItem
    let mbNative = getMock rrItem
    nextResult <- note (mockDecodingFailed flowGUID flowStep (show recordingEntry)) mbNative
    pure (recordingEntry, rrItem, nextResult)

compareRRItems
  :: forall rrItem m native
   . RRItem rrItem
  => Show rrItem
  => MonadIO m
  => PlayerRuntime
  -> (RecordingEntry, rrItem, native)
  -> rrItem
  -> m ()
compareRRItems playerRt@PlayerRuntime{..} (recordingEntry, rrItem, _) flowRRItem = do
  when (rrItem /= flowRRItem) $ do
    let flowStep = show flowRRItem
    setReplayingError playerRt $ itemMismatch flowGUID flowStep (showRecEntry @rrItem recordingEntry) -- show recordingEntry)

getCurrentEntryReplayMode :: MonadIO m => PlayerRuntime -> m EntryReplayingMode
getCurrentEntryReplayMode PlayerRuntime{resRecording = ResultRecording{..}, ..} = do
  cur <- readMVar stepMVar
  pure $ fromMaybe Normal $ do
    (RecordingEntry _ mode _ _) <- recording !? cur
    pure mode

replayWithGlobalConfig
  :: forall rrItem native m
   . MonadIO m
  => Show rrItem
  => MockedResult rrItem native
  => PlayerRuntime
  -> m native
  -> (native -> rrItem)
  -> Either PlaybackError (RecordingEntry, rrItem, native)
  -> m native
replayWithGlobalConfig playerRt  ioAct mkRRItem eNextRRItemRes = do
  let tag = getTag $ Proxy @rrItem
  let config = checkForReplayConfig playerRt tag
  case config of
    GlobalNoVerify -> case eNextRRItemRes of
      Left err        -> setReplayingError playerRt err
      Right (_, _, r) -> pure r
    GlobalNormal    -> case eNextRRItemRes of
        Left err -> setReplayingError playerRt err
        Right stepInfo@(_, _, r) -> do
          compareRRItems playerRt stepInfo $ mkRRItem r
          pure r
    GlobalNoMocking -> ioAct
    GlobalSkip -> ioAct

checkForReplayConfig :: PlayerRuntime -> String -> GlobalReplayingMode
checkForReplayConfig  PlayerRuntime{..} tag | tag `elem` disableMocking = GlobalNoMocking
                                            | tag `elem` disableVerify  = GlobalNoVerify
                                            | otherwise                 = GlobalNormal

replay
  :: forall rrItem native m
   . MonadIO m
  => Show rrItem
  => MockedResult rrItem native
  => PlayerRuntime
  -> (native -> rrItem)
  -> m native
  -> m native
replay playerRt@PlayerRuntime{..} mkRRItem ioAct
  | getTag (Proxy @rrItem) `elem` skipEntries = ioAct
  | otherwise = do
      entryReplayMode <- getCurrentEntryReplayMode playerRt
      eNextRRItemRes <- popNextRRItemAndResult playerRt
      case entryReplayMode of
        Normal -> do
          replayWithGlobalConfig playerRt ioAct mkRRItem eNextRRItemRes
        NoVerify -> case eNextRRItemRes of
          Left err        -> setReplayingError playerRt err
          Right (_, _, r) -> pure r
        NoMock -> ioAct

record
  :: forall rrItem native m
   . MonadIO m
  => RRItem rrItem
  => RecorderRuntime
  -> (native -> rrItem)
  -> m native
  -> m native
record recorderRt@RecorderRuntime{..} mkRRItem ioAct = do
  native <- ioAct
  let tag = getTag $ Proxy @rrItem
  when (tag `notElem` disableEntries)
    $ pushRecordingEntry recorderRt $ toRecordingEntry (mkRRItem native) 0 Normal
  pure native


withRunMode
  :: MonadIO m
  => Show rrItem
  => MockedResult rrItem native
  => RunMode
  -> (native -> rrItem)
  -> m native
  -> m native
withRunMode RegularMode _ act = act
withRunMode (RecordingMode recorderRt) mkRRItem act =
  record recorderRt mkRRItem act
withRunMode (ReplayingMode playerRt) mkRRItem act =
  replay playerRt mkRRItem act
