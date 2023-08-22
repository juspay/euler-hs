{- |
Module      :  EulerHS.ART.EnvVars
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable

This module contains functions to read environment variables
eg : RECORD_FORK_FLOW, IS_REPLAY_ENABLED, RUNNING_MODE, MOCK_SERVER_URL
to be used in ART
-}

{-# LANGUAGE ScopedTypeVariables #-}

module EulerHS.ART.EnvVars where

import qualified Juspay.Extra.Config as Conf
import           EulerHS.Prelude

getDirPath :: Text
getDirPath = fromMaybe "/" $ Conf.lookupEnvT "RECORDER_RECORDINGS_DIR"

shouldRecordForkFLow :: Bool
shouldRecordForkFLow = Just True == (readMaybe =<< Conf.lookupEnvT "RECORD_FORK_FLOW")

isArtReplayEnabled :: Bool
isArtReplayEnabled = Just True == (readMaybe =<< Conf.lookupEnvT "IS_REPLAY_ENABLED")

isArtRecEnabled :: Bool
isArtRecEnabled = Just True == (readMaybe =<< Conf.lookupEnvT "RUNNING_MODE")

mockServerURL :: Text
mockServerURL = fromMaybe "http://localhost:7777" $ readMaybe =<< Conf.lookupEnvT "MOCK_SERVER_URL"
