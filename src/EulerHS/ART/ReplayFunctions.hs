{- |
Module      :  EulerHS.ART.ReplayFunctions
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE DerivingStrategies      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

module EulerHS.ART.ReplayFunctions
  ( callBrahmaReplayR,
    callBrahmaReplayDB
  ) where

import Data.Aeson as A
import EulerHS.Prelude hiding (Proxy)
import EulerHS.Types ()
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified EulerHS.Options ()
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client as NC
import EulerHS.ART.EnvVars
import           Data.Maybe (fromJust)
import Control.Exception

getApiHIT :: HTTP.Request -> IO (HTTP.Response LBS.ByteString)
getApiHIT httpReq = do
  let manager = NC.defaultManagerSettings
  mang <- HTTP.newManager manager
  HTTP.httpLbs httpReq mang

data ReplayDecodeException = ReplayDecodeException String deriving (Show, Exception)

data LOG = LOG {
  req :: ByteString
  , resp :: ByteString
}
  deriving (Generic,Show)
  deriving anyclass (ToJSON)

data MissingSessionID = MissingSessionID Text
  deriving (Generic,Show)
  deriving anyclass (ToJSON,Exception)

callBrahmaReplayR :: (ToJSON b) => b  -> Maybe Text-> IO LBS.ByteString
callBrahmaReplayR recEntry sessionId = do
  _ <- maybe (throw (MissingSessionID "Missing session-id" )) pure sessionId
  let url = mockServerURL <> "/mockRedis?guuid=" <> fromJust sessionId
  httpRequest <- HTTP.parseRequest $ Text.unpack url
  let finalRequest = httpRequest {HTTP.requestBody = HTTP.RequestBodyLBS (A.encode recEntry),
  HTTP.method = "POST"}
  eResponse <- getApiHIT finalRequest
  _ <- putStr $ A.encode $ LOG {req = LBS.toStrict $ A.encode recEntry,resp = LBS.toStrict $ HTTP.responseBody eResponse }
  pure $ HTTP.responseBody eResponse

callBrahmaReplayDB :: (ToJSON b) => b -> Maybe Text-> IO LBS.ByteString
callBrahmaReplayDB recEntry sessionId = do
  _ <- maybe (throw (MissingSessionID "Missing session-id" )) pure sessionId
  let url = mockServerURL <> "/mockDB?guuid=" <> fromJust sessionId
  httpRequest <- HTTP.parseRequest $ Text.unpack url
  let finalRequest = httpRequest {HTTP.requestBody = HTTP.RequestBodyLBS (A.encode recEntry),
  HTTP.method = "POST"}
  eResponse <- getApiHIT finalRequest
  _ <- putStr $ A.encode $ LOG {req = LBS.toStrict $A.encode recEntry,resp = LBS.toStrict $ HTTP.responseBody eResponse }
  pure $ HTTP.responseBody eResponse
