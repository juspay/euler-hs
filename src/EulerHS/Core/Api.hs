{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      :  EulerHS.Core.Api
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2021
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable

This module contains implementation of the low-level HTTP client subsystem.

This is an internal module. Import EulerHS.Types instead.
-}


module EulerHS.Core.Api where

import           EulerHS.Prelude
import qualified Servant.Client as SC
import qualified Servant.Client.Core as SCC
import           Servant.Client.Core.RunClient (RunClient)
import qualified Servant.Client.Free as SCF
import qualified Servant.Client.Internal.HttpClient as SCIHC
import qualified Network.HTTP.Types as HTTP

data LogServantRequest = LogServantRequest
  { url :: SCF.BaseUrl
  , method :: HTTP.Method
  , body :: String
  , headers :: Seq HTTP.Header
  , queryString :: Seq HTTP.QueryItem
  }
  deriving (Show)

newtype EulerClient a = EulerClient (Free SCF.ClientF a)
    deriving newtype (Functor, Applicative, Monad, RunClient)

client :: SC.HasClient EulerClient api => Proxy api -> SC.Client EulerClient api
client api = SCC.clientIn api $ Proxy @EulerClient

-- Servant >=0.18.1 changes
-- interpretClientF :: (String -> IO ()) -> SCC.BaseUrl -> SCF.ClientF a -> SC.ClientM a
-- interpretClientF _   _    (SCF.Throw e)             = throwM e
-- interpretClientF logMsg bUrl (SCF.RunRequest req next) = do
--   liftIO $ logServantRequest logMsg bUrl req
--   res <- SCC.runRequestAcceptStatus Nothing req
--   liftIO . logMsg $ show res
--   pure $ next res

interpretClientF :: (String -> IO ()) -> SCC.BaseUrl -> SCF.ClientF a -> SC.ClientM a
interpretClientF _   _    (SCF.Throw e)             = throwM e
interpretClientF log bUrl (SCF.RunRequest req next) = do
  case SCC.requestBody req of
    Just (body, _) -> liftIO . log $ show body
    Nothing -> pure ()
  liftIO . log $ show (SCIHC.requestToClientRequest bUrl req)
  res <- SCC.runRequest req
  liftIO . log $ show (res)
  pure $ next res

logServantRequest :: (String -> IO ()) -> SCC.BaseUrl -> SCC.Request -> IO ()
logServantRequest log url req = do
  log $ show $ LogServantRequest
    { url = url
    , method = method
    , body = body
    , headers = headers
    , queryString = queryString
    }
  where
    body = case SCC.requestBody req of
      Just (b, _) -> show b
      Nothing -> "body = (empty)"
    method = SCC.requestMethod req
    headers = SCC.requestHeaders req
    queryString = SCC.requestQueryString req
  -- liftIO . log $ show (SCIHC.requestToClientRequest bUrl req)

runEulerClient :: (String -> IO()) -> SCC.BaseUrl -> EulerClient a -> SCIHC.ClientM a
runEulerClient log bUrl (EulerClient f) = foldFree (interpretClientF log bUrl) f
