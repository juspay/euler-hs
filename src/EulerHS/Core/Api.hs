{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      :  EulerHS.Core.Api
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
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
import qualified Data.ByteString.Lazy as LBS(toStrict)
import qualified Data.Text as Text (unpack)
import qualified EulerHS.Core.Types.Logger as Log (LogMaskingConfig(..))
import           EulerHS.Core.Masking

newtype EulerClient a = EulerClient (Free SCF.ClientF a)
    deriving newtype (Functor, Applicative, Monad, RunClient)

data LogServantRequest
  = LogServantRequest
    { url :: SCF.BaseUrl
    , method :: HTTP.Method
    , body :: String
    , headers :: Seq HTTP.Header
    , queryString :: Seq HTTP.QueryItem
    }
    deriving (Show)

data LogServantResponse
  = LogServantResponse
    { statusCode :: HTTP.Status
    , headers :: Seq HTTP.Header
    , httpVersion :: HTTP.HttpVersion
    , body :: String
    }
    deriving (Show)

client :: SC.HasClient EulerClient api => Proxy api -> SC.Client EulerClient api
client api = SCC.clientIn api $ Proxy @EulerClient

interpretClientF :: (String -> IO ()) -> Maybe Log.LogMaskingConfig -> SCC.BaseUrl -> SCF.ClientF a -> SC.ClientM a
interpretClientF _   _   _ (SCF.Throw e)             = throwM e
interpretClientF log mbMaskConfig bUrl (SCF.RunRequest req next) = do
  liftIO $ logServantRequest log mbMaskConfig bUrl req
  res <- SCC.runRequestAcceptStatus Nothing req
  liftIO $ logServantResponse log mbMaskConfig res
  pure $ next res

runEulerClient :: (String -> IO()) -> Maybe Log.LogMaskingConfig -> SCC.BaseUrl -> EulerClient a -> SCIHC.ClientM a
runEulerClient log mbMaskConfig bUrl (EulerClient f) = foldFree (interpretClientF log mbMaskConfig bUrl) f

logServantRequest :: (String -> IO ()) -> Maybe Log.LogMaskingConfig -> SCC.BaseUrl -> SCC.Request -> IO ()
logServantRequest log mbMaskConfig url req = do
  log $ show $ LogServantRequest
    { url = url
    , method = method
    , body = body
    , headers = headers
    , queryString = queryString
    }

  where
    body = case SCC.requestBody req of
      Just (reqbody, _) ->
        case reqbody of
          SCC.RequestBodyBS s -> Text.unpack $ parseRequestResponseBody (shouldMaskKey mbMaskConfig) getMaskText (getContentTypeForServant . toList $ SCC.requestHeaders req) s
          SCC.RequestBodyLBS s -> Text.unpack $ parseRequestResponseBody (shouldMaskKey mbMaskConfig) getMaskText (getContentTypeForServant . toList $ SCC.requestHeaders req) $ LBS.toStrict s
          SCC.RequestBodySource sr -> show $ SCC.RequestBodySource sr
      Nothing -> "body = (empty)"

    method = SCC.requestMethod req
    headers = maskServantHeaders (shouldMaskKey mbMaskConfig) getMaskText $ SCC.requestHeaders req
    queryString = maskQueryStrings (shouldMaskKey mbMaskConfig) getMaskText $ SCC.requestQueryString req

    getMaskText :: Text
    getMaskText = maybe defaultMaskText (fromMaybe defaultMaskText . Log._maskText) mbMaskConfig

logServantResponse ::  (String -> IO ()) -> Maybe Log.LogMaskingConfig -> SCC.Response -> IO ()
logServantResponse log mbMaskConfig res =
  log $ show $ LogServantResponse
    { statusCode = status
    , headers = responseheaders
    , httpVersion = version
    , body = responseBody
    }
    where
      status = SCC.responseStatusCode res
      responseheaders = maskServantHeaders (shouldMaskKey mbMaskConfig) getMaskText $ SCC.responseHeaders res
      version = SCC.responseHttpVersion res
      responseBody =
          Text.unpack
        . parseRequestResponseBody (shouldMaskKey mbMaskConfig) getMaskText (getContentTypeForServant . toList $ SCC.responseHeaders res)
        . LBS.toStrict
        $ SCC.responseBody res

      getMaskText :: Text
      getMaskText = maybe defaultMaskText (fromMaybe defaultMaskText . Log._maskText) mbMaskConfig