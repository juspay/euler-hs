{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}

module EulerHS.HttpAPI
    (
      -- * HTTP manager builder stuff
      HTTPClientSettings
    , buildSettings
    , withProxy
    , withMbProxy
    , withClientTls
    , withMbClientTls
    , withCustomCA
      -- * X509 utilities
    , makeCertificateStoreFromMemory
      -- * Common types and convenience methods
    , HTTPRequest(..)
    , HTTPRequestMasked
    , HTTPResponse(..)
    , HTTPMethod(..)
    , HTTPCert(..)
    , HTTPIOException(HTTPIOException)
    , defaultTimeout
    , extractBody
    , httpGet
    , httpPut
    , httpPost
    , httpDelete
    , httpHead
    , defaultRequest
    , withHeader
    , withOptionalHeader
    , withBody
    , withFormBody
    , withJSONBody
    , withTimeout
    , withRedirects
    , withNoCheckLeafV3
    , maskHTTPRequest
    , maskHTTPResponse
    , mkHttpApiCallLogEntry
    ) where

import qualified EulerHS.BinaryString as T
import qualified EulerHS.Logger.Types as Log
import           EulerHS.Masking (defaultMaskText, getContentTypeForHTTP,
                                  maskHTTPHeaders, parseRequestResponseBody,
                                  shouldMaskKey)
import           EulerHS.Prelude hiding (ord)
import           Juspay.Extra.Text (formUrlEncode)

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import           Data.Default
import qualified Data.Map as Map
import           Data.Monoid (All (..))
import           Data.PEM (pemContent, pemParseBS)
import           Data.String.Conversions (convertString)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.X509 (Certificate (certSerial), HashALG (..),
                            decodeSignedCertificate, encodeSignedObject,
                            getCertificate)
import           Data.X509.CertificateStore (CertificateStore, listCertificates,
                                             makeCertificateStore)
import           Data.X509.Validation (checkLeafV3, defaultChecks, defaultHooks,
                                       validate)
import           Generics.Deriving.Monoid (mappenddefault, memptydefault)
import qualified Network.Connection as Conn
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS
import           System.IO.Unsafe (unsafePerformIO)
import           System.X509 (getSystemCertificateStore)

newtype CertificateStore'
  = CertificateStore'
    { getCertificateStore :: CertificateStore
    }
  deriving newtype (Semigroup, Monoid)

instance Eq CertificateStore' where
  (==) a b = a' == b' where
    a' = sortedSignedObjects a
    b' = sortedSignedObjects b

sortedSignedObjects :: CertificateStore' -> [ByteString]
sortedSignedObjects = sort . fmap encodeSignedObject . listCertificates . getCertificateStore

instance Ord CertificateStore' where
  compare a b = a' `compare` b' where
    a' = sortedSignedObjects a
    b' = sortedSignedObjects b

instance Hashable CertificateStore' where
  hashWithSalt salt = hashWithSalt salt .
    fmap (certSerial . getCertificate) . listCertificates . getCertificateStore


-- |
data HTTPClientSettings = HTTPClientSettings
  { httpClientSettingsProxy             :: Last ProxySettings
  , httpClientSettingsClientCertificate :: Last HTTPCert
  , httpClientSettingsCustomStore       :: CertificateStore'
  , httpClientSettingsCheckLeafV3       :: All
  }
  deriving stock (Eq, Ord, Generic)
  -- use DeriveVia?
  -- see https://hackage.haskell.org/package/generic-deriving-1.14/docs/Generics-Deriving-Default.html

instance Hashable HTTPClientSettings where
  hashWithSalt salt settings = hashWithSalt salt $
    ( getLast $ httpClientSettingsProxy             settings
    , getLast $ httpClientSettingsClientCertificate settings
    , httpClientSettingsCustomStore settings
    )


-- instance Hashable a => Hashable (Last a) where
--   hashWithSalt salt = hashWithSalt salt . getLast

data ProxySettings
  = InsecureProxy
  { proxySettingsHost :: Text
  , proxySettingsPort :: Int
  }
  deriving stock (Eq, Ord, Generic)

instance Hashable ProxySettings


instance Semigroup (HTTPClientSettings) where
  (<>)  = mappenddefault

instance Monoid (HTTPClientSettings) where
  mempty  = memptydefault { httpClientSettingsCheckLeafV3 = All True }

-- | The simplest settings builder
buildSettings :: HTTPClientSettings -> HTTP.ManagerSettings
buildSettings HTTPClientSettings{..} =
    applyProxySettings $ baseSettings
  where
    applyProxySettings = HTTP.managerSetProxy proxyOverride

    proxyOverride = case getLast httpClientSettingsProxy of
      Just (InsecureProxy host port) -> HTTP.useProxy $ HTTP.Proxy (Text.encodeUtf8 host) port
      Nothing -> HTTP.noProxy

    baseSettings = case getLast httpClientSettingsClientCertificate of
      Just HTTPCert{..} ->
        case TLS.credentialLoadX509ChainFromMemory getCert getCertChain getCertKey of
          Right creds ->
            let hooks = def { TLS.onCertificateRequest =
                                \_ -> return $ Just creds
                            }
                clientParams = mkClientParams hooks
            in mkSettings clientParams
          -- TODO?
          Left err -> error $ "cannot load client certificate data: " <> Text.pack err
      Nothing ->
        let clientParams = mkClientParams def
        in mkSettings clientParams

    mkClientParams hooks =
      let defs = TLS.defaultParamsClient empty ""
      in
        defs
          { TLS.clientShared = (TLS.clientShared defs)
              { TLS.sharedCAStore = sysStore <> getCertificateStore httpClientSettingsCustomStore }
          , TLS.clientSupported = (TLS.clientSupported defs)
              { TLS.supportedCiphers = TLS.ciphersuite_default }
          , TLS.clientHooks = hooks
              { TLS.onServerCertificate =
                  validate HashSHA256 defaultHooks $ defaultChecks
                    { checkLeafV3 = getAll httpClientSettingsCheckLeafV3 }
              }
          }

    mkSettings clientParams = let
        tlsSettings = Conn.TLSSettings clientParams
      in
        TLS.mkManagerSettings tlsSettings Nothing

    sysStore = memorizedSysStore

{-# NOINLINE memorizedSysStore #-}
memorizedSysStore :: CertificateStore
memorizedSysStore = unsafePerformIO getSystemCertificateStore

type SimpleProxySettings = (Text, Int)

-- | Add unconditional proxying (for both http/https, regardless
-- HTTP.Client's request proxy settings).
withProxy :: SimpleProxySettings -> HTTPClientSettings
withProxy (host, port) =
    mempty {httpClientSettingsProxy = Last $ proxySettings}
  where
    proxySettings = Just $ InsecureProxy host port

-- | The same as 'withProxy' but to use with optionally existsting settings.
withMbProxy :: Maybe SimpleProxySettings -> HTTPClientSettings
withMbProxy (Just s) = withProxy s
withMbProxy Nothing  = mempty

-- | Adds a client certificate to do client's TLS authentication
withClientTls :: HTTPCert -> HTTPClientSettings
withClientTls httpCert =
    mempty {httpClientSettingsClientCertificate = Last $ Just $ httpCert}

withMbClientTls :: Maybe HTTPCert -> HTTPClientSettings
withMbClientTls (Just cert) = withClientTls cert
withMbClientTls Nothing     = mempty

-- | Adds an additional store with trusted CA certificates. There is no Maybe version
-- since 'CertificateStore` is a monoid.
withCustomCA :: CertificateStore -> HTTPClientSettings
withCustomCA store = mempty {httpClientSettingsCustomStore = CertificateStore' store}

-- | Make a store from in-memory certs
makeCertificateStoreFromMemory :: [ByteString] -> Either String CertificateStore
makeCertificateStoreFromMemory =
  fmap makeCertificateStore . mapM decodeSignedCertificate
       <=< fmap (fmap pemContent . join) . mapM pemParseBS

-- | Turns off the check that all certs are X509 v3 ones
{-# WARNING withNoCheckLeafV3 "Don't use in production code, use X509 v3 certs instead." #-}
withNoCheckLeafV3 :: HTTPClientSettings
withNoCheckLeafV3 = mempty { httpClientSettingsCheckLeafV3 = All False }

data HTTPRequest
  = HTTPRequest
    { getRequestMethod    :: HTTPMethod
    , getRequestHeaders   :: Map.Map HeaderName HeaderValue
    , getRequestBody      :: Maybe T.LBinaryString
    , getRequestURL       :: Text
    , getRequestTimeout   :: Maybe Int                        -- ^ timeout, in microseconds
    , getRequestRedirects :: Maybe Int
    }
    deriving stock (Eq, Ord, Generic, Show)
    deriving anyclass (ToJSON)

data HTTPRequestMasked
  = HTTPRequestMasked
    { getRequestMethod    :: HTTPMethod
    , getRequestHeaders   :: Map.Map HeaderName HeaderValue
    , getRequestBody      :: Maybe A.Value
    , getRequestURL       :: Text
    , getRequestTimeout   :: Maybe Int                        -- ^ timeout, in microseconds
    , getRequestRedirects :: Maybe Int
    }
    deriving stock (Eq, Generic)
    deriving anyclass (ToJSON)

data HTTPResponse
  = HTTPResponse
    { getResponseBody    :: T.LBinaryString
    , getResponseCode    :: Int
    , getResponseHeaders :: Map.Map HeaderName HeaderValue
    , getResponseStatus  :: Text
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON)

data HTTPResponseMasked
  = HTTPResponseMasked
    { getResponseBody    :: A.Value
    , getResponseCode    :: Int
    , getResponseHeaders :: Map.Map HeaderName HeaderValue
    , getResponseStatus  :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON)

data HTTPCert
  = HTTPCert
    { getCert      :: B.ByteString
    , getCertChain :: [B.ByteString]
    , getCertHost  :: String        -- ^ Defines the name of the server, along with an extra
                                     -- ^ service identification blob (not supported in Euler ATM).
                                     -- ^ This is important that the hostname part is properly
                                     -- ^ filled for security reason, as it allows to properly
                                     -- ^ as it allow to properly associate the remote side
                                     -- ^ with the given certificate during a handshake.
    , getCertKey   :: B.ByteString
    }
    deriving stock (Eq, Ord, Generic)

instance Hashable HTTPCert

data HTTPMethod
  = Get
  | Put
  | Post
  | Delete
  | Head
  | Trace
  | Connect
  | Options
  | Patch
  deriving (Eq, Ord, Show, Generic, ToJSON)

type HeaderName = Text
type HeaderValue = Text

data HttpApiCallLogEntry = HttpApiCallLogEntry
  { url         :: Text
  , method      :: Text
  , req_headers :: A.Value
  , req_body    :: Maybe A.Value
  , res_code    :: Int
  , res_body    :: A.Value
  , res_headers :: A.Value
  , latency     :: Integer
  }
  deriving stock (Show,Generic)
  deriving anyclass A.ToJSON

mkHttpApiCallLogEntry :: Integer -> HTTPRequestMasked -> HTTPResponseMasked -> HttpApiCallLogEntry
mkHttpApiCallLogEntry lat req res = HttpApiCallLogEntry
  { url = req.getRequestURL
  , method = show $ req.getRequestMethod
  , req_headers = A.toJSON $ req.getRequestHeaders
  , req_body = req.getRequestBody
  , res_code = res.getResponseCode
  , res_body = res.getResponseBody
  , res_headers = A.toJSON $ res.getResponseHeaders
  , latency = lat
  }


-- | Used when some IO (or other) exception ocurred during a request
data HTTPIOException
  = HTTPIOException
    { errorMessage :: Text
    , request      :: HTTPRequestMasked
    }
  deriving (Eq, Generic, ToJSON)

-- Not Used anywhere
-- getMaybeUtf8 :: T.LBinaryString -> Maybe LazyText.Text
-- getMaybeUtf8 body = case LazyText.decodeUtf8' (T.getLBinaryString body) of
--   -- return request body as base64-encoded text (not valid UTF-8)
--   Left e -> Nothing
--   -- return request body as UTF-8 decoded text
--   Right utf8Body -> Just utf8Body

--------------------------------------------------------------------------
-- Convenience functions
--------------------------------------------------------------------------

-- | HTTP GET request.
--
-- > httpGet "https://google.com"
httpGet :: Text -> HTTPRequest
httpGet = defaultRequest Get

httpPut :: Text -> HTTPRequest
httpPut = defaultRequest Put

httpPost :: Text -> HTTPRequest
httpPost = defaultRequest Post

httpDelete :: Text -> HTTPRequest
httpDelete = defaultRequest Delete

httpHead :: Text -> HTTPRequest
httpHead = defaultRequest Head

defaultRequest :: HTTPMethod -> Text -> HTTPRequest
defaultRequest method url
  = HTTPRequest
    { getRequestMethod = method
    , getRequestHeaders = Map.empty
    , getRequestBody = Nothing
    , getRequestURL = url
    , getRequestTimeout = Just defaultTimeout
    , getRequestRedirects = Just 10
    }

defaultTimeout :: Int
defaultTimeout = 9000000

-- | Add a header to an HTTPRequest
--
--  > httpGet "https://google.com"
--  >   & withHeader "Content-Type" "application/json"
--
withHeader :: HeaderName -> HeaderValue -> HTTPRequest -> HTTPRequest
withHeader headerName headerValue request@HTTPRequest {getRequestHeaders} =
  let headers = Map.insert headerName headerValue getRequestHeaders
  in  request { getRequestHeaders = headers }

withOptionalHeader :: HeaderName -> Maybe HeaderValue -> HTTPRequest -> HTTPRequest
withOptionalHeader headerName (Just headerValue) = withHeader headerName headerValue
withOptionalHeader _ Nothing = id

-- | Sets timeout, in microseconds
withTimeout :: Int -> HTTPRequest -> HTTPRequest
withTimeout timeout request =
  request {getRequestTimeout = Just timeout}

-- | Sets the maximum number of redirects
withRedirects :: Int -> HTTPRequest -> HTTPRequest
withRedirects redirects request =
  request {getRequestRedirects = Just redirects}

{-# DEPRECATED withBody "use withFormBody and withJSONBody instead" #-}
-- TODO: Rename to `withFormData` or some such?
withBody :: [(Text, Text)] -> HTTPRequest -> HTTPRequest
withBody pairs request = request {getRequestBody = Just body}
  where
    body = T.LBinaryString $ formUrlEncode pairs

-- | Sets an http form-based body
withFormBody :: [(Text, Text)] -> HTTPRequest -> HTTPRequest
withFormBody = withBody

-- | Sets a JSON body
withJSONBody :: ToJSON b => b -> HTTPRequest -> HTTPRequest
withJSONBody body req@HTTPRequest{getRequestHeaders} =
  let json = A.encode body
      headers = Map.insert "content-type" "application/json" getRequestHeaders
  in req
    { getRequestBody = Just $ convertString json
    , getRequestHeaders = headers
    }

extractBody :: HTTPResponse -> Text
extractBody HTTPResponse{getResponseBody} = decodeUtf8With lenientDecode $ convertString getResponseBody

maskHTTPRequest :: Maybe Log.LogMaskingConfig -> HTTPRequest -> HTTPRequestMasked
maskHTTPRequest mbMaskConfig request = HTTPRequestMasked
    { getRequestHeaders = maskHTTPHeaders (shouldMaskKey mbMaskConfig) getMaskText requestHeaders
    , getRequestBody = maskedRequestBody
    , getRequestMethod = request.getRequestMethod
    , getRequestURL = request.getRequestURL
    , getRequestTimeout = request.getRequestTimeout
    , getRequestRedirects = request.getRequestRedirects
    }
  where
    requestHeaders = request.getRequestHeaders

    requestBody = request.getRequestBody

    getMaskText = maybe defaultMaskText (fromMaybe defaultMaskText . Log._maskText) mbMaskConfig

    maskedRequestBody =
          parseRequestResponseBody (shouldMaskKey mbMaskConfig) getMaskText (getContentTypeForHTTP requestHeaders)
        . LB.toStrict
        . T.getLBinaryString <$> requestBody

maskHTTPResponse :: Maybe Log.LogMaskingConfig -> HTTPResponse -> HTTPResponseMasked
maskHTTPResponse mbMaskConfig response = HTTPResponseMasked
  { getResponseHeaders = maskHTTPHeaders (shouldMaskKey mbMaskConfig) getMaskText responseHeaders
  , getResponseBody = maskedResponseBody
  , getResponseCode = response.getResponseCode
  , getResponseStatus = response.getResponseStatus
  }
  where
    responseHeaders = response.getResponseHeaders

    responseBody = response.getResponseBody

    getMaskText = maybe defaultMaskText (fromMaybe defaultMaskText . Log._maskText) mbMaskConfig

    maskedResponseBody =
      parseRequestResponseBody (shouldMaskKey mbMaskConfig) getMaskText (getContentTypeForHTTP responseHeaders)
        . LB.toStrict
        $ T.getLBinaryString responseBody
