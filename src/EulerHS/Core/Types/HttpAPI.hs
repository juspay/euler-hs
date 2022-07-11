{-# LANGUAGE DeriveAnyClass #-}

module EulerHS.Core.Types.HttpAPI
    (
    -- * Core Logger
    -- ** Types
      HTTPRequest(..)
    , HTTPResponse(..)
    , HTTPMethod(..)
    , HTTPCert(..)
    , HTTPRequestResponse(HTTPRequestResponse)
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
    , withTimeout
    , withRedirects
    , maskHTTPRequest
    , maskHTTPResponse
    ) where

import           EulerHS.Prelude                 hiding ((.=), ord)
import qualified EulerHS.Core.Types.BinaryString as T

import qualified Data.ByteString                 as B
import qualified Data.ByteString.Lazy            as LB
import           Data.ByteString.Lazy.Builder    (Builder)
import qualified Data.ByteString.Lazy.Builder    as Builder
import qualified Data.Char                       as Char
import qualified Data.Map                        as Map
import           Data.String.Conversions         (convertString)
import qualified Data.Text.Encoding              as Text
import           EulerHS.Core.Masking
import qualified EulerHS.Core.Types.Logger as Log (LogMaskingConfig(..))


data HTTPRequest
  = HTTPRequest
    { getRequestMethod    :: HTTPMethod
    , getRequestHeaders   :: Map.Map HeaderName HeaderValue
    , getRequestBody      :: Maybe T.LBinaryString
    , getRequestURL       :: Text
    , getRequestTimeout   :: Maybe Int                        -- ^ timeout, in microseconds
    , getRequestRedirects :: Maybe Int
    }
    deriving (Eq, Ord, Generic, ToJSON)

data HTTPResponse
  = HTTPResponse
    { getResponseBody    :: T.LBinaryString
    , getResponseCode    :: Int
    , getResponseHeaders :: Map.Map HeaderName HeaderValue
    , getResponseStatus  :: Text
    }
    deriving (Eq, Ord, Generic, ToJSON)

data HTTPCert
  = HTTPCert
    { getCert      :: B.ByteString
    , getCertChain :: [B.ByteString]
    , getCertHost  :: String
    , getCertKey   :: B.ByteString
    }

data HTTPMethod
  = Get
  | Put
  | Post
  | Delete
  | Head
  deriving (Eq, Ord, Generic, ToJSON)

type HeaderName = Text
type HeaderValue = Text

data HTTPRequestResponse
  = HTTPRequestResponse
    { request  :: HTTPRequest
    , response :: HTTPResponse
    }
  deriving (Eq, Ord, Generic, ToJSON)

-- | Used when some IO (or other) exception ocurred during a request
data HTTPIOException
  = HTTPIOException
    { errorMessage :: Text
    , request      :: HTTPRequest
    }
  deriving (Eq, Ord, Generic, ToJSON)


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
withHeader headerName headerValue (request@HTTPRequest {getRequestHeaders}) =
  let headers = Map.insert headerName headerValue getRequestHeaders
  in  request { getRequestHeaders = headers }

withOptionalHeader :: HeaderName -> Maybe HeaderValue -> HTTPRequest -> HTTPRequest
withOptionalHeader headerName (Just headerValue) = withHeader headerName headerValue
withOptionalHeader _ Nothing = id

-- | Sets timeout, in microseconds
withTimeout :: Int -> HTTPRequest -> HTTPRequest
withTimeout timeout request =
  request {getRequestTimeout = Just timeout}

withRedirects :: Int -> HTTPRequest -> HTTPRequest
withRedirects redirects request =
  request {getRequestRedirects = Just redirects}

-- TODO: Rename to `withFormData` or some such?
withBody :: [(Text, Text)] -> HTTPRequest -> HTTPRequest
withBody pairs request = request {getRequestBody = Just body}
  where
    body = T.LBinaryString $ formUrlEncode pairs

extractBody :: HTTPResponse -> Text
extractBody HTTPResponse{getResponseBody} = decodeUtf8With lenientDecode $ convertString getResponseBody

formUrlEncode :: [(Text, Text)] -> LB.ByteString
formUrlEncode = Builder.toLazyByteString . mconcat . intersperse amp . map encodePair
  where
    equals = Builder.word8 (ord '=')
    amp = Builder.word8 (ord '&')
    percent = Builder.word8 (ord '%')
    plus = Builder.word8 (ord '+')

    encodePair :: (Text, Text) -> Builder
    encodePair (key, value) = encode key <> equals <> encode value

    encode :: Text -> Builder
    encode = escape . Text.encodeUtf8

    escape :: ByteString -> Builder
    escape = mconcat . map f . B.unpack
      where
        f :: Word8 -> Builder
        f c
          | p c = Builder.word8 c
          | c == ord ' ' = plus
          | otherwise = percentEncode c

        p :: Word8 -> Bool
        p c =
             ord 'a' <= c && c <= ord 'z'
          || c == ord '_'
          || c == ord '*'
          || c == ord '-'
          || c == ord '.'
          || ord '0' <= c && c <= ord '9'
          || ord 'A' <= c && c <= ord 'Z'

    ord :: Char -> Word8
    ord = fromIntegral . Char.ord

    percentEncode :: Word8 -> Builder
    percentEncode n = percent <> hex hi <> hex lo
      where
        (hi, lo) = n `divMod` 16

    hex :: Word8 -> Builder
    hex n = Builder.word8 (offset + n)
      where
        offset
          | n < 10    = 48
          | otherwise = 55

maskHTTPRequest :: Maybe Log.LogMaskingConfig -> HTTPRequest -> HTTPRequest
maskHTTPRequest mbMaskConfig request =
  request
    { getRequestHeaders = maskHTTPHeaders (shouldMaskKey mbMaskConfig) getMaskText requestHeaders
    , getRequestBody = maskedRequestBody
    }
  where
    requestHeaders = getRequestHeaders request

    requestBody = getRequestBody request

    getMaskText = maybe defaultMaskText (fromMaybe defaultMaskText . Log._maskText) mbMaskConfig

    maskedRequestBody =
      T.LBinaryString
        . encodeUtf8
        . parseRequestResponseBody (shouldMaskKey mbMaskConfig) getMaskText (getContentTypeForHTTP requestHeaders)
        . LB.toStrict
        . T.getLBinaryString <$> requestBody

maskHTTPResponse :: Maybe Log.LogMaskingConfig -> HTTPResponse -> HTTPResponse
maskHTTPResponse mbMaskConfig response =
  response
    { getResponseHeaders = maskHTTPHeaders (shouldMaskKey mbMaskConfig) getMaskText responseHeaders
    , getResponseBody = maskedResponseBody
    }
  where
    responseHeaders = getResponseHeaders response

    responseBody = getResponseBody response

    getMaskText = maybe defaultMaskText (fromMaybe defaultMaskText . Log._maskText) mbMaskConfig

    maskedResponseBody =
      T.LBinaryString
        . encodeUtf8
        . parseRequestResponseBody (shouldMaskKey mbMaskConfig) getMaskText (getContentTypeForHTTP responseHeaders)
        . LB.toStrict
        $ T.getLBinaryString responseBody
