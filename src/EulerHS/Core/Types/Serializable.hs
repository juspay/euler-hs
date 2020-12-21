{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}


module EulerHS.Core.Types.Serializable
  (
    -- * Core Serializable
    -- ** Class
    Serializable(..)
    -- ** Bytestrings
  , ByteStringS
  , fromByteString
  , toByteString
  -- ** JSONEx
  , JSONEx
  , resolveJSONEx
  , fromJSONMaybe
  ) where


import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as BS
import           Data.ByteString.Base64.Type as B64
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import           EulerHS.Prelude
import qualified Network.HTTP.Media as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Servant.Client as S
import qualified Servant.Client.Core.Request as S



----------------------------------------------------------------------

class EitherC c d where
    resolve :: (c => r) -> (d => r) -> r

instance {-# OVERLAPPABLE #-} d => EitherC c d where
    resolve _ r = r


----------------------------------------------------------------------

class Serializable a where
    jsonEncode :: a       -> A.Value
    jsonDecode :: A.Value -> Maybe a

type JSONEx a = EitherC (Serializable a) (ToJSON a, FromJSON a)

resolveJSONEx
    :: forall a r
     . JSONEx a
    => (Serializable a => r)
    -> ((ToJSON a, FromJSON a) => r)
    -> r
resolveJSONEx sf jf = resolve @(Serializable a) @(ToJSON a, FromJSON a) sf jf

----------------------------------------------------------------------

fromJSONMaybe :: FromJSON a => A.Value -> Maybe a
fromJSONMaybe = A.parseMaybe parseJSON

----------------------------------------------------------------------

instance JSONEx a => Serializable (Maybe a) where
    jsonEncode = resolveJSONEx @a
        (encodeWith jsonEncode)
        (encodeWith toJSON)
        where
            encodeWith el = toJSON . fmap el

    jsonDecode = resolveJSONEx @a
        (decodeWith jsonDecode)
        (decodeWith fromJSONMaybe)
        where
          decodeWith dl val =
            case fmap (fmap dl) $ fromJSONMaybe val of
              Just (Just (Just v)) -> Just (Just v)
              Just (Nothing)       -> Just (Nothing)
              _                    -> Nothing





instance JSONEx a => EitherC (Serializable (Maybe a)) d where resolve r _ = r

----------------------------------------------------------------------

instance (JSONEx a, JSONEx b) => Serializable (Either a b) where
    jsonEncode = resolveJSONEx @a
        (resolveJSONEx @b
            (encodeWith jsonEncode jsonEncode)
            (encodeWith jsonEncode toJSON))
        (resolveJSONEx @b
            (encodeWith toJSON jsonEncode)
            (encodeWith toJSON toJSON))
        where
            encodeWith el er = toJSON . bimap el er

    jsonDecode = resolveJSONEx @a
        (resolveJSONEx @b
            (decodeWith jsonDecode jsonDecode)
            (decodeWith jsonDecode (A.parseMaybe parseJSON)))
        (resolveJSONEx @b
            (decodeWith (A.parseMaybe parseJSON) jsonDecode)
            (decodeWith (A.parseMaybe parseJSON) (A.parseMaybe parseJSON)))
        where
            decodeWith dl dr = A.parseMaybe parseJSON >=> either (fmap Left . dl) (fmap Right . dr)



instance (JSONEx a, JSONEx b) => EitherC (Serializable (Either a b)) d where resolve r _ = r

----------------------------------------------------------------------

instance (JSONEx a, JSONEx b) => Serializable (a, b) where
    jsonEncode = resolveJSONEx @a
        (resolveJSONEx @b
            (encodeWith jsonEncode jsonEncode)
            (encodeWith jsonEncode toJSON))
        (resolveJSONEx @b
            (encodeWith toJSON jsonEncode)
            (encodeWith toJSON toJSON))
        where
            encodeWith el er = toJSON . bimap el er

    jsonDecode = resolveJSONEx @a
        (resolveJSONEx @b
            (decodeWith jsonDecode jsonDecode)
            (decodeWith jsonDecode (A.parseMaybe parseJSON)))
        (resolveJSONEx @b
            (decodeWith (A.parseMaybe parseJSON) jsonDecode)
            (decodeWith (A.parseMaybe parseJSON) (A.parseMaybe parseJSON)))
        where
            decodeWith dl dr = A.parseMaybe parseJSON >=> \(va, vb) -> (,) <$> dl va <*> dr vb


instance (JSONEx a, JSONEx b) => EitherC (Serializable (a, b)) d where resolve r _ = r


----------------------------------------------------------------------

instance JSONEx a => Serializable [a] where
    jsonEncode = toJSON . fmap (resolveJSONEx @a jsonEncode toJSON)
    jsonDecode = join . fmap (sequence . fmap (resolveJSONEx @a jsonDecode fromJSONMaybe)) . fromJSONMaybe


instance JSONEx a => EitherC (Serializable [a]) d where resolve r _ = r

----------------------------------------------------------------------

instance Serializable ByteString where
    jsonEncode bs = A.object ["b64" A..= mkBS64 bs, "utf8" A..= decodeUtf8 @Text bs]
    jsonDecode = A.parseMaybe . A.withObject "bs" $ \o -> fmap getBS64 (o A..: "b64")

instance Serializable ByteString64 where
    jsonEncode = toJSON
    jsonDecode = A.parseMaybe parseJSON

instance EitherC (Serializable ByteString) d where resolve r _ = r

----------------------------------------------------------------------

instance Serializable S.ClientError where
    jsonEncode = toJSON . fromClientError
    jsonDecode = fmap toClientError . A.parseMaybe parseJSON

instance EitherC (Serializable S.ClientError) d where resolve r _ = r

----------------------------------------------------------------------

newtype ByteStringS = ByteStringS [Word8]
  deriving (Generic, ToJSON, FromJSON)

fromByteString :: ByteString -> ByteStringS
fromByteString = ByteStringS . BS.unpack

toByteString :: ByteStringS -> ByteString
toByteString (ByteStringS a)= BS.pack a

----------------------------------------------------------------------

fromCIByteString :: CI.CI ByteString -> ByteStringS
fromCIByteString = fromByteString . CI.original

toCIByteString :: ByteStringS -> CI.CI ByteString
toCIByteString = CI.mk . toByteString


----------------------------------------------------------------------
-- ClientError serializer/deserializer
----------------------------------------------------------------------

data HttpVersionS
  = HttpVersionS
    { httpMajor :: !Int
    , httpMinor :: !Int
    } deriving (Generic, ToJSON, FromJSON)

fromHttpVersion :: HTTP.HttpVersion -> HttpVersionS
fromHttpVersion HTTP.HttpVersion {..} = HttpVersionS {..}

toHttpVersion :: HttpVersionS -> HTTP.HttpVersion
toHttpVersion HttpVersionS {..} = HTTP.HttpVersion {..}

----------------------------------------------------------------------

data HTTPStatusS
  = StatusS
  { statusCode    :: Int
  , statusMessage :: ByteStringS
  } deriving (Generic, ToJSON, FromJSON)

fromHTTPStatus :: HTTP.Status -> HTTPStatusS
fromHTTPStatus HTTP.Status {..} = StatusS {statusMessage = fromByteString statusMessage, ..}

toHTTPStatus :: HTTPStatusS -> HTTP.Status
toHTTPStatus StatusS {..} = HTTP.Status {statusMessage = toByteString statusMessage, ..}

----------------------------------------------------------------------

data ResponseS = ResponseS
  { responseStatusCode  :: HTTPStatusS
  , responseHeaders     :: [(ByteStringS, ByteStringS)]
  , responseHttpVersion :: HttpVersionS
  , responseBody        :: ByteStringS
  } deriving (Generic, ToJSON, FromJSON)

fromResponse :: S.Response -> ResponseS
fromResponse S.Response {..} = ResponseS
  { responseStatusCode  = fromHTTPStatus responseStatusCode
  , responseHttpVersion = fromHttpVersion responseHttpVersion
  , responseBody        = fromByteString $ BSL.toStrict responseBody
  , responseHeaders     = toList $ fmap (bimap fromCIByteString fromByteString) $ responseHeaders
  }

toResponse :: ResponseS -> S.Response
toResponse ResponseS {..} = S.Response
  { responseStatusCode  = toHTTPStatus responseStatusCode
  , responseHttpVersion = toHttpVersion responseHttpVersion
  , responseBody        = BSL.fromStrict $ toByteString responseBody
  , responseHeaders     = Seq.fromList $ fmap (bimap toCIByteString toByteString) $ responseHeaders
  }

----------------------------------------------------------------------

data ConnectionException = ConnectionException
  deriving (Show, Generic, Exception, ToJSON, FromJSON)

----------------------------------------------------------------------

data MediaTypeS = MediaTypeS
  { mainType   :: ByteStringS
  , subType    :: ByteStringS
  , parameters :: [(ByteStringS, ByteStringS)]
  } deriving (Generic, ToJSON, FromJSON)

fromMediaType :: HTTP.MediaType -> MediaTypeS
fromMediaType mt = MediaTypeS
  { mainType   = fromCIByteString mainType'
  , subType    = fromCIByteString subType'
  , parameters = bimap fromCIByteString fromCIByteString <$> Map.assocs parameters'
  }
  where
    mainType'   = HTTP.mainType mt
    subType'    = HTTP.subType mt
    parameters' = HTTP.parameters mt

toMediaType :: MediaTypeS -> HTTP.MediaType
toMediaType MediaTypeS {..} =
  foldl' (HTTP./:)
    (toByteString mainType HTTP.// toByteString subType)
    (bimap toByteString toByteString <$> parameters)

----------------------------------------------------------------------

data RequestS = RequestS
  { requestPath        :: (S.BaseUrl, ByteStringS)
  , requestQueryString :: [(ByteStringS, Maybe ByteStringS)]
  , requestBody        :: Maybe ((), MediaTypeS)
  , requestAccept      :: [MediaTypeS]
  , requestHeaders     :: [(ByteStringS, ByteStringS)]
  , requestHttpVersion :: HttpVersionS
  , requestMethod      :: ByteStringS
  } deriving (Generic, Typeable, ToJSON, FromJSON)


fromRequest :: S.RequestF () (S.BaseUrl, ByteString) -> RequestS
fromRequest S.Request {..} = RequestS
  { requestPath        = second fromByteString requestPath
  , requestQueryString = toList $ fmap (bimap fromByteString (fmap fromByteString)) requestQueryString
  , requestBody        = fmap (second fromMediaType) requestBody
  , requestAccept      = toList $ fmap fromMediaType requestAccept
  , requestHeaders     = toList $ fmap (bimap fromCIByteString fromByteString) requestHeaders
  , requestHttpVersion = fromHttpVersion requestHttpVersion
  , requestMethod      = fromByteString requestMethod
  }

toRequest :: RequestS -> S.RequestF () (S.BaseUrl, ByteString)
toRequest RequestS {..} = S.Request
  { requestPath        = second toByteString requestPath
  , requestQueryString = Seq.fromList $ fmap (bimap toByteString (fmap toByteString)) requestQueryString
  , requestBody        = fmap (second toMediaType) requestBody
  , requestAccept      = Seq.fromList $ fmap toMediaType requestAccept
  , requestHeaders     = Seq.fromList $ fmap (bimap toCIByteString toByteString) requestHeaders
  , requestHttpVersion = toHttpVersion requestHttpVersion
  , requestMethod      = toByteString requestMethod
  }

----------------------------------------------------------------------

data ClientErrorS
  = FailureResponse RequestS ResponseS
  | DecodeFailure Text ResponseS
  | UnsupportedContentType MediaTypeS ResponseS
  | InvalidContentTypeHeader ResponseS
  | ConnectionError ConnectionException
  deriving (Generic, ToJSON, FromJSON)

fromClientError :: S.ClientError -> ClientErrorS
fromClientError (S.FailureResponse req res)        = FailureResponse (fromRequest req) (fromResponse res)
fromClientError (S.DecodeFailure i res)            = DecodeFailure i (fromResponse res)
fromClientError (S.UnsupportedContentType t res)   = UnsupportedContentType (fromMediaType t) (fromResponse res)
fromClientError (S.InvalidContentTypeHeader res)   = InvalidContentTypeHeader (fromResponse res)
-- Note: We do not preserve actual error, for now
fromClientError (S.ConnectionError _)              = ConnectionError ConnectionException

toClientError :: ClientErrorS -> S.ClientError
toClientError (FailureResponse req res)        = S.FailureResponse (toRequest req) (toResponse res)
toClientError (DecodeFailure i res)            = S.DecodeFailure i (toResponse res)
toClientError (UnsupportedContentType t res)   = S.UnsupportedContentType (toMediaType t) (toResponse res)
toClientError (InvalidContentTypeHeader res)   = S.InvalidContentTypeHeader (toResponse res)
toClientError (ConnectionError e)              = S.ConnectionError $ toException e


----------------------------------------------------------------------
