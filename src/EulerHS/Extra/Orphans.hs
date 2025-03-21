{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}


module EulerHS.Extra.Orphans where

import Data.Aeson as A
import qualified Data.Aeson.Types as A (Parser)
import Prelude as P
import Data.ByteString
import qualified Data.ByteArray as BA
import Data.ByteString.Char8 as BSC
import Universum as U
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson.KeyMap as KM
import qualified Database.Redis as R
import qualified Data.Text as T
import Data.Scientific
import qualified Network.HTTP.Client as HC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import qualified Data.Vector.NonEmpty as DV
import Data.Text.Encoding.Error (UnicodeException(..))
import qualified Codec.Compression.Zstd as ZSTD
import           Jose.Jwt (JwtError(..))
import qualified Network.DNS.Types as DNS (DNSError(..))
import Jose.Jwt (Payload(..))
import           Crypto.PubKey.RSA (PublicKey(..), PrivateKey(..))
import qualified Crypto.Store.PKCS8 as CSP
import qualified Data.X509 as X
import qualified Crypto.PubKey.Ed448 as Ed448
import qualified Data.ByteString as BS
import Foreign.Ptr (castPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import System.IO.Unsafe (unsafePerformIO)
import EulerHS.ART.V2.Types
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Crypto.PubKey.Curve448 as Curve448
import qualified Crypto.PubKey.Curve25519 as Curve25519
import qualified Crypto.PubKey.DSA as DSA
import qualified Crypto.PubKey.ECC.Types as ECCT
import qualified Crypto.Error as Crypto
import qualified Crypto.PubKey.RSA.Types as RSATypes
import qualified Amazonka.S3 as S3

instance ToJSON ByteString where -- Need to check is this right way
  toJSON x = A.toJSON (BSC.unpack x)

instance FromJSON ByteString where -- Need to check is this right way
  parseJSON (A.String val) = pure . BSC.pack . T.unpack $ val
  parseJSON val = (pure . BSL.toStrict . A.encode $ val)

instance ToJSON BA.ScrubbedBytes where
    toJSON sb = A.toJSON (BA.convert sb :: ByteString)

instance FromJSON BA.ScrubbedBytes where
    parseJSON v = do
        bs <- parseJSON v :: A.Parser ByteString
        pure (BA.convert bs :: BA.ScrubbedBytes)

instance ToJSON R.XReadOpts where
  toJSON R.XReadOpts{..} = A.Object $ KM.fromList [("block", toJSON block),("recordCount",toJSON recordCount)]

instance FromJSON R.XReadOpts where
  parseJSON (A.Object hm) = do
    let block =  toInteger' (KM.lookup "block" hm)
        recordCount = toInteger' (KM.lookup "recordCount" hm)
        noAck' = toBoolean (KM.lookup "recordCount" hm)
    pure (R.XReadOpts block recordCount noAck')
    where
      toBoolean (Just (A.String v)) = Just True == (readMaybe $ T.unpack v)
      toBoolean (Just (A.Bool v)) = v
      toBoolean _ = False

      toInteger' (Just (A.String v)) = readMaybe $ T.unpack v
      toInteger' (Just (A.Number v)) = Just $ coefficient v
      toInteger' _ = Nothing
  parseJSON _ = pure R.defaultXreadOpts

instance (ToJSON a) => ToJSON (DV.NonEmptyVector a) where
  toJSON = toJSON . DV.toList

instance ToJSON (CI.CI ByteString) where
  toJSON = toJSON . CI.original 

instance FromJSON (CI.CI ByteString) where
  parseJSON a = case fromJSON a of
    Success res -> pure $ CI.mk res 
    Error err   -> fail err

instance ToJSON BSL.ByteString where
  toJSON = toJSON . BSL.toStrict

instance FromJSON BSL.ByteString where
  parseJSON (String val) = pure . BSL.fromStrict . BSC.pack . T.unpack $ val
  parseJSON val = pure . encode $ val

instance ToJSON HC.HttpException where
  toJSON e = 
    case e of
      HC.InvalidUrlException field reason -> object [("errorType", A.String "InvalidUrlException"), ("field", A.toJSON field), ("reason", A.toJSON reason)]
      HC.HttpExceptionRequest req reason -> object [("errorType", A.String "HttpExceptionRequest"), ("reason", A.String $ U.show reason), ("request", toJSON req)]

instance ToJSON HC.Request where
  toJSON req = object [
                ("method", A.toJSON $ HC.method req),
                ("secure", A.toJSON $ HC.secure req),
                ("host", A.toJSON $ HC.host req),
                ("port", A.toJSON $ HC.port req),
                ("path", A.toJSON $ HC.path req),
                ("queryString", A.toJSON $ HC.queryString req),
                ("requestHeaders", A.Object . KM.fromHashMapText . HM.fromList . fmap (\(h,v) -> (decodeUtf8 $ CI.original h, toJSON v)) $ HC.requestHeaders req),
                ("proxy", A.String . U.show $ HC.proxy req),
                ("redirectCount", A.toJSON $ HC.redirectCount req),
                ("responseTimeout", A.String . U.show $ HC.responseTimeout req),
                ("requestVersion", A.String . U.show $ HC.requestVersion req)
                ]

-- instance A.ToJSON UnicodeException where
--     toJSON err = case err of
--         DecodeError _e _ -> A.object [("errorType", A.String "DecodeError"), ("errorReason", A.String "Could not decode a byte sequence because it was invalid under the given encoding, or ran out of input in mid-decode.")]
--         _ -> A.object [("errorType", A.String "EncodeError"), ("errorReason", "Tried to encode a character that could not be represented under the given encoding, or ran out of input in mid-encode.")]

instance A.ToJSON JwtError where
  toJSON err = 
    case err of
      KeyError e -> getErrObject "KeyError" "No suitable key or wrong key type" $ e
      BadAlgorithm e -> getErrObject "BadAlgorithm" "The supplied algorithm is invalid" $ e
      BadDots n -> getErrObject "BadDots" "Wrong number of \".\" characters in the JWT" $ T.pack ("Got " <> P.show n <> " \".\" in JWT")
      BadHeader header -> getErrObject "BadHeader" "Header couldn't be decoded or contains bad data" $ "Got bad header: " <> header
      BadClaims -> getErrObject "BadClaims" "Claims part couldn't be decoded or contains bad data" $ "Claims part is invalid."
      BadSignature -> getErrObject "BadSignature" "Signature is invalid" $ "Signature in JWT is invalid."
      BadCrypto -> getErrObject "BadCrypto" "A cryptographic operation failed" $ "Invalid cryptographic operation or value."
      Base64Error e -> getErrObject "Base64Error" "A base64 decoding error" $ T.pack e
    where
      getErrObject errType errDesc errReason = A.object [("errorType", A.String errType), ("errorDescription", A.String errDesc), ("errorReason", A.String errReason)]

instance A.ToJSON DNS.DNSError where
    toJSON err =
        case err of
            DNS.SequenceNumberMismatch -> A.object [("errorType", A.String "DNSError"), ("errorReason", A.String "SequenceNumberMismatch")]
            DNS.QuestionMismatch -> A.object [("errorType", A.String "DNSError"), ("errorReason", A.String "QuestionMismatch")]
            DNS.InvalidAXFRLookup -> A.object [("errorType", A.String "DNSError"), ("errorReason", A.String "InvalidAXFRLookup")]
            DNS.RetryLimitExceeded -> A.object [("errorType", A.String "DNSError"), ("errorReason", A.String "RetryLimitExceeded")]
            DNS.TimeoutExpired -> A.object [("errorType", A.String "DNSError"), ("errorReason", A.String "TimeoutExpired")]
            DNS.UnexpectedRDATA -> A.object [("errorType", A.String "DNSError"), ("errorReason", A.String "UnexpectedRDATA")]
            DNS.IllegalDomain -> A.object [("errorType", A.String "DNSError"), ("errorReason", A.String "IllegalDomain")]
            DNS.FormatError -> A.object [("errorType", A.String "DNSError"), ("errorReason", A.String "FormatError")]
            DNS.ServerFailure -> A.object [("errorType", A.String "DNSError"), ("errorReason", A.String "ServerFailure")]
            DNS.NameError -> A.object [("errorType", A.String "DNSError"), ("errorReason", A.String "NameError")]
            DNS.NotImplemented -> A.object [("errorType", A.String "DNSError"), ("errorReason", A.String "NotImplemented")]
            DNS.OperationRefused -> A.object [("errorType", A.String "DNSError"), ("errorReason", A.String "OperationRefused")]
            DNS.BadOptRecord -> A.object [("errorType", A.String "DNSError"), ("errorReason", A.String "BadOptRecord")]
            DNS.BadConfiguration -> A.object [("errorType", A.String "DNSError"), ("errorReason", A.String "BadConfiguration")]
            DNS.NetworkFailure ioe -> A.object [("errorType", A.String "DNSError"), ("errorReason", A.String "NetworkFailure"), ("errorDescription", A.toJSON $ displayException ioe)]
            DNS.DecodeError str -> A.object [("errorType", A.String "DNSError"), ("errorReason", A.String "DecodeError"), ("errorDescription", A.toJSON str)]
            DNS.UnknownDNSError -> A.object [("errorType", A.String "DNSError"), ("errorReason", A.String "UnknownDNSError")]

instance ToJSON ZSTD.Dict where
  toJSON a = toJSON (ZSTD.fromDict a)

instance FromJSON ZSTD.Dict where
  parseJSON a = ZSTD.mkDict <$> (parseJSON a)

deriving instance A.ToJSON R.Status
deriving instance A.FromJSON R.Status

deriving instance Generic R.XReadResponse
deriving instance A.ToJSON R.XReadResponse
deriving instance A.FromJSON R.XReadResponse

deriving instance Generic R.StreamsRecord
deriving instance A.ToJSON R.StreamsRecord
deriving instance A.FromJSON R.StreamsRecord

deriving instance Generic Jose.Jwt.Payload
deriving instance A.ToJSON Jose.Jwt.Payload
deriving instance A.FromJSON Jose.Jwt.Payload

deriving instance Generic Jose.Jwt.JwtError
deriving instance A.FromJSON Jose.Jwt.JwtError

deriving instance Generic PublicKey
deriving instance A.ToJSON PublicKey
deriving instance A.FromJSON PublicKey

deriving instance Generic PrivateKey
deriving instance A.ToJSON PrivateKey
deriving instance A.FromJSON PrivateKey

convertByteAccessArrayToByteString :: BA.ByteArrayAccess ba => ba -> ByteString
convertByteAccessArrayToByteString byteAccessArray = unsafePerformIO $ BA.withByteArray byteAccessArray $ \_ -> do
    let len = BA.length byteAccessArray
    allocaBytes len $ \buf -> do
        BA.copyByteArrayToPtr byteAccessArray buf
        BS.packCStringLen (castPtr buf, len)

instance ToJSON Ed448.PublicKey where
  toJSON = toJSON . convertByteAccessArrayToByteString

instance FromJSON Ed448.PublicKey where
  parseJSON val = do
    (parsedBS :: ByteString) <- parseJSON val
    let cryptoParser = Ed448.publicKey parsedBS
    maybe (fail "Failed to parse secret key") (pure) $ Crypto.maybeCryptoError cryptoParser

instance ToJSON Ed448.SecretKey where
  toJSON = toJSON . convertByteAccessArrayToByteString

instance FromJSON Ed448.SecretKey where
  parseJSON val = do
    (parsedBS :: ByteString) <- parseJSON val
    let cryptoParser = Ed448.secretKey parsedBS
    maybe (fail "Failed to parse secret key") (pure) $ Crypto.maybeCryptoError cryptoParser

instance ToJSON Ed25519.PublicKey where
  toJSON = toJSON . convertByteAccessArrayToByteString

instance FromJSON Ed25519.PublicKey where
  parseJSON val = do
    (parsedBS :: ByteString) <- parseJSON val
    let cryptoParser = Ed25519.publicKey parsedBS
    maybe (fail "Failed to parse secret key") (pure) $ Crypto.maybeCryptoError cryptoParser

instance ToJSON Ed25519.SecretKey where
  toJSON = toJSON . convertByteAccessArrayToByteString

instance FromJSON Ed25519.SecretKey where
  parseJSON val = do
    (parsedBS :: ByteString) <- parseJSON val
    let cryptoParser = Ed25519.secretKey parsedBS
    maybe (fail "Failed to parse secret key") (pure) $ Crypto.maybeCryptoError cryptoParser

instance ToJSON Curve448.PublicKey where
  toJSON = toJSON . convertByteAccessArrayToByteString

instance FromJSON Curve448.PublicKey where
  parseJSON val = do
    (parsedBS :: ByteString) <- parseJSON val
    let cryptoParser = Curve448.publicKey parsedBS
    maybe (fail "Failed to parse secret key") (pure) $ Crypto.maybeCryptoError cryptoParser

instance ToJSON Curve25519.PublicKey where
  toJSON = toJSON . convertByteAccessArrayToByteString

instance FromJSON Curve25519.PublicKey where
  parseJSON val = do
    (parsedBS :: ByteString) <- parseJSON val
    let cryptoParser = Curve25519.publicKey parsedBS
    maybe (fail "Failed to parse secret key") (pure) $ Crypto.maybeCryptoError cryptoParser

instance ToJSON Curve448.SecretKey where
  toJSON = toJSON . convertByteAccessArrayToByteString

instance FromJSON Curve448.SecretKey where
  parseJSON val = do
    (parsedBS :: ByteString) <- parseJSON val
    let cryptoParser = Curve448.secretKey parsedBS
    maybe (fail "Failed to parse secret key") (pure) $ Crypto.maybeCryptoError cryptoParser

instance ToJSON Curve25519.SecretKey where
  toJSON = toJSON . convertByteAccessArrayToByteString

instance FromJSON Curve25519.SecretKey where
  parseJSON val = do
    (parsedBS :: ByteString) <- parseJSON val
    let cryptoParser = Curve25519.secretKey parsedBS
    maybe (fail "Failed to parse secret key") (pure) $ Crypto.maybeCryptoError cryptoParser

instance ArtRecordable a => ArtRecordable (CSP.OptProtected a) where
  isRecordable = True

  toArtRecordingValue (CSP.Unprotected a) = toArtRecordingValue a
  toArtRecordingValue _ = Left "NON_REPLAYABLE_FUNCTION"

  fromArtRecordedValue (A.String "NON_REPLAYABLE_FUNCTION") = Left "CANNOT GENERATE FUNCTION HANDLER"
  fromArtRecordedValue v = CSP.Unprotected <$> fromArtRecordedValue v

deriving anyclass instance ArtRecordable (IORef a)

deriving anyclass instance ArtRecordable (TVar a)

deriving instance Generic X.PrivKey
deriving instance A.ToJSON X.PrivKey
deriving instance A.FromJSON X.PrivKey

deriving instance Generic X.PrivKeyEC
deriving instance A.ToJSON X.PrivKeyEC
deriving instance A.FromJSON X.PrivKeyEC

deriving instance Generic X.PubKeyEC
deriving instance A.ToJSON X.PubKeyEC
deriving instance A.FromJSON X.PubKeyEC

deriving instance Generic DSA.PrivateKey
deriving instance A.ToJSON DSA.PrivateKey
deriving instance A.FromJSON DSA.PrivateKey

deriving instance Generic DSA.PublicKey
deriving instance A.ToJSON DSA.PublicKey
deriving instance A.FromJSON DSA.PublicKey

deriving instance Generic DSA.Params
deriving instance A.ToJSON DSA.Params
deriving instance A.FromJSON DSA.Params

deriving instance Generic ECCT.CurveName
deriving instance A.ToJSON ECCT.CurveName
deriving instance A.FromJSON ECCT.CurveName

deriving instance Generic X.SerializedPoint
deriving anyclass instance A.ToJSON X.SerializedPoint
deriving anyclass instance A.FromJSON X.SerializedPoint

deriving instance Generic X.PubKey
deriving instance A.ToJSON X.PubKey
deriving instance A.FromJSON X.PubKey

deriving instance Generic UnicodeException
deriving instance A.ToJSON UnicodeException
deriving instance A.FromJSON UnicodeException

deriving instance Generic Crypto.CryptoError
deriving instance A.ToJSON Crypto.CryptoError
deriving instance A.FromJSON Crypto.CryptoError

deriving instance Generic RSATypes.Error
deriving instance A.ToJSON RSATypes.Error
deriving instance A.FromJSON RSATypes.Error

deriving instance A.ToJSON S3.PutObjectResponse
deriving instance A.FromJSON S3.PutObjectResponse

deriving anyclass instance A.ToJSON S3.ObjectVersionId
deriving anyclass instance A.FromJSON S3.ObjectVersionId

deriving anyclass instance A.ToJSON S3.ETag
deriving anyclass instance A.FromJSON S3.ETag

deriving anyclass instance A.ToJSON S3.ObjectKey
deriving anyclass instance A.FromJSON S3.ObjectKey
