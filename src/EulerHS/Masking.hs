{-# LANGUAGE RecordWildCards,CPP #-}

module EulerHS.Masking where

import           Data.HashSet (member)
import           EulerHS.Prelude
import Data.String.Conversions hiding ((<>))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as AK
import qualified Data.HashSet as HS

#if defined(UNMASK_NUMERIC_KEYS)
import qualified Data.Char as Char
#endif

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified EulerHS.Encryption as EE
import qualified EulerHS.Extra.Regex as Regex
import qualified EulerHS.Logger.Types as Log
import qualified Network.HTTP.Types as HTTP
import qualified EulerHS.EnvVars as EnvVars
import qualified Data.Text.Encoding as DTE
import qualified Xmlbf as Xml
import qualified Xmlbf.Xeno as Xml
import           EulerHS.Logger.XmlToJson ()

shouldMaskKey :: Maybe Log.LogMaskingConfig -> Text -> Bool
shouldMaskKey Nothing _ = False
shouldMaskKey (Just Log.LogMaskingConfig{..}) key =
  case _keyType of
    Log.WhiteListKey -> not $ member key _maskKeys
    Log.BlackListKey -> member key _maskKeys

encryptReqOrResBody :: ByteString -> Maybe ByteString -> Maybe Text -> Maybe Text -> Aeson.Value
encryptReqOrResBody body contentType mbKey mbIv
  | Just key <- mbKey, Just iv <- mbIv =
    case EE.aesEncryptText key iv (decodeUtf8 body) of
      Right res -> Aeson.String $ "encrypted:::" <> res
      Left  _   -> notSupportedPlaceHolder contentType
  | otherwise = notSupportedPlaceHolder contentType

defaultMaskText :: Text
defaultMaskText = "***"

maskHTTPHeaders :: (Text -> Bool) -> Text -> Map.Map Text Text ->  Map.Map Text Text
maskHTTPHeaders shouldMask maskText = Map.mapWithKey maskHeader
  where
    maskHeader :: Text -> Text -> Text
    maskHeader key value  = if shouldMask key then maskText else value

maskServantHeaders :: (Text -> Bool) -> Text -> Seq HTTP.Header -> Seq HTTP.Header
maskServantHeaders shouldMask maskText headers = maskHeader <$> headers
  where
    maskHeader :: HTTP.Header -> HTTP.Header
    maskHeader (headerName,headerValue) =
      if shouldMask (decodeUtf8 $ CI.original headerName)
        then (headerName,encodeUtf8 maskText)
        else (headerName,headerValue)

maskQueryStrings :: (Text -> Bool) -> Text -> Seq HTTP.QueryItem -> Seq HTTP.QueryItem
maskQueryStrings shouldMask maskText queryStrings = maskQueryString <$> queryStrings
  where
    maskQueryString :: HTTP.QueryItem -> HTTP.QueryItem
    maskQueryString (key,value) =
      if shouldMask (decodeUtf8 key)
        then (key,Just $ encodeUtf8 maskText)
        else (key,value)

parseRequestResponseBody :: (Text -> Bool) -> Text -> Maybe Text -> Maybe Text -> Maybe ByteString -> ByteString -> Aeson.Value
parseRequestResponseBody shouldMask maskText mbLogEncKey mbLogEncIv mbContentType req
  | isContentTypeXmlAndSupported mbContentType =
    let req' = changeDoctypeElement
     in
      case Xml.fromRawXml req' :: Either String [Xml.Node] of
        -- BREAKING CHANGE. runParser renamed to parse, in xmlbf v0.7
        Right xmlNodes  -> case Xml.parse Xml.fromXml xmlNodes of
          Right value -> maskJSON shouldMask maskText mbContentType value
          Left _      -> notSupportedPlaceHolder mbContentType
        Left  _         ->  notSupportedPlaceHolder mbContentType
  | isHtmlContentType mbContentType && BS.isPrefixOf (DTE.encodeUtf8 "<") req = encryptReqOrResBody req mbContentType mbLogEncKey mbLogEncIv
  | otherwise =
      case Aeson.eitherDecodeStrict req of
        Right value -> maskJSON shouldMask maskText mbContentType value
        Left _      -> handleNonJsonReqResBody req shouldMask maskText mbLogEncKey mbLogEncIv mbContentType
  where
    changeDoctypeElement = DTE.encodeUtf8 $ Text.replace ("<!") ("<?") (DTE.decodeUtf8 req)

handleNonJsonReqResBody :: ByteString -> (Text -> Bool) -> Text -> Maybe Text -> Maybe Text -> Maybe ByteString -> Aeson.Value
handleNonJsonReqResBody req shouldMask maskText mbLogEncKey mbLogEncIv mbContentType
  | isContentTypeBlockedForLogging mbContentType = encryptReqOrResBody req mbContentType mbLogEncKey mbLogEncIv
  | otherwise = maskJSON shouldMask maskText mbContentType $ handleQueryString req

maskJSON :: (Text -> Bool) -> Text -> Maybe ByteString -> Aeson.Value -> Aeson.Value
maskJSON shouldMask maskText mbContentType (Aeson.Object r) = Aeson.Object $ handleObject shouldMask maskText mbContentType r
maskJSON shouldMask maskText mbContentType (Aeson.Array r) =  Aeson.Array $ maskJSON shouldMask maskText mbContentType <$> r
maskJSON shouldMask maskText mbContentType (Aeson.String r) =
  bool (Aeson.String r) (decodeToObject) (doesContentTypeHaveNestedStringifiedJSON mbContentType)
  where
    decodeToObject =
      case Aeson.eitherDecodeStrict $ encodeUtf8 $ r of
        Right val ->
          case val of
            (Aeson.Object v) -> Aeson.Object $ handleObject shouldMask maskText Nothing v
            (Aeson.Array _) -> maskJSON shouldMask maskText Nothing val
            _ -> val
        Left _ -> Aeson.String r
maskJSON _ _ _ value = value

maskCheck :: (Text -> Bool) -> Text -> Bool
maskCheck shouldMask key =
    shouldMask key
#if defined(UNMASK_NUMERIC_KEYS)
    && (not (all Char.isDigit (Text.unpack key)))
#endif

handleObject :: (Text -> Bool) -> Text -> Maybe ByteString -> Aeson.Object -> Aeson.Object
handleObject shouldMask maskText mbContentType = KM.mapWithKey maskingFn
  where
    maskingFn key value = bool (maskJSON shouldMask maskText mbContentType value) (Aeson.String maskText) $ (maskCheck shouldMask (AK.toText key))

handleQueryString :: ByteString -> Aeson.Value
handleQueryString strg = Aeson.Object . fmap (Aeson.String . fromMaybe "") . KM.fromList $ map convertTuple $ HTTP.parseQueryText strg
  where
    convertTuple :: (Text, Maybe Text) -> (AK.Key, Maybe Text)
    convertTuple (key, value) = (AK.fromText key, value)

notSupportedPlaceHolder :: Maybe ByteString -> Aeson.Value
notSupportedPlaceHolder (Just bs) = Aeson.String $ "Logging Not Support For this content " <> decodeUtf8 bs
notSupportedPlaceHolder Nothing = Aeson.String "Logging Not Support For this content "

isHtmlContentType :: Maybe ByteString -> Bool
isHtmlContentType Nothing = False
isHtmlContentType (Just contentType) =
  let contentTypeT = Text.toLower $ decodeUtf8 contentType
    in Text.isPrefixOf "text/html" contentTypeT || Text.isPrefixOf "application/html" contentTypeT || Text.isInfixOf "html" contentTypeT

isPlainTextContentType :: Maybe ByteString -> Bool
isPlainTextContentType Nothing = False
isPlainTextContentType (Just contentType) =
  let contentTypeT = Text.toLower $ decodeUtf8 contentType
    in Text.isPrefixOf "text/plain" contentTypeT || Text.isInfixOf "plain" contentTypeT

isContentTypeBlockedForLogging :: Maybe ByteString -> Bool
isContentTypeBlockedForLogging ct = isHtmlContentType ct || isPlainTextContentType ct

isContentTypeXmlAndSupported :: Maybe ByteString -> Bool
isContentTypeXmlAndSupported Nothing = False
isContentTypeXmlAndSupported (Just contentType) = Text.isInfixOf "xml" (Text.toLower $ decodeUtf8 contentType) && EnvVars.shouldLogXmlContent

-- NOTE: This logic is added because we are sending stringified JSON as Value
-- TODO: Can we convert these into application/json api calls ?
doesContentTypeHaveNestedStringifiedJSON :: Maybe ByteString -> Bool
doesContentTypeHaveNestedStringifiedJSON Nothing = False
doesContentTypeHaveNestedStringifiedJSON (Just contentType) = (("application/x-www-form-urlencoded" :: ByteString) == contentType)

getContentTypeForServant :: HTTP.ResponseHeaders -> Maybe ByteString
getContentTypeForServant = List.lookup HTTP.hContentType

getContentTypeForHTTP :: Map.Map Text Text -> Maybe ByteString
getContentTypeForHTTP header = getContentTypeForServant getTupleList
  where
    getTupleList = makeHeaderLableCI <$> Map.assocs header
    makeHeaderLableCI (headerName,headerValue) = (CI.mk $ encodeUtf8 headerName, encodeUtf8 headerValue)

-- PS Implemention for masking XML [blacklisting]
-- TODO: move away from regex
maskXMLText :: Maybe (HS.HashSet Text) -> Text.Text -> Text.Text
maskXMLText (Just customMaskingKeys) xml = foldl' (\acc x -> maskXMLForTAG x $ maskXMLForAttribute x acc) xml customMaskingKeys
maskXMLText Nothing xml = foldl' (\acc x -> maskXMLForTAG x $ maskXMLForAttribute x acc) xml defaultMaskingKeys

maskXMLForAttribute :: Text.Text -> Text.Text -> Text.Text
maskXMLForAttribute key xmlToMask =
  case (Regex.regex ("(" <> key <> ")=\"[^>\"]*(\")" :: Text.Text)) of
    Left _ -> "[HIDDEN]" -- "ISSUE WITH REGEX"
    Right cRegex -> Regex.replace cRegex (((toSBSFromText key) <>  "=\"FILTERED\"") :: SBS) xmlToMask 

maskXMLForTAG :: Text.Text -> Text.Text -> Text.Text
maskXMLForTAG key xmlToMask = 
  case (Regex.regex ("<((" <> key <> ")" <> attributePattern <> ")>((?!</).)*</(" <> key <> ")>" :: Text.Text)) of
    Left _ -> "[HIDDEN]" -- "ISSUE WITH REGEX"
    Right cRegex -> Regex.replaceMGGeneric cRegex replacer xmlToMask 
  where
    attributePattern :: Text
    attributePattern = "([^>]*)"

    replacer :: [SBS] -> SBS
    replacer [] = (("<"  <> (toSBSFromText key) <>  ">FILTERED</" <> (toSBSFromText key) <> ">") :: SBS)
    replacer (mg : _) = (("<"  <> mg <>  ">FILTERED</" <> (toSBSFromText key) <> ">") :: SBS)
 
toSBSFromText :: Text.Text -> ByteString
toSBSFromText = encodeUtf8

-- This is taken from euler-ps 
defaultMaskingKeys :: HS.HashSet Text 
defaultMaskingKeys = HS.fromList ["cardNumber", "card_number", "card_exp_month", "cardExpMonth", "card_exp_year", "cardExpYear", "card_security_code",
    "cardSecurityCode", "secretKey", "appId", "vpc_CardExp", "vpc_CardNum", "vpc_CardSecurityCode", "vpc_Card", "vpc_AccessCode", "vpc_User",
    "vpc_Password", "vpc_Merchant", "vpc_key", "paydata", "billDeskMerchantId", "card_expiryMonth", "card_expiryYear", "card_cvv", "card_holder",
    "accessToken", "olaPublicKey", "merchantPrivateKey", "xTenantKey", "xAuthKey", "access_token", "txnToken", "channelId", "mid", "cardInfo",
    "clientId", "ccnum", "ccname", "ccvv", "ccexpmon", "ccexpyr", "zero_click_token", "card[number]", "card[name]", "card[expiry_month]",
    "card[expiry_year]", "card[cvv]", "token", "expyear", "expmonth", "card", "password", "cvv2", "pass", "login", "mdd", "signature",
    "cnumber", "expmon", "expyr", "tranportalId", "PaymentID", "merchanttypekey", "vpc_SecureHash", "hash", "userAccessToken", "couponCode",
    "MID", "PAYMENT_DETAILS", "SSOToken", "CHECKSUMHASH", "CHANNEL_ID", "offer_key", "key", "access_code", "X-API-KEY", "ifsc", "bankIFSC",
    "accountNumber", "IFSC", "payerIfsc", "ifscCode", "encryptionKey", "encryptionIV", "iFSC", "accNo", "beneficiaryAccountNumber",
    "bankAccountNumber", "merchantGatewayAccount", "merchantAccount", "accountDetails", "cardData", "txnCardInfo", "shippingAddress",
    "billingAddress", "walletAccount", "object_reference_id", "email", "mobile_number", "first_name", "last_name", "customer_id",
    "customer_email", "customer_phone", "billing_address", "metadata", "customerPhone", "customerId", "customerEmail", "objectReferenceId",
    "mobileNumber", "firstName", "lastName", "shipping_address", "api_key", "apiKey", "account_details", "bank_account[account_number]",
    "bank_account[ifsc]", "expirationMonth", "expirationYear", "cvNumber", "name_on_card", "card_data", "c:proxyPAN", "expiry_date",
    "phone_number", "phone", "mobile", "contact", "phoneNumber", "contact_number", "contactNumber", "email_id", "customer_ip", "cardIsin",
    "cardEpYear", "cardReference", "cardFingerprint", "card_holder_name", "card_expiry_date", "number", "enc_card_number", "encrypted_pan",
    "month", "enc_expiry_month", "expiry_month", "expiry", "card_expiry_month", "encrypted_expiry_month", "year", "enc_expiry_year",
    "expiry_year", "card_expiry_year", "encrypted_expiry_year", "securityCode", "cvv_number", "cvv", "encryptedcvv", "nameOnCard", "udf1",
    "cname2", "name", "card_name", "member", "nameoncard", "CUST_MOBILE", "udf3", "customerMobileNumber", "billing_tel", "delivery_tel",
    "MOBILE_NO", "mobile_no", "mobileNo", "mobileNUmber", "CUST_EMAIL", "udf2", "billing_email", "delivery_email", "EMAIL", "email_address",
    "Authorization", "authorization", "Cookie", "cookie", "Proxy-Authorization", "proxy-authorization", "expday", "payerVpa", "wsse:Password"
  ]
