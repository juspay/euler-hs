module MaskingSpec (spec) where

import EulerHS.Prelude hiding (readFile)
import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as LBS
import qualified EulerHS.Types as CType
import qualified Data.HashSet as HashSet
import           Test.Hspec

spec :: Spec
spec =
  describe "Outgoing API call log masking" $ do
    it "Should Mask All the blackListed Keys" $ do
      let rawRequest = inputJSON
      let maskText = "$$$"
      let mbMaskConfig = Just $ makeLogMaskingConfig CType.BlackListKey ["id", "url1","a"] maskText
      let maskedValue = CType.parseRequestResponseBody (CType.shouldMaskKey mbMaskConfig) maskText Nothing (LBS.toStrict rawRequest)
      maskedValue `shouldBe` expectedOutput
    it "Should Mask All the blackListed Keys" $ do
      let rawRequest = inputJSON
      let maskText = "$**$"
      let mbMaskConfig = Just $ makeLogMaskingConfig CType.WhiteListKey ["id", "url1","a"] maskText
      let maskedValue = CType.parseRequestResponseBody (CType.shouldMaskKey mbMaskConfig) maskText (Just (encodeUtf8 ("application/json" :: Text))) (LBS.toStrict rawRequest)
      maskedValue `shouldBe` expectedOutput'
    it "Should Not Mask Any Keys" $ do
      let rawRequest = inputJSON
      let maskText = "$**$"
      let mbMaskConfig = Nothing
      let maskedValue = CType.parseRequestResponseBody (CType.shouldMaskKey mbMaskConfig) maskText Nothing (LBS.toStrict rawRequest)
      maskedValue `shouldBe` expectedOutput''
    it "Should Mask Complete Body for HTML content type" $ do
      let rawRequest = inputJSON
      let maskText = "$**$"
      let mbMaskConfig = Nothing
      let maskedValue = CType.parseRequestResponseBody (CType.shouldMaskKey mbMaskConfig) maskText (Just (encodeUtf8 ("application/html" :: Text))) (LBS.toStrict rawRequest)
      maskedValue `shouldBe` expectedOutput'''

expectedOutput :: A.Value
expectedOutput =  A.object
  [ "status" .= ("INIT" :: Text)
  , "txnId" .= ("paypal-tatapay_740-1" :: Text)
  , "txnDetailId" .= ("2148428442" :: Text)
  , "responseAttempted" .= A.object
      [ "lastUpdated" .= ("2020-09-25T05:58:13Z" :: Text)
      , "gatewayAuthReqParams" .= ("{\"euler-api-gateway\":\"fehfioe\"}"  :: Text)
      , "dateCreated" .= ("2020-09-25T05:58:13Z"  :: Text)
      ,"challengesAttempted" .= (0 :: Int)
      ,"canAcceptResponse" .= True
      ,"id" .= ("$$$"  :: Text)
      ]
  , "version" .= (0 :: Int)
  , "url1" .= ("$$$" :: Text)
  , "type" .= ("VBV" :: Text)
  ]

expectedOutput' :: A.Value
expectedOutput' =  A.object
  [ "status" .= ("$**$" :: Text)
  , "txnId" .= ("$**$" :: Text)
  , "txnDetailId" .= ("$**$" :: Text)
  , "responseAttempted" .= ("$**$" :: Text)
  , "version" .= ("$**$" :: Text)
  , "url1" .= [A.object ["a" .= ("b" :: Text)], A.String "wefojoefwj"]
  , "type" .= ("$**$" :: Text)
  ]

expectedOutput'' :: A.Value
expectedOutput'' =  A.object
  [ "status" .= ("INIT" :: Text)
  , "txnId" .= ("paypal-tatapay_740-1" :: Text)
  , "txnDetailId" .= ("2148428442" :: Text)
  , "responseAttempted" .= A.object
      [ "lastUpdated" .= ("2020-09-25T05:58:13Z" :: Text)
      , "gatewayAuthReqParams" .= ("{\"euler-api-gateway\":\"fehfioe\"}"  :: Text)
      , "dateCreated" .= ("2020-09-25T05:58:13Z"  :: Text)
      ,"challengesAttempted" .= (0 :: Int)
      ,"canAcceptResponse" .= True
      ,"id" .= ("2148361678"  :: Text)
      ]
  , "version" .= (0 :: Int)
  , "url1" .= [A.object ["a" .= ("b" :: Text)], A.String "wefojoefwj"]
  , "type" .= ("VBV" :: Text)
  ]

expectedOutput''' :: A.Value
expectedOutput''' =  A.String  "Logging Not Support For this content application/html"


inputJSON :: LBS.ByteString
inputJSON = "{\"version\": 0,\"url1\": [{\"a\":\"b\"},\"wefojoefwj\"],\"type\": \"VBV\",\"txnId\": \"paypal-tatapay_740-1\",\"txnDetailId\": \"2148428442\",\"status\": \"INIT\",\"responseAttempted\": {\"lastUpdated\": \"2020-09-25T05:58:13Z\",\"id\": \"2148361678\",\"gatewayAuthReqParams\": \"{\\\"euler-api-gateway\\\":\\\"fehfioe\\\"}\",\"dateCreated\": \"2020-09-25T05:58:13Z\",\"challengesAttempted\": 0,\"canAcceptResponse\": true}}"

makeLogMaskingConfig :: CType.MaskKeyType -> [Text] -> Text ->  CType.LogMaskingConfig
makeLogMaskingConfig keyType keyList maskText =
  CType.LogMaskingConfig
    { _maskKeys =  HashSet.fromList keyList
    , _maskText =  Just maskText
    , _keyType  =  keyType
    }
