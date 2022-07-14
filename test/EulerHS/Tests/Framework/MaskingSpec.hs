module EulerHS.Tests.Framework.MaskingSpec (spec) where

import EulerHS.Prelude hiding (readFile)
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
  
expectedOutput :: Text
expectedOutput = "{\"status\":\"INIT\",\"txnId\":\"paypal-tatapay_740-1\",\"txnDetailId\":\"2148428442\",\"responseAttempted\":{\"lastUpdated\":\"2020-09-25T05:58:13Z\",\"gatewayAuthReqParams\":\"{\\\"euler-api-gateway\\\":\\\"fehfioe\\\"}\",\"dateCreated\":\"2020-09-25T05:58:13Z\",\"challengesAttempted\":0,\"canAcceptResponse\":true,\"id\":\"$$$\"},\"version\":0,\"url1\":\"$$$\",\"type\":\"VBV\"}"

expectedOutput' :: Text
expectedOutput' = "{\"status\":\"$**$\",\"txnId\":\"$**$\",\"txnDetailId\":\"$**$\",\"responseAttempted\":\"$**$\",\"version\":\"$**$\",\"url1\":[{\"a\":\"b\"},\"wefojoefwj\"],\"type\":\"$**$\"}"

expectedOutput'' :: Text
expectedOutput'' = "{\"status\":\"INIT\",\"txnId\":\"paypal-tatapay_740-1\",\"txnDetailId\":\"2148428442\",\"responseAttempted\":{\"lastUpdated\":\"2020-09-25T05:58:13Z\",\"gatewayAuthReqParams\":\"{\\\"euler-api-gateway\\\":\\\"fehfioe\\\"}\",\"dateCreated\":\"2020-09-25T05:58:13Z\",\"challengesAttempted\":0,\"canAcceptResponse\":true,\"id\":\"2148361678\"},\"version\":0,\"url1\":[{\"a\":\"b\"},\"wefojoefwj\"],\"type\":\"VBV\"}"

expectedOutput''' :: Text
expectedOutput''' = "Logging Not Support For this content"

inputJSON :: LBS.ByteString
inputJSON = "{\"version\": 0,\"url1\": [{\"a\":\"b\"},\"wefojoefwj\"],\"type\": \"VBV\",\"txnId\": \"paypal-tatapay_740-1\",\"txnDetailId\": \"2148428442\",\"status\": \"INIT\",\"responseAttempted\": {\"lastUpdated\": \"2020-09-25T05:58:13Z\",\"id\": \"2148361678\",\"gatewayAuthReqParams\": \"{\\\"euler-api-gateway\\\":\\\"fehfioe\\\"}\",\"dateCreated\": \"2020-09-25T05:58:13Z\",\"challengesAttempted\": 0,\"canAcceptResponse\": true}}"

makeLogMaskingConfig :: CType.MaskKeyType -> [Text] -> Text ->  CType.LogMaskingConfig
makeLogMaskingConfig keyType keyList maskText =
  CType.LogMaskingConfig
    { _maskKeys =  HashSet.fromList keyList
    , _maskText =  Just maskText
    , _keyType  =  keyType
    }