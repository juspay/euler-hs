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
  [ "status" .= ("S" :: Text)
  , "txnId" .= ("jjx-jjy_007" :: Text)
  , "txnDetailId" .= ("9999999" :: Text)
  , "responseAttempted" .= A.object
      [ "lastUpdated" .= ("2020-09-25T05:58:13Z" :: Text)
      , "dateCreated" .= ("2020-09-25T05:58:13Z"  :: Text)
      ,"id" .= ("$$$"  :: Text)
      ]
  , "version" .= (0 :: Int)
  , "url1" .= ("$$$" :: Text)
  , "type" .= ("ABC" :: Text)
  ]

expectedOutput' :: A.Value
expectedOutput' =  A.object
  [ "status" .= ("$**$" :: Text)
  , "txnId" .= ("$**$" :: Text)
  , "txnDetailId" .= ("$**$" :: Text)
  , "version" .= ("$**$" :: Text)
  , "url1" .= [A.object ["a" .= ("b" :: Text)], A.String "wefojoefwj"]
  , "type" .= ("$**$" :: Text)
  ]

expectedOutput'' :: A.Value
expectedOutput'' =  A.object
  [ "status" .= ("S" :: Text)
  , "txnId" .= ("jjx-jjy_007" :: Text)
  , "txnDetailId" .= ("9999999" :: Text)
  , "responseAttempted" .= A.object
      [ "lastUpdated" .= ("2020-09-25T05:58:13Z" :: Text)
      , "dateCreated" .= ("2020-09-25T05:58:13Z"  :: Text)
      ,"id" .= ("1111111"  :: Text)
      ]
  , "version" .= (0 :: Int)
  , "url1" .= [A.object ["a" .= ("b" :: Text)], A.String "wefojoefwj"]
  , "type" .= ("ABC" :: Text)
  ]

expectedOutput''' :: A.Value
expectedOutput''' =  A.String  "Logging Not Support For this content application/html"


inputJSON :: LBS.ByteString
inputJSON = "{\"version\": 0,\"url1\": [{\"a\":\"b\"},\"wefojoefwj\"],\"type\": \"ABC\",\"txnId\": \"jjx-jjy_007\",\"txnDetailId\": \"9999999\",\"status\": \"S\",\"responseAttempted\": {\"lastUpdated\": \"2020-09-25T05:58:13Z\",\"id\": \"1111111\",\"dateCreated\": \"2020-09-25T05:58:13Z\"}}"

makeLogMaskingConfig :: CType.MaskKeyType -> [Text] -> Text ->  CType.LogMaskingConfig
makeLogMaskingConfig keyType keyList maskText =
  CType.LogMaskingConfig
    { _maskKeys =  HashSet.fromList keyList
    , _maskText =  Just maskText
    , _keyType  =  keyType
    }
