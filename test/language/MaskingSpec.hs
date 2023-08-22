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
  [ ]

expectedOutput' :: A.Value
expectedOutput' =  A.object
  [ ]

expectedOutput'' :: A.Value
expectedOutput'' =  A.object
  [ ]

expectedOutput''' :: A.Value
expectedOutput''' =  A.String  "Logging Not Support For this content application/html"


inputJSON :: LBS.ByteString
inputJSON = ""

makeLogMaskingConfig :: CType.MaskKeyType -> [Text] -> Text ->  CType.LogMaskingConfig
makeLogMaskingConfig keyType keyList maskText =
  CType.LogMaskingConfig
    { _maskKeys =  HashSet.fromList keyList
    , _maskText =  Just maskText
    , _keyType  =  keyType
    }
