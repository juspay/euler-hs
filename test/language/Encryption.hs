
module Encryption where

import           EulerHS.Prelude
import           Test.Hspec (Spec, describe, it, shouldBe)
import qualified EulerHS.Encryption as EE

spec :: Spec
spec = do
    describe "Testing AES CTR encryption and decryption" $ do
      it "Should match plaintext and decryptedtext" $ do
        let plaintext = "Plaintext Data for Encryption" :: Text
            key = "12345678901234561234567890123456" :: Text  -- 256-bit key
            iv = "1234567890123456" :: Text  -- 128-bit IV
            eCiphertext = EE.aesEncryptText key iv plaintext
            ciphertext = (either (const "Invalid ciphertext") id eCiphertext)
            eDecryptedtext = EE.aesDecryptText key iv ciphertext
            decryptedtext = (either (const "Invalid decryptedtext") id eDecryptedtext)
        plaintext `shouldBe` decryptedtext
