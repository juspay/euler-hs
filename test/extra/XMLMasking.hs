module XMLMasking where

import           EulerHS.Masking
import           EulerHS.Prelude
import           Test.Hspec
import qualified Data.HashSet as HS


spec :: Spec
spec = do
    describe "maskXMLTest" $ do
        it "Mask Tag along with multiple attributes in same tag" $ do
            maskXMLText mbHashSet1 testXML `shouldBe` hashSet1Result
        it "Mask multiple attributes in same tag" $ do
            maskXMLText mbHashSet2 testXML `shouldBe` hashSet2Result
        it "Mask multiple attributes in different tags" $ do
            maskXMLText mbHashSet3 testXML `shouldBe` hashSet3Result
        it "No key present to mask" $ do
            maskXMLText mbHashSet4 testXML `shouldBe` hashSet4Result
        it "Mask tag only in tag with multiple attributes" $ do
            maskXMLText mbHashSet5 testXML `shouldBe` hashSet5Result
        it "Mask tag only in tag without any attributes" $ do
            maskXMLText mbHashSet6 testXML `shouldBe` hashSet6Result

testWithIO :: IO ()
testWithIO = do
  liftIO $ print $ maskXMLText mbHashSet1 testXML
  liftIO $ print $ maskXMLText mbHashSet2 testXML
  liftIO $ print $ maskXMLText mbHashSet3 testXML
  liftIO $ print $ maskXMLText mbHashSet4 testXML
  liftIO $ print $ maskXMLText mbHashSet5 testXML
  liftIO $ print $ maskXMLText mbHashSet6 testXML

mbHashSet1 :: Maybe (HashSet Text)
mbHashSet1 = Just $ HS.fromList ["dummyTag:dummyTag2", "attr1", "attr2"]

mbHashSet2 :: Maybe (HashSet Text)
mbHashSet2 = Just $ HS.fromList ["attr1", "attr2"]

mbHashSet3 :: Maybe (HashSet Text)
mbHashSet3 = Just $ HS.fromList ["attr2", "attr3"]

mbHashSet4 :: Maybe (HashSet Text)
mbHashSet4 = Just $ HS.fromList ["notInXml"]

mbHashSet5 :: Maybe (HashSet Text)
mbHashSet5 = Just $ HS.fromList ["dummyTag:dummyTag2"]

mbHashSet6 :: Maybe (HashSet Text)
mbHashSet6 = Just $ HS.fromList ["dummyTag"]

testXML :: Text
testXML =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?><soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\"><soapenv:Header><wsse:Security soapenv:mustUnderstand=\"1\" xmlns:wsse=\"http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd\"><dummyTag:dummyTag2 attr1=\"attribute /1 value\" attr2=\"attribute /2 value\">some / val / with slash</dummyTag:dummyTag2><nestedTag attr3=\"attr 3 value\"><dummyTag>Dummy Tag Value</dummyTag></nestedTag>"

hashSet1Result :: Text
hashSet1Result = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\"><soapenv:Header><wsse:Security soapenv:mustUnderstand=\"1\" xmlns:wsse=\"http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd\"><dummyTag:dummyTag2 attr1=\"FILTERED\" attr2=\"FILTERED\">FILTERED</dummyTag:dummyTag2><nestedTag attr3=\"attr 3 value\"><dummyTag>Dummy Tag Value</dummyTag></nestedTag>"

hashSet2Result :: Text
hashSet2Result = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\"><soapenv:Header><wsse:Security soapenv:mustUnderstand=\"1\" xmlns:wsse=\"http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd\"><dummyTag:dummyTag2 attr1=\"FILTERED\" attr2=\"FILTERED\">some / val / with slash</dummyTag:dummyTag2><nestedTag attr3=\"attr 3 value\"><dummyTag>Dummy Tag Value</dummyTag></nestedTag>"

hashSet3Result :: Text
hashSet3Result = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\"><soapenv:Header><wsse:Security soapenv:mustUnderstand=\"1\" xmlns:wsse=\"http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd\"><dummyTag:dummyTag2 attr1=\"attribute /1 value\" attr2=\"FILTERED\">some / val / with slash</dummyTag:dummyTag2><nestedTag attr3=\"FILTERED\"><dummyTag>Dummy Tag Value</dummyTag></nestedTag>"

hashSet4Result :: Text
hashSet4Result = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\"><soapenv:Header><wsse:Security soapenv:mustUnderstand=\"1\" xmlns:wsse=\"http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd\"><dummyTag:dummyTag2 attr1=\"attribute /1 value\" attr2=\"attribute /2 value\">some / val / with slash</dummyTag:dummyTag2><nestedTag attr3=\"attr 3 value\"><dummyTag>Dummy Tag Value</dummyTag></nestedTag>"

hashSet5Result :: Text
hashSet5Result = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\"><soapenv:Header><wsse:Security soapenv:mustUnderstand=\"1\" xmlns:wsse=\"http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd\"><dummyTag:dummyTag2 attr1=\"attribute /1 value\" attr2=\"attribute /2 value\">FILTERED</dummyTag:dummyTag2><nestedTag attr3=\"attr 3 value\"><dummyTag>Dummy Tag Value</dummyTag></nestedTag>"

hashSet6Result :: Text
hashSet6Result = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\"><soapenv:Header><wsse:Security soapenv:mustUnderstand=\"1\" xmlns:wsse=\"http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd\"><dummyTag:dummyTag2 attr1=\"attribute /1 value\" attr2=\"attribute /2 value\">some / val / with slash</dummyTag:dummyTag2><nestedTag attr3=\"attr 3 value\"><dummyTag>FILTERED</dummyTag></nestedTag>"