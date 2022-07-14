{-# LANGUAGE DeriveAnyClass #-}

module EulerHS.TestData.Types where

import qualified Data.Aeson as A
import           EulerHS.Prelude
import           EulerHS.Types


data UrlKey = UrlKey
  deriving (Generic, Typeable, Show, Eq, ToJSON)


data TestStringKey = TestStringKey
  deriving (Generic, Typeable, Show, Eq, ToJSON)

data TestStringKey2 = TestStringKey2
  deriving (Generic, Typeable, Show, Eq, ToJSON)


data TestIntKey = TestIntKey
  deriving (Generic, Typeable, Show, Eq, ToJSON)

data TestIntKey2 = TestIntKey2
  deriving (Generic, Typeable, Show, Eq, ToJSON)


data TestStringKeyAnotherEnc = TestStringKeyAnotherEnc
  deriving (Generic, Typeable, Show, Eq)

data TestStringKey2AnotherEnc = TestStringKey2AnotherEnc
  deriving (Generic, Typeable, Show, Eq)


data TestKeyWithStringPayload = TestKeyWithStringPayload String
  deriving (Generic, Typeable, Show, Eq, ToJSON)

data TestKeyWithIntPayload = TestKeyWithIntPayload Int
  deriving (Generic, Typeable, Show, Eq, ToJSON)


data TestKeyWithStringPayloadAnotherEnc = TestKeyWithStringPayloadAnotherEnc String
  deriving (Generic, Typeable, Show, Eq)

data TestKeyWithIntPayloadAnotherEnc = TestKeyWithIntPayloadAnotherEnc Int
  deriving (Generic, Typeable, Show, Eq)



newtype NTTestKeyWithStringPayload = NTTestKeyWithStringPayload String
  deriving (Generic, Typeable, Show, Eq, ToJSON)

newtype NTTestKeyWithIntPayload = NTTestKeyWithIntPayload Int
  deriving (Generic, Typeable, Show, Eq, ToJSON)


newtype NTTestKeyWithStringPayloadAnotherEnc = NTTestKeyWithStringPayloadAnotherEnc String
  deriving (Generic, Typeable, Show, Eq)

newtype NTTestKeyWithIntPayloadAnotherEnc = NTTestKeyWithIntPayloadAnotherEnc Int
  deriving (Generic, Typeable, Show, Eq)



instance A.ToJSON TestStringKeyAnotherEnc
  where toJSON = A.genericToJSON $ A.defaultOptions { A.tagSingleConstructors = True }

instance A.ToJSON TestStringKey2AnotherEnc
  where toJSON = A.genericToJSON $ A.defaultOptions { A.tagSingleConstructors = True }


instance A.ToJSON TestKeyWithStringPayloadAnotherEnc
  where toJSON = A.genericToJSON $ A.defaultOptions { A.tagSingleConstructors = True }

instance A.ToJSON TestKeyWithIntPayloadAnotherEnc
  where toJSON = A.genericToJSON $ A.defaultOptions { A.tagSingleConstructors = True }


instance A.ToJSON NTTestKeyWithStringPayloadAnotherEnc
  where toJSON = A.genericToJSON $ A.defaultOptions { A.tagSingleConstructors = True }

instance A.ToJSON NTTestKeyWithIntPayloadAnotherEnc
  where toJSON = A.genericToJSON $ A.defaultOptions { A.tagSingleConstructors = True }

instance OptionEntity UrlKey String
instance OptionEntity TestStringKey String
instance OptionEntity TestStringKey2 String
instance OptionEntity TestIntKey Int
instance OptionEntity TestIntKey2 Int

instance OptionEntity TestStringKeyAnotherEnc String
instance OptionEntity TestStringKey2AnotherEnc String

instance OptionEntity TestKeyWithStringPayload String
instance OptionEntity TestKeyWithIntPayload String

instance OptionEntity TestKeyWithStringPayloadAnotherEnc String
instance OptionEntity TestKeyWithIntPayloadAnotherEnc String

instance OptionEntity NTTestKeyWithStringPayload String
instance OptionEntity NTTestKeyWithIntPayload Int

instance OptionEntity NTTestKeyWithStringPayloadAnotherEnc String
instance OptionEntity NTTestKeyWithIntPayloadAnotherEnc Int



data TestKVals = TestKVals
  { mbTestStringKey                          :: Maybe String
  , mbTestStringKey2                         :: Maybe String
  , mbTestIntKey                             :: Maybe Int
  , mbTestIntKey2                            :: Maybe Int
  , mbTestStringKeyAnotherEnc                :: Maybe String
  , mbTestStringKey2AnotherEnc               :: Maybe String
  , mbTestKeyWithStringPayloadS1             :: Maybe String
  , mbTestKeyWithStringPayloadS2             :: Maybe String
  , mbTestKeyWithIntPayloadS1                :: Maybe String
  , mbTestKeyWithIntPayloadS2                :: Maybe String
  , mbTestKeyWithStringPayloadAnotherEncS1   :: Maybe String
  , mbTestKeyWithStringPayloadAnotherEncS2   :: Maybe String
  , mbTestKeyWithIntPayloadAnotherEncS1      :: Maybe String
  , mbTestKeyWithIntPayloadAnotherEncS2      :: Maybe String
  , mbNTTestKeyWithStringPayloadS1           :: Maybe String
  , mbNTTestKeyWithStringPayloadS2           :: Maybe String
  , mbNTTestKeyWithIntPayloadS1              :: Maybe Int
  , mbNTTestKeyWithIntPayloadS2              :: Maybe Int
  , mbNTTestKeyWithStringPayloadAnotherEncS1 :: Maybe String
  , mbNTTestKeyWithStringPayloadAnotherEncS2 :: Maybe String
  , mbNTTestKeyWithIntPayloadAnotherEncS1    :: Maybe Int
  , mbNTTestKeyWithIntPayloadAnotherEncS2    :: Maybe Int
  }
  deriving (Show, Eq)

----------------------------------



