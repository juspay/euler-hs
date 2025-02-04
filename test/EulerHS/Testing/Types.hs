{-# LANGUAGE DeriveDataTypeable #-}
module EulerHS.Testing.Types where

import           Data.Data
import           EulerHS.Prelude

data FlowMockedValues' = FlowMockedValues'
  { mockedCallServantAPI :: [Any]
  , mockedRunIO          :: [Any]
  , mockedGetOption      :: [ByteString]
  , mockedGenerateGUID   :: [Text]
  , mockedRunSysCmd      :: [String]
  , mockedDNS            :: [Maybe Text]
  } deriving (Generic, Typeable)



type FlowMockedValues = MVar FlowMockedValues'
