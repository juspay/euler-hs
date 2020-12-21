{-# LANGUAGE DeriveDataTypeable #-}
module EulerHS.Testing.Types where

import qualified Data.ByteString.Lazy as BSL
import           Data.Data
import           EulerHS.Prelude

data FlowMockedValues' = FlowMockedValues'
  { mockedCallServantAPI :: [Any]
  , mockedRunIO          :: [Any]
  , mockedGetOption      :: [BSL.ByteString]
  , mockedGenerateGUID   :: [Text]
  , mockedRunSysCmd      :: [String]
  } deriving (Generic, Typeable)



type FlowMockedValues = MVar FlowMockedValues'
