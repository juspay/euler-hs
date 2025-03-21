{-# LANGUAGE DeriveDataTypeable #-}
module EulerHS.Testing.Types where

import           Data.Data
import           EulerHS.Prelude
import           Data.UUID (UUID)

data FlowMockedValues' = FlowMockedValues'
  { mockedCallServantAPI :: [Any]
  , mockedRunIO          :: [Any]
  , mockedGetOption      :: [ByteString]
  , mockedGenerateGUID   :: [UUID]
  , mockedRunSysCmd      :: [String]
  } deriving (Generic, Typeable)



type FlowMockedValues = MVar FlowMockedValues'
