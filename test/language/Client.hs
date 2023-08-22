{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators      #-}

module Client
  (
    Book(..), User(..),
    port, externalServerPort, serverPort, api, server,
    getUser, getBook
  ) where

import           EulerHS.Prelude
import           EulerHS.Types (EulerClient)
import           EulerHS.ApiHelpers (client)
import           Servant.API (Get, JSON, type (:>), (:<|>) ((:<|>)))
import           Servant.Mock (mock)
import           Servant.Server (Server)
import           Test.QuickCheck (Arbitrary (arbitrary, shrink))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary,
                                                    genericShrink)
import           Test.QuickCheck.Instances.Text ()

data User = User {
  firstName :: {-# UNPACK #-} !Text,
  lastName  :: {-# UNPACK #-} !Text ,
  userGUID  :: {-# UNPACK #-} !Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary User where
  arbitrary = genericArbitrary
  shrink = genericShrink

data Book = Book {
  author :: {-# UNPACK #-} !Text,
  name   :: {-# UNPACK #-} !Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary Book where
  arbitrary = genericArbitrary
  shrink = genericShrink

type API = "user" :> Get '[JSON] User
      :<|> "book" :> Get '[JSON] Book

-- | port number to bind test server's socket
serverPort :: Int
serverPort = 8081

-- | external server for some tests (for local use only)
externalServerPort :: Int
externalServerPort = serverPort

-- | port to connect to when running tests
port :: Int
port = serverPort

api :: Proxy API
api = Proxy

-- This rather bizarre construction is needed because of the way the 'client'
-- function works. The third line is a pattern match on the result, which a
-- sorta-kinda Servant API type, with additional wrapping. However, because it's
-- a value match, the identifiers are promoted to the top level, and thus need
-- their own signatures. - Koz
getUser :: EulerClient User
getBook :: EulerClient Book
(getUser :<|> getBook) = client api

context :: Proxy '[]
context = Proxy

server :: Server API
server = mock api context
