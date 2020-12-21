module EulerHS.TestData.API.Client where

import           EulerHS.Prelude

import           EulerHS.Types
import           Servant.API
import           Servant.Mock (mock)
import           Servant.Server (Server)

import           EulerHS.TestData.Types


type API = "user" :> Get '[JSON] User
      :<|> "book" :> Get '[JSON] Book


port :: Int
port = 8081

api :: Proxy API
api = Proxy

context :: Proxy '[]
context = Proxy

getUser :: EulerClient User
getBook :: EulerClient Book
(getUser :<|> getBook) = client api

server :: Server API
server = mock api context
