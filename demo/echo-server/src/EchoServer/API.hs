{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module EchoServer.API where

import EulerHS.Prelude
import Servant


data EchoRequest = EchoRequest
  { phrase :: Text
  , number :: Int
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


data EchoMessage = EchoMessage
  { phrase :: Text
  , number :: Int
  , easterEgg :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


type EchoAPI =
  ( "get_echo"
    :> QueryParam "phrase" Text
    :> QueryParam "number" Int
    :> Get '[JSON] EchoMessage
  )
  :<|>
  ( "post_echo"
    :> ReqBody '[JSON] EchoRequest
    :> Post '[JSON] EchoMessage
  )


echoAPI :: Proxy EchoAPI
echoAPI = Proxy
