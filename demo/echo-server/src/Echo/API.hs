{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Echo.API where

import EulerHS.Prelude
import Servant

import Echo.Domain


data EchoRequest = EchoRequest
  { phrase :: Text
  , number :: Int
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
