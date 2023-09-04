{- |
Module      :  EulerHS.PubSub.Language
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module EulerHS.PubSub.Language where

import qualified Database.Redis as R
import           EulerHS.KVDB.Types (KVDBReply)
import           EulerHS.Prelude

newtype Channel        = Channel        ByteString
newtype ChannelPattern = ChannelPattern ByteString
newtype Payload        = Payload        ByteString

data PubSubF next
  = Publish     Channel         Payload            (Either KVDBReply Integer -> next)
  | Subscribe  [Channel       ] R.MessageCallback  (IO ()                      -> next)
  | PSubscribe [ChannelPattern] R.PMessageCallback (IO ()                      -> next)
  deriving Functor

type PubSub = F PubSubF

publish :: Channel -> Payload -> PubSub (Either KVDBReply Integer)
publish channel payload = liftFC $ Publish channel payload id

subscribe :: [Channel] -> R.MessageCallback -> PubSub (IO ())
subscribe channels cb = liftFC $ Subscribe channels cb id

psubscribe :: [ChannelPattern] -> R.PMessageCallback -> PubSub (IO ())
psubscribe channels cb = liftFC $ PSubscribe channels cb id
