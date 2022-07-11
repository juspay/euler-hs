{-# LANGUAGE DeriveFunctor #-}

module EulerHS.Core.PubSub.Language where

import           EulerHS.Prelude

import qualified Database.Redis as R
import qualified EulerHS.Types as T

newtype Channel        = Channel        ByteString
newtype ChannelPattern = ChannelPattern ByteString
newtype Payload        = Payload        ByteString

data PubSubF next
  = Publish     Channel         Payload            (Either T.KVDBReply Integer -> next)
  | Subscribe  [Channel       ] R.MessageCallback  (IO ()                      -> next)
  | PSubscribe [ChannelPattern] R.PMessageCallback (IO ()                      -> next)
  deriving Functor

type PubSub = F PubSubF

publish :: Channel -> Payload -> PubSub (Either T.KVDBReply Integer)
publish channel payload = liftFC $ Publish channel payload id

subscribe :: [Channel] -> R.MessageCallback -> PubSub (IO ())
subscribe channels cb = liftFC $ Subscribe channels cb id

psubscribe :: [ChannelPattern] -> R.PMessageCallback -> PubSub (IO ())
psubscribe channels cb = liftFC $ PSubscribe channels cb id


