{-# LANGUAGE DeriveFunctor #-}

{- |
Module      :  EulerHS.Core.PubSub.Language
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2021
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable

Experimental PubSub subsystem (Redis-based)

This module is internal and should not imported in the projects.
Import 'EulerHS.Language' instead.
-}

module EulerHS.Core.PubSub.Language where

import           EulerHS.Prelude

import qualified Database.Redis as R
import qualified EulerHS.Types as T

-- | Channel
newtype Channel        = Channel        ByteString

-- | Channel pattern
newtype ChannelPattern = ChannelPattern ByteString

-- | Payload
newtype Payload        = Payload        ByteString

-- | Algebra for the PubSub mechanism
data PubSubF next
  = Publish     Channel         Payload            (Either T.KVDBReply Integer -> next)
  | Subscribe  [Channel       ] R.MessageCallback  (IO ()                      -> next)
  | PSubscribe [ChannelPattern] R.PMessageCallback (IO ()                      -> next)
  deriving Functor

-- | PubSub language
type PubSub = F PubSubF

-- | Publish some payload into channel
publish :: Channel -> Payload -> PubSub (Either T.KVDBReply Integer)
publish channel payload = liftFC $ Publish channel payload id

-- | Subscribe to channel
subscribe :: [Channel] -> R.MessageCallback -> PubSub (IO ())
subscribe channels cb = liftFC $ Subscribe channels cb id

-- | Subscribe to channels with this pattern
psubscribe :: [ChannelPattern] -> R.PMessageCallback -> PubSub (IO ())
psubscribe channels cb = liftFC $ PSubscribe channels cb id
