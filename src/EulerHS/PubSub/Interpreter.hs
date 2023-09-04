{- |
Module      :  EulerHS.PubSub.Interpreter
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module EulerHS.PubSub.Interpreter
  (
    interpretPubSubF,
    runPubSub
  ) where

import           Data.Coerce (coerce)
import qualified Database.Redis as R
import           EulerHS.Prelude
import           EulerHS.PubSub.Language (Channel (Channel),
                                          ChannelPattern (ChannelPattern),
                                          Payload (Payload), PubSub,
                                          PubSubF (PSubscribe, Publish, Subscribe))
import qualified EulerHS.Types as T

interpretPubSubF
  :: R.PubSubController
  -> R.Connection
  -> PubSubF a
  -> IO a
interpretPubSubF pubSubController conn = \case
  Publish ch pl next       ->
    fmap (next . first T.hedisReplyToKVDBReply) .
    R.runRedis conn .
    R.publish (coerce ch) .
    coerce $ pl
  Subscribe chs cb next    ->
    fmap next .
    R.addChannelsAndWait pubSubController (zip (coerce chs) . repeat $ cb) $ []
  PSubscribe patts cb next ->
    fmap next .
    R.addChannelsAndWait pubSubController [] .
    zip (coerce patts) .
    repeat $ cb

runPubSub :: R.PubSubController -> R.Connection -> PubSub a -> IO a
runPubSub pubSubController conn = foldF (interpretPubSubF pubSubController conn)
