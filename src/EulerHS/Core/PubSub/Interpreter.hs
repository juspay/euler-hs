{- |
Module      :  EulerHS.Core.PubSub.Interpreter
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module EulerHS.Core.PubSub.Interpreter where

import           EulerHS.Prelude

import           Data.Coerce
import qualified Database.Redis as R
import qualified EulerHS.Types as T

import           EulerHS.Core.PubSub.Language


interpretPubSubF
  :: R.PubSubController
  -> R.Connection
  -> PubSubF a
  -> IO a
interpretPubSubF _ conn (Publish ch pl next) =
    fmap next $
        fmap (first T.hedisReplyToKVDBReply) $ R.runRedis conn $ R.publish bsch bspl
  where
    bsch = coerce ch
    bspl = coerce pl

interpretPubSubF pubSubController _ (Subscribe chs cb next) =
    fmap next $
        R.addChannelsAndWait pubSubController (zip bsChs $ repeat cb) []
  where
    bsChs = coerce chs

interpretPubSubF pubSubController _ (PSubscribe patts cb next) =
    fmap next $
        R.addChannelsAndWait pubSubController [] (zip bsPatts $ repeat cb)
  where
    bsPatts = coerce patts

runPubSub :: R.PubSubController -> R.Connection -> PubSub a -> IO a
runPubSub pubSubController conn =
  foldF (interpretPubSubF pubSubController conn)





