module EulerHS.Core.PubSub.Interpreter where

import           EulerHS.Prelude

import           Data.Coerce
import qualified Database.Redis as R
import qualified EulerHS.Core.Playback.Machine as P
import qualified EulerHS.Types as T

import           EulerHS.Core.PubSub.Entries
import           EulerHS.Core.PubSub.Language


interpretPubSubF
  :: T.RunMode
  -> R.PubSubController
  -> R.Connection
  -> PubSubF a
  -> IO a
interpretPubSubF runMode _ conn (Publish ch pl next) =
    fmap next $
      P.withRunMode runMode (mkPublishEntry bsch bspl) $
        fmap (first T.hedisReplyToKVDBReply) $ R.runRedis conn $ R.publish bsch bspl
  where
    bsch = coerce ch
    bspl = coerce pl

interpretPubSubF runMode pubSubController _ (Subscribe chs cb next) =
    fmap next $
      P.withRunMode runMode (mkSubscribeEntry bsChs) $
        R.addChannelsAndWait pubSubController (zip bsChs $ repeat cb) []
  where
    bsChs = coerce chs

interpretPubSubF runMode pubSubController _ (PSubscribe patts cb next) =
    fmap next $
      P.withRunMode runMode (mkPSubscribeEntry bsPatts) $
        R.addChannelsAndWait pubSubController [] (zip bsPatts $ repeat cb)
  where
    bsPatts = coerce patts

runPubSub :: T.RunMode -> R.PubSubController -> R.Connection -> PubSub a -> IO a
runPubSub runMode pubSubController conn =
  foldF (interpretPubSubF runMode pubSubController conn)





