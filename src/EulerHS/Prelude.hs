{-# OPTIONS -fno-warn-orphans #-}

{- |
Module      :  EulerHS.Prelude
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable

Custom prelude based on @universum@ by Serokell.

In contrast with the latter, it exports unsafe versions of such functions as
@head@, @last@ etc. It also has some other tiny changes here and there.
You may want to get familiar with the @universum@ documentation first.
-}

module EulerHS.Prelude
  -- TODO: This entire export lists needs to be explicit
  ( module X
  , liftFC
  , catchAny
  -- JSON
  , stripLensPrefixOptions
  , stripAllLensPrefixOptions
  , jsonSetField
  , encodeJSON
  , decodeJSON
  ) where

import           Control.Concurrent as X (ThreadId, forkIO, killThread,
                                          threadDelay)
import           Control.Concurrent.STM as X (retry)
import           Control.Concurrent.STM.TChan as X (TChan, newTChan, newTChanIO,
                                                    readTChan, tryReadTChan,
                                                    writeTChan)
import           Control.Concurrent.STM.TMVar as X (TMVar, newEmptyTMVar,
                                                    newEmptyTMVarIO, newTMVar,
                                                    newTMVarIO, putTMVar,
                                                    readTMVar, takeTMVar,
                                                    tryReadTMVar)
import           Control.Concurrent.STM.TVar as X (modifyTVar)
import           Control.Exception as X (SomeException (..))
import           Control.Lens as X (at, (.=))
import           Control.Lens.TH as X (makeFieldsNoPrefix, makeLenses)
import           Control.Monad as X (liftM, unless, void, when)
import           Control.Monad.Free as X (Free (..), foldFree, liftF)
import           Control.Monad.Free.Church as X (F (..), foldF, fromF, iter,
                                                 iterM, retract)
import           Control.Newtype.Generics as X (Newtype, O, pack, unpack)
import           Data.Aeson as X (FromJSON, FromJSONKey, ToJSON, ToJSONKey,
                                  genericParseJSON, genericToJSON, parseJSON,
                                  toJSON)
import           Data.Function as X ((&))
import           Data.Kind as X (Type)
import           Data.Maybe as X (fromJust, fromMaybe)
import           Data.Serialize as X (Serialize)
import           Fmt as X ((+|), (+||), (|+), (||+))
import           GHC.Base as X (until)
import           GHC.Generics as X (Generic)
import           Text.Read as X (read, readsPrec)

-- includes Data.IORef
import           Universum as X hiding (All, Option, Set, Type, head, init,
                                 last, set, tail, trace, catchAny)
import           Universum (catchAny)
import           Universum.Functor.Fmap as X ((<<$>>))
import           Universum.Unsafe as X (head, init, last, tail, (!!))

import           EulerHS.Extra.Aeson (
  stripLensPrefixOptions, stripAllLensPrefixOptions, jsonSetField, encodeJSON, decodeJSON)

import qualified Control.Monad.Free.Church as CF
import qualified Control.Monad.Free.Class as MF




-- Lift for Church encoded Free
liftFC :: (Functor f, MF.MonadFree f m) => f a -> m a
liftFC = CF.liftF