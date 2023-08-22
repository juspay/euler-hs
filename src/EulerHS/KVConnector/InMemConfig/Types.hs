{- |
Module      :  EulerHS.KVConnector.InMemConfig.Types
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE  ScopedTypeVariables #-}
module EulerHS.KVConnector.InMemConfig.Types 

    where

import           EulerHS.Prelude hiding (maximum)
import  Data.Aeson as A
import           EulerHS.Options (OptionEntity)
import           EulerHS.KVDB.Types (MeshError)
import qualified EulerHS.Types as T

type KeysRequiringRedisFetch = Text

data IMCEnabledTables = IMCEnabledTables
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance T.OptionEntity IMCEnabledTables [Text]

data IsIMCEnabled = IsIMCEnabled
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance T.OptionEntity IsIMCEnabled Bool

data InMemCacheResult table where
  EntryValid :: (Show table) => table -> InMemCacheResult table
  EntryExpired :: (Show table) => table -> Text -> InMemCacheResult table
  EntryNotFound :: Text -> InMemCacheResult table
  TableIneligible :: InMemCacheResult table
  UnknownError :: MeshError -> InMemCacheResult table



type KeyForInMemConfig = Text

data LooperStarted  = LooperStarted Text
  deriving (Generic, A.ToJSON, Typeable, Show)

instance OptionEntity LooperStarted Bool

data RecordId = RecordId Text
  deriving (Generic, Typeable, Show, Eq, ToJSON, FromJSON)

instance OptionEntity RecordId Text

type LatestRecordId = Text
type RecordKeyValues = (Text, ByteString)

data ImcStreamCommand = ImcInsert | ImcDelete
  deriving (Generic, Typeable, Show, Eq, ToJSON, FromJSON)


data ImcStreamValue table = 
  ImcStreamValue {
    command :: ImcStreamCommand,
    tableRow :: table 
}
  deriving (Generic, Typeable, Show, Eq, ToJSON, FromJSON)