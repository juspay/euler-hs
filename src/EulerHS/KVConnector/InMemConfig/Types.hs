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
import qualified Streamly.Data.Serialize.Instances ()
import qualified Streamly.Data.MutByteArray as MBA

type KeysRequiringRedisFetch = Text

data IMCEnabledTables = IMCEnabledTables
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data IsIMCEnabled = IsIMCEnabled
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data InMemCacheResult table where
  EntryValid :: (Show table) => table -> InMemCacheResult table
  EntryExpired :: (Show table) => table -> Text -> InMemCacheResult table
  EntryNotFound :: Text -> InMemCacheResult table
  TableIneligible :: InMemCacheResult table
  UnknownError :: MeshError -> InMemCacheResult table


data QueryType = 
    FIND_ALL
  | FIND_ONE
  deriving stock (Show, Generic)


-- data InMemCacheResult table => (Show table) = EntryValid (table) | 
--                         EntryExpired (table)  KeyForInMemConfig | 
--                         EntryNotFound KeyForInMemConfig |
--                         TableIneligible | 
--                         UnknownError MeshError 
--   deriving (Show)

type KeyForInMemConfig = Text

data LooperStarted  = LooperStarted Text
  deriving (Generic, A.ToJSON, Typeable, Show)

data RecordId = RecordId Text
  deriving (Generic, Typeable, Show, Eq, ToJSON, FromJSON)

type LatestRecordId = Text
type RecordKeyValues = (Text, ByteString)

data ImcStreamCommand = ImcInsert | ImcDelete
  deriving (Generic, Typeable, Show, Eq, ToJSON, FromJSON)

data ImcStreamValue table = 
  ImcStreamValue {
    command :: ImcStreamCommand,
    tableRow :: table,
    cacheKeyPrefixes :: Maybe [Text]
}
  deriving (Generic, Typeable, Show, Eq, ToJSON, FromJSON)
--------------------------------------------------------------------------------
-- TH Instances
--------------------------------------------------------------------------------

$(MBA.deriveSerialize [d|instance MBA.Serialize IMCEnabledTables|])
$(MBA.deriveSerialize [d|instance MBA.Serialize IsIMCEnabled|])
$(MBA.deriveSerialize [d|instance MBA.Serialize LooperStarted|])
$(MBA.deriveSerialize [d|instance MBA.Serialize RecordId|])

instance T.OptionEntity IMCEnabledTables [Text]
instance T.OptionEntity IsIMCEnabled Bool
instance OptionEntity LooperStarted Bool
instance OptionEntity RecordId Text
