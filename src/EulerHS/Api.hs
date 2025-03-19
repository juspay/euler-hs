{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE  RankNTypes #-}
{-# LANGUAGE  ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}

module EulerHS.Api where


import qualified Data.Aeson as A
import           GHC.Generics ()
import           EulerHS.Prelude
import           Servant.Client.Core.RunClient (RunClient)
import qualified Servant.Client.Free as SCF
import qualified EulerHS.Options as T
import qualified EulerHS.Logger.Types as Log
import qualified EulerHS.HttpAPI as InternalHttp
import qualified Streamly.Data.MutByteArray as MBA

newtype EulerClient a = EulerClient (Free SCF.ClientF a)
    deriving newtype (Functor, Applicative, Monad, RunClient)
    -- deriving stock (Show,Generic)

data LogServantRequest
  = LogServantRequest
    { url         :: String
    , method      :: Text
    , body        :: A.Value
    , headers     :: Seq (Text, Text)
    , queryString :: Seq (Text, Maybe Text)
    }
    deriving stock (Show,Generic)
    deriving anyclass A.ToJSON

data LogServantResponse
  = LogServantResponse
    { statusCode  :: String
    , headers     :: Seq (Text,Text)
    , httpVersion :: String
    , body        :: A.Value
    }
    deriving stock (Show,Generic)
    deriving anyclass A.ToJSON

data ServantApiCallLogEntry = ServantApiCallLogEntry
  { url :: String
  , method :: Text
  , req_headers :: A.Value
  , req_body :: A.Value
  , res_code :: Int
  , res_body :: A.Value
  , res_headers :: A.Value
  , latency :: Maybe Integer
  , req_query_params :: A.Value
  , api_tag :: Maybe Text
  , req_type    :: InternalHttp.RequestType
  , error_info :: Maybe Log.ErrorInfo
  }
    deriving stock (Show,Generic)

instance ToJSON ServantApiCallLogEntry where
  toJSON = A.genericToJSON A.defaultOptions { A.omitNothingFields = True }

data EncryptedServantApiCallLogEntry = EncryptedServantApiCallLogEntry
  { req_body :: A.Value
  , res_body :: A.Value
  }
    deriving stock (Show,Generic)

instance ToJSON EncryptedServantApiCallLogEntry where
  toJSON = A.genericToJSON A.defaultOptions { A.omitNothingFields = True }

data ApiTag = ApiTag
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (ToJSON, FromJSON)

data OutApiTag = OutApiTag
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (ToJSON, FromJSON)

data IsLoadTesting = IsLoadTesting
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- TH Instances
--------------------------------------------------------------------------------

$(MBA.deriveSerialize [d|instance MBA.Serialize ApiTag|])
$(MBA.deriveSerialize [d|instance MBA.Serialize OutApiTag|])
$(MBA.deriveSerialize [d|instance MBA.Serialize IsLoadTesting|])

instance T.OptionEntity ApiTag Text
instance T.OptionEntity OutApiTag Text
instance T.OptionEntity IsLoadTesting Bool
