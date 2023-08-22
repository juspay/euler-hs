{- |
Module      :  EulerHS.Api
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable

This module contains the definition of the `EulerClient` type, which is a wrapper around `Servant.Client.Client` type.
It also contains the definition of the `ApiTag` type, which is used to tag API calls in the logs.
-}

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
import qualified Servant.Client as SC
import qualified Servant.Client.Core as SCC

newtype EulerClient a = EulerClient (Free SCF.ClientF a)
    deriving newtype (Functor, Applicative, Monad, RunClient)
    -- deriving stock (Show,Generic)

client :: SC.HasClient EulerClient api => Proxy api -> SC.Client EulerClient api
client api = SCC.clientIn api $ Proxy @EulerClient

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
  , error_info :: Maybe Log.ErrorInfo
  }
    deriving stock (Show,Generic)

instance ToJSON ServantApiCallLogEntry where
  toJSON = A.genericToJSON A.defaultOptions { A.omitNothingFields = True }

data ApiTag = ApiTag
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (ToJSON, FromJSON)

instance T.OptionEntity ApiTag Text
