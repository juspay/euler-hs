{- |
Module      :  EulerHS.ART.Types
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable

This module contains types for ART recording.
Import this module qualified as import EulerHS.ART.Types as ART
-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE DeriveAnyClass     #-}

module EulerHS.ART.Types where

import           EulerHS.Prelude
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import           Data.Time (LocalTime)
import qualified EulerHS.Options as T
import EulerHS.Types


data MethodRecording = MethodRecording
  { jsonRequest   :: A.Value
  , jsonResponse  :: A.Value
  , entries       :: [MethodRecordingEntry]
  , methodConfigs :: Maybe MethodConfigs
  , sessionId     :: Text
  , guid          :: Maybe Text
  , parameters    :: HM.HashMap Text Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data Header = Header Text Text
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data MethodRecordingEntry = MethodRecordingEntry
  { index     :: Int
  , entryName :: Text
  , entry     :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data MethodConfigs = MethodConfigs
  { rawBody              :: Text
  , queryParams          :: HM.HashMap Text Text
  , routeParams          :: HM.HashMap Text Text
  , headers              :: Either Text [Header]
  , rawHeaders           :: HM.HashMap Text Text
  , methodUrl            :: Text
  , sourceIP             :: Text
  , userAgent            :: Text
  , authenticationParams :: HM.HashMap Text Text
  , flowOptions          :: HM.HashMap Text Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data MethodRecordingDescription = MethodRecordingDescription
  { methodName      :: Text
  , methodRecording :: MethodRecording
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RecordingEntry
  = CallAPIEntryT CallAPIEntry
  | RunDBEntryT RunDBEntry
  | RunKVDBEntryT RunKVDBEntry
  | RunInMemEntryT RunInMemEntry
  | ForkFlowEntryT ForkFlowEntry
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data ErrorPayload = ErrorPayload
  { isError      :: Bool
  , errorMessage :: Text
  , userMessage  :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data CallAPIEntry = CallAPIEntry
  { jsonRequest :: HTTPRequest
  , jsonResult  :: Either ErrorPayload A.Value
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data ForkFlowEntry = ForkFlowEntry
  { description :: Text
  , guid        :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RunDBEntry = RunDBEntry
  { dbMethod   :: Text
  , setClauseRecording    :: A.Value
  , whereClauseRecording :: A.Value
  , model      :: Text
  , jsonResult :: A.Value
  , timestamp  :: LocalTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RunInMemEntry = RunInMemEntry
  {
    dbMethod   :: Text
    , setClauseRecording    :: A.Value
    , whereClauseRecording :: A.Value
    , model      :: A.Value
    , jsonResult :: Either A.Value A.Value
    , timestamp  :: LocalTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RunKVDBEntry =
  RExpireBT RExpireB
  | RDelBT RDelB
  | RExistsBT RExistsB
  | RHGetT RHGet
  | RGetBT RGetB
  | RHSetBT RHSetB
  | RSetBT RSetB
  | RIncrBT RIncrB
  | RSetexBT RSetexB
  | RSetexBulkBT RSetexBulkB
  | RSetOptsBT RSetOptsB
  | RXreadBT RXreadB
  | RXrevrangeBT RXrevrangeB
  | RSaddT RSadd
  | RSismemberT RSismember
  | RZAddT RZAdd
  | RZRangeByScoreT RZRangeByScore
  | RZRangeByScoreWithLimitT RZRangeByScoreWithLimit
  | RZRemRangeByScoreT RZRemRangeByScore
  | RZRemT RZRem
  | RZCardT RZCard
  | RXaddBT RXaddB
  | RSRemBT RSRemB
  | RSmembersBT RSmembersB
  | RXLenBT RXLenB
  | RXDelBT RXDelB
  | RXGroupCreateBT RXGroupCreateB
  | RXReadGroupBT RXReadGroupB
  | RXReadOptsBT RXReadOptsB
  | RXRevRangeBT RXRevRangeB
  | RSAddBT RSAddB
  | RZAddBT RZAddB
  | RZRangeBT RZRangeB
  | RLRangeBT RLRangeB
  | RLPushBT RLPushB
  | RSMoveBT RSMoveB
  | RSMemBT RSMemB
  | RRawBT RRawB
  | RPingBT RPingB
  | RMultiExecWithHashT RMultiExecWithHash
  | RMultiExecT RMultiExec
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RRawB = RRawB
  {
   args :: [ByteString]
  , jsonResult :: Either A.Value A.Value
  , timestamp :: LocalTime
  , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RPingB = RPingB
  {
  jsonResult :: Either A.Value A.Value
  , timestamp :: LocalTime
  , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RSMemB =  RSMemB {
  key1 :: ByteString
  , key2 :: ByteString
  , jsonResult :: Either A.Value A.Value
  , timestamp :: LocalTime
  , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RSMoveB = RSMoveB {
  key1 :: ByteString
  , key2 :: ByteString
  , value :: ByteString
  , jsonResult :: Either A.Value A.Value
  , timestamp :: LocalTime
  , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RLPushB = RLPushB {
  key :: ByteString
  , value :: [ByteString]
  , jsonResult :: Either A.Value A.Value
  , timestamp :: LocalTime
  , redisName :: Text
}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RLRangeB = RLRangeB {
  key :: ByteString
  , start :: Integer
  , stop :: Integer
  , jsonResult :: Either A.Value A.Value
  , timestamp :: LocalTime
  , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RZRangeB = RZRangeB {
  key :: ByteString
  , startRank :: Integer
  , stopRank :: Integer
  , jsonResult :: Either A.Value A.Value
  , timestamp :: LocalTime
  , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RSAddB = RSAddB {
  key :: ByteString
  , value :: [ByteString]
  , jsonResult :: Either A.Value A.Value
  , timestamp :: LocalTime
  , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RZAddB = RZAddB {
  key :: ByteString
  , value :: [(Double, ByteString)]
  , jsonResult :: Either A.Value A.Value
  , timestamp :: LocalTime
  , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RXRevRangeB = RXRevRangeB {
  streamName :: ByteString
  , streamEnd :: A.Value
  , streamStart :: A.Value
  , count :: Maybe Integer
  , jsonResult :: Either A.Value Integer
  , timestamp :: LocalTime
  , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RXReadOptsB = RXReadOptsB {
  strObjs :: A.Value
  , readOpts :: A.Value
  , jsonResult :: Either A.Value Integer
  , timestamp :: LocalTime
  , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RXReadGroupB = RXReadGroupB {
  groupName :: A.Value
  , consumerName :: A.Value
  , streamsAndIds :: [(ByteString,ByteString)]
  , readOpts :: A.Value
  , jsonResult :: Either A.Value Integer
  , timestamp :: LocalTime
  , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RXDelB = RXDelB {
  streamName :: ByteString
  , streamEntryID :: A.Value
  , jsonResult :: Either A.Value Integer
  , timestamp :: LocalTime
  , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RXGroupCreateB = RXGroupCreateB {
  streamName :: ByteString
  , groupName :: A.Value
  , recordId :: A.Value
  , jsonResult :: Either A.Value Integer
  , timestamp :: LocalTime
  , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RXLenB = RXLenB {
    streamName :: ByteString
    , jsonResult :: Either A.Value Integer
    , timestamp :: LocalTime
    , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RSRemB = RSRemB {
    key :: ByteString
    , pKeyList :: [ByteString]
    , jsonResult :: Either A.Value Integer
    , timestamp :: LocalTime
    , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RSmembersB = RSmembersB {
    key :: ByteString
    , jsonResult :: Either A.Value A.Value
    , timestamp :: LocalTime
    , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RXaddB = RXaddB {
    streamName :: ByteString
    , streamEntry :: ByteString
    , streamItem :: A.Value
    , jsonResult :: Either A.Value A.Value
    , timestamp :: LocalTime
    , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RZCard = RZCard {
    key :: ByteString
    , jsonResult :: Either A.Value Integer
    , timestamp :: LocalTime
    , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RZRem = RZRem {
    key :: ByteString
    , value :: [ByteString]
    , jsonResult :: Either A.Value Integer
    , timestamp :: LocalTime
    , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RZRemRangeByScore = RZRemRangeByScore {
    key :: ByteString
    , minScore :: Double
    , maxScore :: Double
    , jsonResult :: Either A.Value Integer
    , timestamp :: LocalTime
    , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RZRangeByScoreWithLimit = RZRangeByScoreWithLimit {
    key :: ByteString
    , minScore :: Double
    , maxScore :: Double
    , offset :: Integer
    , count :: Integer
    , jsonResult :: Either A.Value A.Value
    , timestamp :: LocalTime
    , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RZRangeByScore = RZRangeByScore {
    key :: ByteString
    , minScore :: Double
    , maxScore :: Double
    , jsonResult :: Either A.Value A.Value
    , timestamp :: LocalTime
    , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RZAdd = RZAdd {
    key :: ByteString
    , value :: [(Double,ByteString)]
    , jsonResult :: Either A.Value Integer
    , timestamp :: LocalTime
    , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RSismember = RSismember {
    key :: ByteString
    , value :: ByteString
    , jsonResult :: Either A.Value Bool
    , timestamp :: LocalTime
    , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RSadd = RSadd {
    key :: ByteString
    , value :: [ByteString]
    , jsonResult :: Either A.Value Integer
    , timestamp :: LocalTime
    , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RXrevrangeB = RXrevrangeB {
    stream :: ByteString
    , send :: ByteString
    , sstart :: ByteString
    , count :: Maybe Integer
    , jsonResult :: Either A.Value A.Value
    , timestamp :: LocalTime
    , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RXreadB = RXreadB {
    stream :: ByteString
    , entryId :: ByteString
    , jsonResult :: Either A.Value (Maybe A.Value)
    , timestamp :: LocalTime
    , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RSetOptsB = RSetOptsB {
    key :: ByteString
    , value :: A.Value
    , ttl :: A.Value
    , setCondition :: A.Value
    , jsonResult :: Either A.Value A.Value
    , timestamp :: LocalTime
    , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RSetexBulkB = RSetexBulkB {
    kvMap :: A.Value
    , ttl :: Integer
    , jsonResult :: Either A.Value A.Value
    , timestamp :: LocalTime
    , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RIncrB = RIncrB {
    key :: ByteString
    , jsonResult :: Either A.Value A.Value
    , timestamp :: LocalTime
    , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RSetexB = RSetexB {
    key :: ByteString
    , ttl :: Integer
    , value :: A.Value
    , jsonResult :: Either A.Value A.Value
    , timestamp :: LocalTime
    , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RSetB = RSetB {
    key :: ByteString
    , value :: A.Value
    , jsonResult :: Either A.Value A.Value
    , timestamp :: LocalTime
    , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RHSetB = RHSetB {
    key :: ByteString
    , field :: A.Value
    , value :: A.Value
    , jsonResult :: Either A.Value A.Value
    , timestamp :: LocalTime
    , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RHGet = RHGet {
    key :: ByteString
    , field :: A.Value
    , jsonResult :: Maybe A.Value
    , timestamp :: LocalTime
    , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RGetB = RGetB {
    key :: ByteString
    , jsonResult :: Maybe A.Value
    , timestamp :: LocalTime
    , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RDelB = RDelB {
    key :: [ByteString]
    , jsonResult :: Either A.Value A.Value
    , timestamp :: LocalTime
    , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RExistsB = RExistsB {
    key :: ByteString
    , jsonResult :: Either A.Value A.Value
    , timestamp :: LocalTime
    , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RExpireB = RExpireB {
    key :: ByteString
    , ttl :: Integer
    , jsonResult :: Either A.Value A.Value
    , timestamp :: LocalTime
    , redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RMultiExecWithHash = RMultiExecWithHash {
    key :: ByteString,
    jsonResult :: Either A.Value A.Value,
    timestamp :: LocalTime,
    redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data RMultiExec = RMultiExec {
    jsonResult :: Either A.Value A.Value,
    timestamp :: LocalTime,
    redisName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON,FromJSON)

data WaiRequest = WaiRequest
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance T.OptionEntity WaiRequest HTTPRequest
