{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module EulerHS.KVConnector.Metrics where

import           EulerHS.Prelude
import           EulerHS.ART.V2.Types (ArtRecordable)
import qualified EulerHS.Language as L
import           EulerHS.Options  (OptionEntity)
import           Euler.Events.MetricApi.MetricApi
import qualified Juspay.Extra.Env as Env
import           EulerHS.KVConnector.Types  (DBLogEntry(..), Source(..), IdSource(..))
import           EulerHS.Types ( ApiTag(ApiTag) , Operation(..))
import qualified Streamly.Data.MutByteArray as MBA

incrementKVMetric :: L.MonadFlow m => KVMetricHandler -> KVMetric -> DBLogEntry a -> Bool -> Text -> m ()
incrementKVMetric handle metric dblog isLeftRes connTag = do
  let mid = fromMaybe "" $ _merchant_id dblog
  let tag = fromMaybe "" $ _apiTag dblog
  let source = _source dblog
  let model = _model dblog
  let action = _operation dblog
  let idSource = fromMaybe SQL' $ _idSource dblog
      latency = _latency dblog
      diffFound = isJust $ _whereDiffCheckRes dblog
  L.runIO $ kvCounter handle (metric, tag, action, source, idSource, model, mid, latency, diffFound, isLeftRes, connTag)

incrementKVRedisCallsMetric :: L.MonadFlow m => KVMetricHandler -> Text -> Text -> Text -> Int ->Bool -> Bool -> m()
incrementKVRedisCallsMetric handler tag action model redisCalls redisSoftLimitExceeded redisHardLimitExceeded = do
      L.runIOWithART "EulerHS.KVConnector.Metrics::incrementKVRedisCallsMetric::kvCalls" kvCalls handler (tag, action, model, redisCalls, redisSoftLimitExceeded,redisHardLimitExceeded)

incrementDbQueryEvaluationMetric :: L.MonadFlow m => KVMetricHandler -> Text -> Text -> Text -> Bool -> m()
incrementDbQueryEvaluationMetric handler tag query tableName disablePartitionKey = do
      L.runIOWithART "EulerHS.KVConnector.Metrics::incrementDbQueryEvaluationMetric::dbQueryEvaluation" dbQueryEvaluation handler (tag, query, tableName, disablePartitionKey)

data KVMetricHandler = KVMetricHandler
  { kvCounter :: (KVMetric, Text, Operation, Source,IdSource, Text, Text, Maybe Int, Bool, Bool, Text) -> IO (),
    kvCalls :: (Text, Text, Text,Int, Bool,Bool) -> IO(),
    dbQueryEvaluation :: (Text, Text, Text, Bool) -> IO()
  }
  deriving (ArtRecordable)

data KVMetric = KVAction

mkKVMetricHandler :: IO KVMetricHandler
mkKVMetricHandler = do
  metrics <- register collectionLock
  pure $ KVMetricHandler
    (\case
      (KVAction, tag, action, source, idSource, model , mid, _latency, diffFound, isLeftRes, connTag) -> do
            inc (metrics </> #kv_action_counter_mid)  action source model mid connTag
            inc (metrics </> #kv_action_counter_tag)  tag action source model
            when (action == CREATE_RETURNING || action == CREATE) $ inc (metrics </> #kv_snowflake_counter)  action model idSource
            when diffFound $ inc (metrics </> #kv_diff_counter) tag action source model
            when isLeftRes $ inc (metrics </> #kv_sql_error_counter) tag action source model mid)
    (\case
      (tag,action,model,_redisCalls,redisSoftLimitExceeded, redisHardLimitExceeded) -> do
            when redisSoftLimitExceeded (inc (metrics </> #kvRedis_soft_db_limit_exceeded) tag action model)
            when redisHardLimitExceeded (inc (metrics </> #kvRedis_hard_db_limit_exceeded) tag action model))
    (\case
      (tag, query, tableName, disablePartitionKey) -> do
            inc (metrics </> #db_query_evaluation_metric) tag query tableName disablePartitionKey)


kv_action_counter_mid = counter #kv_action_counter_mid
      .& lbl @"action" @Operation
      .& lbl @"source" @Source
      .& lbl @"model" @Text
      .& lbl @"mid" @Text
      .& lbl @"dbSource" @Text
      .& build

kv_action_counter_tag = counter #kv_action_counter_tag
      .& lbl @"tag" @Text
      .& lbl @"action" @Operation
      .& lbl @"source" @Source
      .& lbl @"model" @Text
      .& build

kv_diff_counter = counter #kv_diff_counter
      .& lbl @"tag" @Text
      .& lbl @"action" @Operation
      .& lbl @"source" @Source
      .& lbl @"model" @Text
      .& build

kv_sql_error_counter = counter #kv_sql_error_counter
      .& lbl @"tag" @Text
      .& lbl @"action" @Operation
      .& lbl @"source" @Source
      .& lbl @"model" @Text
      .& lbl @"mid" @Text
      .& build

kv_latency_observe = histogram #kv_latency_observe
      .& lbl @"tag" @Text
      .& lbl @"action" @Operation
      .& lbl @"source" @Source
      .& lbl @"model" @Text
      .& build

kvRedis_calls_observe = histogram #kvRedis_calls_observe
      .& lbl @"tag" @Text
      .& lbl @"action" @Text
      .& lbl @"model" @Text
      .& build

kvRedis_soft_db_limit_exceeded = counter #kvRedis_soft_db_limit_exceeded
      .& lbl @"tag" @Text
      .& lbl @"action" @Text
      .& lbl @"model" @Text
      .& build

kvRedis_hard_db_limit_exceeded = counter #kvRedis_hard_db_limit_exceeded
      .& lbl @"tag" @Text
      .& lbl @"action" @Text
      .& lbl @"model" @Text
      .& build

kv_snowflake_counter = counter #kv_snowflake_counter
      .& lbl @"action" @Operation
      .& lbl @"model" @Text
      .& lbl @"idSource" @IdSource
      .& build
      
db_query_evaluation_metric = counter #db_query_evaluation_metric
      .& lbl @"tag" @Text
      .& lbl @"query" @Text
      .& lbl @"tableName" @Text
      .& lbl @"disablePartitionKey" @Bool
      .& build

collectionLock =
     kv_action_counter_mid
  .> kv_action_counter_tag
  .> kv_diff_counter
  .> kv_sql_error_counter
  .> kv_latency_observe
  .> kvRedis_calls_observe
  .> kvRedis_soft_db_limit_exceeded
  .> kvRedis_hard_db_limit_exceeded
  .> kv_snowflake_counter
  .> db_query_evaluation_metric
  .> MNil


---------------------------------------------------------

data KVMetricCfg = KVMetricCfg
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

---------------------------------------------------------

isKVMetricEnabled :: Bool
isKVMetricEnabled =
    let envType =  Env.JuspayEnv
                    { key = "KV_METRIC_ENABLED"
                    , actionLeft = Env.mkDefaultEnvAction (True :: Bool)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType
    
isDbQueryEvaluationEnabled:: Bool
isDbQueryEvaluationEnabled =
    let envType =  Env.JuspayEnv
                    { key = "DB_QUERY_EVALUATION_ENABLED"
                    , actionLeft = Env.mkDefaultEnvAction (True :: Bool)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

--------------------------------------------------------------------------------
-- TH Instances
--------------------------------------------------------------------------------

$(MBA.deriveSerialize [d|instance MBA.Serialize KVMetricCfg|])

instance OptionEntity KVMetricCfg KVMetricHandler

incrementMetric :: (HasCallStack, L.MonadFlow m) => KVMetric -> DBLogEntry a -> Bool -> Text -> m ()
incrementMetric metric dblog isLeftRes connTag = when isKVMetricEnabled $ do
  env <- L.getOption KVMetricCfg
  case env of
    Just val -> incrementKVMetric val metric dblog isLeftRes connTag
    Nothing -> pure ()

incrementRedisCallMetric :: (HasCallStack , L.MonadFlow m) => Text -> Text -> Int -> Bool -> Bool -> m()
incrementRedisCallMetric  action model dbCalls redisSoftLimitExceeded redisHardLimitExceeded  = when isKVMetricEnabled $ do
      env <- L.getOption KVMetricCfg
      case env of
            Just val -> do
                   tag <- fromMaybe "" <$> L.getOptionLocal ApiTag
                   incrementKVRedisCallsMetric val tag action model dbCalls redisSoftLimitExceeded redisHardLimitExceeded
            Nothing -> pure ()
            
incrementDbQueryEvalMetric :: (HasCallStack , L.MonadFlow m) => Text -> Text -> Bool -> m()
incrementDbQueryEvalMetric query tableName disablePartitionKey = when isDbQueryEvaluationEnabled $ do
      env <- L.getOption KVMetricCfg
      case env of
            Just val -> do
                   tag <- fromMaybe "" <$> L.getOptionLocal ApiTag
                   incrementDbQueryEvaluationMetric val tag query tableName disablePartitionKey
            Nothing -> pure ()
