{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module EulerHS.KVConnector.DBSync where

import           EulerHS.Prelude
import           EulerHS.KVConnector.Types (KVConnector, MeshMeta(..), DBName(..), StreamMetadata(..), MerchantID(..), MeshConfig(..), PKvKey)
import           EulerHS.KVConnector.Utils (getPKeyAndValueList, meshModelTableEntityDescriptor, toPSJSON, getSchemaNameWithMigrationDbConf)
import qualified EulerHS.Language as L
import           EulerHS.Types (ModelDBConfig)
import qualified Data.Aeson as A
import           Data.Aeson ((.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as AK
import qualified Data.Text as T
import qualified Database.Beam as B
import qualified Database.Beam.Schema.Tables as B
import           Sequelize (Model, Where, Clause(..), Term(..), Column, fromColumnar')
import           Text.Casing (pascal)


-- For storing DBCommands in stream

type Tag = Text


data DBCommandVersion = V1
  deriving (Generic, Show, ToJSON, FromJSON)

getCreateQuery :: (ToJSON (table Identity)) => Text -> DBCommandVersion -> StreamMetadata -> Double -> DBName -> table Identity -> A.Value
getCreateQuery model cmdVersion piiMeta timestamp dbName dbObject = A.object
    [ "contents" .= A.toJSON
        [ A.toJSON cmdVersion
        , A.toJSON piiMeta
        , A.toJSON timestamp
        , A.toJSON dbName
        , A.object
            [ "contents" .= dbObject,
              "tag" .= ((T.pack . pascal . T.unpack) model <> "Object")
            ]
        ]
    , "tag" .= ("Create" :: Text)
    ]
-- | This will take updateCommand from getDbUpdateCommandJson and returns Aeson value of Update DBCommand
getUpdateQuery :: DBCommandVersion -> StreamMetadata -> Double -> DBName -> A.Value -> A.Value
getUpdateQuery cmdVersion piiMeta timestamp dbName updateCommand = A.object
    [ "contents" .= A.toJSON
        [ A.toJSON cmdVersion
        , A.toJSON piiMeta
        , A.toJSON timestamp
        , A.toJSON dbName
        , updateCommand
        ]
    , "tag" .= ("Update" :: Text)
    ]

getDbUpdateCommandJson :: forall be table. (Model be table, MeshMeta be table) => Text -> [(Text, A.Value)] -> Where be table -> A.Value
getDbUpdateCommandJson model upd whereClause = A.object
  [ "contents" .= A.toJSON
      [ updValToJSON . (toPSJSON @be @table) <$> upd
      , [whereClauseToJson whereClause]
      ]
  , "tag" .= ((T.pack . pascal . T.unpack) model <> "Options")
  ]

getDbUpdateCommandJsonWithPrimaryKey :: forall be beM table. (KVConnector (table Identity), Model be table, MeshMeta be table, A.ToJSON (table Identity)) => ModelDBConfig beM -> Text -> [(Text, A.Value)] -> table Identity -> Where be table -> A.Value
getDbUpdateCommandJsonWithPrimaryKey dbConf model upd table whereClause = A.object
  [ "contents" .= A.toJSON
      [ updValToJSON . (toPSJSON @be @table) <$> upd
      , [(whereClauseJsonWithPrimaryKey @be) dbConf table $ whereClauseToJson whereClause]
      ]
  , "tag" .= ((T.pack . pascal . T.unpack) model <> "Options")
  ]  

whereClauseJsonWithPrimaryKey :: forall be beM table. (HasCallStack, KVConnector (table Identity), A.ToJSON (table Identity), MeshMeta be table) => ModelDBConfig beM -> table Identity -> A.Value -> A.Value
whereClauseJsonWithPrimaryKey dbConf table whereClause =
  case whereClause of
    A.Object o -> 
      let mbClause = KM.lookup "value1" o
      in case mbClause of
          Just clause -> 
            let pKeyValueList = getPKeyAndValueList dbConf table
                modifiedKeyValueList = modifyKeyValue <$> pKeyValueList
                andOfKeyValueList = A.toJSON $ KM.singleton ("$and" :: AK.Key) $ A.toJSON modifiedKeyValueList
                modifiedClause = A.toJSON $ KM.singleton ("$and" :: AK.Key) $ A.toJSON [clause, andOfKeyValueList]
                modifiedObject = KM.insert ("value1" :: AK.Key) modifiedClause o
            in A.toJSON modifiedObject
          Nothing -> error "Invalid whereClause, contains no item value1"
    _ -> error "Cannot modify whereClause that is not an Object"

  where
    modifyKeyValue :: (Text, A.Value) -> A.Value
    modifyKeyValue (key, value) = A.toJSON $ KM.singleton (AK.fromText key) (snd $ (toPSJSON @be @table) (key, value))

getDeleteQuery :: DBCommandVersion -> StreamMetadata -> Double -> DBName -> A.Value -> A.Value
getDeleteQuery cmdVersion tag timestamp dbName deleteCommand = A.object
  [ "contents" .= A.toJSON
      [ A.toJSON cmdVersion
      , A.toJSON tag
      , A.toJSON timestamp
      , A.toJSON dbName
      , deleteCommand
      ]
  , "tag" .= ("Delete" :: Text)
  ]

getDbDeleteCommandJson :: forall be table. (Model be table, MeshMeta be table) => Text -> Where be table -> A.Value
getDbDeleteCommandJson model whereClause = A.object
  [ "contents" .= whereClauseToJson whereClause
  , "tag" .= ((T.pack . pascal . T.unpack) model <> "Options")
  ]

getDbDeleteCommandJsonWithPrimaryKey :: forall be table beM. (HasCallStack, KVConnector (table Identity), Model be table, MeshMeta be table, A.ToJSON (table Identity)) => ModelDBConfig beM -> Text -> table Identity -> Where be table -> A.Value
getDbDeleteCommandJsonWithPrimaryKey dbConf model table whereClause = A.object
  [ "contents" .= ((whereClauseJsonWithPrimaryKey @be) dbConf table $ whereClauseToJson whereClause)
  , "tag" .= ((T.pack . pascal . T.unpack) model <> "Options")
  ]

updValToJSON :: (Text, A.Value) -> A.Value
updValToJSON (k, v) = A.object [ "value0" .= k, "value1" .= v ]

whereClauseToJson :: (Model be table, MeshMeta be table) => Where be table -> A.Value
whereClauseToJson whereClause = A.object
    [ ("value0" :: AK.Key) .= ("where" :: Text)
    , "value1" .= modelEncodeWhere whereClause
    ]
     
modelEncodeWhere ::
  forall be table.
  (Model be table, MeshMeta be table) =>
  Where be table ->
  A.Object
modelEncodeWhere = encodeWhere meshModelTableEntityDescriptor

{-# INLINE encodeWhere #-}
encodeWhere ::
  forall be table.
  (MeshMeta be table) =>
  B.DatabaseEntityDescriptor be (B.TableEntity table) ->
  Where be table ->
  A.Object
encodeWhere dt = encodeClause dt . And

{-# INLINE encodeClause #-}
encodeClause ::
  forall be table.
  (MeshMeta be table) =>
  B.DatabaseEntityDescriptor be (B.TableEntity table) ->
  Clause be table ->
  A.Object
encodeClause _ w =
  let foldWhere' = \case
        And cs -> foldAnd cs
        Or cs -> foldOr cs
        Is column val -> foldIs column val
      foldAnd = \case
        [] -> KM.empty
        [x] -> foldWhere' x
        xs -> KM.singleton "$and" (A.toJSON $ map foldWhere' xs)
      foldOr = \case
        [] -> KM.empty
        [x] -> foldWhere' x
        xs -> KM.singleton "$or" (A.toJSON $ map foldWhere' xs)
      foldIs :: A.ToJSON a => Column table value -> Term be a -> A.Object
      foldIs column term =
        let key =
              B._fieldName . fromColumnar' . column $ columnized @be @table
         in KM.singleton (AK.fromText key) $ (encodeTerm @table) key term
   in foldWhere' w

encodeTerm :: forall table be value. (A.ToJSON value, MeshMeta be table) => Text -> Term be value -> A.Value
encodeTerm key = \case
  In vals -> array "$in" (modifyToPsFormat <$> vals)
  Eq val -> modifyToPsFormat val
  Null -> A.Null
  GreaterThan val -> single "$gt" (modifyToPsFormat val)
  GreaterThanOrEq val -> single "$gte" (modifyToPsFormat val)
  LessThan val -> single "$lt" (modifyToPsFormat val)
  LessThanOrEq val -> single "$lte" (modifyToPsFormat val)
  -- Like val -> single "$like" (modifyToPsFormat val)
  -- Not (Like val) -> single "$notLike" (modifyToPsFormat val)
  Not (In vals) -> array "$notIn" (modifyToPsFormat <$> vals)
  Not (Eq val) -> single "$ne" (modifyToPsFormat val)
  Not Null -> single "$ne" A.Null
  Not term -> single "$not" ((encodeTerm @table) key term)
  _ -> error "Error while encoding - Term not supported"

  where
    modifyToPsFormat val = snd $ (toPSJSON @be @table) (key, A.toJSON val)

array :: Text -> [A.Value] -> A.Value
array k vs = A.toJSON $ KM.singleton (AK.fromText k) vs

single :: Text -> A.Value -> A.Value
single k v = A.toJSON $ KM.singleton (AK.fromText k) v

makeStreamMetadata :: (L.MonadFlow m) => ModelDBConfig beM -> Maybe Text -> PKvKey -> MeshConfig -> DBName -> m [StreamMetadata]
makeStreamMetadata dbConf mbKID tg meshCfg dbName = do 
  (schemaNames, dbPrefix) <- getSchemaNameWithMigrationDbConf dbConf dbName meshCfg
  maybeMid <- L.getOptionLocal MerchantID
  pure $ (\schema -> StreamMetadata
    { keyID = mbKID
    , kvTag = tg.prefixedKey
    , mid = maybeMid
    , dbPrefix
    , schemaName = Just schema
    }) <$> schemaNames

getFieldsFromClause :: (Model be table, MeshMeta be table) => Where be table -> Text
getFieldsFromClause whereClause = 
  let columns = getColumns meshModelTableEntityDescriptor $ And whereClause
  in T.intercalate "|" (sort columns)
  where
    getColumns ::forall be table. (MeshMeta be table) => B.DatabaseEntityDescriptor be (B.TableEntity table) -> Clause be table -> [Text]
    getColumns _ w =
      let foldWhere' = \case
            And cs -> foldAnd cs
            Or cs -> foldOr cs
            Is column _ -> foldIs column
          foldAnd = concatMap foldWhere'
          foldOr = concatMap foldWhere'
          foldIs :: Column table value -> [Text]
          foldIs column =
            let key = B._fieldName . fromColumnar' . column $ columnized @be @table
            in [key]
      in foldWhere' w
   