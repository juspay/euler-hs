module KV.TestHelper where

import           EulerHS.Prelude

import           KV.FlowHelper
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import           Text.Casing (quietSnake)
import qualified EulerHS.Language as L
import           EulerHS.KVConnector.Types hiding(kvRedis)
import           EulerHS.KVConnector.Utils (getPKeyWithShard, getSecondaryLookupKeys, decodeToField)
import qualified EulerHS.KVConnector.Flow as DB
import           Database.Beam.MySQL (MySQLM)
import qualified EulerHS.Types as T
import           KV.TestSchema.Mesh
import qualified Database.Beam.MySQL as BM
import           Sequelize (Model)
import qualified Data.Serialize as Serialize

getValueFromPrimaryKey :: (HasCallStack,FromJSON a, Serialize a) => Text -> L.Flow (Maybe a)
getValueFromPrimaryKey pKey = do
  res <- L.runKVDB kvRedis $ L.get $ encodeUtf8 pKey
  case res of
    Right (Just val) -> either (error . show) (pure . listToMaybe) (fst $ decodeToField $ BSL.fromChunks [val])
    Right Nothing -> L.logInfoT "KEY_NOT_FOUND" pKey $> Nothing
    Left err -> error $ show err

getValueFromSecondaryKeys :: (HasCallStack,FromJSON a, Serialize a) => [Text] -> L.Flow [(Text,a)]
getValueFromSecondaryKeys secKeys = do
  eitherRefKeys <- L.runKVDB kvRedis $ L.smembers (encodeUtf8 $ partialHead secKeys)
  case eitherRefKeys of
    Right refKeys -> fmap catMaybes $ sequence $ go <$> refKeys
    Left err -> error $ show err
  where
    go :: (FromJSON a, Serialize a) => ByteString -> L.Flow (Maybe (Text,a))
    go bkey = do
      let key = decodeUtf8 bkey
      mbRes <- getValueFromPrimaryKey key
      pure $ case mbRes of
                Just res -> Just (key,res)
                Nothing -> Nothing

deleteTableEntryValueFromKV :: (KVConnector (table Identity)) => table Identity -> L.Flow ()
deleteTableEntryValueFromKV sc = do
  let pKey = getPKeyWithShard sc
  let secKeys = getSecondaryLookupKeys sc
  void $ fromEitherToMaybe <$> (L.runKVDB kvRedis $ L.del ([encodeUtf8 pKey]))
  void $ fromEitherToMaybe <$> (L.runKVDB kvRedis $ L.del (encodeUtf8 <$> secKeys))

fromEitherToMaybe :: Either a b -> Maybe b
fromEitherToMaybe (Left _) = Nothing
fromEitherToMaybe (Right r) = Just r

partialHead :: HasCallStack => [a] -> a
partialHead xs =
  case listToMaybe xs of
    Just x -> x
    Nothing -> error "Found empty List"

fromRightErr :: (HasCallStack,Show a) => Either a b -> b
fromRightErr eitherVal = either (error . show) (id) eitherVal

fromJustErr :: HasCallStack => Maybe a -> a
fromJustErr mbVal = maybe (error "Value not present") id mbVal

peekAutoIncrId :: Text -> L.Flow (Maybe Integer)
peekAutoIncrId tName = do
  let key = (Text.pack . quietSnake . Text.unpack) tName <> "_auto_increment_id"
  getValueFromPrimaryKey key

withTableEntry ::
    ( HasCallStack
    , Model BM.MySQL table
    , FromJSON (table Identity)
    , ToJSON (table Identity)
    , KVConnector (table Identity)
    , Show (table Identity)
    , Serialize.Serialize (table Identity))
    => table Identity -> ((table Identity) -> T.DBConfig MySQLM -> L.Flow a) -> L.Flow a
withTableEntry tableEntry act = do
  dbConf <- getEulerDbConf
  fmap fst $ generalBracket
    (DB.createWithKVConnector dbConf meshConfig tableEntry)
    (\_ _ -> deleteTableEntryValueFromKV tableEntry)
    (\eitherSC -> either (\err -> error $ show err) (\entry -> act entry dbConf) eitherSC)
