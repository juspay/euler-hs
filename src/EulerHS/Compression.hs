{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE DeriveAnyClass, DerivingStrategies, ScopedTypeVariables  #-}

module EulerHS.Compression where

import qualified Amazonka as AWS
import qualified Amazonka.Data as AWS
import qualified Amazonka.S3 as S3
import qualified Amazonka.S3.GetObject as S3

import qualified Control.Lens as AWS
import qualified Data.Generics.Product as AWS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder.Internal (chunkOverhead)
import EulerHS.Prelude hiding (fromStrict,toStrict,show)
import Data.ByteString.Lazy (fromStrict,toStrict)
import           Data.Conduit.Binary (sinkLbs)
import qualified Codec.Compression.Zstd as ZSTD
import Codec.Compression.GZip as GZIP
import qualified Data.Aeson as A
import           Data.Bits (testBit, shiftL, (.|.))
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HM
import           Text.Show (Show(show))
import qualified Data.Text as T
import Data.Text (isInfixOf,null)
import Data.List (nub,any)
import           EulerHS.Extra.Monitoring.Flow (getOptionLocalIO, modifyOptionIO)
import           EulerHS.Logger.Interpreter (runLogger)
import qualified EulerHS.Logger.Language as L
import           EulerHS.Logger.Types (LogLevel(Error), Message(..))
import qualified EulerHS.Logger.Runtime as R
import qualified EulerHS.Types as EHT
import qualified EulerHS.Framework.Runtime as R
import qualified Streamly.Data.MutByteArray as MBA
import Juspay.Extra.Env as Env
import EulerHS.Common (FlowGUID)

data CompressionConfig =
      NO_COMPRESSION
    | ZSTD_C Int (Maybe ZSTD.Dict)
    | GZIP_C (Maybe Int) (Maybe Int) (Maybe Int) String (Maybe Int) (Maybe ByteString)

data DecompressionConfig =
      NO_DECOMPRESSION
    | ZSTD_D (Maybe ZSTD.Dict)
    | GZIP_D (Maybe Int) (Maybe Int) (Maybe ByteString) Bool

data CompressionHeaderValue = ZSTD | ZSTDD | GZIP | GZIPD
    deriving (Read,Eq)

instance Show CompressionHeaderValue where
  show ZSTD  = "ZSTD"
  show ZSTDD = "ZSTDD"
  show GZIP  = "GZIP"
  show GZIPD = "GZIPD"

compressionHeader :: CI.CI ByteString
compressionHeader = (CI.mk "x-compression-info")

mkDecompressionConfig :: Maybe CompressionHeaderValue -> DecompressionConfig
mkDecompressionConfig (Just ZSTD ) =  ZSTD_D Nothing
mkDecompressionConfig (Just ZSTDD) =  ZSTD_D (mkDictFromByteString <$> getDictonary)
mkDecompressionConfig (Just GZIP ) =  GZIP_D getCompressionWindowBits getCompressBufferSize Nothing True
mkDecompressionConfig (Just GZIPD) =  GZIP_D getCompressionWindowBits getCompressBufferSize getDictonary True
mkDecompressionConfig _            =  NO_DECOMPRESSION

-- When using for API payload compression pass header "x-compression-info" with the following CompressionHeaderValue

mkCompressionConfig :: (CompressionConfig, Maybe CompressionHeaderValue)
mkCompressionConfig =
    let !dict = getDictonary
        isDictGiven = isJust dict
    in case getCompressionMethod of
            "ZSTD" -> (ZSTD_C (getCompressionLevel) (mkDictFromByteString <$> dict),Just $ if isDictGiven then ZSTD else ZSTDD)
            "GZIP" -> (GZIP_C (Just getCompressionLevel) getCompressionWindowBits getMemoryLevel getCompressStrategy getCompressBufferSize dict,Just $ if isDictGiven then GZIP else GZIPD)
            _ -> (NO_COMPRESSION,Nothing)

getCompressionMethod :: String
getCompressionMethod =
    let envType =  Env.JuspayEnv
                    { key = "COMPRESSION_METHOD"
                    , actionLeft = Env.mkDefaultEnvAction ("" :: String)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

getCompressionLevel :: Int
getCompressionLevel =
    let envType =  Env.JuspayEnv
                    { key = "COMPRESSION_LEVEL"
                    , actionLeft = Env.mkDefaultEnvAction (6 :: Int)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

getCompressionLevelLogEncryption :: Int
getCompressionLevelLogEncryption =
    let envType =  Env.JuspayEnv
                    { key = "COMPRESSION_LEVEL_LOG_ENCRYPTION"
                    , actionLeft = Env.mkDefaultEnvAction (6 :: Int)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

getCompressionWindowBits :: Maybe Int
getCompressionWindowBits =
    let envType =  Env.JuspayEnv
                    { key = "COMPRESSION_WINDOW_BITS"
                    , actionLeft = Env.mkDefaultEnvAction (Nothing :: Maybe Int)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

getMemoryLevel :: Maybe Int
getMemoryLevel =
    let envType =  Env.JuspayEnv
                    { key = "COMPRESSION_MEMORY_LEVEL"
                    , actionLeft = Env.mkDefaultEnvAction (Nothing :: Maybe Int)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

getCompressStrategy :: String
getCompressStrategy =
    let envType =  Env.JuspayEnv
                    { key = "COMPRESSION_STRATEGY"
                    , actionLeft = Env.mkDefaultEnvAction ("DefaultStrategy" :: String)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

getCompressBufferSize :: Maybe Int
getCompressBufferSize =
    let envType =  Env.JuspayEnv
                    { key = "COMPRESSION_BUFFER_SIZE"
                    , actionLeft = Env.mkDefaultEnvAction (Nothing :: Maybe Int)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

-- getDictonary :: Maybe ByteString
-- getDictonary =
--     let envType =  Env.JuspayEnv
--                     { key = "COMPRESSION_DICTONARY"
--                     , actionLeft = Env.mkDefaultEnvAction (Nothing :: Maybe ByteString)
--                     , decryptFunc = (\x -> (pure $ Just $ ("(encodeUtf8 @Text @ByteString x)" :: ByteString)))
--                     , logWhenThrowException = Nothing
--                     }
--     in Env.lookupEnv envType

getDictonary :: Maybe ByteString
getDictonary =
    let envType =  Env.JuspayEnv
                    { key = "COMPRESSION_DICTONARY"
                    , actionLeft = Env.mkDefaultEnvAction (Nothing :: Maybe Text)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in maybe (Nothing :: Maybe ByteString) (Just . encodeUtf8 @Text @ByteString) $ Env.lookupEnv envType

getChunkSizeForStream :: Int64
getChunkSizeForStream =
    let envType =  Env.JuspayEnv
                    { key = "CHUNK_SIZE_FOR_STREAM"
                    , actionLeft = Env.mkDefaultEnvAction (20 :: Int64)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

defaultGzipCompression :: Maybe Int  -> Maybe Int -> Maybe Int -> String -> Maybe Int -> Maybe ByteString -> GZIP.CompressParams
defaultGzipCompression mCompressLevel mCompressionWindowBits mMemoryLevel compressStrategyStr mCompressBufferSize compressDictionary = 
    GZIP.CompressParams {
        compressLevel = maybe GZIP.bestSpeed GZIP.compressionLevel mCompressLevel
        , compressMethod = GZIP.deflateMethod
        , compressWindowBits = maybe GZIP.defaultWindowBits GZIP.windowBits  mCompressionWindowBits
        , compressMemoryLevel = maybe GZIP.defaultMemoryLevel GZIP.memoryLevel mMemoryLevel
        , compressStrategy =
            case compressStrategyStr of
                "DefaultStrategy" -> GZIP.defaultStrategy
                "Filtered" -> GZIP.filteredStrategy
                "HuffmanOnly" -> GZIP.huffmanOnlyStrategy
                _ -> GZIP.defaultStrategy
        , compressBufferSize = maybe (16 * 1024 - chunkOverhead) id mCompressBufferSize
        , compressDictionary
    }

defaultGzipDecompressionParams :: (Maybe Int) -> (Maybe Int) -> (Maybe ByteString) -> Bool -> DecompressParams
defaultGzipDecompressionParams mdecompressWindowBits mdecompressBufferSize decompressDictionary decompressAllMembers =
    GZIP.DecompressParams{
        decompressWindowBits = maybe (GZIP.defaultWindowBits) (GZIP.windowBits) mdecompressWindowBits
        ,   decompressBufferSize = maybe (32 * 1024 - chunkOverhead) id mdecompressBufferSize
        ,   decompressDictionary
        ,   decompressAllMembers
    }

mkDictFromByteString :: ByteString -> ZSTD.Dict
mkDictFromByteString = ZSTD.mkDict

compress :: CompressionConfig -> BL.ByteString -> BL.ByteString
compress cfg str =
    case cfg of
        (ZSTD_C lvl Nothing) -> BL.fromStrict $ ZSTD.compress lvl (BL.toStrict str)
        (ZSTD_C lvl (Just dict)) -> BL.fromStrict $ ZSTD.compressUsingDict dict lvl (BL.toStrict str)
        (GZIP_C mCompressLevel mCompressionWindowBits mMemoryLevel compressStrategyStr mCompressBufferSize mCompressDictionary)
            -> GZIP.compressWith (defaultGzipCompression mCompressLevel mCompressionWindowBits mMemoryLevel compressStrategyStr mCompressBufferSize mCompressDictionary) str
        _ -> str

zstdExtractDecompress :: ZSTD.Decompress -> Either String BL.ByteString
zstdExtractDecompress res =
    case res of
        (ZSTD.Decompress dStr) -> Right $ fromStrict dStr
        ZSTD.Skip -> Left "Either the compressed frame was empty, or it was compressed in streaming mode and so its size is not known."
        (ZSTD.Error err) -> Left err

decompress :: DecompressionConfig -> BL.ByteString -> Either String BL.ByteString
decompress cfg str =
    case cfg of
        (ZSTD_D Nothing) -> zstdExtractDecompress $ ZSTD.decompress $ toStrict str
        (ZSTD_D (Just dict)) -> zstdExtractDecompress $ ZSTD.decompressUsingDict dict $ toStrict str
        (GZIP_D mdecompressWindowBits mdecompressBufferSize mdecompressDictionary decompressAllMembers)
            -> Right $ GZIP.decompressWith (defaultGzipDecompressionParams mdecompressWindowBits mdecompressBufferSize mdecompressDictionary decompressAllMembers) str
        _ -> Right str

hardKillCompression :: Bool
hardKillCompression =
    let envType =  Env.JuspayEnv
                    { key = "HARD_KILL_COMPRESSION"
                    , actionLeft = Env.mkDefaultEnvAction (False :: Bool)
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in Env.lookupEnv envType

whitelistedClusterHostForCompression :: [Text]
whitelistedClusterHostForCompression =
    let envType = Env.JuspayEnv
            { key = "WHITELISTED_CLUSTER_HOST_FOR_COMPRESSION"
            , actionLeft = Env.mkDefaultEnvAction (mempty :: [Text])
            , decryptFunc = pure
            , logWhenThrowException = Nothing
            }
    in filter (not . Data.Text.null) . nub $ Env.lookupEnv envType

blacklistedAPITagsForCompression :: [Text]
blacklistedAPITagsForCompression =
    let envType = Env.JuspayEnv
                    { key = "BLACKLISTED_API_TAGS_FOR_COMPRESSION"
                    , actionLeft = Env.mkDefaultEnvAction (mempty :: [Text])
                    , decryptFunc = pure
                    , logWhenThrowException = Nothing
                    }
    in filter (not . Data.Text.null) . nub $ Env.lookupEnv envType

compressionDecider :: Text -> Text -> Bool
compressionDecider apiTag url =
    let hostEnabledForCompression = Data.List.any (\x -> x `isInfixOf` url) whitelistedClusterHostForCompression
    in if hardKillCompression
        then False
        else hostEnabledForCompression && (apiTag `EulerHS.Prelude.notElem` blacklistedAPITagsForCompression)

data RedisCompressionConfig = RedisCompressionConfig {compEnabled :: Bool, dictId :: Text, compLevel :: Maybe Int}
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (ToJSON, FromJSON)

data RedisCompressionConf = RedisCompressionConf
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (ToJSON, FromJSON)

data RedisZstdDictConf = RedisZstdDictConf
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (ToJSON, FromJSON)

$(MBA.deriveSerialize [d|instance MBA.Serialize RedisCompressionConf|])
$(MBA.deriveSerialize [d|instance MBA.Serialize RedisZstdDictConf|])

instance EHT.OptionEntity RedisCompressionConf RedisCompressionConfig
instance EHT.OptionEntity RedisZstdDictConf (HM.HashMap Text ZSTD.Dict)

logCompressionHelper :: Maybe FlowGUID -> R.FlowRuntime -> ByteString -> IO (Maybe ByteString)
logCompressionHelper mbFlowGuid flowRt logEntryWithoutMaskingBS = do
  mbCompConf <- getOptionLocalIO (R._optionsLocal flowRt) RedisCompressionConf
  case mbCompConf of
    Just compConf -> do
      if length logEntryWithoutMaskingBS > redisCompresionEligibleLength
        then do
          mbDict <- (HM.lookup compConf.dictId =<<) <$> getOptionLocalIO (R._options flowRt) RedisZstdDictConf
          case mbDict of
            Just dict -> (pure . Just) $ ZSTD.compressUsingDict dict getCompressionLevelLogEncryption logEntryWithoutMaskingBS
            _ -> do
              logHelper mbFlowGuid flowRt Error "LOG_ZSTD_COMPRESS" "LOG_COMPRESSION" ("Dict not found while compressing log for dictId: " <> compConf.dictId)
              pure Nothing
        else (pure . Just) logEntryWithoutMaskingBS
    _ -> do
      logHelper mbFlowGuid flowRt Error "LOG_ZSTD_COMPRESS" "LOG_COMPRESSION" ("Compression Config not found")
      pure Nothing

redisCompressHelper :: Maybe FlowGUID -> R.FlowRuntime -> ByteString -> IO ByteString
redisCompressHelper mbFlowGuid flowRt v = do
  mbCompConf <- getOptionLocalIO (R._optionsLocal flowRt) RedisCompressionConf
  case mbCompConf of
    Just compConf ->
      if length v > redisCompresionEligibleLength && compConf.compEnabled
        then compress' compConf
        else pure v
    Nothing -> pure v
  
  where
  compress' compConf = do
    mbDictConf <- (HM.lookup compConf.dictId =<<) <$> getOptionLocalIO (R._options flowRt) RedisZstdDictConf
    case mbDictConf of
      Just d -> pure $ ZSTD.compressUsingDict d (fromMaybe 3 compConf.compLevel) v
      Nothing -> do
        logHelper mbFlowGuid flowRt Error "REDIS_ZSTD_COMPRESS" "REDIS_COMPRESSION" ("Dict not found while compressing redis value for dictId: " <> compConf.dictId) -- $ ErrorL Nothing "REDIS_COMP_DICT_NOT_FOUND" ""
        pure v

redisCompresionEligibleLength :: Int
redisCompresionEligibleLength = 
  let envType = Env.JuspayEnv
        { key = "REDIS_COMPRESSION_ELIGIBLE_LENGTH"
        , actionLeft = Env.mkDefaultEnvAction (200 :: Int)
        , decryptFunc = pure
        , logWhenThrowException = Nothing
        }
    in Env.lookupEnv envType
    
logHelper :: Maybe FlowGUID -> R.FlowRuntime -> LogLevel -> Text -> Text -> Text -> IO ()
logHelper mbFlowGuid flowRt _level tag action msg = runLogger mbFlowGuid (R._loggerRuntime . R._coreRuntime $ flowRt)
      $ L.masterLogger _level tag "DOMAIN" (Just action) (Nothing) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Message Nothing (Just $ A.toJSON msg)) Nothing


redisDecompressHelper :: Maybe FlowGUID -> R.FlowRuntime -> BS.ByteString -> IO (Either EHT.KVDBReply BS.ByteString)
redisDecompressHelper mbFlowGuid flowRt v = do
  case extractDictIdFromCData v of
    Just dictId -> do
      mbDict <- (HM.lookup dictId =<<) <$> getOptionLocalIO (R._options flowRt) RedisZstdDictConf
      case mbDict of
        Just d -> pure $ transformDecompressRes $ ZSTD.decompressUsingDict d v
        Nothing -> do
          logHelper mbFlowGuid flowRt Error "REDIS_ZSTD_DECOMPRESS_ERROR" "REDIS_COMPRESSION" ("Zstd dict not found while decompressing redis value for dictId: " <> dictId)
          eitherDict <- fetchDictFromS3 (dictId <> ".dict")
          case eitherDict of
            Left err -> do
              logHelper mbFlowGuid flowRt Error "REDIS_ZSTD_DECOMPRESS_ERROR" "REDIS_COMPRESSION" ("Zstd dict not found in S3 for dictId: " <> dictId <> " Err: " <> T.pack err)
              pure $ Right v
            Right d -> do
              void $ modifyOptionIO (R._options flowRt) RedisZstdDictConf (HM.insert dictId d)
              pure $ transformDecompressRes $ ZSTD.decompressUsingDict d v
    Nothing -> pure $ Right v
  
  where
    transformDecompressRes = \case
      ZSTD.Decompress r -> Right r
      ZSTD.Skip -> Right v
      ZSTD.Error err -> Left $ EHT.DecompressError err

redisDecompressHelperPure :: Maybe (HM.HashMap Text ZSTD.Dict) -> BS.ByteString -> (Either String BS.ByteString)
redisDecompressHelperPure mbDictMap v = do
  case extractDictIdFromCData v of
    Just dictId -> do
      let mbDict = (HM.lookup dictId =<<) mbDictMap
      case mbDict of
        Just d -> transformDecompressRes $ ZSTD.decompressUsingDict d v
        Nothing -> Right v -- Left $ ("Redis dict not found while decompressing redis value for dictId: " <> T.unpack dictId)
    Nothing -> Right v
  
  where
    transformDecompressRes = \case
      ZSTD.Decompress r -> Right r
      ZSTD.Skip -> Right v
      ZSTD.Error err -> Left err


-- Zstd compression format: https://github.com/facebook/zstd/blob/dev/doc/zstd_compression_format.md
extractDictIdFromCData :: ByteString -> Maybe Text
extractDictIdFromCData cData = 
  if length cData > 9 && (BS.take 4 cData) == magicNumber
    then
      let cDataWOMagicNum = BS.drop 4 cData
          frameHeaderW = BS.index cDataWOMagicNum 0
          dictIdSize = getDictionaryLengthFromDictID (testBit frameHeaderW 1, testBit frameHeaderW 0)
          singleSegmentFlag = testBit frameHeaderW 5
          cDataStartDictId = BS.drop (if singleSegmentFlag then 1 else 2) cDataWOMagicNum
          dictID = BS.take dictIdSize cDataStartDictId
       in Just (T.pack $ decodeDictId dictID)
    else Nothing

  where
    getDictionaryLengthFromDictID :: (Bool,Bool) -> Int
    getDictionaryLengthFromDictID = \case
      (False,False) -> 1
      (False,True)  -> 1
      (True,False)  -> 2
      (True,True)   -> 4

    magicNumber = "(\181/\253"

extractDictIdFromDict :: ByteString -> String
extractDictIdFromDict dictB = do
  let dictWOMagicNum = BS.drop 4 dictB
      dictID = BS.take 4 dictWOMagicNum
  decodeDictId dictID


decodeDictId :: BS.ByteString -> String
decodeDictId dictId = show $ case (fromIntegral :: Word8 -> Int) <$> BS.unpack dictId of
  [f] -> f
  [s8, f8] -> (f8 `shiftL` 8) .|. (s8)
  [a8,b8,c8] -> c8 `shiftL` 16 .|. ((b8 `shiftL` 8) .|. (a8))
  [a8,b8,c8,d8] -> ((d8 `shiftL` 8) .|. (c8)) `shiftL` 16 .|. ((b8 `shiftL` 8) .|. (a8))
  _ -> error $ "Decode dict failed for: " <> T.pack (show dictId)


fetchDictFromS3 :: Text -> IO (Either String ZSTD.Dict)
fetchDictFromS3 fileName = do
    let (ob :: Either String S3.ObjectKey) = AWS.fromText (redisZstdDictS3FolderPath <> fileName)
    case ob of
        Right obk -> doGetObject obk
        Left err -> pure $ Left err
    
    where
    redisZstdDictS3FolderPath :: Text
    redisZstdDictS3FolderPath =
      let envType = Env.JuspayEnv
            { key = "REDIS_ZSTD_DICT_S3_FOLDER_PATH"
            , actionLeft = Env.mkDefaultEnvAction ("redis-zstd-dictionaries/" :: Text)
            , decryptFunc = pure
            , logWhenThrowException = Nothing
            }
       in Env.lookupEnv envType

doGetObject :: S3.ObjectKey -> IO (Either String ZSTD.Dict)
doGetObject key = do
  res <- try @_ @SomeException $ do
    let bucketName = redisZstdDictS3Bucket
    bucketName' <- getBucketName bucketName
    logger <- AWS.newLogger AWS.Debug stdout
    env <- AWS.newEnv AWS.discover <&> AWS.configureService getS3ServicePoint <&> AWS.set (AWS.field @"logger") logger . AWS.set (AWS.field @"region") AWS.Mumbai
    print bucketName'
    AWS.runResourceT $ do
      result <- AWS.send env $ S3.newGetObject bucketName' key
      (result ^. S3.getObjectResponse_body) `AWS.sinkBody` sinkLbs
  case res of
    Left err -> pure $ Left $ show err
    Right val -> pure $ Right (ZSTD.mkDict $ toStrict val)

  where
    redisZstdDictS3Bucket :: Text
    redisZstdDictS3Bucket =
      let envType = Env.JuspayEnv
            { key = "REDIS_ZSTD_DICT_S3_BUCKET"
            , actionLeft = Env.mkDefaultEnvAction ("jp-euler-app-assets" :: Text)
            , decryptFunc = pure
            , logWhenThrowException = Nothing
            }
       in Env.lookupEnv envType

    getBucketName :: Text -> IO S3.BucketName
    getBucketName bucketName = do
        let (bk :: Either String S3.BucketName) = AWS.fromText bucketName
        case bk of
          Right bucketName' -> return bucketName'
          Left e -> do
              print e
              return ""

    getS3ServicePoint :: AWS.Service
    getS3ServicePoint = do
      let envType = Env.JuspayEnv
            { key = "NODE_ENV"
            , actionLeft = Env.mkDefaultEnvAction ("development" :: Text)
            , decryptFunc = pure
            , logWhenThrowException = Nothing
            }
          env = Env.lookupEnv envType
      if env == ("development" :: Text)
        then AWS.setEndpoint False "s3.localhost.localstack.cloud" 4566 S3.defaultService  --Can be used with localstack
        else S3.defaultService