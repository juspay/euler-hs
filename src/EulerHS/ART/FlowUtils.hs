{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module EulerHS.ART.FlowUtils
  ( getDirPath,
    dateTimeFormatSeparatedBy_,
    defaultHTTPrequest,
    addRecToState,
    writeRecToFile,
    shouldRecordForkFLow,
    getResponseHttp,
    getRecording,
    readRecordingsAndWriteToFile,
    readRecordingsAndWriteToFileForkFLow,
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           EulerHS.ART.Types
import           EulerHS.ART.Utils
import qualified EulerHS.ART.Types as ART
import qualified EulerHS.Language as L
import           EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified Data.Text.Lazy as TL
import qualified Servant as S
import qualified Data.Map                                  as Map
import EulerHS.ART.EnvVars
import Crypto.Hash (MD5, Digest, hash)
import Data.ByteArray (convert)
import Numeric (showIntAtBase)

hashOrderId :: Text -> Text
hashOrderId text = base62 md5Hash
  where
    byteString :: ByteString
    byteString = encodeUtf8 text

    md5Hash :: ByteString
    md5Hash = convert (hash byteString :: Digest MD5)

intToBase62Char :: Int -> Char
intToBase62Char n
  | n < 10    = chr (ord '0' + n)    -- '0'-'9'
  | n < 36    = chr (ord 'a' + (n - 10))  -- 'a'-'z'
  | n < 62    = chr (ord 'A' + (n - 36))  -- 'A'-'Z'
  | otherwise = error "intToBase62Char: not a base62 digit"

base62 :: ByteString -> Text
base62 bs = T.pack $ showIntAtBase 62 intToBase62Char (bsToInteger bs) ""

bsToInteger :: ByteString -> Integer
bsToInteger = foldl' (\acc x -> acc * 256 + fromIntegral x) 0

dateTimeFormatSeparatedBy_ :: String
dateTimeFormatSeparatedBy_ = "%F_%H-%M-%S"

addRecToState :: (L.MonadFlow m) => ART.RecordingEntry -> m ()
addRecToState newRecording = do
  shouldRecord <- isArtRecEnabled
  if shouldRecord then L.appendRecordingLocal newRecording else pure ()

writeRecToFile :: (L.MonadFlow m) => Text -> Text -> Text -> ART.MethodRecordingDescription -> m ()
writeRecToFile apitag orderID reqId mrd = do
    case removeTrailingSlash getDirPath of
      Just dirPath -> do
        let fileName = dirPath <> "/" <> apitag <> "_"  <> hashOrderId orderID <> "_" <> reqId <> ".json"
        res <- L.runIO $ tryWriteFile fileName
        case res of
          Right _ -> pure ()
          Left err -> L.logErrorT "Recording Failed" (T.pack $ displayException err)
      Nothing -> L.logErrorT "The directory path seems to be wrong" getDirPath
    where
      tryWriteFile :: Text -> IO (Either SomeException ())
      tryWriteFile fileName = catch (Right <$> BSL.writeFile (T.unpack $ fileName) (A.encode mrd)) (return . Left)

      removeTrailingSlash :: T.Text -> Maybe T.Text
      removeTrailingSlash text =
        case T.unsnoc text of
          Just (initText, lastChar)
            | lastChar == '/' -> removeTrailingSlash initText
            | otherwise -> Just text
          Nothing -> Nothing

readRecordingsAndWriteToFileForkFLow :: Text -> Text -> L.Flow ()
readRecordingsAndWriteToFileForkFLow desc guId = do
  entriesList :: [RecordingEntry] <- L.getRecordingLocal
  msessionId <- L.getLoggerContext "x-request-id"
  mOrderId <- tryBoth (L.getLoggerContext "order_id") (L.getLoggerContext "orderId")
  mApiTag <- L.getOptionLocal ET.ApiTag
  let apitag = fromMaybe "NO_API_TAG" mApiTag
      methodRecordingEnties = loopOverEntriesWithIndex 0 entriesList []
      recordings =
        MethodRecording
          { jsonRequest = A.Null,
            jsonResponse = A.Null,
            entries = methodRecordingEnties,
            methodConfigs = Nothing,
            sessionId = fromMaybe "NO_REQUEST_ID" msessionId,
            apiTag = apitag,
            guid = Just guId ,
            parameters = HM.singleton "description" desc
          }
      recordingDescription =
        MethodRecordingDescription
          { methodName = "FORK-FLOW",
            methodRecording = recordings
          }
  L.delRecordingLocal
  writeRecToFile apitag (fromMaybe "" mOrderId) (getHostName <> "--" <> guId) recordingDescription

readRecordingsAndWriteToFile :: ET.HTTPMethod -> ET.HTTPRequest -> ET.HTTPResponse -> Text -> Maybe Text -> HM.HashMap Text Text -> L.Flow ()
readRecordingsAndWriteToFile _method request response sId mUrl rps = do
  entriesList :: [RecordingEntry] <- L.getRecordingLocal
  mApiTag <- L.getOptionLocal ET.ApiTag
  mOrderId <- tryBoth (L.getLoggerContext "order_id") (L.getLoggerContext "orderId")
  let methodRecordingEnties = loopOverEntriesWithIndex 0 entriesList []
      apitag = fromMaybe "NO_API_TAG" mApiTag
      recordings =
        MethodRecording
          { jsonRequest = toJSON request,
            jsonResponse = toJSON response,
            entries = methodRecordingEnties,
            methodConfigs = Nothing,
            sessionId = sId,
            apiTag = apitag,
            guid = Nothing,
            parameters = rps
          }
      recordingDescription =
        MethodRecordingDescription
          { methodName = fromMaybe "NO_URL" mUrl,
            methodRecording = recordings
          }
  writeRecToFile apitag (fromMaybe "" mOrderId) sId  recordingDescription

loopOverEntriesWithIndex :: Int -> [RecordingEntry] -> [MethodRecordingEntry] -> [MethodRecordingEntry]
loopOverEntriesWithIndex _ [] res = res
loopOverEntriesWithIndex i (x:xs) res = loopOverEntriesWithIndex (i + 1) xs ([convertToMethodRecordingEntry x i] <> res)

convertToMethodRecordingEntry :: RecordingEntry -> Int -> MethodRecordingEntry
convertToMethodRecordingEntry (RunInMemEntryT runInMemEntryT) i =
  MethodRecordingEntry {
    index = i + 1,
    entryName = "RunInMemEntry",
    entry = TL.toStrict . A.encodeToLazyText $ runInMemEntryT
  }
convertToMethodRecordingEntry (ForkFlowEntryT forkFlowEntry) i =
  MethodRecordingEntry {
    index = i + 1,
    entryName = "ForkFlowEntry",
    entry = TL.toStrict . A.encodeToLazyText $ forkFlowEntry
  }
convertToMethodRecordingEntry (CallAPIEntryT callApiEntry) i =
  MethodRecordingEntry {
    index = i + 1,
    entryName = "CallAPIEntry",
    entry = TL.toStrict . A.encodeToLazyText $ callApiEntry
  }
convertToMethodRecordingEntry (RunDBEntryT runDBEntry) i =
  MethodRecordingEntry {
    index = i + 1,
    entryName = "RunDBEntry",
    entry = TL.toStrict . A.encodeToLazyText $ runDBEntry
  }
convertToMethodRecordingEntry (RunKVDBEntryT runKVDBEntry) i =
  MethodRecordingEntry {
    index = i + 1,
    entryName = "RunKVDBEntry",
    entry = TL.toStrict . A.encodeToLazyText $ runKVDBEntry
  }
convertToMethodRecordingEntry (TimeStampEntryT timeStampEntry) i =
  MethodRecordingEntry {
    index = i + 1,
    entryName = "TimeStampEntry",
    entry = TL.toStrict . A.encodeToLazyText $ timeStampEntry
  }
convertToMethodRecordingEntry (RandomRIOEntryT randomRIOEntry) i =
  MethodRecordingEntry {
    index = i + 1,
    entryName = "RandomRIOEntry",
    entry = TL.toStrict . A.encodeToLazyText $ randomRIOEntry
  }
convertToMethodRecordingEntry (RandomBytesEntryT randomBytesEntry) i =
  MethodRecordingEntry {
    index = i + 1,
    entryName = "RandomBytesEntry",
    entry = TL.toStrict . A.encodeToLazyText $ randomBytesEntry
  }
convertToMethodRecordingEntry (RunIOWithArtEntryT runIOWithArtEntry) i =
  MethodRecordingEntry {
    index = i + 1,
    entryName = "RunIOWithArtEntry",
    entry = TL.toStrict . A.encodeToLazyText $ runIOWithArtEntry
  }
convertToMethodRecordingEntry (GlobalOptionsEntryT globalOptionsEntry) i =
  MethodRecordingEntry {
    index = i + 1,
    entryName = "GlobalOptionsEntry",
    entry = TL.toStrict . A.encodeToLazyText $ globalOptionsEntry
  }
convertToMethodRecordingEntry (ARTConfigEntryT artConfigEntry) i =
  MethodRecordingEntry {
    index = i + 1,
    entryName = "ConfigEntry",
    entry = TL.toStrict . A.encodeToLazyText $ artConfigEntry
  }
convertToMethodRecordingEntry (UuidEntryT uuidEntry) i =
  MethodRecordingEntry {
    index = i + 1,
    entryName = "UuidEntry",
    entry = TL.toStrict . A.encodeToLazyText $ uuidEntry
  }

defaultHTTPrequest :: ET.HTTPRequest
defaultHTTPrequest =
  ET.HTTPRequest{ getRequestMethod = ET.Get
            , getRequestHeaders = Map.empty
            , getRequestBody = Nothing
            , getRequestURL = "http://localhost:8080/"
            , getRequestTimeout = Just ET.defaultTimeout
            , getRequestRedirects = Just 10
            }

getResponseHttp :: (ToJSON a,L.MonadFlow m) => Either S.ServerError a -> m ET.HTTPResponse
getResponseHttp val = do
  case val of
    Left err -> do
      let defResponse = ET.HTTPResponse
            {  getResponseBody = ET.LBinaryString $ (S.errBody err),
                getResponseCode = S.errHTTPCode err, 
                getResponseHeaders = Map.empty,
                getResponseStatus = T.pack $ S.errReasonPhrase err
            }
      pure defResponse
    Right b -> do
      pure $ ET.HTTPResponse {
        getResponseBody = ET.LBinaryString $ A.encode $ toJSON b,
        getResponseCode = 0,
        getResponseHeaders = Map.empty,
        getResponseStatus = ""
      }

getRecording :: (ToJSON a )=> ET.HTTPRequest -> Either S.ServerError a -> Bool -> Text -> L.Flow ()
getRecording reqHttp res runningMode sessId =  do
      shouldRecord <- isArtRecEnabled
      let reqMethod =  ET.getRequestMethod reqHttp
          reqUrl = ET.getRequestURL reqHttp
          reqHeaders = ET.getRequestHeaders reqHttp
      resHttP <-  getResponseHttp res
      when (runningMode && shouldRecord) $ do
        readRecordingsAndWriteToFile reqMethod reqHttp resHttP sessId (Just reqUrl) (HM.fromList $ Map.toList reqHeaders)

tryBoth :: L.MonadFlow m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
tryBoth action1 action2 = do
    result1 <- action1
    case result1 of
        Just _  -> return result1
        Nothing -> action2