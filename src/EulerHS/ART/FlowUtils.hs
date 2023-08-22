{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}




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
    readRecordingsAndWriteToFileForkFLow
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           EulerHS.ART.Types
import qualified EulerHS.ART.Types as ART
import qualified EulerHS.Language as L
import           EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified Data.Text.Lazy as TL
import qualified Servant as S
import qualified Data.Map                                  as Map
import EulerHS.ART.EnvVars

dateTimeFormatSeparatedBy_ :: String
dateTimeFormatSeparatedBy_ = "%F_%H-%M-%S"

addRecToState :: (L.MonadFlow m) => ART.RecordingEntry -> m ()
addRecToState newRecording = if isArtRecEnabled then L.appendRecordingLocal newRecording else pure ()

writeRecToFile :: (L.MonadFlow m) => Text -> ART.MethodRecordingDescription -> m ()
writeRecToFile reqId mrd = do
    case removeTrailingSlash getDirPath of
      Just dirPath -> do
        let fileName = dirPath <> "/" <> reqId <> ".json"
        res <- L.runIO $ tryWriteFile fileName
        case res of
          Right _ -> pure ()
          Left err -> L.logErrorT "Recording Failed" (T.pack $ displayException err)
      Nothing -> L.logErrorT "The directory path seems to be wrong" getDirPath
    where
      tryWriteFile :: Text -> IO (Either SomeException ())
      tryWriteFile fileName = catch (Right <$> BS.writeFile (T.unpack $ fileName) (A.encode mrd)) (return . Left)

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
  let methodRecordingEnties = loopOverEntriesWithIndex 0 entriesList []
  let recordings =
        MethodRecording
          { jsonRequest = A.Null,
            jsonResponse = A.Null,
            entries = methodRecordingEnties,
            methodConfigs = Nothing,
            sessionId = fromMaybe "NO_REQUEST_ID" msessionId,
            guid = Just guId ,
            parameters = HM.singleton "description" desc
          }
      recordingDescription =
        MethodRecordingDescription
          { methodName = "FORK-FLOW",
            methodRecording = recordings
          }
  L.delRecordingLocal
  writeRecToFile (guId) recordingDescription

readRecordingsAndWriteToFile :: ET.HTTPMethod -> ET.HTTPRequest -> ET.HTTPResponse -> Text -> Maybe Text -> HM.HashMap Text Text -> L.Flow ()
readRecordingsAndWriteToFile _method request response sId mUrl rps = do
  entriesList :: [RecordingEntry] <- L.getRecordingLocal
  let methodRecordingEnties = loopOverEntriesWithIndex 0 entriesList []
  let recordings =
        MethodRecording
          { jsonRequest = toJSON request,
            jsonResponse = toJSON response,
            entries = methodRecordingEnties,
            methodConfigs = Nothing,
            sessionId = sId,
            guid = Nothing ,
            parameters = rps
          }
      recordingDescription =
        MethodRecordingDescription
          { methodName = fromMaybe "NO_URL" mUrl,
            methodRecording = recordings
          }
  writeRecToFile sId recordingDescription

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
  let defResponse = ET.HTTPResponse
            {  getResponseBody = ET.LBinaryString $ A.encode $ toJSON $ T.pack "jsonNull",
                getResponseCode = 0, 
                getResponseHeaders = Map.empty,
                getResponseStatus = ""
            }
  case val of
    Left _ -> pure defResponse
    Right b -> do
      pure $ ET.HTTPResponse {
        getResponseBody = ET.LBinaryString $ A.encode $ toJSON b,
        getResponseCode = 0,
        getResponseHeaders = Map.empty,
        getResponseStatus = ""
      }

getRecording :: (ToJSON a )=> ET.HTTPRequest -> Either S.ServerError a -> Bool -> Text -> L.Flow ()
getRecording reqHttp res runningMode sessId =  do
      let reqMethod =  ET.getRequestMethod reqHttp
          reqUrl = ET.getRequestURL reqHttp
          reqHeaders = ET.getRequestHeaders reqHttp
      resHttP <-  getResponseHttp res
      when (runningMode && isArtRecEnabled) $ do
        readRecordingsAndWriteToFile reqMethod reqHttp resHttP sessId (Just reqUrl) (HM.fromList $ Map.toList reqHeaders)
