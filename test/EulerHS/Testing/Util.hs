module EulerHS.Testing.Util where

import           Control.Monad.Except (MonadError, runExceptT, throwError)
import           Data.Aeson (Result (..), Value (..))
import qualified Data.Aeson as Aeson
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Vector (Vector (..), (!?))
import qualified Data.Vector as Vector
import           EulerHS.Prelude

import           EulerHS.Testing.CommonLog (CommonLog (..), HasCommonLog)
import qualified EulerHS.Testing.CommonLog as CommonLog
import qualified EulerHS.Testing.HSLog as HSLog
import qualified EulerHS.Testing.PSLog as PSLog


readJSONFile :: FilePath -> IO (Maybe Aeson.Value)
readJSONFile fp = Aeson.decodeFileStrict fp

purescriptEntries :: Value -> Maybe (Vector Value)
purescriptEntries v = do
  methodRec <- caseJSONObject (HashMap.lookup "methodRecording") v
  entries   <- caseJSONObject (HashMap.lookup "entries") methodRec
  caseJSONArray Just entries

haskellEntries :: Value -> Maybe (Vector Value)
haskellEntries v = do
  methodRec <- caseJSONObject (HashMap.lookup "methodRecording") v
  entries   <- caseJSONObject (HashMap.lookup "mrEntries") methodRec
  recording <- caseJSONObject (HashMap.lookup "recording") entries
  caseJSONArray Just recording

caseJSONObject :: (HashMap Text Value -> Maybe a) -> Value -> Maybe a
caseJSONObject f v = case v of
    Object o -> f o
    _        -> Nothing

caseJSONArray :: (Vector Value -> Maybe a) -> Value -> Maybe a
caseJSONArray f v = case v of
  Array a -> f a
  _       -> Nothing

data CheckJSONError
  = MissingEventError CommonLog
  | LoadJsonError
  | ImpossibleError
  deriving (Eq, Ord, Show)

checkJSONFiles :: (MonadIO m) => FilePath -> FilePath -> m (Either CheckJSONError ())
checkJSONFiles psfile haskellfile = runExceptT $ do
  psVal <- liftIO $ readJSONFile psfile
  hsVal <- liftIO $ readJSONFile haskellfile
  psLogs <- processPSFile psVal
  hsLogs <- processHSFile hsVal
  compareLogs psLogs hsLogs
  pure $ ()

compareLogs :: (MonadError CheckJSONError m) => Vector CommonLog -> Vector CommonLog -> m ()
compareLogs psLogs hsLogs = case psLogs !? 0 of
  Nothing  -> pure ()
  Just log -> case log `elem` hsLogs of
    False -> throwError $ MissingEventError log
    True  -> compareLogs (Vector.take 1 psLogs) (drop1of log hsLogs)

drop1of :: forall a. (Eq a) => a -> Vector a -> Vector a
drop1of x xs = fst $ foldr go (Vector.empty, Vector.empty) xs
  where
    go :: a -> (Vector a, Vector a) -> (Vector a, Vector a)
    go new acc@(l, r) = if Vector.null r
      then if (new == x) then (l, Vector.cons new r) else (Vector.cons new l, r)
      else acc


processPSFile :: (MonadError CheckJSONError m) => Maybe Value ->  m (Vector CommonLog)
processPSFile maybeValue = do
  case maybeValue of
    Nothing -> throwError LoadJsonError
    Just val -> do
      case (  fmap CommonLog.fromPSLog . catMaybeVec . fmap (aesonMaybe . Aeson.fromJSON)) <$> purescriptEntries val of
        Nothing -> throwError ImpossibleError
        Just vec -> pure . Vector.filter (not . CommonLog.isLog) $ catMaybeVec vec

processHSFile :: (MonadError CheckJSONError m) => Maybe Value ->  m (Vector CommonLog)
processHSFile maybeValue = do
  case maybeValue of
    Nothing -> throwError LoadJsonError
    Just val -> do
      case (fmap CommonLog.fromHSLog . catMaybeVec . fmap (aesonMaybe . Aeson.fromJSON)) <$> haskellEntries val of
        Nothing  -> throwError ImpossibleError
        Just vec -> pure $ catMaybeVec vec

aesonMaybe :: Result a -> Maybe a
aesonMaybe = \case
  Error _ -> Nothing
  Success a -> Just a

catMaybeVec :: Vector (Maybe a) -> Vector a
catMaybeVec = foldr go Vector.empty
  where
    go :: Maybe a -> Vector a ->  Vector a
    go new acc = case new of
      Nothing -> acc
      Just n  -> acc <> Vector.singleton n

-- psVal :: Maybe Value


-- the plan
-- 1) read in json from file for haskell and PS ( Aeson value ) - done
-- 2) parse the value up into a data type (Haskell)
-- 3) parse the PS value up into a data type
-- 4) parse both values into some comparison type, which unifies discrepancies
-- 5) check that the haskell log has effects in the same order as the purescript log




