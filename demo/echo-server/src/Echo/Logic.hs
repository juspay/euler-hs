module Echo.Logic where

import           EulerHS.Prelude

import qualified EulerHS.Language as L

import qualified Echo.Domain as D

-- Business logic

echoFlow :: Text -> Maybe Text -> Maybe Int -> L.Flow D.EchoMessage
echoFlow easterEgg mbPhrase mbNumber = do
  L.logDebug ("echoFlow" :: Text) $ "Prase and number got: " <> show (mbPhrase, mbNumber)
  let phrase = fromMaybe "" mbPhrase
  let number = fromMaybe 0 mbNumber
  pure $ D.EchoMessage phrase number easterEgg
