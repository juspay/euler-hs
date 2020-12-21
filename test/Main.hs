module Main where

import           EulerHS.Prelude
import           Test.Hspec

import qualified EulerHS.Tests.Framework.ArtSpec as Art
import qualified EulerHS.Tests.Framework.FlowSpec as Framework
import qualified EulerHS.Tests.Framework.KVDBArtSpec as KVDB
import qualified EulerHS.Tests.Framework.PubSubSpec as PubSub
import qualified EulerHS.Tests.Framework.SQLArtSpec as SQL
import qualified EulerHS.Types as T

import System.Process

withRedis :: IO () -> IO ()
withRedis action = do
  cmdHandle <- spawnCommand "redis-server"
  action
  terminateProcess cmdHandle

logsEnabled :: Maybe T.LoggerConfig
logsEnabled = Just $ T.LoggerConfig
  { T._logToFile = False,
    T._logFilePath = "",
    T._isAsync = False,
    T._logLevel = T.Debug,
    T._logToConsole = True,
    T._maxQueueSize = 1000,
    T._logRawSql = False
  }

logsDisabled :: Maybe T.LoggerConfig
logsDisabled = Nothing

main :: IO ()
main = do
  withRedis $ hspec $ do
    Framework.spec logsDisabled
    Art.spec
    KVDB.spec
    SQL.spec
    PubSub.spec
