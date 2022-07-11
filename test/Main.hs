{-# OPTIONS_GHC -Werror #-}

module Main (main) where

-- import qualified ArtSpec as Art
-- import           Control.Exception.Safe (bracket)
import           EulerHS.Prelude hiding (bracket)
import qualified EulerHS.Types as T
import qualified EulerHS.Tests.Framework.FlowSpec as Flow
-- import qualified KVDBArtSpec as KVDB
-- import qualified PubSubSpec as PubSub
-- import qualified SQLArtSpec as SQL
import qualified EulerHS.Tests.Framework.MaskingSpec as MaskSpec
-- import qualified CachedDBSpec as CachedSqlDBQuery
-- import           System.Directory (createDirectory, getTemporaryDirectory,
--                                    removePathForcibly)
-- import           System.FilePath ((<.>), (</>))
-- import           System.Process.Typed (proc, startProcess, stopProcess)
-- import           System.Random (getStdRandom, random)
import           Test.Hspec (hspec)

main :: IO ()
main = do
  -- Redis not works on CI
  -- withRedis $
  hspec $ do
    Flow.spec logsDisabled
    MaskSpec.spec

    -- Wait for Redis on CI
    -- CachedSqlDBQuery.spec

    -- ART removed and these tests not work anymore
    -- Art.spec
    -- KVDB.spec
    -- SQL.spec
    -- PubSub.spec




-- Helpers

-- withRedis :: IO () -> IO ()
-- withRedis act = withTempRedisDir $ \redisDir ->
--   withTempRedisConfig redisDir go
--   where
--     go :: FilePath -> IO ()
--     go redisConfPath =
--       bracket (startProcess . proc "redis-server" $ [redisConfPath])
--               stopProcess
--               (const act)

logsDisabled :: Maybe T.LoggerConfig
logsDisabled = Nothing

-- withTempRedisDir :: (FilePath -> IO a) -> IO a
-- withTempRedisDir act = do
--   rand :: Word <- liftIO . getStdRandom $ random
--   tmp <- liftIO getTemporaryDirectory
--   let tempDir = tmp </> ("redis" <> show rand)
--   bracket (liftIO . createDirectory $ tempDir)
--           (\_ -> liftIO . removePathForcibly $ tempDir)
--           (\_ -> act tempDir)

-- withTempRedisConfig :: FilePath -> (FilePath -> IO ()) -> IO ()
-- withTempRedisConfig tmpRedisDir act = do
--   let tmpRedisConfPath = tmpRedisDir </> "redis" <.> "conf"
--   bracket (withFile tmpRedisConfPath WriteMode go)
--           (\_ -> removePathForcibly tmpRedisConfPath)
--           (\_ -> act tmpRedisConfPath)
--   where
--     go :: Handle -> IO ()
--     go h = hPutStrLn @String h $ "dir " +| tmpRedisDir |+ ""
