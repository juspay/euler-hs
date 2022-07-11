{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}

module EulerHS.Tests.Framework.SQLArtSpec
  (
    -- spec
  ) where

-- import           Common
-- import           DBSetup
-- import           Data.Aeson as A
-- import           Data.Aeson.Encode.Pretty
-- import qualified Data.Map as Map
-- import qualified Database.Beam as B
-- import qualified Database.Beam.Backend.SQL as B
-- import qualified Database.Beam.Query as B
-- import           Database.Beam.Sqlite.Connection (Sqlite, SqliteM)
-- import           EulerHS.Interpreters as I
-- import           EulerHS.Language as L
-- import           EulerHS.Prelude
-- import           EulerHS.Runtime
-- import           EulerHS.Types as T
-- import           Test.Hspec

-- Write record to file or to stdout. Choose at 'runWithSQLConn'
-- writeRecord :: IO ()
-- writeRecord = withEmptyDB $ \rt -> do
--   void $ runWithSQLConn $ do
--     conn <- connectOrFail sqliteCfg
--     L.runDB conn $ do
--       L.insertRows $ B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates"]
--       res <- L.findRow $ B.select $ B.filter_ (\u -> _userGUID u B.==. 1) $ B.all_ (users userDB)
--       L.deleteRows $ B.delete (users userDB) (\u -> _userGUID u B.==. 1)
--       pure res

-- Record and play scenario. Return result.
-- recordAndPlay :: IO ()
-- recordAndPlay = withEmptyDB $ \rt -> do
--   record <- runFlowWithArt $ do
--     conn <- connectOrFail sqliteCfg
--     L.runDB conn $ do
--       L.insertRows $ B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates"]
--       res <- L.findRow $ B.select $ B.filter_ (\u -> _userGUID u B.==. 1) $ B.all_ (users userDB)
--       L.deleteRows $ B.delete (users userDB) (\u -> _userGUID u B.==. 1)
--       pure res
--   putStrLn $ encodePretty record


-- run = replayRecording
-- -- run _ = runWithSQLConn

-- -- Tests

-- spec :: Spec
-- spec =
--   around (withEmptyDB) $ do
--     describe "ART SQL tests" $ do

--       it "success to get one correct row" $ \rt -> do
--         result <- run getRowRecord $ do
--           conn <- connectOrFail sqliteCfg
--           L.runDB conn $ do
--             L.insertRows $ B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates"]
--             res <- L.findRow $ B.select $ B.filter_ (\u -> _userGUID u B.==. 1) $ B.all_ (users userDB)
--             L.deleteRows $ B.delete (users userDB) (\u -> _userGUID u B.==. 1)
--             pure res
--         result `shouldBe` Right (Just (User {_userGUID = 1, _firstName = "Bill", _lastName = "Gates"}))

--       it "fail to get one wrong row" $ \rt -> do
--         result <- run getWrongRowRecord $ do
--           conn <- connectOrFail sqliteCfg
--           L.runDB conn $ do
--             L.insertRows $ B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates"]
--             res <- L.findRow $ B.select $ B.filter_ (\u -> _userGUID u B.==. 2) $ B.all_ (users userDB)
--             L.deleteRows $ B.delete (users userDB) (\u -> _userGUID u B.==. 1)
--             pure res
--         result `shouldBe` Right Nothing

--       it "success to get correct rows" $ \rt -> do
--         result <- run getRowsRecord $ do
--           conn <- connectOrFail sqliteCfg
--           L.runDB conn $ do
--             L.insertRows $ B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates", User 2 "Stive" "Jobs"]
--             res <- L.findRows $ B.select $ B.filter_ (\u -> _userGUID u `B.in_` [1,2]) $ B.all_ (users userDB)
--             L.deleteRows $ B.delete (users userDB) (\u -> (_userGUID u) `B.in_` [1,2])
--             pure res
--         result `shouldBe` Right
--           [ User {_userGUID = 1, _firstName = "Bill", _lastName = "Gates"}
--           , User {_userGUID = 2, _firstName = "Stive", _lastName = "Jobs"}
--           ]

--       it "fail to get an uncorrect rows" $ \rt -> do
--         result <- run getWrongRowsRecord $ do
--           conn <- connectOrFail sqliteCfg
--           L.runDB conn $ do
--             L.insertRows $ B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates", User 2 "Stive" "Jobs"]
--             res <- L.findRows $ B.select $ B.filter_ (\u -> _userGUID u `B.in_` [3,4]) $ B.all_ (users userDB)
--             L.deleteRows $ B.delete (users userDB) (\u -> (_userGUID u) `B.in_` [1,2])
--             pure res
--         result `shouldBe` Right []

--       it "success to delete existing rows" $ \rt -> do
--         result <- run deleteRowsRecord $ do
--           conn <- connectOrFail sqliteCfg
--           L.runDB conn $ do
--             L.insertRows $ B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates", User 2 "Stive" "Jobs"]
--             L.deleteRows $ B.delete (users userDB) (\u -> (_userGUID u) `B.in_` [1,2])
--             res <- L.findRows $ B.select $ B.filter_ (\u -> _userGUID u `B.in_` [1,2]) $ B.all_ (users userDB)
--             pure res
--         result `shouldBe` Right []

--       it "fail to delete wrong rows" $ \rt -> do
--         result <- run deleteWrongRowsRecord $ do
--           conn <- connectOrFail sqliteCfg
--           L.runDB conn $ do
--             L.insertRows $ B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates", User 2 "Stive" "Jobs"]
--             L.deleteRows $ B.delete (users userDB) (\u -> (_userGUID u) `B.in_` [3,4])
--             res <- L.findRows $ B.select $ B.filter_ (\u -> _userGUID u `B.in_` [1,2]) $ B.all_ (users userDB)
--             L.deleteRows $ B.delete (users userDB) (\u -> (_userGUID u) `B.in_` [1,2])
--             pure res
--         result `shouldBe` Right
--           [ User {_userGUID = 1, _firstName = "Bill", _lastName = "Gates"}
--           , User {_userGUID = 2, _firstName = "Stive", _lastName = "Jobs"}
--           ]

--       it "success to update rows" $ \rt -> do
--         result <- run updateRowRecord $ do
--           conn <- connectOrFail sqliteCfg
--           L.runDB conn $ do
--             L.insertRows $ B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates", User 2 "Stive" "Jobs"]
--             L.updateRows $ B.update (users userDB)
--                 (\user -> _firstName user B.<-. B.val_ "Robert")
--                 (\user -> _userGUID user B.==. 1)
--             res <- L.findRows $ B.select $ B.filter_ (\u -> _userGUID u `B.in_` [1,2]) $ B.all_ (users userDB)
--             L.deleteRows $ B.delete (users userDB) (\u -> (_userGUID u) `B.in_` [1,2])
--             pure res
--         result `shouldBe` Right
--           [ User {_userGUID = 1, _firstName = "Robert", _lastName = "Gates"}
--           , User {_userGUID = 2, _firstName = "Stive", _lastName = "Jobs"}
--           ]

--       it "success to update rows with IO action in between" $ \rt -> do
--         result <- run updateRowWithDelayRecord $ do
--           conn <- connectOrFail sqliteCfg
--           L.runDB conn $ do
--             L.insertRows $ B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates", User 2 "Stive" "Jobs"]
--             L.updateRows $ B.update (users userDB)
--                 (\user -> _firstName user B.<-. B.val_ "Robert")
--                 (\user -> _userGUID user B.==. 1)
--           L.runIO $ threadDelay (2 * 10 ^ 6)
--           L.runDB conn $ do
--             res <- L.findRows $ B.select $ B.filter_ (\u -> _userGUID u `B.in_` [1,2]) $ B.all_ (users userDB)
--             L.deleteRows $ B.delete (users userDB) (\u -> (_userGUID u) `B.in_` [1,2])
--             pure res
--         result `shouldBe` Right
--           [ User {_userGUID = 1, _firstName = "Robert", _lastName = "Gates"}
--           , User {_userGUID = 2, _firstName = "Stive", _lastName = "Jobs"}
--           ]


-- -- Use testRecord to generate record log to 'recorder' file
-- getRowRecord :: ResultRecording
-- getRowRecord = fromJust $ decode
--   "{\"recording\":[{\"_entryName\":\"GetSqlDBConnectionEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/language/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"getConnResult\":{\"Left\":[{\"tag\":\"ConnectionDoesNotExist\"},\"Connection for SQliteDB does not exists.\"]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"InitSqlDBConnectionEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/language/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"initConnResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunDBEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"rawSql\":[\"INSERT INTO \\\"users\\\"(\\\"id\\\", \\\"first_name\\\", \\\"last_name\\\") VALUES (?, ?, ?);\\n-- With values: [SQLInteger 1,SQLText \\\"Bill\\\",SQLText \\\"Gates\\\"]\",\"SELECT \\\"t0\\\".\\\"id\\\" AS \\\"res0\\\", \\\"t0\\\".\\\"first_name\\\" AS \\\"res1\\\", \\\"t0\\\".\\\"last_name\\\" AS \\\"res2\\\" FROM \\\"users\\\" AS \\\"t0\\\" WHERE (\\\"t0\\\".\\\"id\\\")=(?);\\n-- With values: [SQLInteger 1]\",\"DELETE FROM \\\"users\\\" WHERE (\\\"id\\\")=(?);\\n-- With values: [SQLInteger 1]\"],\"jsonResult\":{\"Right\":{\"_userGUID\":1,\"_lastName\":\"Gates\",\"_firstName\":\"Bill\"}}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{},\"safeRecordings\":{}}"

-- getWrongRowRecord :: ResultRecording
-- getWrongRowRecord = fromJust $ decode
--   "{\"recording\":[{\"_entryName\":\"GetSqlDBConnectionEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/language/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"getConnResult\":{\"Left\":[{\"tag\":\"ConnectionDoesNotExist\"},\"Connection for SQliteDB does not exists.\"]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"InitSqlDBConnectionEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/language/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"initConnResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunDBEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"rawSql\":[\"INSERT INTO \\\"users\\\"(\\\"id\\\", \\\"first_name\\\", \\\"last_name\\\") VALUES (?, ?, ?);\\n-- With values: [SQLInteger 1,SQLText \\\"Bill\\\",SQLText \\\"Gates\\\"]\",\"SELECT \\\"t0\\\".\\\"id\\\" AS \\\"res0\\\", \\\"t0\\\".\\\"first_name\\\" AS \\\"res1\\\", \\\"t0\\\".\\\"last_name\\\" AS \\\"res2\\\" FROM \\\"users\\\" AS \\\"t0\\\" WHERE (\\\"t0\\\".\\\"id\\\")=(?);\\n-- With values: [SQLInteger 2]\",\"DELETE FROM \\\"users\\\" WHERE (\\\"id\\\")=(?);\\n-- With values: [SQLInteger 1]\"],\"jsonResult\":{\"Right\":null}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{},\"safeRecordings\":{}}"

-- getRowsRecord :: ResultRecording
-- getRowsRecord = fromJust $ decode
--   "{\"recording\":[{\"_entryName\":\"GetSqlDBConnectionEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/language/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"getConnResult\":{\"Left\":[{\"tag\":\"ConnectionDoesNotExist\"},\"Connection for SQliteDB does not exists.\"]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"InitSqlDBConnectionEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/language/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"initConnResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunDBEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"rawSql\":[\"INSERT INTO \\\"users\\\"(\\\"id\\\", \\\"first_name\\\", \\\"last_name\\\") VALUES (?, ?, ?), (?, ?, ?);\\n-- With values: [SQLInteger 1,SQLText \\\"Bill\\\",SQLText \\\"Gates\\\",SQLInteger 2,SQLText \\\"Stive\\\",SQLText \\\"Jobs\\\"]\",\"SELECT \\\"t0\\\".\\\"id\\\" AS \\\"res0\\\", \\\"t0\\\".\\\"first_name\\\" AS \\\"res1\\\", \\\"t0\\\".\\\"last_name\\\" AS \\\"res2\\\" FROM \\\"users\\\" AS \\\"t0\\\" WHERE (\\\"t0\\\".\\\"id\\\") IN (?, ?);\\n-- With values: [SQLInteger 1,SQLInteger 2]\",\"DELETE FROM \\\"users\\\" WHERE (\\\"id\\\") IN (?, ?);\\n-- With values: [SQLInteger 1,SQLInteger 2]\"],\"jsonResult\":{\"Right\":[{\"_userGUID\":1,\"_lastName\":\"Gates\",\"_firstName\":\"Bill\"},{\"_userGUID\":2,\"_lastName\":\"Jobs\",\"_firstName\":\"Stive\"}]}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{},\"safeRecordings\":{}}"

-- getWrongRowsRecord :: ResultRecording
-- getWrongRowsRecord = fromJust $ decode
--   "{\"recording\":[{\"_entryName\":\"GetSqlDBConnectionEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/language/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"getConnResult\":{\"Left\":[{\"tag\":\"ConnectionDoesNotExist\"},\"Connection for SQliteDB does not exists.\"]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"InitSqlDBConnectionEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/language/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"initConnResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunDBEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"rawSql\":[\"INSERT INTO \\\"users\\\"(\\\"id\\\", \\\"first_name\\\", \\\"last_name\\\") VALUES (?, ?, ?), (?, ?, ?);\\n-- With values: [SQLInteger 1,SQLText \\\"Bill\\\",SQLText \\\"Gates\\\",SQLInteger 2,SQLText \\\"Stive\\\",SQLText \\\"Jobs\\\"]\",\"SELECT \\\"t0\\\".\\\"id\\\" AS \\\"res0\\\", \\\"t0\\\".\\\"first_name\\\" AS \\\"res1\\\", \\\"t0\\\".\\\"last_name\\\" AS \\\"res2\\\" FROM \\\"users\\\" AS \\\"t0\\\" WHERE (\\\"t0\\\".\\\"id\\\") IN (?, ?);\\n-- With values: [SQLInteger 3,SQLInteger 4]\",\"DELETE FROM \\\"users\\\" WHERE (\\\"id\\\") IN (?, ?);\\n-- With values: [SQLInteger 1,SQLInteger 2]\"],\"jsonResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{},\"safeRecordings\":{}}"


-- deleteRowsRecord :: ResultRecording
-- deleteRowsRecord = fromJust $ decode
--   "{\"recording\":[{\"_entryName\":\"GetSqlDBConnectionEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/language/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"getConnResult\":{\"Left\":[{\"tag\":\"ConnectionDoesNotExist\"},\"Connection for SQliteDB does not exists.\"]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"InitSqlDBConnectionEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/language/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"initConnResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunDBEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"rawSql\":[\"INSERT INTO \\\"users\\\"(\\\"id\\\", \\\"first_name\\\", \\\"last_name\\\") VALUES (?, ?, ?), (?, ?, ?);\\n-- With values: [SQLInteger 1,SQLText \\\"Bill\\\",SQLText \\\"Gates\\\",SQLInteger 2,SQLText \\\"Stive\\\",SQLText \\\"Jobs\\\"]\",\"DELETE FROM \\\"users\\\" WHERE (\\\"id\\\") IN (?, ?);\\n-- With values: [SQLInteger 1,SQLInteger 2]\",\"SELECT \\\"t0\\\".\\\"id\\\" AS \\\"res0\\\", \\\"t0\\\".\\\"first_name\\\" AS \\\"res1\\\", \\\"t0\\\".\\\"last_name\\\" AS \\\"res2\\\" FROM \\\"users\\\" AS \\\"t0\\\" WHERE (\\\"t0\\\".\\\"id\\\") IN (?, ?);\\n-- With values: [SQLInteger 1,SQLInteger 2]\"],\"jsonResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{},\"safeRecordings\":{}}"

-- deleteWrongRowsRecord :: ResultRecording
-- deleteWrongRowsRecord = fromJust $ decode
--   "{\"recording\":[{\"_entryName\":\"GetSqlDBConnectionEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/language/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"getConnResult\":{\"Left\":[{\"tag\":\"ConnectionDoesNotExist\"},\"Connection for SQliteDB does not exists.\"]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"InitSqlDBConnectionEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/language/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"initConnResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunDBEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"rawSql\":[\"INSERT INTO \\\"users\\\"(\\\"id\\\", \\\"first_name\\\", \\\"last_name\\\") VALUES (?, ?, ?), (?, ?, ?);\\n-- With values: [SQLInteger 1,SQLText \\\"Bill\\\",SQLText \\\"Gates\\\",SQLInteger 2,SQLText \\\"Stive\\\",SQLText \\\"Jobs\\\"]\",\"DELETE FROM \\\"users\\\" WHERE (\\\"id\\\") IN (?, ?);\\n-- With values: [SQLInteger 3,SQLInteger 4]\",\"SELECT \\\"t0\\\".\\\"id\\\" AS \\\"res0\\\", \\\"t0\\\".\\\"first_name\\\" AS \\\"res1\\\", \\\"t0\\\".\\\"last_name\\\" AS \\\"res2\\\" FROM \\\"users\\\" AS \\\"t0\\\" WHERE (\\\"t0\\\".\\\"id\\\") IN (?, ?);\\n-- With values: [SQLInteger 1,SQLInteger 2]\",\"DELETE FROM \\\"users\\\" WHERE (\\\"id\\\") IN (?, ?);\\n-- With values: [SQLInteger 1,SQLInteger 2]\"],\"jsonResult\":{\"Right\":[{\"_userGUID\":1,\"_lastName\":\"Gates\",\"_firstName\":\"Bill\"},{\"_userGUID\":2,\"_lastName\":\"Jobs\",\"_firstName\":\"Stive\"}]}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{},\"safeRecordings\":{}}"

-- updateRowRecord :: ResultRecording
-- updateRowRecord = fromJust $ decode
--   "{\"recording\":[{\"_entryName\":\"GetSqlDBConnectionEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/language/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"getConnResult\":{\"Left\":[{\"tag\":\"ConnectionDoesNotExist\"},\"Connection for SQliteDB does not exists.\"]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"InitSqlDBConnectionEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/language/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"initConnResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunDBEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"rawSql\":[\"INSERT INTO \\\"users\\\"(\\\"id\\\", \\\"first_name\\\", \\\"last_name\\\") VALUES (?, ?, ?), (?, ?, ?);\\n-- With values: [SQLInteger 1,SQLText \\\"Bill\\\",SQLText \\\"Gates\\\",SQLInteger 2,SQLText \\\"Stive\\\",SQLText \\\"Jobs\\\"]\",\"UPDATE \\\"users\\\" SET \\\"first_name\\\"=? WHERE (\\\"id\\\")=(?);\\n-- With values: [SQLText \\\"Robert\\\",SQLInteger 1]\",\"SELECT \\\"t0\\\".\\\"id\\\" AS \\\"res0\\\", \\\"t0\\\".\\\"first_name\\\" AS \\\"res1\\\", \\\"t0\\\".\\\"last_name\\\" AS \\\"res2\\\" FROM \\\"users\\\" AS \\\"t0\\\" WHERE (\\\"t0\\\".\\\"id\\\") IN (?, ?);\\n-- With values: [SQLInteger 1,SQLInteger 2]\",\"DELETE FROM \\\"users\\\" WHERE (\\\"id\\\") IN (?, ?);\\n-- With values: [SQLInteger 1,SQLInteger 2]\"],\"jsonResult\":{\"Right\":[{\"_userGUID\":1,\"_lastName\":\"Gates\",\"_firstName\":\"Robert\"},{\"_userGUID\":2,\"_lastName\":\"Jobs\",\"_firstName\":\"Stive\"}]}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{},\"safeRecordings\":{}}"

-- updateRowWithDelayRecord :: ResultRecording
-- updateRowWithDelayRecord = fromJust $ decode
--   "{\"recording\":[{\"_entryName\":\"GetSqlDBConnectionEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/language/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"getConnResult\":{\"Left\":[{\"tag\":\"ConnectionDoesNotExist\"},\"Connection for SQliteDB does not exists.\"]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"InitSqlDBConnectionEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"dBConfig\":{\"tag\":\"SQLitePoolConf\",\"contents\":[\"SQliteDB\",\"test/language/EulerHS/TestData/test.db\",{\"resourcesPerStripe\":50,\"stripes\":1,\"keepAlive\":10}]},\"initConnResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunDBEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"rawSql\":[\"INSERT INTO \\\"users\\\"(\\\"id\\\", \\\"first_name\\\", \\\"last_name\\\") VALUES (?, ?, ?), (?, ?, ?);\\n-- With values: [SQLInteger 1,SQLText \\\"Bill\\\",SQLText \\\"Gates\\\",SQLInteger 2,SQLText \\\"Stive\\\",SQLText \\\"Jobs\\\"]\",\"UPDATE \\\"users\\\" SET \\\"first_name\\\"=? WHERE (\\\"id\\\")=(?);\\n-- With values: [SQLText \\\"Robert\\\",SQLInteger 1]\"],\"jsonResult\":{\"Right\":[]}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":3,\"_entryPayload\":{\"jsonResult\":[],\"description\":\"\"},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunDBEntry\",\"_entryIndex\":4,\"_entryPayload\":{\"rawSql\":[\"SELECT \\\"t0\\\".\\\"id\\\" AS \\\"res0\\\", \\\"t0\\\".\\\"first_name\\\" AS \\\"res1\\\", \\\"t0\\\".\\\"last_name\\\" AS \\\"res2\\\" FROM \\\"users\\\" AS \\\"t0\\\" WHERE (\\\"t0\\\".\\\"id\\\") IN (?, ?);\\n-- With values: [SQLInteger 1,SQLInteger 2]\",\"DELETE FROM \\\"users\\\" WHERE (\\\"id\\\") IN (?, ?);\\n-- With values: [SQLInteger 1,SQLInteger 2]\"],\"jsonResult\":{\"Right\":[{\"_userGUID\":1,\"_lastName\":\"Gates\",\"_firstName\":\"Robert\"},{\"_userGUID\":2,\"_lastName\":\"Jobs\",\"_firstName\":\"Stive\"}]}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{},\"safeRecordings\":{}}"



