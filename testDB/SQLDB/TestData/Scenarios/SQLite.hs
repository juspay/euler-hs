{-# LANGUAGE RecordWildCards #-}

module SQLDB.TestData.Scenarios.SQLite where

import           Database.Beam ((/=.), (<-.), (==.))
import qualified Database.Beam as B
import qualified Database.Beam.Sqlite as BS
import qualified EulerHS.Language as L
import           EulerHS.Prelude
import qualified EulerHS.Types as T
import           SQLDB.TestData.Connections
import           SQLDB.TestData.Types

-- Scenarios

deleteTestValues :: T.DBConfig BS.SqliteM -> L.Flow ()
deleteTestValues cfg = do
  conn <- connectOrFail cfg -- $ T.mkSQLiteConfig testDBName
  void $ L.runDB conn
    $ L.deleteRows
    $ B.delete (_users eulerDb) (\u -> _userId u /=. B.val_ 0)
  void $ L.runDB conn
    $ L.updateRows
    $ B.update (_sqlite_sequence sqliteSequenceDb)
          (\SqliteSequence {..} -> mconcat [_seq <-. B.val_ 0])
          (\SqliteSequence {..} -> _name ==. B.val_ "users")

insertTestValues :: T.DBConfig BS.SqliteM -> L.Flow ()
insertTestValues cfg = do
  conn <- connectOrFail cfg
  void $ L.runDB conn
    $ L.insertRows
    $ B.insert (_users eulerDb)
    $ B.insertExpressions
          [ User B.default_
              ( B.val_ "John" )
              ( B.val_ "Doe"  )
          , User B.default_
              ( B.val_ "Doe"  )
              ( B.val_ "John" )
          ]
  L.deinitSqlDBConnection conn

uniqueConstraintViolationDbScript :: T.DBConfig BS.SqliteM -> L.Flow (T.DBResult ())
uniqueConstraintViolationDbScript cfg = do
  connection <- connectOrFail cfg

  _ <- L.runDB connection
    $ L.insertRows
    $ B.insert (_users eulerDb)
    $ B.insertValues [User 1 "Eve" "Beon"]

  L.runDB connection
    $ L.insertRows
    $ B.insert (_users eulerDb)
    $ B.insertValues [User 1 "Eve" "Beon"]

selectUnknownDbScript :: T.DBConfig BS.SqliteM -> L.Flow (T.DBResult (Maybe User))
selectUnknownDbScript cfg = do
  connection <- connectOrFail cfg

  L.runDB connection $ do
    let predicate User {..} = _userFirstName ==. B.val_ "Unknown"

    L.findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (_users eulerDb)

selectOneDbScript :: T.DBConfig BS.SqliteM -> L.Flow (T.DBResult (Maybe User))
selectOneDbScript cfg = do
  connection <- connectOrFail cfg

  L.runDB connection $ do
    let predicate User {..} = _userFirstName ==. B.val_ "John"

    L.findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (_users eulerDb)

updateAndSelectDbScript :: T.DBConfig BS.SqliteM -> L.Flow (T.DBResult (Maybe User))
updateAndSelectDbScript cfg = do
  connection <- connectOrFail cfg

  L.runDB connection $ do
    let predicate1 User {..} = _userFirstName ==. B.val_ "John"

    L.updateRows $ B.update (_users eulerDb)
      (\User {..} -> mconcat
        [ _userFirstName <-. B.val_ "Leo"
        , _userLastName  <-. B.val_ "San"
        ]
      )
      predicate1

    let predicate2 User {..} = _userFirstName ==. B.val_ "Leo"
    L.findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate2
      $ B.all_ (_users eulerDb)

insertReturningScript :: T.DBConfig BS.SqliteM -> L.Flow (T.DBResult [User])
insertReturningScript cfg = do
  connection <- connectOrFail cfg
  L.runDB connection
    $ L.insertRowsReturningList
    $ B.insert (_users eulerDb)
    $ B.insertExpressions
      [ User B.default_
        ( B.val_ "John" )
        ( B.val_ "Doe"  )
      , User B.default_
        ( B.val_ "Doe"  )
        ( B.val_ "John" )
      ]
