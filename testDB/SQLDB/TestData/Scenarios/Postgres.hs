{-# LANGUAGE RecordWildCards #-}

module SQLDB.TestData.Scenarios.Postgres where

import           Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import qualified Database.Beam.Postgres as BP
import qualified EulerHS.Language as L
import           EulerHS.Prelude
import qualified EulerHS.Types as T
import           SQLDB.TestData.Types

-- Scenarios

uniqueConstraintViolationDbScript :: T.DBConfig BP.Pg -> L.Flow (T.DBResult ())
uniqueConstraintViolationDbScript dbcfg = do
    econn <- L.getSqlDBConnection dbcfg

    flip (either $ error "Unable to get connection") econn $ \conn -> do
      _ <- L.runDB conn
        $ L.insertRows
        $ B.insert (_users eulerDb)
        $ B.insertValues [User 2 "Eve" "Beon"]

      L.runDB conn
        $ L.insertRows
        $ B.insert (_users eulerDb)
        $ B.insertValues [User 2 "Eve" "Beon"]


selectUnknownDbScript :: T.DBConfig BP.Pg -> L.Flow (T.DBResult (Maybe User))
selectUnknownDbScript dbcfg = do
    econn <- L.getSqlDBConnection dbcfg

    flip (either $ error "Unable to get connection") econn $ \conn ->
      L.runDB conn $ do
        let predicate User {..} = _userFirstName ==. "Unknown"
        L.findRow
          $ B.select
          $ B.limit_ 1
          $ B.filter_ predicate
          $ B.all_ (_users eulerDb)


selectOneDbScript :: T.DBConfig BP.Pg -> L.Flow (T.DBResult (Maybe User))
selectOneDbScript dbcfg = do
    econn <- L.getSqlDBConnection dbcfg

    flip (either $ error "Unable to get connection") econn $ \conn -> do
      _ <- L.runDB conn
        $ L.insertRows
        $ B.insert (_users eulerDb)
        $ B.insertExpressions (mkUser <$> susers)

      L.runDB conn $ do
        let predicate User {..} = _userFirstName ==. "John"

        L.findRow
          $ B.select
          $ B.limit_ 1
          $ B.filter_ predicate
          $ B.all_ (_users eulerDb)


insertReturningScript :: T.DBConfig BP.Pg -> L.Flow (T.DBResult [User])
insertReturningScript dbcfg = do
    econn <- L.getSqlDBConnection dbcfg

    flip (either $ error "Unable to get connection") econn $ \conn ->
      L.runDB conn
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


updateAndSelectDbScript :: T.DBConfig BP.Pg -> L.Flow (T.DBResult (Maybe User))
updateAndSelectDbScript dbcfg = do
    econn <- L.getSqlDBConnection dbcfg

    flip (either $ error "Unable to get connection") econn $ \conn ->
      L.runDB conn $ do
        let predicate1 User {..} = _userFirstName ==. "John"

        L.updateRows $ B.update (_users eulerDb)
          (\User {..} -> mconcat
            [ _userFirstName <-. "Leo"
            , _userLastName  <-. "San"
            ]
          )
          predicate1

        let predicate2 User {..} = _userFirstName ==. "Leo"
        L.findRow
          $ B.select
          $ B.limit_ 1
          $ B.filter_ predicate2
          $ B.all_ (_users eulerDb)
