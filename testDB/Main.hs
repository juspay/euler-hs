module Main where

import           EulerHS.Prelude
import qualified SQLDB.Tests.QueryExamplesSpec as Ex
import qualified SQLDB.Tests.SQLiteDBSpec as SQLiteDB
import           Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  SQLiteDB.spec
  Ex.spec

  -- Disable until it work here  in jenkins
  -- KVDB.spec
  -- PGDB.spec
  -- PGDBP.spec
  -- MySQL.spec
  -- MySQLP.spec
