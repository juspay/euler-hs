module Main where

import           EulerHS.Prelude
import           Test.Hspec


import qualified KVDB.KVDBSpec as KVDB
import qualified SQLDB.Tests.SQLiteDBSpec as SQLiteDB
import qualified SQLDB.Tests.QueryExamplesSpec as Ex

-- Prepare your DBs environment and uncomment these lines
-- if you need integration testing of DB backents.
-- import qualified SQLDB.Tests.PostgresDBSpec as PGDB
-- import qualified SQLDB.Tests.PostgresDBPoolSpec as PGDBP
-- import qualified SQLDB.Tests.MySQLDBSpec as MySQL

main :: IO ()
main = hspec $ do
  KVDB.spec
  SQLiteDB.spec
  Ex.spec
  -- PGDB.spec
  -- PGDBP.spec
  -- MySQL.spec
  -- MySQLP.spec
