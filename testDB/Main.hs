module Main where

import           EulerHS.Prelude
import           Test.Hspec


-- import qualified KVDB.KVDBSpec as KVDB
-- import qualified SQLDB.Tests.PostgresDBSpec as PGDB
import qualified SQLDB.Tests.SQLiteDBSpec as SQLiteDB
-- import qualified SQLDB.Tests.PostgresDBPoolSpec as PGDBP
-- import qualified SQLDB.Tests.MySQLDBSpec as MySQL
import qualified SQLDB.Tests.QueryExamplesSpec as Ex

main = hspec $ do
  SQLiteDB.spec
  Ex.spec

  -- Disable until it work here  in jenkins
  -- KVDB.spec
  -- PGDB.spec
  -- PGDBP.spec
  -- MySQL.spec
  -- MySQLP.spec
