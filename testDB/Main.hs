module Main where

import           EulerHS.Prelude
import           Test.Hspec

import qualified SQLDB.Tests.SQLiteDBSpec as SQLiteDB
import qualified SQLDB.Tests.QueryExamplesSpec as Ex

main = hspec $ do
  SQLiteDB.spec
  Ex.spec
