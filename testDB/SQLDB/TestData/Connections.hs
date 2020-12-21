module SQLDB.TestData.Connections where

import           EulerHS.Prelude

import           EulerHS.Interpreters
import qualified EulerHS.Language as L
import           EulerHS.Runtime (withFlowRuntime)
import qualified EulerHS.Runtime as R
import qualified EulerHS.Types as T


connectOrFail :: T.DBConfig beM -> L.Flow (T.SqlConn beM)
connectOrFail cfg = L.initSqlDBConnection cfg >>= \case
    Left e     -> error $ show e -- L.throwException $ toException $ show e
    Right conn -> pure conn

testDBName :: String
testDBName = "./test/EulerHS/TestData/test.db"

testDBTemplateName :: String
testDBTemplateName = "./test/EulerHS/TestData/test.db.template"

rmTestDB :: L.Flow ()
rmTestDB = void $ L.runSysCmd $ "rm -f " <> testDBName

prepareTestDB :: (T.DBConfig beM -> L.Flow ()) -> T.DBConfig beM -> L.Flow ()
prepareTestDB insertValues cfg = do
  rmTestDB
  void $ L.runSysCmd $ "cp " <> testDBTemplateName <> " " <> testDBName
  insertValues cfg

withEmptyDB :: (T.DBConfig beM -> L.Flow ()) -> T.DBConfig beM -> (R.FlowRuntime -> IO ()) -> IO ()
withEmptyDB insertValues cfg act = withFlowRuntime Nothing (\rt -> do
  try (runFlow rt $ prepareTestDB insertValues cfg) >>= \case
    Left (e :: SomeException) ->
      runFlow rt rmTestDB
      `finally` error ("Preparing test values failed: " <> show e)
    Right _ -> act rt `finally` runFlow rt rmTestDB
    )
