{-# LANGUAGE CPP #-}

module EulerHS.Language
  ( module X,
    Y.Flow,
    Y.FlowMethod (..),
    Y.MonadFlow (..),
    Y.Loggable (..),
    Y.ReaderFlow,
    -- * logging
    Y.logCallStack,
    Y.logExceptionCallStack,
    --
    Y.logInfo,
    Y.logError,
    Y.logEncryptedInfo,
    Y.logEncryptedError,
    Y.logDebug,
    Y.logWarning,
    --
    Y.logInfoM,
    Y.logErrorM,
    Y.logDebugM,
    Y.logWarningM,
    Y.logInfoV,
    Y.logErrorV,
    Y.logDebugV,
    Y.logWarningV,
    Y.logException,
    Y.logErrorWithCategory,
    Y.logErrorWithCategoryV,
    -- * Calling external services
    Y.callAPI,
    Y.callAPI',
    Y.callHTTP,
    Y.callHTTP',
    Y.callHTTPWithCert,
    Y.callHTTPWithManager,
    Y.callHTTPWithCert',
    Y.callHTTPWithManager',
    -- * other
    Y.runIO,
    Y.generateGUIDWithTag,
    Y.withRunFlow,
    Y.forkFlow,
    Y.forkFlow'',
    Y.forkFlow',
    Y.foldFlow,
    Y.getMySQLConnection,
    -- * dbAndRedisMetric
    Y.DBAndRedisMetricHandler,
    Y.DBAndRedisMetric (..),
    Y.mkDBAndRedisMetricHandler,
    Y.DBMetricCfg (..),
    Y.TenantConfigObj (..),
    Y.MigrationConfig(..) ,
    Y.TenantDBMigrationMode(..),
    Y.TenantConfig(..),
    Y.CachePrefixMigrationMode(..),
    Y.PrefixMigrationMode(..),
    Y.callHttpWithDigest,
    ART.runIOWithART
  ) where

import           EulerHS.Extra.Language as X
import           EulerHS.Framework.Language as Y hiding (runDB, runDBWithConn)
#if defined(REDIS_CORE_EXPORT)
import           EulerHS.KVDB.Language as X
#else
-- Do not export Redis helper functions from here; export types only.
import           EulerHS.KVDB.Language as X (KVDBTx)
#endif
import           EulerHS.Logger.Language as X
import           EulerHS.PubSub.Language as X hiding (psubscribe, publish,
                                               subscribe)
import           EulerHS.SqlDB.Language as X
import           EulerHS.ART.IOReplay as ART
