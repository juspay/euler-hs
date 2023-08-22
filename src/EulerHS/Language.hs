{- |
Module      :  EulerHS.Language
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
This module provides you a public interface to the free monadic eDSLs of the framework.
The `Flow` type or its derivations for different monad stacks can be used
to describe business logic of a typical web application.
This language provides you with several features out-of-the-box:
- Logging
- SQL DB subsystem. Supported SQL DB backends:
  * MySQL
  * Postgres
  * SQLite
- KV DB subsystem (Redis)
- Fork/await of flows (async evaluation)
- Typed options
- Servant-like HTTP client runner
- Low-level HTTP client runner
- Arbitrary IO effects
- Exception throwing and handling
- Redis-based PubSub connector (experimental)
The `Flow` is a monad, so you can write sequential scenarios in a monadic form:
@
import EulerHS.Prelude
import qualified EulerHS.Types as T
import qualified EulerHS.Language as L
import qualified Servant.Client as S
myFlow :: L.Flow (Either T.ClientError User)
myFlow = do
  L.runIO $ putStrLn @String "Hello there!"
  L.logInfo "myFlow" "This is a message from myFlow."
  let url = S.BaseUrl Http "127.0.0.1" 8081 ""
  L.callAPI Nothing url getUser
-- HTTP API powered by Servant
type API = "user" :> Get '[JSON] User
getUser :: T.EulerClient User
getUser = client api
@
To run this logic, you need to create an instance of `FlowRuntime`,
and pass @myFlow@ to the `runFlow` method.
This module is better imported as qualified.
-}
  
module EulerHS.Language
  ( module X,
    Y.Flow,
    Y.FlowMethod (..),
    Y.MonadFlow (..),
    Y.ReaderFlow,
    -- * logging
    Y.logCallStack,
    Y.logExceptionCallStack,
    --
    Y.logInfo,
    Y.logError,
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
    Y.XTenantHostHeader (..)
  ) where

import           EulerHS.Extra.Language as X
import           EulerHS.Framework.Language as Y
import           EulerHS.KVDB.Language as X
import           EulerHS.Logger.Language as X
import           EulerHS.PubSub.Language as X hiding (psubscribe, publish,
                                               subscribe)
import           EulerHS.SqlDB.Language as X
