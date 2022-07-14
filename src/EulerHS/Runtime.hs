{- |
Module      :  EulerHS.Runtime
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable

This is a top module that reexports all the runtime-specific types and functions.

This layer of the framework contains methods for creating and disposing runtimes
of different subsystems: logger, SQL, state and others.

You typically create a single `FlowRuntime` instance and then use it to run your
`Flow` scenarios.

This module is better imported as qualified.

@
import qualified EulerHS.Types as T
import qualified EulerHS.Language as L
import qualified EulerHS.Runtime as R
import qualified EulerHS.Interpreters as R

myFlow :: L.Flow ()
myFlow = L.runIO $ putStrLn @String "Hello there!"

runApp :: IO ()
runApp = do
  let mkLoggerRt = R.createLoggerRuntime T.defaultFlowFormatter T.defaultLoggerConfig
  R.withFlowRuntime (Just mkLoggerRt) $ \flowRt -> R.runFlow flowRt myFlow
@

-}

module EulerHS.Runtime
  ( module X
  ) where

import           EulerHS.Core.Runtime as X
import           EulerHS.Framework.Runtime as X
