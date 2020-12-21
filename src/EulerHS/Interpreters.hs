{- |
Module      :  EulerHS.Interpreters
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2021
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable

This module contains interpreters and methods for running `Flow` scenarios.

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

module EulerHS.Interpreters
  ( module X
  ) where

import           EulerHS.Core.Interpreters as X
import           EulerHS.Framework.Interpreters as X
