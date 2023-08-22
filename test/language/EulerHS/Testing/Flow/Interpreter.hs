{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module EulerHS.Testing.Flow.Interpreter where

import           Data.Generics.Product.Fields (HasField', getField, setField)
import           EulerHS.Language (Flow, FlowMethod, foldFlow)
import qualified EulerHS.Language as L
import           EulerHS.Prelude
import           EulerHS.Runtime (FlowRuntime)
import           EulerHS.Testing.Types (FlowMockedValues)
import           GHC.TypeLits (KnownSymbol, Symbol)
import           Type.Reflection (typeRep)
import           Unsafe.Coerce (unsafeCoerce)

runFlowWithTestInterpreter :: FlowMockedValues -> FlowRuntime -> Flow a -> IO a
runFlowWithTestInterpreter mv flowRt = foldFlow (interpretFlowMethod mv flowRt)

interpretFlowMethod :: FlowMockedValues -> FlowRuntime -> FlowMethod a -> IO a
interpretFlowMethod mmv _ = \case
  L.RunIO _ _ next -> next . unsafeCoerce <$> takeMockedVal @"mockedRunIO" mmv
  L.CallServantAPI _ _ _ _ _ next ->
    next . unsafeCoerce <$> takeMockedVal @"mockedCallServantAPI" mmv
  L.GetOption _ next -> next <$> unsafeCoerce (Just $ takeMockedVal @"mockedGetOption" mmv)
  L.SetOption _ _ next -> pure . next $ ()
  L.GenerateGUID next -> next <$> takeMockedVal @"mockedGenerateGUID" mmv
  L.RunSysCmd _ next -> next <$> takeMockedVal @"mockedRunSysCmd" mmv
  _ -> error "not yet supported."

takeMockedVal :: forall (f :: Symbol) (a :: Type) (r :: Type)
  .  (KnownSymbol f, Typeable r, HasField' f r [a])
  => MVar r -> IO a
takeMockedVal mmv = do
  mv <- takeMVar mmv
  (v,t) <- case getField @f mv of
    [] -> error $ "empty " <> show (typeRep @f) <> " in " <> show (typeRep @r)
    (x:xs) -> pure (x,xs)
  putMVar mmv $ setField @f t mv
  pure v
