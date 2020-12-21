# EulerHS usage tutorial

**N.B. This is a draft of the tutorial. Updated tutorial will be uploaded soon.**

* [Your first flow](#Your-first-flow)
* [Running your flows](#Running-your-flows)
* [Servant based web server (coming soon)](Servant-based-web-server)
* [Working with SQL subsystem (coming soon)](Working-with-SQL-subsystem)
* [State handling (coming soon)](#State-handling)
* [Automatic regression testing (coming soon)](#Automatic-regression-testing)
* [See also](#See-also)

## Your first flow

You typically want to write some business logic (BL) code that is not encrusted by implementation details. Such code is much simpler to write and maintain; and it becomes possible to update implementation details without affecting the BL code. The framework provides you the `Flow` monad and its derivables for defining pure, safe abstractions suitable for most backend apps.

See [the documentation](./src/EulerHS/Framework/Flow/Language.hs) on the `Flow` language for more insights on the provided API.

Sample `Flow` monadic scenario follows here. It greats the user, logs some message and makes an HTTP call via the integrated servant-client subsystem:

```haskell
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
```

It's recommended to import the modules of the framework qualified to avoid name clashes and to keep your code in order.

The method returns `Either T.ClientError User`. Normally, you should think about this code as safe, exception-free. Neither of methods of the framework throw (sync) exceptions, and this code should be safe.

  **N.B. There is a mechanism for working with exceptions in the framework, and it should be used carefully.**

  **N.B. Generally, framework is sync exceptions-free. All the exceptions from the lower implementation level are guarded by the interpreters, converted into error types and returned as Eithers.**

  **N.B. It should be considered that the `Flow` code is also async-exceptions free. The framework is not yet polished to protect from async exceptions, but the current practice of usage shows that such problems have almost a zero-like chance to happen. The further development of the framework will be towards even more exception safety.**

## Running your flows

Flows are just declarative descriptions on what your logic should do. You need to run them in order to take real action. To do that, you obtain a `FlowRuntime` instance and pass both runtime and your flow into the `runFlow` function:

```haskell
import qualified EulerHS.Interpreters as R

runMyFlow :: R.FlowRuntime -> IO ()
runMyFlow flowRt = R.runFlow flowRT myFlow
```

`FlowRuntime` is a structure in which all the operational data will be managed by the framework. This data is needed for the framework to keep connections, bookkeep threads, handle external libraries. You can create an instance using various functions from the `EulerHS.Runtime` module.

```haskell
import qualified EulerHS.Types as T
import qualified EulerHS.Runtime as R

runApp :: IO ()
runApp = do
  -- Default logger creation function
  let mkLoggerRt = R.createLoggerRuntime T.defaultFlowFormatter T.defaultLoggerConfig

  -- Bracket-like helper which will free your FlowRuntime gracefully when the flow finishes:
  R.withFlowRuntime (Just mkLoggerRt) $ \flowRt -> runMyFlow flowRt
```

You typically need only a single `FlowRuntime` structure for the whole backend app.
You can create this structure before starting an HTTP server, and use it in all the handlers to run your business logic.
`FlowRuntime` is a thread-safe structure, and it's okay to run several `Flows` in parallel. Just don't try to change the internals of the runtime yourself.

## Servant based web server

**Coming soon...**

## Working with SQL subsystem

**Coming soon...**

## State handling

**Coming soon...**

# Automatic regression testing

**Coming soon...**

## See also

The `EulerHS` framework is build using Hierarchical Free Monads approach. This approach was developed by Alexander Granin, and there are many different materials showing its usage and philosophy. For more info, consider the `Hydra` framework which is a "lesser brother" of the `EulerHS` framework. It's a showcase framework for demonstrating the approach, but they have a lot of things in common, including the design of several subsystems. In the `Hydra` repo, you can find showcase projects which architecture and structure can be directly derived for your `EulerHS` apps.

* [The Hydra framework](https://github.com/graninas/Hydra)
