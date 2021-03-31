# EulerHS usage tutorial

**N.B. This is a draft of the tutorial. Updated tutorial will be uploaded soon.**

* [Your first flow](#Your-first-flow)
* [Running your flows](#Running-your-flows)
* [Servant based web server](#Servant-based-web-server)
* [State handling](#State-handling)
* [Working with SQL subsystem](#Working-with-SQL-subsystem)
* [Automatic regression testing (coming soon)](#Automatic-regression-testing)
* [See also](#See-also)

## Your first flow

You typically want to write some business logic (BL) code that is not encrusted by implementation details. Such code is much simpler to write and maintain; and it becomes possible to update implementation details without affecting the BL code. The framework provides you the `Flow` monad and its derivables for defining pure, safe abstractions suitable for most backend apps.

See [Flow's documentation](./src/EulerHS/Framework/Flow/Language.hs) and [README](./README) for more insights on the provided API.

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

Flows are just declarative descriptions on what your logic should do. You need to run them in order to take the real action. To do that, you obtain a `FlowRuntime` instance and pass both runtime and your flow into the `runFlow` function:

```haskell
import qualified EulerHS.Runtime as R
import qualified EulerHS.Interpreters as I

runMyFlow :: R.FlowRuntime -> IO ()
runMyFlow flowRt = I.runFlow flowRT myFlow
```

`FlowRuntime` is a (safely concurrent) structure in which all the operational data will be managed by the framework. This data is needed for the framework to keep connections, threads, handle external libraries. You can create an instance of `FlowRuntime` using various functions from the `EulerHS.Runtime` module. You'll have to provide a logger creation function for that.

```haskell
import qualified EulerHS.Types as T
import qualified EulerHS.Runtime as R

runApp :: IO ()
runApp = do
  -- Default logger creation function
  let mkLoggerRt = R.createLoggerRuntime T.defaultFlowFormatter T.defaultLoggerConfig

  -- Bracket-like helper which will free your FlowRuntime gracefully when the flow finishes:
  R.withFlowRuntime (Just mkLoggerRt) $ \flowRt ->
    runMyFlow flowRt
```

You typically need only a single `FlowRuntime` structure for the whole backend app.
You can create this structure before starting an HTTP server, and use it in all the handlers to run your business logic.
`FlowRuntime` is a thread-safe structure, and it's okay to run several `Flows` in parallel. Just don't try to change the internals of the runtime yourself.

## Servant based web server

Consider [echo server demo app](./demo/echo-server) to get an idea on how you can structure your backend.

This server awaits for some input from user and reflects it back as a response.

It has two routes:

get /get_echo
post /post_echo

By default, it uses the 8080 port. You can start this application and make queries to it via curl:

```bash
$ stack exec echo-server

Creating sync loggers...
Creating console logger...
Starting Echo Server on port 8080...
```

```bash
$ curl http://localhost:8080/get_echo?phrase=fff&number=11

{"easterEgg":"This is an easter egg.","phrase":"fff","number":0}
```

The GET query supports two optional params. The POST query expects a JSON payload of specific type to be passed. See the full definition of API here: [API.hs](./demo/echo-server/Echo/API.hs).

Servant application architecture has several layers:

- Application layer (IO monad based)
- Servant server logic (ServerT & IO monads based)
- HTTP methods handlers (ExceptT & IO monads based)
- Business logic (custom monad stack; for example, Flow monad based)

Typical monad stacks for the Servant server and method handlers are presented below:

```haskell
type MethodHandler = ReaderT Env (ExceptT ServerError IO)
type AppServer  = ServerT EchoAPI MethodHandler
```

The `Env` type is custom for echo-server. It contains a `FlowRuntime` instance and a simple immutable application state:

```haskell
data AppState = AppState
  { easterEgg :: Text
  }

data Env = Env !R.FlowRuntime !AppState
```

Obtain `FlowRuntime` and configure your server as shown in [Server.hs](./demo/echo-server/Echo/Server.hs).

## State handling

There is a number of methods to work with state in your `Flow` scenarios.

##### StateT monad

You can wrap `Flow` into `StateT` and put any scenario-wide state into this context:

```haskell
data MyState = MyState
  { counter :: !Int
  }

type FlowSt a = StateT MyState Flow a

incCounter :: FlowSt ()
incCounter = do
  MyState cnt <- get
  put $ MyState $ cnt + 1
```

N.B. Initially, the `Flow` monad is represented by the `FlowMonad` type class, and it's been specified for many monad transformers including `StateT`. So you can call all the `Flow` methods without lifting.

###### Impure state

Impure state (`IORef`, `MVar` and `STM`) is also possible although a bit limited due to the ART presence. Let me remind you, every `Flow` method has to return a serializable value. For example:

```haskell
runIO :: (HasCallStack, MonadFlow m, T.ToJSON a, T.FromJSON a) => IO a -> m a
```

Thus, you can't create a variable of any those state types:

```haskell
mkMyIORef :: Flow (IORef Text)
mkMyIORef = runIO $ newIORef "abc"   -- wont' compile: IORef is not serializable

useMyIORef :: Flow ()
useMyIORef = do
  myIORef <- mkMyIORef
  value   <- runIO $ readIORef myIORef
  runIO $ print value
```

However you can pre-create your variables outside the `Flow` monad, for example in the Servant handlers like this:

```haskell
myHandler :: MethodHandler ()
myHandler = do
  myIORef <- liftIO $ newIORef "abc"
  runFlow $ useMyIORef myIORef

  where
    useMyIORef :: IORef Text -> Flow ()
    useMyIORef mkMyIORef = do
      value   <- runIO $ readIORef myIORef
      runIO $ print value
```

Prohibiting of uncontrolled variables creation is not a bad thing. The handlers of your backend app will be queried many times per second, and you definitely don't want your flow scenarios to consume memory. Additionally, this may be not thread safe enough if done wrong.

Still, when you really need this, you can run the `runUntracedIO` method. It doesn't have any restrictions placed by ART, so you can freely create any variables with it. But be aware that your scripts will stop being ART-replayable.

## Working with SQL subsystem

[README # SQL subsystem](https://github.com/juspay/euler-hs#SQL-subsystem) provides a good overview on possibilities the framework provides to work with relational data bases.

# Automatic regression testing

**Coming soon...**

## See also

The `EulerHS` framework is build using Hierarchical Free Monads approach. This approach was developed by Alexander Granin, and there are many different materials showing its usage and philosophy. For more info, consider the `Hydra` framework which is a "lesser brother" of the `EulerHS` framework. It's a showcase framework for demonstrating the approach, but they have a lot of things in common, including the design of several subsystems. In the `Hydra` repo, you can find showcase projects which architecture and structure can be directly derived for your `EulerHS` apps.

* [The Hydra framework](https://github.com/graninas/Hydra)
