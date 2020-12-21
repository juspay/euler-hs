# EulerHS Project

### EulerHS Framework

***euler-hs/Flow*** is a free monadic framework for building backend and console applications in Haskell.

The framework exports the Flow monad which provides the following facilities:

  - SQL DB interaction (using the `beam` library). Postgres, MySQL and SQLite DBs supported.
  - KV DB interaction. Redis is supported.
  - Forking flows in separate threads (green threads are used).
  - HTTP services interaction (using servant-client facilities).
  - Logging (tiny-logger inside).
  - Typed mutable options.
  - Pub/Sub mechanism (using Redis pub sub subsystem).
  - Safe call to IO actions.
  - Running system commands.
  - ART (Automatic Regression Testing) - white box testing facilities.
  - Integration testing framework.

# Installation

### Build tools

You can use any of the two building tools supported:

- **Stack**, install from [https://docs.haskellstack.org/en/stable/README/](https://docs.haskellstack.org/en/stable/README/)

### Dependencies

**Install development tools and libraries with your distro package manager:**

- binutils
- libssl-dev
- libpq-dev
- postgresql-server-dev-all
- libmysqlclient-dev
- libsqlite
- postgresql
- mysql-server
- ... maybe some others

~~git clone [git@bitbucket.org](mailto:git@bitbucket.org):juspay/euler-hs.git~~

or

~~git clone [https://user_name@bitbucket.org/juspay/euler-hs.git](https://user_name@bitbucket.org/juspay/euler-hs.git)~~

### Building and testing

#### Stack

**Build**

- `stack build` (Will build all the projects)
- `stack build --fast -j4` (Will skip some optimisations)
- `stack build euler-hs`
- `stack build euler-backend`

**Tests:**

- All tests:

    `stack test`

- backend dsl language tests:
    - `stack test euler-hs:language`
- SQL DB sub set of backend dsl (by default enabled only sqlite tests, if you want to test MySQL and Postgres then uncomment corresponding specs in `lib/euler-hs/testSqlDB/Main.hs` , you need an appropriate DB and tables)
    - `stack test euler-hs:sql`
- euler-backend tests:
    - `stack test euler-backend`
- ART tests:
    - `cd ./app/euler-backend`
    - `art.sh`

**Run:**

- `stack run euler-backend`
- Alternatively, you can use the shell script `./runEulerBackend.sh` that will set environment variables and run the program with some RTS options.

# Usage guidelines

***See also:***

* [Tutorial](./TUTORIAL.md)
* [Architecture diagram](./docs/Architecture.png)
* [Beam query examples](./lib/euler-hs/testDB/SQLDB/Tests/QueryExamplesSpec.hs)

### State handling

Sometimes you need to handle some (possibly mutable) state in your flows.
The framework doesn't support the state right as is,
but there are several ways to do this with other tools.

***Simple argument passing state***

This is the simplest way. You pass some values as arguments across the flow functions.

```haskell
orderCreate :: OrderCreateRequest -> MerchantAccount -> Flow OrderCreateResponse
orderCreate req mAccnt = do
  order <- validateOrderCreateRequest req
  orderCreate' order mAccnt

orderCreate' :: Order -> MerchantAccount -> Flow AOrderCreateResponse
orderCreate' order mAccnt = ...
```

Here, all the arguments can be treated as immutable state.
You can't probably do that much with this kind of state.

***StateT state***

You can use StateT for handling immutable state in flows.

```haskell

type FlowT a = StateT Order a

orderCreate :: OrderCreateRequest -> MerchantAccount -> Flow OrderCreateResponse
orderCreate req mAccnt = do
  order <- validateOrderCreateRequest req
  runStateT (orderCreate' mAccnt) order

orderCreate' :: MerchantAccount -> FlowT AOrderCreateResponse
orderCreate' mAccnt = do
  order <- get
  lift someFlowMethod
  put order
  ...

someFlowMethod :: Flow ()
someFlowMethod = ...
```

You handle your state with the StateT monad transformer wrapped around the Flow monad.
It allows to do 'mutability' but you'll have to lift the Flow methods.

This state will be thread safe.

***Options like a mutable state***

Typically, options are not intended to be used as user defined state.
But it's not prohibited somehow. Do it if you know what are you doing.
The only restriction here is that the state should be serializable (ToJSON / FromJSON)
because options should be like this.

```haskell

data Order = Order
  { someFiled :: Int
  }
  deriving (Generic, Typeable, Show, Eq, ToJSON, FromJSON)

data OrderKey = OrderKey
  deriving (Generic, Typeable, Show, Eq, ToJSON, FromJSON)

instance OptionEntity OrderKey Order

orderCreate :: OrderCreateRequest -> MerchantAccount -> Flow OrderCreateResponse
orderCreate req mAccnt = do
  order :: Order <- validateOrderCreateRequest req
  setOption OrderKey order
  orderCreate' mAccnt

orderCreate' :: MerchantAccount -> FlowT AOrderCreateResponse
orderCreate' mAccnt = do
  order <- getOption OrderKey
  ...
```

This state will be thread safe itself but with forked flows it's possible
to have race conditions anyway.

***Mutable impure state***

The idea is to have IORef, MVar or TVar defined outside the flows and use it
in flows to store data.

It is preferable to create any of these outside of the Flow monad
as the `runIO` method can't return a created variable. Let's elaborate.

This code will work, but it's kinda useless because doesn't allow to expose
the internal IORef state:

```haskell
someFlow :: Flow ()
someFlow = do
  n :: Int <- runIO $ do
    ref <- newIORef 100
    readIORef ref
  doSomethingWithN n
```

This flow won't work because the runIO method is not able to return IORef.
IORef is not serializable:

```haskell
someFlow :: Flow ()
someFlow = do
  ref :: IORef Int <- runIO $ newIORef 100   -- won't compile
  n <- runIO $ readIORef ref
  doSomethingWithN n
```

The only way to work with IORef (or MVar which is better) is to pre-create it
before the Flow scenario. Sample:

```haskell
-- API method for the Servant server
orderCreate
  :: OrderCreateRequest -> Handler ApiOrder.OrderCreateResponse
orderCreate req = do

  mVar :: MVar Order <- liftIO newEmptyMVar
  ref :: IORef (Maybe Order) <- liftIO $ newIORef Nothing

  runFlow $ Flows.orderCreate mVar ref req

orderCreate
  :: MVar Order
  -> IORef (Maybe Order)
  -> OrderCreateRequest
  -> Flow OrderCreateResponse
orderCreate mVar ref req = do
  order :: Order <- validateOrderCreateRequest req
  runIO $ writeIORef ref $ Just order   -- works
  runIO $ putMVar mVar order            -- works
  orderCreate' mAccnt

orderCreate'
  :: MVar Order
  -> IORef (Maybe Order)
  -> FlowT AOrderCreateResponse
orderCreate' mVar ref = do
  order <- runIO $ readIORef ref    -- works
  order <- runIO $ readMVar mVar    -- works
  ...
```

MVar and STM is thread safe, IORef is not thread safe.
Still, race coniditions are possible even with MVars and STM.

***Untraced IO and STM***
It is possible to run IO actions outside of the ART tracing system, however
it should be used with extreme caution as this means the following:

    1. no trace will be collected
    2. replay is not possible; instead, untraced IO-actions are re-executed on playback

Such functionality only really makes sense for two scenarios:

    1. mutation of in-memory data structures using `STM` -- in particular, the use of `atomically` and `newTVarIO`.
    2. reading of sensitive data, such as API keys

For example:

```haskell
countStuff :: Flow Int
  countVar <- runUntracedIO $ newTVarIO (0 :: Int)
  forkFlow "counter1" $ void $ runUntracedIO $ countTo100 countVar
  forkFlow "counter2" $ void $ runUntracedIO $ countTo100 countVar
  count <- runUntracedIO $ atomically $ readTVar countVar
  return count

countTo100 :: TVar Int -> IO Int
countTo100 countVar = do
  count <- atomically $ updateCount countVar
  if count < 100
    then countTo100 countVar
    else return count

updateCount :: TVar Int -> STM Int
updateCount countVar = do
  count <- readTVar countVar
  when (count < 100) (writeTVar countVar (count + 1))
  readTVar countVar
```

Although such `TVar`s can be allocated outside of `Flow`, this may make
use of local and composable abstractions difficult.

***Untraced IO and Sensitive Data***
Another good use case is reading sensitive data which should not be collected
by the ART system, such as e.g. API keys stored in a config inside of an `IORef`.

Arguably the best way to deal with this is as follows:

    1. store sensitive data as separate configuration state, for example in an `IORef`
    2. read and write to this `IORef` using `runUntracedIO`
    3. use `runIO` for any non-sensitive data

This way the ART traces will never collect sensitive data, and replay/mocking of ART
traces will still work in different execution environments with e.g. test API keys.

***KV DB and SQL DB based state***


***KV DB and SQL DB based state***

You can use KV DB and SQL DB as an external state storage which is significantly
less performant and less convenient.

### Methods for connection management:

*Takes SQL DB config and create connection that can be used in queries.*
```haskell
initSqlDBConnection :: T.DBConfig beM -> Flow (T.DBResult (T.SqlConn beM))
```

*Deinit the given connection if you want to deny access over that connection.*
```haskell
deinitSqlDBConnection :: T.SqlConn beM -> Flow ()
```

*Get existing connection. If there is no such connection, returns error.*
```haskell
getSqlDBConnection ::T.DBConfig beM -> Flow (T.DBResult (T.SqlConn beM))
```

*Get existing SQL connection, or init a new connection.*
```haskell
getOrInitSqlConn :: T.DBConfig beM -> L.Flow (T.DBResult (T.SqlConn beM))
```

### SQL DB subsystem

*Takes connection, sql query (described using BEAM syntax) and make request.*
```haskell
runDB
  ::
    ( T.JSONEx a
    , T.BeamRunner beM
    , T.BeamRuntime be beM
    )
  => T.SqlConn beM
  -> L.SqlDB beM a
  -> Flow (T.DBResult a)
```

Runs outside of a transaction. For transactions you can use `runTransaction`.

*Extracting existing connection from FlowRuntime by given db config and runs sql query (described using BEAM syntax). Acts like 'getSqlDBConnection' + 'runDB'*
```haskell
withDB ::
  ( T.JSONEx a
  , T.BeamRunner beM
  , T.BeamRuntime be beM
  )
  => T.DBConfig beM -> L.SqlDB beM a -> Flow a
```

When you start the application, you can initialize all the connections that you plan to use.
```haskell
keepConnsAliveForSecs :: NominalDiffTime
keepConnsAliveForSecs = 60 * 10 -- 10 mins

maxTotalConns :: Int
maxTotalConns = 8

mySQLCfg :: MySQLConfig
mySQLCfg = MySQLConfig
  { connectHost     = "localhost"
  , connectPort     = 3306
  , connectUser     = "username"
  , connectPassword = "password"
  , connectDatabase = "dbname"
  , connectOptions  = [T.CharsetName "utf8"]
  , connectPath     = ""
  , connectSSL      = Nothing
  }

sqlDBcfg = mkMySQLPoolConfig "eulerMysqlDB" mySQLCfg
    $ PoolConfig 1 keepConnsAliveForSecs maxTotalConns

prepareDBConnections :: Flow ()
prepareDBConnections = do
  ePool <- initSqlDBConnection sqlDBcfg
  throwOnFailedWithLog ePool SqlDBConnectionFailedException "Failed to connect to SQL DB."

```

And then run flow methods with it
```haskell
endpointHandler :: RequestType -> Flow (Maybe Int)
endpointHandler req = do
    logInfo @String "endpointHandler" "endpointHandler started"
    validReq <- validateRequest req
    -- ...
    -- some other actions
    -- ...
    res <- withDB sqlDBcfg $ do
      let predicate DBTableType {idField} =
          (idField    ==. B.val_ (validReq ^. reqIdField))
      findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (dbTableName dbSchema)
    pure $ (^. intField) <$> res
```

Also, you can put your dbConfig in Options and take it back later in specialized `withDB` wrappers. Maybe helpful when you should create config on startup, so config can't be hardcoded as constant and easily passed in methods (e.g. read DB password from env var and decode it with some IO operation). You can manage many different db configs

At first define keys for DBs:
```haskell
data DB1Cfg = DB1Cfg
  deriving (Generic, Typeable, Show, Eq, ToJSON, FromJSON)

instance OptionEntity DB1Cfg (DBConfig MySQLM)

data DB2Cfg = DB2Cfg
  deriving (Generic, Typeable, Show, Eq, ToJSON, FromJSON)

instance OptionEntity DB2Cfg (DBConfig Pg)
```

Then you can define a specialized wrapper for each db:
```haskell
withDB1 :: JSONEx a => SqlDB MySQLM a -> Flow a
withDB1 act = do
  dbcfg <- getOption DB1Cfg
  case dbcfg of
    Just cfg -> withDB cfg act
    Nothing -> do
      logError @String "MissingDB identifier" "Can't find DB1 identifier in options"
      throwException YourException

withDB2 :: JSONEx a => SqlDB Pg a -> Flow a
withDB2 act = do
  dbcfg <- getOption DB2Cfg
  case dbcfg of
    Just cfg -> withDB cfg act
    Nothing -> do
      logError @String "MissingDB identifier" "Can't find DB2 identifier in options"
      throwException YourException
```
On startup initialization just put configs in Options

```haskell
prepareDBConnections :: Flow ()
prepareDBConnections = do
  sqlDBcfg1 <- runIO getFromEnvAndDecodeMySqlDbCfg
  ePool1 <- initSqlDBConnection sqlDBcfg1
  setOption DB1Cfg sqlDBcfg1
  throwOnFailedWithLog ePool SqlDBConnectionFailedException "Failed to connect to SQL DB1."
  sqlDBcfg2 <- runIO getFromEnvAndDecodePostgresDbCfg
  ePool2 <- initSqlDBConnection sqlDBcfg2
  setOption DB2Cfg sqlDBcfg2
  throwOnFailedWithLog ePool SqlDBConnectionFailedException "Failed to connect to SQL DB2."
```
