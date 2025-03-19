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

### Euler Backend

***euler-backend*** is a web/REST/HTTP application, a direct port of euler-ps for implementing
  [Juspay APIs](https://www.juspay.in/docs/api/ec/) in Haskell.
  The application is based on the Servant web framework.
  for HTTP facilities and the Flow framework for business logic.

## Getting started

### Setup

Follow the instructions from [euler-nix-common](https://bitbucket.juspay.net/projects/NIX/repos/euler-nix-common/browse/SETUP.md)

### Development

Enter development shell by running:
```bash
nix develop
```
or enter development shell on `cd` with [direnv + nix-direnv](https://nixos.asia/en/direnv)

Hot reload with `ghcid` on code change for `euler-hs`:
```bash
ghcid -c cabal repl euler-hs
```

If you are facing issues with the setup, run [nix-health](https://crates.io/crates/nix_health) to verify everything is green:
```bash
cd euler-hs
nix --accept-flake-config run github:juspay/nix-browser#nix-health .
```

### Build

Build the library by running:
```bash
nix build
```

## FAQ
### Locally link euler dependencies
Create `cabal.project.local` by running:
```bash
echo 'packages:
  ../euler-events-hs' > cabal.project.local
```
**Note: modify the path for dependencies if needed**

### Override dependencies

#### Add input

##### Internal bitbucket repository
```nix
# In inputs of flake.nix

# Using a commit
euler-events-hs = {
  type = "git";
  # This could be https url or ssh url (preferably ssh)
  url = "ssh://git@ssh.bitbucket.juspay.net/fram/euler-evetns-hs";
  # Branch name
  ref = "emergence";
  # Commit
  rev = "f4ae116f61ffd89fe4ea3e7f19b625544d0ea227";
  inputs.common.follows = "common";
};


# Using only the branch
euler-evetns-hs = {
  type = "git";
  url = "ssh://git@ssh.bitbucket.juspay.net/fram/euler-evetns-hs";
  ref = "emergence";
  inputs.common.follows = "common";
};


# Using tags
# Why not just `ref = "4.0.4`? See here: https://github.com/NixOS/nix/issues/3701#issuecomment-674308574
euler-evetns-hs = {
  type = "git";
  url = "ssh://git@ssh.bitbucket.juspay.net/fram/euler-evetns-hs";
  ref = "refs/tags/4.0.4";
  inputs.common.follows = "common";
};

```
##### Public repositories
```nix
# github
repo = {
  url = "github:<owner>/<repo>/<branch/commit/tag>";
  # If the repo doesn't have a flake.nix/ you don't want to use it
  flake = false;
};
```
Refer here for more URL syntax: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html#url-like-syntax

#### Use input added above to override the haskell package set

Head over to `flake.nix` and follow the instructions here: https://community.flake.parts/haskell-flake/dependency#source

#### Update the lock file

Update all inputs:
```sh
nix flake update
```
Update specific input:
```sh
nix flake lock --update-input euler-events-hs
```

### Find commit and path to a input in the dependency graph

Let's say, I want to find what is the revision/commit of `hedis` input:
```sh
nix flake metadata --json | nix run nixpkgs#jq -- '.. | ."hedis"? | select( . != null )'
```
This will give me the following output:
```json
{
  "flake": false,
  "locked": {
    "lastModified": 1689234100,
    "narHash": "sha256-J4mqtraMzLJguL0BwMUETeBpNJXRf86E2ZaP9xx+4tY=",
    "owner": "juspay",
    "repo": "hedis",
    "rev": "77bf501da57639a60728b577e1c3780f70ddb418",
    "type": "github"
  },
  "original": {
    "owner": "juspay",
    "repo": "hedis",
    "rev": "77bf501da57639a60728b577e1c3780f70ddb418",
    "type": "github"
  }
}
"hedis"

```
Now, I know what is the commit, but which `flake.nix` does this input belong to, to find this we run:
```sh
nix flake metadata --json | nix run nixpkgs#jq -- -c 'paths | select(.[-1] == "hedis")'
```
Which gives us the path that looks like:
```sh
["locks","nodes","euler-hs","inputs","hedis"]
["locks","nodes","hedis"]
```

Now we know that it belongs to `euler-hs`, this is how we know that to modify the commit of `hedis`, we need to update it in the inputs of `euler-hs`'s `flake.nix`

## Usage guidelines

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
  awaitable1 <- forkFlow' "counter1" $ void $ runUntracedIO $ countTo100 countVar
  awaitable2 <- forkFlow' "counter2" $ void $ runUntracedIO $ countTo100 countVar
  void $ await Nothing awaitable1
  void $ await Nothing awaitable2
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

# Enabling SQL Logging

To enable SQL query logging you can use `UnsafeLogSQL_DO_NOT_USE_IN_PRODUCTION`
in your application `LoggerConfig`. However, this will logs data passed into the query,
like API keys and other sensitive data. As such, this should be used for local debugging
only!
