# Juspay's EulerHS Framework

***EulerHS*** is a free monadic framework for easy building concurrent backend and console applications in Haskell. This framework provides you with the most important subsystems already integrated, such as SQL DBs, logging, KV DBs and other.

The framework represents a safe layer with its own philosophy of exception safety, so you don't need to think about those difficult Haskell questions.

The framework also offers two testing mechanisms: integration testing facilities and automatic whitebox testing mechanism. The code you'll be writing will be simple and testable. See the related materials for more info.

Framework is successfully used in production in Juspay and shows impressive results.

* [Framework features](#Framework-features)
* [Business logic code sample](#The-Flow-monad)
* [Important note on forked external libs](#Important-note-on-forked-external-libs)
  - [hedis library for Redis](#hedis-library-for-Redis)
  - [beam-mysql library for MySql](#beam-mysql-library-for-MySql)
  - [beam](#beam)
* [EulerHS application architecture](#EulerHS-application-architecture)
* [Flow monad sample usages](#Flow-monad-sample-usages)
  - [Logging](#Logging)
  - [Typed options](#Typed-options)
  - [SQL subsystem](#SQL-subsystem)
  - [mtl style support](#mtl-style-support)
  - [KV DB subsystem](#KV-DB-subsystem)
  - [Forking and awaiting child flows](#Forking-and-awaiting-child-flows)
* [State handling](#State-handling)
* [Building the framework](#Building-the-framework)
* [Testing the framework](#Testing-the-framework)
* [EulerHS tutorials and template projects](#EulerHS-tutorials-and-template-projects)
* [Background materials](#Background-materials)

## Framework features

The framework exports the `Flow` monad which provides the following facilities:

  - Custom Prelude (universum-based)
  - SQL DB interaction using the `beam` library. Supported SQL backends:
      * Postgres
      * MySQL
      * SQLite
  - KV DB interaction. Supported KV DBs:
      * Redis
  - Async and parallel flows evaluation
  - Client HTTP in two forms:
      * Servant-based HTTP client interaction
      * Low-level HTTP client
  - Logging (tiny-logger inside)
  - Typed mutable options
  - Safe IO actions
  - Exception-handling mechanism
  - Running OS system commands
  - ART (Automatic Regression Testing) - white box testing facilities
  - Integration testing framework
  - Experimental Pub/Sub mechanism (using Redis pub sub subsystem)

## Business logic code sample

A sample scenario you may find [here](./test/EulerHS/TestData/Scenarios/Scenario1.hs).

```haskell
import           EulerHS.Prelude
import qualified EulerHS.Language as L
import           EulerHS.TestData.API.Client
import           EulerHS.TestData.Types
import           Servant.Client (BaseUrl (..), Scheme (..))

testScenario1 :: L.Flow User
testScenario1 = do
  logDebug "testScenario1" "Running sys cmd whoami..."
  localUserName <- L.runSysCmd "whoami"

  logDebug "testScenario1" "Reading a guid from file..."
  localGUID <- L.runIO (readFile "my_guid.txt")

  logDebug "testScenario1" "Generating new guid..."
  guid <- L.generateGUID

  logDebug "testScenario1" "Obtaining URL..."
  url <- maybe (mkUrl "127.0.0.1") mkUrl <$> L.getOption UrlKey

  logDebug "testScenario1" "Calling some HTTP API..."
  res <- L.callServantAPI Nothing url getUser

  logDebug "testScenario1" "Finished."
  pure $ case res of
    Right u | localGUID /= userGUID u -> u
    Right u | otherwise -> User localUserName "" $ toString guid
    _ -> User localUserName "Smith" $ toString guid
  where
    mkUrl :: String -> BaseUrl
    mkUrl host = BaseUrl Http host port ""
```

## Important note on forked external libs

Juspay made forks of several libraries to fix their problems and to support
specific cases. You might want to be aware of these Juspay-specific fixes. If they are not suitable for your needs, it might need to avoid them till EulerHS v3.0.

### hedis library for Redis

[hedis](https://github.com/juspay/hedis) is stapled to a Juspay-specific fork as current. This fork is multiple releases behind the current mainline, incompatibly.

### beam-mysql library for MySql

The [beam-mysql](https://github.com/juspay/beam-mysql) is rewritten almost completely. The original version doesn't have protection from SQL injections, and also is written with some internal problems. The updated version fixes that.

More info on `beam` usage can be found in [BEAM-NOTES](./BEAM-NOTES.md).

### beam

We made several minor improvements of the original `beam` library in [our fork](https://github.com/juspay/beam). These changes do not have anything Juspay-specific, but yet to be pushed to the upstream.

More info on `beam` usage can be found in [BEAM-NOTES](./BEAM-NOTES.md).

## EulerHS application architecture

The EulerHS framework slices your application into several layers preventing the implementation details to leak into the business logic. This helps to keep the business logic code simple, maintainable and more reliable. It also helps to tweak the implementation without affecting the business logic. The further development of the framework will be towards improving its safety and performance, but this won't require updating of the business logic code due to this clear layering.

Layers of the typical web backend application with Servant:

**Application layer:**

* Application (`IO` monad)
  - Processes app configs and command line arguments
  - Manages `FlowRuntime`
  - Runs Servant HTTP server
* API types model
  - Types used in Servant to handle queries and API methods
* Servant method handler (Servant monad stack: `ReaderT FlowRuntime (ExceptT ServerError IO)`)
  - Handles errors and exceptions
  - Validates requests
  - Converts API types into domain types and back
  - Uses API types model
  - Runs business logic scenarios using `FlowRuntime` and interpreters

**Business domain layer:**

* EulerHS language (the `Flow` monad and its derivables)
  - Provides an abstracted, purified set of eDSLs which are free from implementation details
  - Helps to organize business logic code
* Domain model (ADT types mostly)
  - Represents types and functions directly related to the domain
* DB model (`beam`-powered schema)
  - DB representation of domain
* Business logic scenarios (`Flow` monad and its derivables)
  - Interacts with domain model and DB model
  - Isolated from the other layers

**Implementation layer:**

* EulerHS runtime (`FlowRuntime`)
  - Keeps operational data of the framework
  - Handles resources, manages connections
* EulerHS interpreters (`IO`-based monadic stack)
  - Connects `Flow` scenarios to real subsytems and libraries
  - Handles exceptions, threads, ART system
  - Uses `FlowRuntime`

Checkout [Background materials](#Background-materials) to know more about this layering.

## Flow monad sample usages

### Logging

Framework provides logging mechanism out-of-the-box. It provides 4 logging functions:

```haskell
logInfo    :: Show tag => tag -> Message -> Flow ()
logError   :: Show tag => tag -> Message -> Flow ()
logDebug   :: Show tag => tag -> Message -> Flow ()
logWarning :: Show tag => tag -> Message -> Flow ()
```

Usage is quite simple.

```haskell
import qualified EulerHS.Language as L

myFlow :: L.Flow ()
myFlow = L.logInfo "myFlow" "Hello world!"
```

Notice there is no anything related to a specific logging library. It's all hidden behind the `Flow` interface.

The logging subsystem can be configured on the start of the application. You should specify what logger you want passing a logger creation function into `withFlowRuntime` or `createFlowRuntime` functions.

- No logger (`createVoidLoggerRuntime`)
- Memory logger (`createMemoryLoggerRuntime`). Can eat your memory very fast!
- File logger (a special flag in `LoggerConfig`). Will flush logs into a file (the action is not immediate).
- Console logger (a special flag in `LoggerConfig`). Will show logs in console.

You can also choose should your file and console logger be sync or async (a special flag in `LoggerConfig`).

N.B. Async logger is not properly tested yet, it can have performance implications.

```haskell
import qualified EulerHS.Types as T
import qualified EulerHS.Runtime as R
import qualified EulerHS.Interpreters as I

loggerConfig :: T.LoggerConfig
loggerConfig = T.LoggerConfig
    { T._isAsync = False
    , T._logLevel = Debug
    , T._logFilePath = "/tmp/logs/myFlow.log"
    , T._logToConsole = True
    , T._logToFile = True
    , T._maxQueueSize = 1000
    , T._logRawSql = False
    }

runApp :: IO ()
runApp = do
  let mkLoggerRt = R.createLoggerRuntime T.defaultFlowFormatter loggerConfig
  R.withFlowRuntime (Just mkLoggerRt)
    $ \flowRt -> I.runFlow flowRt myFlow
```

The framework also supports different logger formatters for different flows. See documentation on `FlowFormatter` for more info.

On `FlowRuntime` disposal, the rest of the queue of logs will be flushed gracefully.

Log entries are considered app-wide, but there is a `LogCounter` that is counting entries and helping to see their ordering.

Ordering itself is not guaranteed.

### Typed options

Just typed key-value options.

```haskell
getOption :: (OptionEntity k v) => k -> Flow (Maybe v)
setOption :: (OptionEntity k v) => k -> v -> Flow ()
delOption :: (OptionEntity k v) => k -> Flow ()
```

Options work as a shared concurrent yet mutable state,
so be careful to not produce data races.

Avoid using it as an operational state of your app, it's better to use `StateT` on top of the `Flow`. See [State handling](#State-handling) for more info.

```haskell
data TestIntKey = TestIntKey
  deriving (Generic, Typeable, Show, Eq, ToJSON, FromJSON)

data TestStringKey = TestStringKey
  deriving (Generic, Typeable, Show, Eq, ToJSON, FromJSON)

instance T.OptionEntity TestStringKey String
instance T.OptionEntity TestIntKey Int

myFlow :: L.Flow (Maybe String, Maybe Int)
myFlow = do
  _  <- L.setOption TestStringKey "lore ipsum"
  _  <- L.setOption TestIntKey 100
  v1 <- L.getOption TestStringKey
  v2 <- L.getOption TestIntKey
  pure (v1, v2)

-- (Just "lore ipsum", Just 100)
```

### SQL subsystem

This subsystem supports several SQL backends:

- MySQL
- Postgres
- SQLite

The goal was to provide a unified interface for all those backends, so that different projects could have a relatively similar code.

Unfortunately, there are two drawbacks here.

- We had to fix many problems in `beam` and related libraries, but were unable to push all the changes to the upstream. This especially true regarding `beam-mysql` that we had to rewrite almost completely.
- Framework cannot provide things specific to any SQL backend. Only a common subset of features.
- `beam` itself is a sophisticated library, and its usage is quite difficult. You may want to consult with our [cheatsheet](./BEAM-NOTES.md) on `beam` usage.

There is a test suite [QueryExamplesSpec](testDB/SQLDB/Tests/QueryExamplesSpec.hs) with `beam` samples,
check it out to get an idea on how to compose queries and how it's all integrated with the `Flow` monad.

In general, you need to define your DB model with tables and relations.

```haskell
import qualified Database.Beam as B

-- Description of the @member@ table

data MemberT f = Member
    { memberId      :: B.C f Int
    , surName       :: B.C f Text
    , firstName     :: B.C f Text
    , address       :: B.C f Text
    , zipCode       :: B.C f Int
    , telephone     :: B.C f Text
    , recommendedBy :: B.C f (Maybe Int)
    , joinDate      :: B.C f LocalTime
    } deriving (Generic, B.Beamable)

-- Description of the primary key type:

instance B.Table MemberT where
  data PrimaryKey MemberT f = MemberId (B.C f Int)
    deriving (Generic, B.Beamable)
  primaryKey = MemberId . memberId

type Member = MemberT Identity
type MemberId = B.PrimaryKey MemberT Identity

-- Field names can be mapped 1:1 to names of the ADT, but it's possible
-- to alter them, like this:

membersEMod
  :: B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity MemberT)
membersEMod = B.modifyTableFields
  B.tableModification
    { memberId      = B.fieldNamed "memid"
    , surName       = B.fieldNamed "surname"
    , firstName     = B.fieldNamed "firstname"
    , address       = B.fieldNamed "address"
    , zipCode       = B.fieldNamed "zipcode"
    , telephone     = B.fieldNamed "telephone"
    , recommendedBy = B.fieldNamed "recommendedby"
    , joinDate      = B.fieldNamed "joindate"
    }

-- The Schema itself:

data ClubDB f = ClubDB
    { members    :: f (B.TableEntity MemberT)   -- members table
    , facilities :: f (B.TableEntity FacilityT) -- facilities table
    , bookings   :: f (B.TableEntity BookingT)  -- bookings table
    } deriving (Generic, B.Database be)

-- DB Schema representation. Use this value to reference to any of tables.
clubDB :: B.DatabaseSettings be ClubDB
clubDB = B.defaultDbSettings `B.withDbModification` B.dbModification
    { facilities = facilitiesEMod     -- Alter field names of tables.
    , members    = membersEMod
    , bookings   = bookingsEMod
    }
```

There can be conversion functions between your domain model and DB models, but in general, it's possible to have only the latter.

Simple SELECT query for the `members` table which requests all members joined after this date:

```haskell
searchByDate :: LocalTime -> L.Flow (T.DBResult [Member])
searchByDate startDate = do
  conn <- connectOrFail sqliteCfg       -- obtain SQLite config somehow
  L.runDB conn                          -- run a query
    $ L.findRows                        -- SELECT query returning rows
    $ B.select
    $ B.filter_ (\m -> joinDate m >=. B.val_ startDate)
    $ B.all_ (members clubDB)
```

Notice that we use `runDB` for running SQL queries expressed in `beam` and wrapped into the `SqlDB` language. This function does not imply transactionality, but if you need it, use `runTransaction` instead. In this case, any query packed into an `SqlDB` monadic block, will be scoped by a single transaction.

Framework allows you to create either permanent, application-wide SQL connections, or immediate single-usage connections in place.

For permanent connections, you need to create them on the __Application Layer__ and pass them into your `Flow` scenarios. One of the possible solutions will be to wrap your `Flow` scenarios into a `ReaderT` stack (so called `ReaderT` pattern), and provide an environment with permanent connections:

```haskell
import qualified Database.Beam.Sqlite as BS

data FlowEnv = FlowEnv
  { sqliteConn1 :: T.SqlConn BS.SqliteM
  , sqliteConn2 :: T.SqlConn BS.SqliteM
  }

type MyFlow a = ReaderT FlowEnv L.Flow a
-- The same as
-- type MyFlow r a = L.ReaderFlow FlowEnv a

searchByDate :: LocalTime -> MyFlow (T.DBResult [Member])
searchByDate startDate = do
  FlowEnv conn1 _ <- ask
  L.runDB conn1                         -- run a query
    $ L.findRows                        -- SELECT query returning rows
    $ B.select
    $ B.filter_ (\m -> joinDate m >=. B.val_ startDate)
    $ B.all_ (members clubDB)
```

Here, `DBResult` is an `Either` type that carries either success or a failure:

```haskell
data DBError = DBError DBErrorType Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

type DBResult a = Either DBError a
```

Where `DBErrorType` is an enum with various reasons of DB failure.

`Flow` has methods for connecting and disconnecting:

```haskell
initSqlDBConnection   :: DBConfig beM -> Flow (DBResult (SqlConn beM))
deinitSqlDBConnection :: SqlConn beM  -> Flow ()
getOrInitSqlConn      :: DBConfig beM -> Flow (DBResult (SqlConn beM))
```

Surely, you should be careful to not produce race conditions when using these methods from different threads.

For more info on the SQL DB subsystem usage, see tutorials and background materials.

### mtl style support

The framework exposes a type class `MonadFlow` with instances for `Flow`, `ReaderT r Flow`, `StateT s Flow`, `WriterT w Flow`, `ExceptT e Flow` and some other transformers. This makes it possible to write your `Flow` scenarios with the `mtl` style.

Let's see how the scenario `searchByDate` will look like:

```haskell
searchByDate
  :: MonadFlow m
  => MonadReader FlowEnv m
  => LocalTime
  -> m (T.DBResult [Member])
searchByDate startDate = do
  FlowEnv conn1 _ <- ask
  L.runDB conn1                         -- run a query
    $ L.findRows                        -- SELECT query returning rows
    $ B.select
    $ B.filter_ (\m -> joinDate m >=. B.val_ startDate)
    $ B.all_ (members clubDB)

```

### KV DB subsystem

The framework supports KV DBs in form of Redis.

The way we work with KV DB connections differs from the SQL DB subsystem. This time, we should specify not the connection instance, but rather its name. Certainly, the connection should be pre-created, otherwise you'll get the error `KVDBConnectionDoesNotExist`.

```haskell

myFlow :: T.KVDBKey -> L.Flow (T.TxResult (Maybe T.KVDBValue))
myFlow k = L.runKVDB "redis" $ L.multiExec $ do
  L.setTx k "bbb"
  res <- L.getTx k
  L.delTx [k]
  pure res

-- returns T.TxSuccess (Just "bbb")
```

See tests for more info:
- [KVDBSpec](./testDB/KVDB/KVDBSpec.hs)
- [KVDBArtSpec](./test/EulerHS/Tests/Framework/KVDBArtSpec.hs)

### Forking and awaiting child flows

It's possible to fork controllable flows and await for their results. This subsystem can be used to compose async-like flows, but the main case is parallel execution.

`Flow` has the following methods to work with forked flows:

```haskell
forkFlow  :: Description -> Flow () -> Flow ()
forkFlow' :: Description -> Flow a -> Flow (Awaitable (Either Text a))
await :: Maybe Microseconds -> Awaitable (Either Text a) -> m (Either AwaitingError a)
```

Notice that in the current framework there are no methods for killing a forked flow.

```haskell
myFlow :: L.Flow (Maybe String, Maybe String)
myFlow = do
  awaitable1 <- L.forkFlow' "Child Flow 1" (L.runIO (threadDelay 10000) >> pure "1")
  awaitable2 <- L.forkFlow' "Child Flow 2" (L.runIO (threadDelay 100000) >> pure "2")
  mbRes1 <- L.await Nothing awaitable1
  mbRes2 <- L.await Nothing awaitable2
  pure (mbRes1, mbRes2)

  -- Returns (Just "1", Just "2") after approximately 100000 ms.
```

## State handling

TODO

## Building the framework

See [BUILD.md](./BUILD.md)

## Testing the framework

You can run `stack test` to see if your system is ready, and the framework can be used.

### Integration tests for SQL backends

There are disabled tests for MySQL and Postgres DB backends. To run them:
- Create DB tables and fill with the appropriate data using these sql queries:
  * [MySQLDBSpec.sql](./testDB/SQLDB/TestData/MySQLDBSpec.sql)
  * [PostgresDBSpec.sql](./testDB/SQLDB/TestData/PostgresDBSpec.sql)
- Uncomment the corresponding specs in `lib/euler-hs/testSqlDB/Main.hs`
- Run tests:
  `stack test euler-hs:sql`

## EulerHS tutorials and template projects

* [Tutorial](./TUTORIAL.md)
* [Beam query examples](./lib/euler-hs/testDB/SQLDB/Tests/QueryExamplesSpec.hs)
* Demo project (**coming soon**)

## Background materials

* [Hierarchical Free Monads](https://github.com/graninas/hierarchical-free-monads-the-most-developed-approach-in-haskell)
* [Automatic whitebox testing with Free monads](https://github.com/graninas/automatic-whitebox-testing-showcase)
