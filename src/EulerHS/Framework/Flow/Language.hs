{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}

module EulerHS.Framework.Flow.Language
  (
  -- * Flow language
    Flow(..)
  , FlowMethod(..)
  , MonadFlow(..)
  , ReaderFlow
  -- ** Extra methods
  -- *** Logging
  , logCallStack
  , logExceptionCallStack
  , logInfo
  , logError
  , logDebug
  , logWarning
  -- *** Other
  , callAPI
  , callAPI'
  , callHTTP
  , runIO
  , forkFlow
  , forkFlow'
  -- *** PublishSubscribe
  , unpackLanguagePubSub
  -- ** Interpretation
  , foldFlow
  ) where

import           Control.Monad.Catch (ExitCase, MonadCatch (catch),
                                      MonadThrow (throwM))
import           Control.Monad.Free.Church (MonadFree)
import           Control.Monad.Trans.RWS.Strict (RWST)
import           Control.Monad.Trans.Writer (WriterT)
import qualified Data.Text as Text
import           EulerHS.Core.Language (KVDB, Logger, logMessage')
import qualified EulerHS.Core.Language as L
import qualified EulerHS.Core.PubSub.Language as PSL
import qualified EulerHS.Core.Types as T
import           EulerHS.Framework.Runtime (FlowRuntime)
import           EulerHS.Prelude hiding (getOption, throwM)
import           Servant.Client (BaseUrl, ClientError)

-- | Flow language.
data FlowMethod (next :: Type) where
  CallServantAPI
    :: HasCallStack
    => Maybe T.ManagerSelector
    -> BaseUrl
    -> T.EulerClient a
    -> (Either ClientError a -> next)
    -> FlowMethod next

  CallHTTP
    :: HasCallStack
    => T.HTTPRequest
    -> Maybe T.HTTPCert
    -> (Either Text T.HTTPResponse -> next)
    -> FlowMethod next

  EvalLogger
    :: HasCallStack
    => Logger a
    -> (a -> next)
    -> FlowMethod next

  RunIO
    :: HasCallStack
    => Text
    -> IO a
    -> (a -> next)
    -> FlowMethod next

  GetOption
    :: HasCallStack
    => Text
    -> (Maybe a -> next)
    -> FlowMethod next

  SetOption
    :: HasCallStack
    => Text
    -> a
    -> (() -> next)
    -> FlowMethod next

  DelOption
    :: HasCallStack
    => Text
    -> (() -> next)
    -> FlowMethod next

  GenerateGUID
    ::  HasCallStack
    => (Text -> next)
    -> FlowMethod next

  RunSysCmd
    :: HasCallStack
    => String
    -> (String -> next)
    -> FlowMethod next

  Fork
    :: HasCallStack
    => T.Description
    -> T.ForkGUID
    -> Flow a
    -> (T.Awaitable (Either Text a) -> next)
    -> FlowMethod next

  Await
    :: HasCallStack
    => Maybe T.Microseconds
    -> T.Awaitable (Either Text a)
    -> (Either T.AwaitingError a -> next)
    -> FlowMethod next

  ThrowException
    :: forall a e next
     . (HasCallStack, Exception e)
    => e
    -> (a -> next)
    -> FlowMethod next

  CatchException
    :: forall a e next
     . (HasCallStack, Exception e)
    => Flow a
    -> (e -> Flow a)
    -> (a -> next)
    -> FlowMethod next

  Mask
    :: forall b next
     . HasCallStack
    => ((forall a . Flow a -> Flow a) -> Flow b)
    -> (b -> next)
    -> FlowMethod next

  UninterruptibleMask
    :: forall b next
     . HasCallStack
    => ((forall a . Flow a -> Flow a) -> Flow b)
    -> (b -> next)
    -> FlowMethod next

  GeneralBracket
    :: forall a b c next
     . HasCallStack
    => Flow a
    -> (a -> ExitCase b -> Flow c)
    -> (a -> Flow b)
    -> ((b, c) -> next)
    -> FlowMethod next

  -- This is technically redundant - we can implement this using something like
  -- bracket, but better. - Koz
  RunSafeFlow
    :: HasCallStack
    => T.SafeFlowGUID
    -> Flow a
    -> (Either Text a -> next)
    -> FlowMethod next

  InitSqlDBConnection
    :: HasCallStack
    => T.DBConfig beM
    -> (T.DBResult (T.SqlConn beM) -> next)
    -> FlowMethod next

  DeInitSqlDBConnection
    :: HasCallStack
    => T.SqlConn beM
    -> (() -> next)
    -> FlowMethod next

  GetSqlDBConnection
    :: HasCallStack
    => T.DBConfig beM
    -> (T.DBResult (T.SqlConn beM) -> next)
    -> FlowMethod next

  InitKVDBConnection
    :: HasCallStack
    => T.KVDBConfig
    -> (T.KVDBAnswer T.KVDBConn -> next)
    -> FlowMethod next

  DeInitKVDBConnection
    :: HasCallStack
    => T.KVDBConn
    -> (() -> next)
    -> FlowMethod next

  GetKVDBConnection
    :: HasCallStack
    => T.KVDBConfig
    -> (T.KVDBAnswer T.KVDBConn -> next)
    -> FlowMethod next

  RunDB
    :: HasCallStack
    => T.SqlConn beM
    -> L.SqlDB beM a
    -> Bool
    -> (T.DBResult a -> next)
    -> FlowMethod next

  RunKVDB
    :: HasCallStack
    => Text
    -> KVDB a
    -> (T.KVDBAnswer a -> next)
    -> FlowMethod next

  RunPubSub
    :: HasCallStack
    => PubSub a
    -> (a -> next)
    -> FlowMethod next

  WithModifiedRuntime
    :: HasCallStack
    => (FlowRuntime -> FlowRuntime)
    -> Flow a
    -> (a -> next)
    -> FlowMethod next

-- Needed due to lack of impredicative instantiation (for stuff like Mask). -
-- Koz

-- Define a functor instance for the `FlowMethod` type.
instance Functor FlowMethod where
  {-# INLINEABLE fmap #-}
  fmap f = \case
    CallServantAPI mSel url client cont ->
      CallServantAPI mSel url client (f . cont)
    CallHTTP req cert cont -> CallHTTP req cert (f . cont)
    EvalLogger logger cont -> EvalLogger logger (f . cont)
    RunIO t act cont -> RunIO t act (f . cont)
    GetOption k cont -> GetOption k (f . cont)
    SetOption k v cont -> SetOption k v (f . cont)
    DelOption k cont -> DelOption k (f . cont)
    GenerateGUID cont -> GenerateGUID (f . cont)
    RunSysCmd cmd cont -> RunSysCmd cmd (f . cont)
    Fork desc guid flow cont -> Fork desc guid flow (f . cont)
    Await time awaitable cont -> Await time awaitable (f . cont)
    ThrowException e cont -> ThrowException e (f . cont)
    CatchException flow handler cont -> CatchException flow handler (f . cont)
    Mask cb cont -> Mask cb (f . cont)
    UninterruptibleMask cb cont -> UninterruptibleMask cb (f . cont)
    GeneralBracket acquire release act cont ->
      GeneralBracket acquire release act (f . cont)
    RunSafeFlow guid flow cont -> RunSafeFlow guid flow (f . cont)
    InitSqlDBConnection conf cont -> InitSqlDBConnection conf (f . cont)
    DeInitSqlDBConnection conn cont -> DeInitSqlDBConnection conn (f . cont)
    GetSqlDBConnection conf cont -> GetSqlDBConnection conf (f . cont)
    InitKVDBConnection conf cont -> InitKVDBConnection conf (f . cont)
    DeInitKVDBConnection conn cont -> DeInitKVDBConnection conn (f . cont)
    GetKVDBConnection conf cont -> GetKVDBConnection conf (f . cont)
    RunDB conn db b cont -> RunDB conn db b (f . cont)
    RunKVDB t db cont -> RunKVDB t db (f . cont)
    RunPubSub pubSub cont -> RunPubSub pubSub (f . cont)
    WithModifiedRuntime g innerFlow cont ->
      WithModifiedRuntime g innerFlow (f . cont)

newtype Flow (a :: Type) = Flow (F FlowMethod a)
  deriving newtype (Functor, Applicative, Monad, MonadFree FlowMethod)

instance MonadThrow Flow where
  {-# INLINEABLE throwM #-}
  throwM e = liftFC . ThrowException e $ id

instance MonadCatch Flow where
  {-# INLINEABLE catch #-}
  catch comp handler = liftFC . CatchException comp handler $ id

instance MonadMask Flow where
  {-# INLINEABLE mask #-}
  mask cb = liftFC . Mask cb $ id
  {-# INLINEABLE uninterruptibleMask #-}
  uninterruptibleMask cb = liftFC . UninterruptibleMask cb $ id
  {-# INLINEABLE generalBracket #-}
  generalBracket acquire release act =
    liftFC . GeneralBracket acquire release act $ id

foldFlow :: (Monad m) => (forall b . FlowMethod b -> m b) -> Flow a -> m a
foldFlow f (Flow comp) = foldF f comp

type ReaderFlow r = ReaderT r Flow

newtype PubSub a = PubSub {
  unpackLanguagePubSub :: HasCallStack => (forall b . Flow b -> IO b) -> PSL.PubSub a
  }

type MessageCallback
    =  ByteString  -- ^ Message payload
    -> Flow ()

type PMessageCallback
    =  ByteString  -- ^ Channel name
    -> ByteString  -- ^ Message payload
    -> Flow ()

{-|
  Fork a unit-returning flow.

  __Note__: To fork a flow that yields a value, use 'forkFlow\'' instead.

  __Warning__: With forked flows, race conditions and dead/live blocking become possible.
  All the rules applied to forked threads in Haskell can be applied to forked flows.

  Generally, the method is thread-safe and exception-free. It doesn't bookkeep the threads, and there is no possibility to kill a thread at the moment.

  Example:

  > myFlow1 = do
  >   logInfoT "myflow1" "logFromMyFlow1"
  >   someAction
  >
  > myFlow2 = do
  >   _ <- runIO someAction
  >   forkFlow "myFlow1 fork" myFlow1
  >   pure ()

  This function runs the given 'Flow ()' in a forked thread, and logs an error if the flow throws an exception.

  @since 1.0.0
-}
forkFlow :: HasCallStack => T.Description -> Flow () -> Flow ()
forkFlow description flow = void $ forkFlow' description $ do
  eitherResult <- runSafeFlow flow
  case eitherResult of
    Left msg -> logError ("forkFlow" :: Text) msg
    Right _  -> pure ()

{-|
  Same as 'forkFlow', but returns an 'Awaitable'.

  __Thread Safe__: Yes
  __Exception Free__: No (May propagate exceptions from the forked flow)

  This function is similar to 'forkFlow', but instead of immediately forking and running the 'Flow a', it returns an 'Awaitable' that can be used to await the result of the forked flow.

  Usage example:

  > myFlow1 = do
  >   logInfoT "myflow1" "logFromMyFlow1"
  >   pure 10
  >
  > myFlow2 = do
  >   awaitable <- forkFlow' "myFlow1 fork" myFlow1
  >   await Nothing awaitable

  This example creates two flows: 'myFlow1' and 'myFlow2'. 'myFlow2' forks 'myFlow1' using 'forkFlow'' and then waits for its result using 'await'.
-}
forkFlow' :: HasCallStack =>
  T.Description -> Flow a -> Flow (T.Awaitable (Either Text a))
forkFlow' description flow = do
    flowGUID <- generateGUID
    logInfo ("ForkFlow" :: Text) $ case Text.uncons description of
      Nothing ->
        "Flow forked. Description: " +| description |+ " GUID: " +| flowGUID |+ ""
      Just _  -> "Flow forked. GUID: " +| flowGUID |+ ""
    liftFC $ Fork description flowGUID flow id

{-|
  Method for calling external HTTP APIs using servant-client.

  __Thread Safe__: Yes
  __Exception Free__: Yes
  
  Allows to specify what manager should be used. If no manager found
  Alias for 'callServantAPI'. Takes a 'ManagerSelector' (if any), the base URL, and the servant client for the specified endpoint. Returns either a 'ClientError' or the API result.

  Example usage:

  > data User = User { firstName :: String, lastName :: String, userGUID :: String }
  >   deriving (Generic, Show, Eq, ToJSON, FromJSON)
  >
  > data Book = Book { author :: String, name :: String }
  >   deriving (Generic, Show, Eq, ToJSON, FromJSON)
  >
  > type API = "user" :> Get '[JSON] User :<|> "book" :> Get '[JSON] Book
  >
  > api :: HasCallStack => Proxy API
  > api = Proxy
  >
  > getUser :: HasCallStack => EulerClient User
  > getBook :: HasCallStack => EulerClient Book
  > (getUser :<|> getBook) = client api
  >
  > url = BaseUrl Http "localhost" port ""
  >
  > myFlow = do
  >   book <- callAPI' Nothing url getBook
  >   user <- callAPI' Nothing url getUser
-}
callAPI' :: (HasCallStack, MonadFlow m) =>
  Maybe T.ManagerSelector -> BaseUrl -> T.EulerClient a -> m (Either ClientError a)
callAPI' = callServantAPI

-- | The same as `callAPI'` but with default manager to be used.
callAPI :: (HasCallStack, MonadFlow m) =>
  BaseUrl -> T.EulerClient a -> m (Either ClientError a)
callAPI = callServantAPI Nothing

{-|
Log message with Info level.

Thread safe.

Sample usage:

> myFlow :: (HasCallStack, MonadFlow m) => m ()
> myFlow = do
>   logInfo ("myFlow" :: Text) "Starting myFlow..."
>   -- ... rest of the flow ...
-}
logInfo :: forall (tag :: Type) (m :: Type -> Type) .
  (HasCallStack, MonadFlow m, Show tag) => tag -> T.Message -> m ()
logInfo tag msg = evalLogger' $ logMessage' T.Info tag msg


{-|
Log message with Error level.

Thread safe.

Sample usage:

> anotherFlow :: (HasCallStack, MonadFlow m) => m ()
> anotherFlow = do
>   result <- someAction
>   case result of
>     Left err -> logError ("anotherFlow" :: Text) $ "Error: " <> err
>     Right val -> logInfo ("anotherFlow" :: Text) $ "Success! Value: " <> show val
-}
logError :: forall (tag :: Type) (m :: Type -> Type) .
  (HasCallStack, MonadFlow m, Show tag) => tag -> T.Message -> m ()
logError tag msg = do
  logCallStack
  evalLogger' $ logMessage' T.Error tag msg

{-|
Log message with Debug level.

Thread safe.

Sample usage:

> debugFlow :: (HasCallStack, MonadFlow m) => m ()
> debugFlow = do
>   logDebug ("debugFlow" :: Text) "Entering debugFlow..."
>   -- ... debug-related actions ...
-}
logDebug :: forall (tag :: Type) (m :: Type -> Type) .
  (HasCallStack, MonadFlow m, Show tag) => tag -> T.Message -> m ()
logDebug tag msg = evalLogger' $ logMessage' T.Debug tag msg

{-|
Log message with Warning level.

Thread safe.

Sample usage:

> warningFlow :: (HasCallStack, MonadFlow m) => m ()
> warningFlow = do
>   logWarning ("warningFlow" :: Text) "This is a warning!"
>   -- ... rest of the flow ...
-}
logWarning :: forall (tag :: Type) (m :: Type -> Type) .
  (HasCallStack, MonadFlow m, Show tag) => tag -> T.Message -> m ()
logWarning tag msg = evalLogger' $ logMessage' T.Warning tag msg

{-|
Run some IO operation, result should have 'ToJSONEx' instance (extended 'ToJSON'),
because we have to collect it in recordings for ART system.

Warning: This method is dangerous and should be used wisely.

Sample usage:

> myFlow :: (HasCallStack, MonadFlow m) => m ()
> myFlow = do
>   content <- runIO $ readFromFile file
>   logDebugT "content id" $ extractContentId content
>   pure content
-}
runIO :: (HasCallStack, MonadFlow m) => IO a -> m a
runIO = runIO' ""

{-|
The same as 'callHTTPWithCert' but does not need certificate data.

Thread safe, exception free.

Takes remote URL and returns either client error or result.

Sample usage:

> myFlow :: (HasCallStack, MonadFlow m) => m ()
> myFlow = do
>   result <- callHTTP url
>   case result of
>     Left err -> logErrorT "HTTP Error" err
>     Right response -> logInfoT "HTTP Response" $ "Received response: " <> show response
-}
callHTTP :: (HasCallStack, MonadFlow m) =>
  T.HTTPRequest -> m (Either Text.Text T.HTTPResponse)
callHTTP url = callHTTPWithCert url Nothing

{-|
MonadFlow implementation for the 'Flow' Monad. This allows the implementation of MonadFlow for
'ReaderT' and other monad transformers.

Omit 'forkFlow' as this will break some monads like 'StateT' (you can lift this manually if you
know what you're doing).

Sample usage:

> instance MonadFlow MyMonad where
>   -- Implementation of MonadFlow methods for MyMonad
-}
class (MonadMask m) => MonadFlow m where

  {-|
    Method for calling external HTTP APIs using servant-client.

    __Thread Safe__: Yes
    __Exception Free__: Yes
    
    Allows to specify what manager should be used. If no manager found
    Alias for 'callServantAPI'. Takes a 'ManagerSelector' (if any), the base URL, and the servant client for the specified endpoint. Returns either a 'ClientError' or the API result.

    Example usage:

    > data User = User { firstName :: String, lastName :: String, userGUID :: String }
    >   deriving (Generic, Show, Eq, ToJSON, FromJSON)
    >
    > data Book = Book { author :: String, name :: String }
    >   deriving (Generic, Show, Eq, ToJSON, FromJSON)
    >
    > type API = "user" :> Get '[JSON] User :<|> "book" :> Get '[JSON] Book
    >
    > api :: HasCallStack => Proxy API
    > api = Proxy
    >
    > getUser :: HasCallStack => EulerClient User
    > getBook :: HasCallStack => EulerClient Book
    > (getUser :<|> getBook) = client api
    >
    > url = BaseUrl Http "localhost" port ""
    >
    > myFlow = do
    >   book <- callAPI' Nothing url getBook
    >   user <- callAPI' Nothing url getUser
  -}
  callServantAPI
    :: HasCallStack
    => Maybe T.ManagerSelector     -- ^ name of the connection manager to be used
    -> BaseUrl                     -- ^ remote url 'BaseUrl'
    -> T.EulerClient a             -- ^ servant client 'EulerClient'
    -> m (Either ClientError a) -- ^ result

  {-|
    Method for calling external HTTP APIs without bothering with types.

    Thread safe, exception free.

    Takes remote URL, optional certificate data, and returns either a client error or result.

    Sample usage:

    > myFlow = do
    >   book <- callHTTPWithCert url cert
  -}
  callHTTPWithCert
    :: HasCallStack
    => T.HTTPRequest                        -- ^ remote url 'Text'
    -> Maybe T.HTTPCert                     -- ^ TLS certificate data
    -> m (Either Text.Text T.HTTPResponse)  -- ^ result

  -- | Evaluates a logging action.
  evalLogger' :: HasCallStack => Logger a -> m a

  -- | The same as runIO, but accepts a description which will be written into the ART recordings
  -- for better clarity.
  --
  -- Warning. This method is dangerous and should be used wisely.
  --
  -- > myFlow = do
  -- >   content <- runIO' "reading from file" $ readFromFile file
  -- >   logDebugT "content id" $ extractContentId content
  -- >   pure content
  runIO' :: HasCallStack => Text -> IO a -> m a

  {-|
    Gets stored a typed option by a typed key.

    Thread safe, exception free.

    Sample usage:

    >  data MerchantIdKey = MerchantIdKey
    > 
    >  instance OptionEntity MerchantIdKey Text
    > 
    >  myFlow = do
    >    _ <- setOption MerchantIdKey "abc1234567"
    >    mKey <- getOption MerchantIdKey
    >    runIO $ putTextLn mKey
  -}
  getOption :: forall k v. (HasCallStack, T.OptionEntity k v) => k -> m (Maybe v)

  {-|
    Sets a typed option using a typed key (a mutable destructive operation).

    Be aware that it's possible to overflow the runtime with options
    created uncontrollably.

    Also, please keep in mind the options are runtime-bound and if you have
    several API methods working with the same option key, you'll get a race.

    Thread safe, exception free.

    Sample usage:

    >  data MerchantIdKey = MerchantIdKey
    > 
    >  instance OptionEntity MerchantIdKey Text
    > 
    >  myFlow = do
    >    _ <- setOption MerchantIdKey "abc1234567"
    >    mKey <- getOption MerchantIdKey
    >    runIO $ putTextLn mKey
  -}
  setOption :: forall k v. (HasCallStack, T.OptionEntity k v) => k -> v -> m ()

  {-|
    Deletes a typed option using a typed key.
    Sample usage:

    >  data MerchantIdKey = MerchantIdKey
    > 
    >  instance OptionEntity MerchantIdKey Text
    > 
    >  myFlow = do
    >    _ <- setOption MerchantIdKey "abc1234567"
    >    mKey <- getOption MerchantIdKey
    >    runIO $ putTextLn mKey
    >    delOption MerchantIdKey
  -}
  -- | Deletes a typed option using a typed key.
  delOption :: forall k v. (HasCallStack, T.OptionEntity k v) => k -> m ()

  {-|
    Generate a version 4 UUID as specified in RFC 4122
    e.g. 25A8FC2A-98F2-4B86-98F6-84324AF28611.

    Thread safe, exception free.

    Sample usage:

    > myFlow = do
    >   guid <- generateGUID
  -}
  generateGUID :: HasCallStack => m Text

  {-|
    Runs a system command and returns its output.

    Warning. This method is dangerous and should be used wisely.

    Sample usage:

    > myFlow = do
    >   currentDir <- runSysCmd "pwd"
    >   logInfoT "Current Directory" $ toText currentDir
    >   ...
  -}
  runSysCmd :: HasCallStack => String -> m String

  -- | Inits an SQL connection using a config.
  --
  -- Returns an error (Left $ T.DBError T.ConnectionAlreadyExists msg)
  -- if the connection already exists for this config.
  --
  -- Thread safe, exception free.
  initSqlDBConnection :: HasCallStack => T.DBConfig beM -> m (T.DBResult (T.SqlConn beM))

  -- | Deinits an SQL connection.
  -- Does nothing if the connection is not found (might have been closed earlier).
  --
  -- Thread safe, exception free.
  deinitSqlDBConnection :: HasCallStack => T.SqlConn beM -> m ()

  -- | Gets the existing connection.
  --
  -- Returns an error (Left $ T.DBError T.ConnectionDoesNotExist)
  -- if the connection does not exist.
  --
  -- Thread safe, exception free.
  getSqlDBConnection :: HasCallStack => T.DBConfig beM -> m (T.DBResult (T.SqlConn beM))

  -- | Inits a KV DB connection using a config.
  --
  -- Returns an error (Left $ KVDBError KVDBConnectionAlreadyExists msg)
  -- if the connection already exists.
  --
  -- Thread safe, exception free.
  initKVDBConnection :: HasCallStack => T.KVDBConfig -> m (T.KVDBAnswer T.KVDBConn)

  -- | Deinits the given KV DB connection.
  -- Does nothing if the connection is not found (might have been closed earlier).
  --
  -- Thread safe, exception free.
  deinitKVDBConnection :: HasCallStack => T.KVDBConn  -> m ()

  -- | Get the existing connection.

  -- Returns an error (Left $ KVDBError KVDBConnectionDoesNotExist)
  -- if the connection does not exits for this config.
  --
  -- Thread safe, exception free.
  getKVDBConnection :: HasCallStack => T.KVDBConfig -> m (T.KVDBAnswer T.KVDBConn)

  -- | Evaluates SQL DB operations outside of any transaction.
  -- It's possible to have a chain of SQL DB calls (within the SqlDB language).
  -- These chains will be executed as a single transaction.
  --
  -- Thread safe, exception free.
  --
  -- The underlying library is beam which allows to access 3 different SQL backends.
  -- See TUTORIAL.md, README.md and QueryExamplesSpec.hs for more info.
  --
  -- > myFlow :: HasCallStack => L.Flow (T.DBResult (Maybe User))
  -- > myFlow = do
  -- >   connection <- L.initSqlDBConnection postgresCfg
  -- >
  -- >   res <- L.runDB connection $ do
  -- >     let predicate1 User {..} = _userFirstName ==. B.val_ "John"
  -- >
  -- >     L.updateRows $ B.update (_users eulerDb)
  -- >       (\User {..} -> mconcat
  -- >         [ _userFirstName <-. B.val_ "Leo"
  -- >         , _userLastName  <-. B.val_ "San"
  -- >         ]
  -- >       )
  -- >       predicate1
  -- >
  -- >     let predicate2 User {..} = _userFirstName ==. B.val_ "Leo"
  -- >     L.findRow
  -- >       $ B.select
  -- >       $ B.limit_ 1
  -- >       $ B.filter_ predicate2
  -- >       $ B.all_ (_users eulerDb)
  -- >
  -- >   L.deinitSqlDBConnection connection
  -- >   pure res
  runDB
    ::
      ( HasCallStack
      , T.BeamRunner beM
      , T.BeamRuntime be beM
      )
    => T.SqlConn beM
    -> L.SqlDB beM a
    -> m (T.DBResult a)

  -- | Like `runDB` but runs inside a SQL transaction.
  runTransaction
    ::
      ( HasCallStack
      , T.BeamRunner beM
      , T.BeamRuntime be beM
      )
    => T.SqlConn beM
    -> L.SqlDB beM a
    -> m (T.DBResult a)

  -- | Await for some a result from the flow.
  -- If the timeout is Nothing than the operation is blocking.
  -- If the timeout is set then the internal mechanism tries to do several (10) checks for the result.
  -- Can return earlier if the result became available.
  -- Returns either an Awaiting error or a result.
  --
  -- Warning. There are no guarantees of a proper thread delaying here.
  --
  -- Thread safe, exception free.
  --
  -- | mbMcs == Nothing: infinite awaiting.
  -- | mbMcs == Just (Microseconds n): await for approximately n seconds.
  --     Awaiting may succeed ealier.
  --
  -- > myFlow1 = do
  -- >   logInfoT "myflow1" "logFromMyFlow1"
  -- >   pure 10
  -- >
  -- > myFlow2 = do
  -- >   awaitable <- forkFlow' "myFlow1 fork" myFlow1
  -- >   await Nothing awaitable
  await
    :: HasCallStack
    => Maybe T.Microseconds
    -> T.Awaitable (Either Text a)
    -> m (Either T.AwaitingError a)

  -- | Throw a given exception.
  --
  -- It's possible to catch this exception using runSafeFlow method.
  --
  -- Thread safe. Exception throwing.
  --
  -- > myFlow = do
  -- >   res <- authAction
  -- >   case res of
  -- >     Failure reason -> throwException err403 {errBody = reason}
  -- >     Success -> ...
  throwException :: forall a e. (HasCallStack, Exception e) => e -> m a
  throwException ex = do
    -- Doubt: Should we just print the exception details without the
    -- contextual details that logError prints. As finding the message inside logError is a bit
    -- cumbersome. Just printing the exception details will be much cleaner if we don't need the
    -- contextual details.
    logExceptionCallStack ex
    throwExceptionWithoutCallStack ex

  throwExceptionWithoutCallStack :: forall a e. (HasCallStack, Exception e) => e -> m a
  throwExceptionWithoutCallStack = throwM

  {-|
  Run a flow safely, catching all exceptions.

  Returns either a result or the exception turned into a text message.

  This includes usual instances of the 'Exception' type class, the `error` exception, and custom user exceptions thrown by the 'throwException' method.

  __Thread Safe__: Yes
  __Exception Free__: Yes

  Examples:

  > myFlow = runSafeFlow $ throwException err403 {errBody = reason}

  > myFlow = do
  >   eitherContent <- runSafeFlow $ runIO $ readFromFile file
  >   case eitherContent of
  >     Left err -> ...
  >     Right content -> ...

  @since 1.0.0
-}
  runSafeFlow :: HasCallStack => Flow a -> m (Either Text a)

  -- | Execute kvdb actions.
  --
  -- Thread safe, exception free.
  --
  -- > myFlow = do
  -- >   kvres <- L.runKVDB $ do
  -- >     set "aaa" "bbb"
  -- >     res <- get "aaa"
  -- >     del ["aaa"]
  -- >     pure res
  runKVDB
    :: HasCallStack
    => Text
    -> KVDB a -- ^ KVDB action
    -> m (T.KVDBAnswer a)

  ---- Experimental Pub Sub implementation using Redis Pub Sub.

  runPubSub
    :: HasCallStack
    => PubSub a
    -> m a

  -- | Publish payload to channel.
  publish
    :: HasCallStack
    => PSL.Channel                        -- ^ Channel in which payload will be send
    -> PSL.Payload                        -- ^ Payload
    -> m (Either T.KVDBReply Integer)  -- ^ Number of subscribers received payload

  -- | Subscribe to all channels from list.
  -- Note: Subscription won't be unsubscribed automatically on thread end.
  -- Use canceller explicitly to cancel subscription
  subscribe
    :: HasCallStack
    => [PSL.Channel]    -- ^ List of channels to subscribe
    -> MessageCallback  -- ^ Callback function.
    -> m (Flow ())   -- ^ Inner flow is a canceller of current subscription

  -- | Subscribe to all channels from list. Respects redis pattern syntax.
  -- Note: Subscription won't be unsubscribed automatically on thread end.
  -- Use canceller explicitly to cancel subscription
  psubscribe
    :: HasCallStack
    => [PSL.ChannelPattern] -- ^ List of channels to subscribe (wit respect to patterns supported by redis)
    -> PMessageCallback     -- ^ Callback function
    -> m (Flow ())       -- ^ Inner flow is a canceller of current subscription

  -- | Run a flow with a modified runtime. The runtime will be restored after
  -- the computation finishes.
  --
  -- @since 2.0.3.1
  withModifiedRuntime
    :: (HasCallStack, MonadFlow m)
    => (FlowRuntime -> FlowRuntime) -- ^ Temporary modification function for runtime
    -> Flow a -- ^ Computation to run with modified runtime
    -> m a

{-|
Implements the 'MonadFlow' type class for the 'Flow' monad. 
This allows various operations to be lifted into the 'Flow' monad, providing a layer of abstraction over different
underlying monads that implement 'MonadFlow'.

The instances include 'ReaderT', 'StateT', 'WriterT', 'ExceptT', and 'RWST' transformers, making it flexible to use
within different monad stacks.
-}
instance MonadFlow Flow where
  {-# INLINEABLE callServantAPI #-}
  callServantAPI mbMgrSel url cl = liftFC $ CallServantAPI mbMgrSel url cl id
  {-# INLINEABLE callHTTPWithCert #-}
  callHTTPWithCert url cert = liftFC $ CallHTTP url cert id
  {-# INLINEABLE evalLogger' #-}
  evalLogger' logAct = liftFC $ EvalLogger logAct id
  {-# INLINEABLE runIO' #-}
  runIO' descr ioAct = liftFC $ RunIO descr ioAct id
  {-# INLINEABLE getOption #-}
  getOption :: forall k v. (HasCallStack, T.OptionEntity k v) => k -> Flow (Maybe v)
  getOption k = liftFC $ GetOption (T.mkOptionKey @k @v k) id
  {-# INLINEABLE setOption #-}
  setOption :: forall k v. (HasCallStack, T.OptionEntity k v) => k -> v -> Flow ()
  setOption k v = liftFC $ SetOption (T.mkOptionKey @k @v k) v id
  {-# INLINEABLE delOption #-}
  delOption :: forall k v. (HasCallStack, T.OptionEntity k v) => k -> Flow ()
  delOption k = liftFC $ DelOption (T.mkOptionKey @k @v k) id
  {-# INLINEABLE generateGUID #-}
  generateGUID = liftFC $ GenerateGUID id
  {-# INLINEABLE runSysCmd #-}
  runSysCmd cmd = liftFC $ RunSysCmd cmd id
  {-# INLINEABLE initSqlDBConnection #-}
  initSqlDBConnection cfg = liftFC $ InitSqlDBConnection cfg id
  {-# INLINEABLE deinitSqlDBConnection #-}
  deinitSqlDBConnection conn = liftFC $ DeInitSqlDBConnection conn id
  {-# INLINEABLE getSqlDBConnection #-}
  getSqlDBConnection cfg = liftFC $ GetSqlDBConnection cfg id
  {-# INLINEABLE initKVDBConnection #-}
  initKVDBConnection cfg = liftFC $ InitKVDBConnection cfg id
  {-# INLINEABLE deinitKVDBConnection #-}
  deinitKVDBConnection conn = liftFC $ DeInitKVDBConnection conn id
  {-# INLINEABLE getKVDBConnection #-}
  getKVDBConnection cfg = liftFC $ GetKVDBConnection cfg id
  {-# INLINEABLE runDB #-}
  runDB conn dbAct = liftFC $ RunDB conn dbAct False id
  {-# INLINEABLE runTransaction #-}
  runTransaction conn dbAct = liftFC $ RunDB conn dbAct True id
  {-# INLINEABLE await #-}
  await mbMcs awaitable = liftFC $ Await mbMcs awaitable id
  {-# INLINEABLE runSafeFlow #-}
  runSafeFlow flow = do
    safeFlowGUID <- generateGUID
    liftFC $ RunSafeFlow safeFlowGUID flow id
  {-# INLINEABLE runKVDB #-}
  runKVDB cName act = liftFC $ RunKVDB cName act id
  {-# INLINEABLE runPubSub #-}
  runPubSub act = liftFC $ RunPubSub act id
  {-# INLINEABLE publish #-}
  publish channel payload = runPubSub $ PubSub $ const $ PSL.publish channel payload
  {-# INLINEABLE subscribe #-}
  subscribe channels cb = fmap (runIO' "subscribe") $
    runPubSub $ PubSub $ \runFlow -> PSL.subscribe channels (runFlow . cb)
  {-# INLINEABLE psubscribe #-}
  psubscribe channels cb = fmap (runIO' "psubscribe") $
    runPubSub $ PubSub $ \runFlow -> PSL.psubscribe channels (\ch -> runFlow . cb ch)
  {-# INLINEABLE withModifiedRuntime #-}
  withModifiedRuntime f flow = liftFC $ WithModifiedRuntime f flow id

instance MonadFlow m => MonadFlow (ReaderT r m) where
  {-# INLINEABLE callServantAPI #-}
  callServantAPI mbMgrSel url = lift . callServantAPI mbMgrSel url
  {-# INLINEABLE callHTTPWithCert #-}
  callHTTPWithCert url = lift . callHTTPWithCert url
  {-# INLINEABLE evalLogger' #-}
  evalLogger' = lift . evalLogger'
  {-# INLINEABLE runIO' #-}
  runIO' descr = lift . runIO' descr
  {-# INLINEABLE getOption #-}
  getOption = lift . getOption
  {-# INLINEABLE setOption #-}
  setOption k = lift . setOption k
  {-# INLINEABLE delOption #-}
  delOption = lift . delOption
  {-# INLINEABLE generateGUID #-}
  generateGUID = lift generateGUID
  {-# INLINEABLE runSysCmd #-}
  runSysCmd = lift . runSysCmd
  {-# INLINEABLE initSqlDBConnection #-}
  initSqlDBConnection = lift . initSqlDBConnection
  {-# INLINEABLE deinitSqlDBConnection #-}
  deinitSqlDBConnection = lift . deinitSqlDBConnection
  {-# INLINEABLE getSqlDBConnection #-}
  getSqlDBConnection = lift . getSqlDBConnection
  {-# INLINEABLE initKVDBConnection #-}
  initKVDBConnection = lift . initKVDBConnection
  {-# INLINEABLE deinitKVDBConnection #-}
  deinitKVDBConnection = lift . deinitKVDBConnection
  {-# INLINEABLE getKVDBConnection #-}
  getKVDBConnection = lift . getKVDBConnection
  {-# INLINEABLE runDB #-}
  runDB conn = lift . runDB conn
  {-# INLINEABLE runTransaction #-}
  runTransaction conn = lift . runTransaction conn
  {-# INLINEABLE await #-}
  await mbMcs = lift . await mbMcs
  {-# INLINEABLE runSafeFlow #-}
  runSafeFlow = lift . runSafeFlow
  {-# INLINEABLE runKVDB #-}
  runKVDB cName = lift . runKVDB cName
  {-# INLINEABLE runPubSub #-}
  runPubSub = lift . runPubSub
  {-# INLINEABLE publish #-}
  publish channel = lift . publish channel
  {-# INLINEABLE subscribe #-}
  subscribe channels = lift . subscribe channels
  {-# INLINEABLE psubscribe #-}
  psubscribe channels = lift . psubscribe channels
  {-# INLINEABLE withModifiedRuntime #-}
  withModifiedRuntime f = lift . withModifiedRuntime f

instance MonadFlow m => MonadFlow (StateT s m) where
  {-# INLINEABLE callServantAPI #-}
  callServantAPI mbMgrSel url = lift . callServantAPI mbMgrSel url
  {-# INLINEABLE callHTTPWithCert #-}
  callHTTPWithCert url = lift . callHTTPWithCert url
  {-# INLINEABLE evalLogger' #-}
  evalLogger' = lift . evalLogger'
  {-# INLINEABLE runIO' #-}
  runIO' descr = lift . runIO' descr
  {-# INLINEABLE getOption #-}
  getOption = lift . getOption
  {-# INLINEABLE setOption #-}
  setOption k = lift . setOption k
  {-# INLINEABLE delOption #-}
  delOption = lift . delOption
  {-# INLINEABLE generateGUID #-}
  generateGUID = lift generateGUID
  {-# INLINEABLE runSysCmd #-}
  runSysCmd = lift . runSysCmd
  {-# INLINEABLE initSqlDBConnection #-}
  initSqlDBConnection = lift . initSqlDBConnection
  {-# INLINEABLE deinitSqlDBConnection #-}
  deinitSqlDBConnection = lift . deinitSqlDBConnection
  {-# INLINEABLE getSqlDBConnection #-}
  getSqlDBConnection = lift . getSqlDBConnection
  {-# INLINEABLE initKVDBConnection #-}
  initKVDBConnection = lift . initKVDBConnection
  {-# INLINEABLE deinitKVDBConnection #-}
  deinitKVDBConnection = lift . deinitKVDBConnection
  {-# INLINEABLE getKVDBConnection #-}
  getKVDBConnection = lift . getKVDBConnection
  {-# INLINEABLE runDB #-}
  runDB conn = lift . runDB conn
  {-# INLINEABLE runTransaction #-}
  runTransaction conn = lift . runTransaction conn
  {-# INLINEABLE await #-}
  await mbMcs = lift . await mbMcs
  {-# INLINEABLE runSafeFlow #-}
  runSafeFlow = lift . runSafeFlow
  {-# INLINEABLE runKVDB #-}
  runKVDB cName = lift . runKVDB cName
  {-# INLINEABLE runPubSub #-}
  runPubSub = lift . runPubSub
  {-# INLINEABLE publish #-}
  publish channel = lift . publish channel
  {-# INLINEABLE subscribe #-}
  subscribe channels = lift . subscribe channels
  {-# INLINEABLE psubscribe #-}
  psubscribe channels = lift . psubscribe channels
  {-# INLINEABLE withModifiedRuntime #-}
  withModifiedRuntime f = lift . withModifiedRuntime f

instance (MonadFlow m, Monoid w) => MonadFlow (WriterT w m) where
  {-# INLINEABLE callServantAPI #-}
  callServantAPI mbMgrSel url = lift . callServantAPI mbMgrSel url
  {-# INLINEABLE callHTTPWithCert #-}
  callHTTPWithCert url = lift . callHTTPWithCert url
  {-# INLINEABLE evalLogger' #-}
  evalLogger' = lift . evalLogger'
  {-# INLINEABLE runIO' #-}
  runIO' descr = lift . runIO' descr
  {-# INLINEABLE getOption #-}
  getOption = lift . getOption
  {-# INLINEABLE setOption #-}
  setOption k = lift . setOption k
  {-# INLINEABLE delOption #-}
  delOption = lift . delOption
  {-# INLINEABLE generateGUID #-}
  generateGUID = lift generateGUID
  {-# INLINEABLE runSysCmd #-}
  runSysCmd = lift . runSysCmd
  {-# INLINEABLE initSqlDBConnection #-}
  initSqlDBConnection = lift . initSqlDBConnection
  {-# INLINEABLE deinitSqlDBConnection #-}
  deinitSqlDBConnection = lift . deinitSqlDBConnection
  {-# INLINEABLE getSqlDBConnection #-}
  getSqlDBConnection = lift . getSqlDBConnection
  {-# INLINEABLE initKVDBConnection #-}
  initKVDBConnection = lift . initKVDBConnection
  {-# INLINEABLE deinitKVDBConnection #-}
  deinitKVDBConnection = lift . deinitKVDBConnection
  {-# INLINEABLE getKVDBConnection #-}
  getKVDBConnection = lift . getKVDBConnection
  {-# INLINEABLE runDB #-}
  runDB conn = lift . runDB conn
  {-# INLINEABLE runTransaction #-}
  runTransaction conn = lift . runTransaction conn
  {-# INLINEABLE await #-}
  await mbMcs = lift . await mbMcs
  {-# INLINEABLE runSafeFlow #-}
  runSafeFlow = lift . runSafeFlow
  {-# INLINEABLE runKVDB #-}
  runKVDB cName = lift . runKVDB cName
  {-# INLINEABLE runPubSub #-}
  runPubSub = lift . runPubSub
  {-# INLINEABLE publish #-}
  publish channel = lift . publish channel
  {-# INLINEABLE subscribe #-}
  subscribe channels = lift . subscribe channels
  {-# INLINEABLE psubscribe #-}
  psubscribe channels = lift . psubscribe channels
  {-# INLINEABLE withModifiedRuntime #-}
  withModifiedRuntime f = lift . withModifiedRuntime f

instance MonadFlow m => MonadFlow (ExceptT e m) where
  {-# INLINEABLE callServantAPI #-}
  callServantAPI mbMgrSel url = lift . callServantAPI mbMgrSel url
  {-# INLINEABLE callHTTPWithCert #-}
  callHTTPWithCert url = lift . callHTTPWithCert url
  {-# INLINEABLE evalLogger' #-}
  evalLogger' = lift . evalLogger'
  {-# INLINEABLE runIO' #-}
  runIO' descr = lift . runIO' descr
  {-# INLINEABLE getOption #-}
  getOption = lift . getOption
  {-# INLINEABLE setOption #-}
  setOption k = lift . setOption k
  {-# INLINEABLE delOption #-}
  delOption = lift . delOption
  {-# INLINEABLE generateGUID #-}
  generateGUID = lift generateGUID
  {-# INLINEABLE runSysCmd #-}
  runSysCmd = lift . runSysCmd
  {-# INLINEABLE initSqlDBConnection #-}
  initSqlDBConnection = lift . initSqlDBConnection
  {-# INLINEABLE deinitSqlDBConnection #-}
  deinitSqlDBConnection = lift . deinitSqlDBConnection
  {-# INLINEABLE getSqlDBConnection #-}
  getSqlDBConnection = lift . getSqlDBConnection
  {-# INLINEABLE initKVDBConnection #-}
  initKVDBConnection = lift . initKVDBConnection
  {-# INLINEABLE deinitKVDBConnection #-}
  deinitKVDBConnection = lift . deinitKVDBConnection
  {-# INLINEABLE getKVDBConnection #-}
  getKVDBConnection = lift . getKVDBConnection
  {-# INLINEABLE runDB #-}
  runDB conn = lift . runDB conn
  {-# INLINEABLE runTransaction #-}
  runTransaction conn = lift . runTransaction conn
  {-# INLINEABLE await #-}
  await mbMcs = lift . await mbMcs
  {-# INLINEABLE runSafeFlow #-}
  runSafeFlow = lift . runSafeFlow
  {-# INLINEABLE runKVDB #-}
  runKVDB cName = lift . runKVDB cName
  {-# INLINEABLE runPubSub #-}
  runPubSub = lift . runPubSub
  {-# INLINEABLE publish #-}
  publish channel = lift . publish channel
  {-# INLINEABLE subscribe #-}
  subscribe channels = lift . subscribe channels
  {-# INLINEABLE psubscribe #-}
  psubscribe channels = lift . psubscribe channels
  {-# INLINEABLE withModifiedRuntime #-}
  withModifiedRuntime f = lift . withModifiedRuntime f

instance (MonadFlow m, Monoid w) => MonadFlow (RWST r w s m) where
  {-# INLINEABLE callServantAPI #-}
  callServantAPI mbMgrSel url = lift . callServantAPI mbMgrSel url
  {-# INLINEABLE callHTTPWithCert #-}
  callHTTPWithCert url = lift . callHTTPWithCert url
  {-# INLINEABLE evalLogger' #-}
  evalLogger' = lift . evalLogger'
  {-# INLINEABLE runIO' #-}
  runIO' descr = lift . runIO' descr
  {-# INLINEABLE getOption #-}
  getOption = lift . getOption
  {-# INLINEABLE setOption #-}
  setOption k = lift . setOption k
  {-# INLINEABLE delOption #-}
  delOption = lift . delOption
  {-# INLINEABLE generateGUID #-}
  generateGUID = lift generateGUID
  {-# INLINEABLE runSysCmd #-}
  runSysCmd = lift . runSysCmd
  {-# INLINEABLE initSqlDBConnection #-}
  initSqlDBConnection = lift . initSqlDBConnection
  {-# INLINEABLE deinitSqlDBConnection #-}
  deinitSqlDBConnection = lift . deinitSqlDBConnection
  {-# INLINEABLE getSqlDBConnection #-}
  getSqlDBConnection = lift . getSqlDBConnection
  {-# INLINEABLE initKVDBConnection #-}
  initKVDBConnection = lift . initKVDBConnection
  {-# INLINEABLE deinitKVDBConnection #-}
  deinitKVDBConnection = lift . deinitKVDBConnection
  {-# INLINEABLE getKVDBConnection #-}
  getKVDBConnection = lift . getKVDBConnection
  {-# INLINEABLE runDB #-}
  runDB conn = lift . runDB conn
  {-# INLINEABLE runTransaction #-}
  runTransaction conn = lift . runTransaction conn
  {-# INLINEABLE await #-}
  await mbMcs = lift . await mbMcs
  {-# INLINEABLE runSafeFlow #-}
  runSafeFlow = lift . runSafeFlow
  {-# INLINEABLE runKVDB #-}
  runKVDB cName = lift . runKVDB cName
  {-# INLINEABLE runPubSub #-}
  runPubSub = lift . runPubSub
  {-# INLINEABLE publish #-}
  publish channel = lift . publish channel
  {-# INLINEABLE subscribe #-}
  subscribe channels = lift . subscribe channels
  {-# INLINEABLE psubscribe #-}
  psubscribe channels = lift . psubscribe channels
  {-# INLINEABLE withModifiedRuntime #-}
  withModifiedRuntime f = lift . withModifiedRuntime f

-- TODO: save a builder in some state for using `hPutBuilder`?
--
-- Doubts:
-- Is it the right place to put it?
-- Should the type be more generic than IO ()?
logCallStack :: (HasCallStack, MonadFlow m) => m ()
logCallStack = logDebug ("CALLSTACK" :: Text) $ Text.pack $ prettyCallStack callStack

logExceptionCallStack :: (HasCallStack, Exception e, MonadFlow m) => e -> m ()
logExceptionCallStack ex = logError ("EXCEPTION" :: Text) $ Text.pack $ displayException ex

