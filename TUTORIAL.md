# Building:

1. Pull in https://bitbucket.org/juspay/euler-hs/src/master/

2. Follow the readme there.

3. Add `euler-hs` in `dependencies` list in `package.yaml`

4. Pull in https://github.com/graninas/beam-mysql

5. Put this in `stack.yaml` (See https://bitbucket.org/juspay/euler-hs/src/master/stack.yaml for details):
    ```
    extra-include-dirs:
    - /usr/local/opt/openssl/include
    extra-lib-dirs:
    - /usr/local/opt/openssl/lib
    ```

Note: To pull in desired external modules either clone desired repo and reference in `stack.yaml`, like this:
```
packages:
- .
- ../../euler-hs/lib/euler-hs
- ../../beam-mysql
```

Or reference the git commit, like this:
```
- git: https://github.com/graninas/beam-mysql
  commit: e5a667fe6396441c7dcf1c7755c69345f00d0eee
```

# Free Monad background
From https://github.com/graninas/software-design-in-haskell:

- http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html
- https://softwareengineering.stackexchange.com/questions/242795/what-is-the-free-monad-interpreter-pattern#
- Euler uses FreeChurch optimization https://hackage.haskell.org/package/free-5.1.3/docs/Control-Monad-Free-Church.html


# EulerHS

Basic framework: _lib/euler-hs/src/EulerHS/Framework/Flow_

## EulerHS/Framework/Flow/Language.hs
`FlowMethod` datatype defines basic operations such as
- State (key value): GetOption, SetOption
- Logging: EvalLogger
- DB connectivity

FreeChurch monad: `type Flow = F FlowMethod`

Smart constructors for operations lifted into `Flow`.  See usage comments in `Language.hs`.


## runFlow

Datatype `FlowRuntime` (_lib/euler-hs/src/EulerHS/Framework/Runtime.hs_) encapsulates db-connections, state, logging, Http manager etc lifted into IO.

Key function is `runFlow` at bottom of _lib/euler-hs/src/EulerHS/Framework/Flow/Interpreter.hs_

See app/euler-backend/src/App.hs for example app.


## Example: Logging
_lib/euler-hs/src/EulerHS/Core/Types/Logger.hs_

_from app/euler-backend/src/App.hs_
```
runEulerBackendApp' :: Settings -> IO ()
runEulerBackendApp' settings = do
  let loggerCfg = T.defaultLoggerConfig
        { T._logToFile = True
        , T._logFilePath = "/tmp/euler-backend.log"
        , T._isAsync = True
        }

  R.withFlowRuntime (Just loggerCfg) ...
```

## Example: Database Connectivity

_from lib/euler-hs/testDB/SQLDB/Tests/SQLiteDBSpec.hs_

```
sqliteCfg = T.mkSQLiteConfig "eulerSQliteDB" "./test/EulerHS/TestData/test.db"

runFlow rt $ do
          eConn1 <- L.initSqlDBConnection sqliteCfg
          eConn2 <- L.getSqlDBConnection sqliteCfg


runFlow rt selectOneDbScript  


selectOneDbScript :: L.Flow (T.DBResult (Maybe User))
selectOneDbScript = do
  connection <- connectOrFail sqliteCfg

  L.runDB connection $ do
    let predicate User {..} = _userFirstName ==. B.val_ "John"

    L.findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (_users eulerDb)
```

## GUID
_lib/euler-hs/src/EulerHS/Framework/Flow/Language.hs_

`generateGUID`

## State
_lib/euler-hs/src/EulerHS/Framework/Flow/Language.hs_

`getOption`

`setOption`

## Server
_app/credit-platform/src/CreditPlatform/Server.hs_
