# Changelog for euler-hs

## [2.5.0.1] - 2020-03-26
  - New documentation

## [2.5.0.0] - 2020-12-01
* EulerHS is prepared for open sourcing.
  - New documentation
  - Improvements and updates in public interface
  - Warning fixes
  - Logger rework
  - License changed to Apache 2.0
  - All builds except stack are removed

## [2.0.0.0] - 2020-07-01

* Use `beam-mysql` based on `mysql-haskell` instead of `mysql`

## [1.10.0.0] - 2020-06-16
* EulerHS 1.10.0.0: a significant update with new features and fixes.
  - Rework of repository structure, each library has its own repo now.
  - Compatibility with GHC 8.8.
  - Updated stack resolver to 15.15.
  - Added `run[Update/Delete]ReturningList` for Postgres.
  - Added `delOption`.
  - Added `runUntracedIO` for reading/writing sensitive data.
  - Added untyped HTTP calls to `Flow`.
  - Lots of various fixes not listed here.
  - Added `insertRowReturningMySql` function which does not use temporary tables internally.
  - Beware of `SqlBool` vs `Bool` when writing `beam` queries, and other gotchas: [see BEAM-NOTES.md](BEAM-NOTES.md),
    read this if you use database at all via `euler-hs`.
    Some of this can have a critical effect on performance, especially on MySQL.

## [1.9.5.0] - 2020-04-13
* EulerHS 1.9.5.0: fixes
  - Async logger mem consumption fixed.
  - Integration tests with MySQL disabled
  - Improved documentation [see README.md](README.md)

## [1.9.0.0] - 2020-04-13
* EulerHS 1.9.0.0: a significant update with new features and fixes.
  - RunSafeFlow method: ability to catch exceptions thrown by throwException
    (breaking change for recordings)
  - Exceptions in forked flows are now handled (breaking change)
  - Options made a bit more effective
  - Raw SQL now is printed into recordings (breaking change for recordings)

## [1.8.0.0] - 2020-04-03
* EulerHS 1.8.0.0: new features and fixes.
  - Redis cluster support (switched to other hedis fork).
  - Framework and Logger performance tuned.

## [1.7.0.0] - 2020-03-30
* EulerHS 1.7.0.0: new features.
  - Granular DB errors added (breaking change)
  - Test framework added

## [1.6.0.0] - 2020-03-27
* EulerHS 1.6.0.0: a significant update with new features and fixes.
  - beam-mysql updated: temporary tables fix, autocommit fix, bytestrings encoding fix
  - MySQL transactions bug fixed
  - New feature: awaiting for results from forked flows added
  - runIO' with description added
  - KV DB hardcorded DB name fixed (breaking change)
  - More documentation on SQL subsystem usage added (see README.md)

## [1.4.0.0] - 2020-03-12
* Euler-hs 1.4.0.0:
  - Performance analysed and tuned
    N.B. Async logger has a lazy mem leak. Will be fixed in the next version.
    Use sync logger for now.
  - Pub-Sub mechanism added
  - Beam-MySQL updated (support of the `Day` type added)
  - Small fixes and additions

## [1.3.0.0] - 2020-02-17
- Breaking changes. Options reworked.
  Interface modified (Typeable instance required now).
  Fixed a bug with identical encoding of different keys.
- Added GHC options: Wcompat Widentities fhide-source-paths
- Added wrappers for kvdb actions
- Added callServantApi request | response logging
- Changed `Serializable` instances for ByteStrings
- Fixed recording forked flow with exception
- Fixed throwException method entry record/replay

## [1.2.0.0] - 2019-12-20

- Added beam-mysql support of insertReturning
- Added beam-mysql BIT / TEXT problem solved
- Added transactions for SQL DB subsystem made right
- Added `getOrInit` methods for SQL DB & KV DB
- Improvements in business logic and tests

## [1.1.0.0] - 2019-12-16

- Added InitKVDB method
- Added Shared connections
- Added `getOrInitKVDB` method

## [1.0.0.0] - 2019-12-09

- Added shared connections `getSqlDBConnection` and  `getOrInitSqlConn` for SQL DB

## [0.9.0.0] - 2019-12-02

### 2019-12

- Added branching policy guide

### 2019-11

- Added metrics base
- Added strictness annotations to reduce memory consumption under heavy logger load
- Tune sqlite to wait for resource in case of concurrent access
- Added Load tester base app
- Agnostic ART record|replay Optimized
- Added ART player/recorder & art.sh script
- Added CODESTYLE guide
- Added redis tests mocked with ART
- Added descriptions. Imports refactored
- Rollback to the BeamRunner instances
- Fixed bugs with DB initialization

### 2019-10

- Add art in KVDB layer
- Fixed fork recording race
- Added own Serializable class
- Added JSONEx as union of Serializable ans To/FromJSON
- Art tests improved
- Added art entries and art support for new methods.
- Added connection pool
- Added deinitSqlDBConn method
- Fork Flow logic fixes and improvements
- Add art rec/rep for InitSqlDBConnection method
- Added ART
- Added mysql tests
- Postgres support added
- MySQL support added
- Introducing kvdb mock
- SQL DB support reworked and improved
- Added KVDB transactions support

### 2019-09

- Added kvdb methods wrappers
- Added KVDB sub-language
- Test DB clearing added
- Tests for SqlDB added
- Run db in transaction
- Added custom ConnectInfo
- SQL DB Support Beam
- Added SQL DB Support
- Added mocked values in test interpreter
- Added additional language methods `GenerateGUID`, `runSysCmd`, `forkFlow`, `throwException`
- Test app implemented
- Added logger logic, language and interpreter
- Added `runIO` and get/setOptions tests
- ServantClient types added
- `runIO`, `getOption`, `setOption` methods added
- `interpretFlowMethodL` and `runFlowMethodL` methods added
- Added basic project layout, test app layout, initial CallAPI facilities, sample API
