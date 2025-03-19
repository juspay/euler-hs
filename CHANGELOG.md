# Changelog for euler-hs

## [9.11.5]
* Add `callBrahmaWithRespCodeCheck` to detect no recording found case
* Modify replay handling of `RandomBytes`, `GetOption` & `RunIOWithART`

## [9.11.4]
* Fixed replay logic for runIOWithART

## [9.11.3]
* Compressing encrypted logs

## [9.11.2]
* Remove `x-art-recording` header in external outgoing API calls

## [9.11.1]
* Added `getErrorReasonWithRequest` for more detailed log in default case

## [9.11.0]
* Add ART replay prefix for tenant agnostic functions
* Add ART replay prefix for all DB queries executed via `runQuery`, `runQueryMySQL`, `runQueryWithConn` functions
* Add `ArtReplayPrefix` option entity
* Add ART replay prefix for all tenant supported redis queries

## [9.10.0]
* Added runIOWithART and depricated runIO for most modules

## [9.9.1]
* Bumped euler-nix-common to automatically extract metadata and store in nix drv

## [9.9.0]
* Moved AES to euler-haskell-common and added test cases for ENV's, added nix support for metadata generation

## [9.8.9]
* Added disable db fallback env

# [9.8.9]
* Filtering headers for external OUTGOING_API calls

## [9.8.8]
* Bump `euler-haskell-common`

## [9.8.7]
* Bump `euler-events-hs`

## [9.8.6]
* Added `x-request-id` to request from loggerContext or `x-euler-session-id` if not already present

## [9.8.5]
* Added rHincrBy and hincrByTx for redis

## [9.8.4]
* Added `x-load-testing` header logic in callservantapi

## [9.8.3]
- ForkFlow GUID Fix
- ForkFlow logs to debug

## [9.8.2]
* Added `x-load-testing` header logic

## [9.8.1]
* Bumped spider and euler-db-dumps

## [9.8.0]
* Pushing encrypted OUTGOING_API logs

## [9.7.10]
* Add & fix ART callstack error logs during ART replay for easier debugging
* Add ART_V2 handling for `rHmsetB` function
* Replace L.getCurrentTimeUTC with library one to avoid redundant calls to mock server for timestamp in ART recording types creation

## [9.7.9]
* Added single resource destroy and retry logic in runDbWithRetry

## [9.7.8]
* Added Recording support for getRandomBytes

## [9.7.7]
* Use `spider` from `euler-nix-common`

## [9.7.6]
* Bumped spider repo commit for keyLookupTracker plugin usage.

## [9.7.5]
* Added hmset to redis

## [9.7.4]
* Fix baseUrl recording for ServantRequest by adding path from baseUrl
* Fix query param recording for Servant Request in CallAPIEntry
* Fix `getResponseStatus` field population when converting Servant response to HTTPResponse
* Fix logging for 5XX cases of API call to mock server
* Change model name in ART recording to use actual DB table name instead of KVConnector table name
* Add missing V2 recording logic for `runWithArtCount` function & fix replay
* Add log for mock server API call failed

## [9.7.3]
Added metric to track first DB query when we setting partition key

## [9.7.2]
* Added Unified Looper Function (For Merging all table specific loopers to single one..)

## [9.7.1]
* Art incr record and replay added

## [9.7.0]
* Bump `sheriff` plugin to detect infinite recursion
* Bump api-contract & fieldInspector plugin

## [9.6.3]
* Use connection manager from flowRt in api call to ART mock server
* Add cpu & mem vitals in outgoing calls to ART mock server
* Add proper logging of outgoing call to ART mock server
* Add separate connection manager name for ART

## [9.6.2]
* Add latency in outgoing calls to mock server for ART
* Add ART_V2 recording for failure cases of servant APIs
* Add full URL in servant recordings & replay object for ART
* Update handling of Servant API call failure cases for ART

## [9.6.1]
* EUL-14500 add organization dek support

## [9.6.0]
*  Add envs for configuring replay of `CONFIG` & `COMMON_CONFIG` tables
*  Add env for configuring replay of NON-KV redis
*  Add `hostname` in `IncomingApiEntry`
*  Add `defaultBlacklistedTagsForRecordingGlobalOptions`
*  Disable mocking if sessionId not present for ART_V2
*  Always mock joins during ART replay irrespective of tables
*  Refactor additional headers logic in outgoing API calls to add it in replay as well
*  Fix aeson encoding/decoding of bytestring & wrappers using conversion to/from string

## [9.5.1]
* added ttl buffer of 1 hour to Skey 
* Upgraded ttl Reset logic 
* added Reset ttl for sKey in one Redis call

## [9.5.0]
* Added changes tenant database migration

## [9.4.0]
* Upgraded to Aeson v2
* Upgraded from GHC-8.10.7 to GHC-9.2.8
* Changed HashMap -> KeyMap
* Upgraded template haskell and other GHC-9.2.8 compatible libraries

## [9.3.13]
* Adding ART status to incoming api log

## [9.3.12]
* Added `disableFallBackToCommonDb` to MeshConfig
* Disable fallback to common db based on meshConfig

## [9.3.11]
* Fix config kv redis limits

## [9.3.10]
* Fix getDictonary function

## [9.3.9]
*  Removed runIOs for which we had FlowMethod
*  Added apiTag in in CallApi and replay logic for api calls

## [9.3.8]
* Added remaining recording logic
* Added url based decider for api call for mocking
* Add push to kafka for V2 recording in interpreter
* Added replay logic for V2 recording in interpreter 

## [9.3.7]
*  Added replay logic for V2 recording in interpreter

## [9.3.6]
* Added remaining recording logic
* Added url based decider for api call for mocking

## [9.3.5]
*  Fix headers not recorded for `CallHTTP` in ART
*  Add `x-art-recording` header for all internal API calls
*  Fix art recording logic for forkflow
*  Add `ToJSON` & `FromJSON` instance for `LogMaskingConfig` & `MaskKeyType` types
*  Add env based recording of timestamp
*  Add env based recording of setOptions
*  Add env based recording via ToJSON & FromJSON instance for global options
*  Fix recording and replay logic for IMC

## [9.3.4]
*  Add IMC recording for V2 for find case
*  Add DecodeKMS module in euler-hs
*  Add decodeKMS on `ART_AES_KEY` and `ART_AES_IV`

## [9.3.3]
*  Changed default value of `REDIS_ZSTD_DICT_S3_FOLDER_PATH`

## [9.3.2]
*  `Bump euler-events-hs`

## [9.3.1]
*  Changed default value of `REDIS_ZSTD_DICT_S3_BUCKET` 

## [9.3.0]
* Added redis zstd dict compression support

## [9.2.0]
*  Add push to kafka for V2 recording in interpreter
*  Add support for disabling tracker table recording in ART
*  Add push to kafka for V2 recording
*  Add Option for art recording cutover
*  Add env vars for kafka props

## [9.1.1]
* Env refactor fix

## [9.1.0]
* EUL-13893 added tenant_id_v2 in `TenantConfig` for multi-tenancy

## [8.1.0]
*  EUL-13893 added tenant_id_v2 in `TenantConfig` for multi-tenancy

## [8.0.4]
*  EUL-14023 fix env reads

## [8.0.3]
*  EUL-13978 fix CPU utilisation spike

## [8.0.2]
*  Bumped euler-haskell-common

## [8.0.1]
*  Added tag to randomRIO function

## [8.0.0] - 2024-09-11
* Add env framework
* Refactor for env Framework

## [7.0.18] - 2024-09-10

- Adding x-euler-session-id in outgoing request

## [7.0.17] - 2024-09-09
* Add support for passing identifier tag to uuid generating function
* Add sheriff check to not use generateGUID function
* Add helper functions `generateGUIDWithTag` and flow function `generateRawGUID` to generate V4 UUID

## [7.0.16] - 2024-08-28
* Skip log encryption for JSON request or response body with incorrect content-type.

## [7.0.15] - 2024-08-28
* Recording support for countRows function.

## [7.0.14] - 2024-08-28
* Jenkins: Use NixCI to build all flake outputs and use in house jenkins in pipeline.

## [7.0.13] - 2024-08-27
* Pointing `spider` repo to ghc-9.2.8 branch.
* Pointed import input to ghc8 version.

## [7.0.12] - 2024-08-26
*  Added support for LRANGE

## [7.0.11] - 2024-08-21
*  Added support for HLEN

## [7.0.10]
*  Recording support for RandomRIO

## [7.0.9]
*  corrected overriding of x-tenant-host in callAPI

## [7.0.8] - 2024-08-06
*  Added recency check before DB calls in KV layer

## [7.0.7] - 2024-08-05
*  EUL-13390 Fixed IMC invalidation with cache key prefix

## [7.0.6] - 2024-07-22
*  Added support for mock conf for DB

## [7.0.5] - 2024-07-29
*  Added recording support for findAll

## [7.0.4] -2024-07-29
* EUL-12957: Added support for incrby, decrby, incrybyfloat & expireAt redis functions

## [7.0.3] - 2024-07-22
*  Added support for recordings in TenantRedisLayer

## [7.0.2] - 2024-07-18
*  EUL-13043: Made changes to log plain text or html req/res body in the outgoing API.
*  Logged req/res body will be AES encrypted.

## [7.0.1] -2024-07-15
* EUL-12957: Added support for tenantDEK based encryption for default pii enc tables.

## [7.0.0] - 2024-07-03
*  EUL-12929: Added support for timestamp recording

## [6.2.3] - 2024-07-10
*  Use `main` branch of `spider`
*  EUL-13070: Add `Loggable` class to allow individual type level logging filtering in code
*  EUL-13069: Fix regex based XML masking function and add unit tests for it

## [6.2.2] - 2024-07-03
*  EUL-12929: Adding LLEN as a feature for redist list

## [6.2.1] - 2024-07-03
*  EUL-12915: Added spider plugin and refactored code to fix spider plugin errors
*  EUL-12915: Added instances for some data types

## [6.2.0] - 2024-07-03
*  EUL-12898: Removing mysql query instances support

## [6.2.0] - 2024-07-04
* GHC upgrade (8.10.7 -> 9.2.8)
* Following major dependencies are upgraded:
 - template-haskell (2.16.0.0 -> 2.18.0.0)
 - aeson (1.5.6.0 -> 2.1.2.1)

## [6.1.0] - 2024-07-02
*  EUL-12952: Added redis prefix migration logic

## [6.0.4] - 2024-06-22
*  EUL-12707: ART recording bugfixes

## [6.0.3] - 2024-06-21
*  EUL-12365: Added more functions in TenantRedisLayer

## [6.0.2] - 2024-06-20
*  EUL-12707: update ART recording filepath

## [6.0.1] - 2024-06-19
*  EUL-12610: Fixed getPrimaryIdFromRedis to support recording

## [6.0.0] - 2024-06-18
* EUL-12365: Added tenant redis layer, which will prepend prefix based on tenant

## [5.1.2] - 2024-06-14
* EUL-12559: Added hdel, hgetall and hsetnx for redis

## [5.1.1] - 2024-06-13
*  EUL-12411: Add sheriff plugin with `show` rule

## [5.1.0] - 2024-06-12
*  EUL-12707 updated isArtRecEnabled to use optionLocal

## [5.0.8] - 2024-05-20
*  EUL-10695: Bump pr for euler-haskell-common

## [5.0.7] - 2024-06-04
*  EUL-12610: Exported digest auth function

## [5.0.6] - 2024-06-04
*  EUL-12491: Added type for api path prefix support

## [5.0.5] - 2024-05-31
*  EUL-12515: Added Digest Auth Support

## [5.0.4] - 2024-05-20
*  EUL-12515: Adding HOSTNAME in file name for ART recordings

## [5.0.3] - 2024-05-20
*  EUL-12090: Adding RPOP as a feature for redist list

## [5.0.2] - 2024-05-10
*  EUL-12304 adding key to rGet redis_exception logline

## [5.0.1] - 2024-05-06
*  Added XmlToJson - Fixed issue for converting child object

## [5.0.0] - 2024-04-03
*  Added schema support and made it mandatory

## [4.10.7] - 2024-04-19
* Export logEncryptedInfo and logEncryptedError 

## [4.10.6] - 2024-04-19
* Added logic to unmask values for keys which contain only digits.
* Need to be built with cabal flag unmask-numeric-keys to enable this feature.

## [4.10.5] - 2024-04-11
*  Added encrypted logging Support

## [4.10.4] - 2024-04-10
*  Add response body logging for CallHTTP Left cases

## [4.10.3] - 2024-04-10
*  Add apiTag in ART Recording

## [4.10.2] - 2024-04-09
* Added logging support for XML requests and responses in outgoing logs.
* Changed default logging version as V2.

## [4.10.1] - 2024-04-04
* Add orphan instances for HttpException and other types
* Change logging for OUTGOING_API network error cases to log in JSON format instead of `show`

## [4.10.0] - 2024-04-03
* Adding KMS and GC Time metrics in Logs

## [4.9.7] - 2024-04-02
* Add incrTx and decrTx fns

## [4.9.6] - 2024-03-22
* Fixed etl stream push in find recache

## [4.9.5] - 2024-03-19
* Added `TenantName` which will be added in localOptions for tenant Visibility.
* Added logging of responseBody in case of serverError for ART

## [4.9.4] - 2024-03-20
* Change precision for secondary redis

## [4.9.3] - 2024-03-15
* Support for pushing to sql write logs stream when KV is enabled

## [4.9.2] - 2024-03-15
* Support for pushing to sql write logs stream

## [4.9.1] - 2024-03-12
* Bump hedis for NOACK fix

## [4.9.0] - 2024-03-04
* Added Default instances for PII type class
* Added Changes to enforce PII

## [4.8.15] - 2024-03-06
* Fix: Add `x-merchant-id` header only for internal outgoing api calls

## [4.8.14] - 2024-03-01
* add support for moodifyOptionLocal

## [4.8.13] - 2024-02-27
* remove `euler.yaml` and adopt haskell-flake

## [4.8.12] - 2024-02-22
* Added tracking ids as header in outgoing internal API calls
* Changed DB Errors log action to "ERROR_TRACKING"
* Added logging of responsecode and responseMessage in case of serverError for ART

## [4.8.11] - 2024-02-21
* Added x-merchant-id for internal API calls

## [4.8.10] - 2024-02-19
* Use redis increment id in clean_db when kv is disabled

## [4.8.9] - 2024-02-09
* Added recache support in kv find

## [4.8.8] -2023-02-08
* Adding helper functions for date computations

## [4.8.7] - 2024-02-06
* Added merchantId in metadata.

## [4.8.6] - 2024-01-30
* Bug Fix/Changed splitOn from breakOn

## [4.8.5] - 2024-01-29
* Add error type for truncation 

## [4.8.4] - 2024-01-11
* toLower bug fix for getWhereClause

## [4.8.3] - 2024-01-11
* Bug fix for updateAllWithKVConnector

## [4.8.2] - 2024-01-10
* Bumping hedis client to handle cluster autoscalling

## [4.8.1] - 2024-01-10
* Added support for default PII enabled tables

## [4.8.0] - 2024-01-04
* Pushed changes to support a separate database for tracker tables.
* Made changes to not push metric if source of the query result is IMC.
* ENVs added: `IS_LOG_AND_INC_IMC_METRIC_ENABLED`
* ENVs removed: `IS_LOG_AND_INC_KV_METRIC_ENABLED`

## [4.7.1] - 2023-12-14
* Added DB Retry Logic
* Created a function RunDBWithRetry that will be consumed if we want to retry DB query again. This is done to reduce 5XX when down scaling
* ENVs added: `DB_RETRY_THREAD_DELAY_TIME`
* ENVs added: `DB_RETRY_ATTEMPTS`

## [4.7.0] - 2023-12-14
* Improved the performance of IMC key generation and mkOption
* Config APIs now use `Array Wor8` instead of `Text` as the key
* ENVs added: `IS_LOG_AND_INC_KV_METRIC_ENABLED`

## [4.6.6] - 2023-12-21
*  Fixed DB logs

## [4.6.5] - 2023-12-21
*  updated aeson to get the new error response

## [4.6.4] - 2023-12-20
*  Made KV entry as nested object with dbname

## [4.6.3] - 2023-12-18
*  Handle escape chars in query

## [4.6.2] - 2023-12-18
* Removed runDB, runDBWithConn from exports.
* Moved runQuery, runMySQLQuery, runQueryMySQL, runQueryWithConn to SqlDbHelper module.
* Added euler-repo flag to check whether module should be exposed or not.

## [4.6.1] - 2023-12-15
* Bumping haskell-sequelize, beam and beam-mysql

## [4.6.0] - 2023-11-29
* Added payload compression support
* ENV's added: COMPRESSION_METHOD , COMPRESSION_LEVEL , COMPRESSION_WINDOW_BITS , COMPRESSION_MEMORY_LEVEL , COMPRESSION_STRATEGY , COMPRESSION_BUFFER_SIZE , COMPRESSION_DICTONARY , CHUNK_SIZE_FOR_STREAM , HARD_KILL_COMPRESSION , WHITELISTED_CLUSTER_HOST_FOR_COMPRESSION , BLACKLISTED_API_TAGS_FOR_COMPRESSION

## [4.5.12] - 2023-12-4
* Added euler latency overhead support 

## [4.5.11] - 2023-11-29
* Custom query generator bugfix

## [4.5.10] - 2023-11-27
* Added attic and nixv support in Jenkins

## [4.5.9] - 2023-11-27
* Added cabal flag for fast builds in local by tweeking ghc options

## [4.5.8] - 2023-11-26
* Changed mesh config type for partition

## [4.5.7] - 2023-11-24
* Bumped euler-build to make compilation mac compatible

## [4.5.6] - 2023-11-17
* Added tracker table creates counter support for ETL accuracy checks

## [4.5.4] - 2023-11-08
* Adding support for segregating internal and external api calls.

## [4.5.3] - 2023-10-18
* Added `callBrahmaReplayA` to replay call Servant API

## [4.5.2] - 2023-11-07
* Adding logContextMap to get logger context

## [4.5.1] - 2023-10-16
* Adding support for decr Redis functionality.

## [4.5.0] - 2023-09-22
* Adding support for doing FIND queries without beam

## [4.4.1] - 2023-10-05
* Replaced replaced dbTableSettings with columnized for caching

## [4.4.0] - 2023-09-28
* Added primary keys and secondary keys in logging structure for drainer service logs

## [4.3.2] - 2023-09-28
* bug Fix: Redis metric latency log check

## [4.3.1] - 2023-09-27
* Fixed UpdateWithKvConnector 

## [4.3.0] - 2023-09-26
* Added support to log IOLatencyMetric 

## [4.2.0] - 2023-09-20
* Art replay fixes

## [4.1.0] - 2023-09-20
* Added Dynamic Version Support for PII

## [4.0.8] - 2023-09-22
* Added type for TenantId

## [4.0.7] - 2023-09-21
* bug Fix: Added check before imcDelete push

## [4.0.6] - 2023-09-13
* bug Fix: Fixed double encode error
## [4.0.5] - 2023-09-12
* Fixed update skey logic
* Update only modified sKeys

## [4.0.4] - 2023-09-11
* bug Fix: changed ReconRedis Config

## [4.0.3] - 2023-09-11
* Added support for findAll IMC

## [4.0.2] - 2023-09-11
* Added secondary redis support

## [4.0.1] - 2023-09-07
* bug fixed : passing PII key config in modifyOneKV

## [4.0.0] - 2023-09-07
* Cached columnized part in getInMemCacheKeyFromWhereClause and getFieldsAndValuesFromClause

## [3.3.2.1] - 2023-08-18
* Added req_type for callServant and added outgoing api tag in options

## [3.3.2.0] - 2023-08-18
* Remove Latency log for FIND call

## [3.3.2.0] - 2023-08-18
* Added update returning support for MySQL

## [3.3.1.0] - 2023-08-17
* Added Redis Based ETL support

## [3.3.0.1] - 2023-08-10
* Fix to support add new secondary which is superset of existing secondary key

## [3.3.0.0] - 2023-08-09
* Added Snowflake Support

## [3.2.0.0] - 2023-08-07
* Made transformSetClause impure to fetch random iv
## [3.1.1.0] - 2023-07-31
* Restructerd IMC flow for improving perf. 
## [3.1.0.4] - 2023-08-03
* Format Error logs same way as 2xx responses
* Fixed show on Text while logging http method
## [3.1.0.3] - 2023-08-01
* Added request parameters in error handler log of CallServant implementation

## [3.1.0.2] - 2023-07-31
* Updating the toJSON instance for Lbinary String type in HTTPRequest

## [3.1.0.1] - 2023-07-22
* Removing CPU latency computes

## [3.1.0.0] - 2023-07-24
* Use `unordered-containers 0.2.13.0` to fix the Tagged encode issue from prefix version

## [3.0.0.0] - 2023-07-24
* GHC upgrade (8.8.4 -> 8.10.7)
* TH function `TupE` takes `[Maybe Exp]` instead of `Exp` as first argument.
* Tagged encode using aeson has changed the order from `{tag:..., contents:...}` to `{contents:..., tag:...}`

## [2.16.0.0] - 2023-07-14
* Added ART support

## [2.15.0.1] - 2023-06-26

* Add PII encryption support
* Add new file for PII encryption - `PiiEncryption`
* Modify DB and KV functions to add PII encryption and decryption

## [2.15.0.0] - 2023-06-20

* added changes for separate domain database
* moved kv test cases and eulerDb file to euler-db
## [2.14.0.1] - 2023-06-02

* reverted hedis changes to cluster branch

## [2.14.0.0] - 2023-06-02

* Added one parameter for deducing the errors in API calls in `callAPI` and `callHTTP`
* You have to pass the error function with type as `(HttpResponse -> Maybe ErrorInfo)` in `callHTTP` and `(ByteString -> Maybe ErrorInfo)` in `callAPI`. If you do not want to pass anything pass (const Nothing) as argument but this is not recommended.
* Added Error Tracking fields in response headers for application use
  Ex : callAPI baseurl api_tag (const Nothing) (EulerClient)
  
## [2.13.0.6] - 2023-06-23

* Added `XTenantHostHeader` to keep track of original host to maintain stickiness
## [2.13.0.5] - 2023-06-12

* bumped hedis to use latest master having timeout connection fix

## [2.13.0.4] - 2023-04-25

* define new log structure
* added `masterLogger` to use versionized logs

## [2.13.0.3] - 2023-03-10

* Add exception handling in async logging in `loggerWorker`

## [2.13.0.2] - 2023-02-15

* Add logging for Exceptions - `logException`

## [2.13.0.1] - 2022-08-03

* Add Configuration for Additional Postgres Database Connection and creating Option Entity for the same - `EulerPsqlDbAdditionalCfg`

## [2.13.0.0] - 2022-04-22

* Update `record-dot-preprocessor` and `juspay-extra` versions
* Import `formUrlEncode` from `juspay-extra`.
* Update `stack's lts` to `16.31`

## [2.12.0.0] - 2022-04-12

* Add logAPI flag to allow applications to control the logging of external API calls. It is a BREAKING change! From now on you have to provide an extra flag - `logAPI` when building logger runtime.

## [2.11.0.0] - 2022-02-01

* Add handler to count log entries by severity. It is a BREAKING change! From now on you have to provide a `SeverityCounterHandle` when building logger runtime. Please refer to this PR as an example https://bitbucket.org/juspay/euler-api-customer/pull-requests/249

## [2.10.0.0] - 2022-01-25

* !!!BREAKING CHANGES!!! Remove non-total functions from re-export.
  Highly recommended to not use them in your code.
  However, in case they are indispensable to code just import from standart Prelude.

## [2.9.4.3] - 2022-01-24

* Labels for threads.

## [2.9.4.2] - 2021-11-25

* Add aeson option to extra stuff. It moved from webservice and euler-db
* Move out non-total json modifiers to euler-db. Just to hide from exteral usage.
* Update `beam-mysql` and `haskell-sequelize` versions.

## [2.9.4.1] - 2021-11-23

* Add `withJSONBody` to make building of JSON HTTP requests easier

## [2.9.4.0] - 2021-11-23

* Moved all non Euler-specific and non `Flow`-dependent code into brand-new `juspay-exra` library and made it a dependency
* Move common Euler-specific utility code from `euler-webservice` and `euler-db` into `Extra` folder, including `EulerHS.Extra.Aeson`, `EulerHS.Extra.Combinators`, `EulerHS.Extra.Time`)

## [2.9.3.1] - 2021-11-09

* Optimize exception passing to db connection pools

## [2.9.3.0] - 2021-10-18

* Treat `Text` and `String` tags in logging entries without show'ing them

## [2.9.2.0] - 2021-09-22

* Add logger functions `logM`  , `log` , `logV` , `logInfoM` , `logInfoV`
  , `logErrorM` , `logErrorV` , `logDebugM` , `logDebugV` , `logWarningM`
  ,  ability to log jsonable values as message.
* Adjust log messages from `callHTTP` and `callAPI` methods

## [2.9.1.2] - 2021-09-22

* Use `Data.ByteString.Builder` instead of deprecated `Data.ByteString.Lazy.Builder`
  in `EulerHS.HttpAPI`

## [2.9.1.1] - 2021-09-03

* Keep previous (as in 2.9.0.2) callHTTPWithCert interface

## [2.9.1.0] - 2021-08-12

* HTTP ManagerSettings builders
* Dynamic HTTP managers
* Unify callHTTP* and callAPI* actions

## [2.9.0.2] - 2021-08-10

* Add `withRunFlow`

## [2.9.0.1] - 2021-08-10

* Fix adding mask config to `LoggerRuntime`

## [2.9.0.0] - 2021-08-04

* Added `generic-lens`, reverted `withField` method to old declaration, changed New `withField` to `withField'`, added `checkFailedWithLog` from webservice.

## [2.8.0.0] - 2021-08-03

* Update `beam-mysql` to 1.3.0.1 version
* Update Servant-* deps and record-dot-preprocessor version
* Refactor and fix stack config

## [2.7.0.0] - 2021-07-27

* Expand `HTTPMethod` with `Trace`, `Connect`, `Options`, `Patch` methods

## [2.6.0.2] - 2021-07-27

* Add `rSetexBulk`, `rSetexBulkB`

## [2.6.0.1] - 2021-07-07

* Remove `MonadFlow m` constraint from `withModifiedRuntime` method

## [2.6.0.0] - 2021-07-06

* Expand `CallHTTP` with `Maybe ManagerSelector`
* Add `callHTTPWithManager`

## [2.5.0.5] - 2021-07-06

* Add dump erroneous value on decoding failure in `rGet`

## [2.5.0.4] - 2021-07-06

* Fix resource cleanup on exception inside `withResource`

## [2.5.0.3] - 2021-07-06

* Add a test-case for callHTTPWithCert

## [2.5.0.2] - 2021-07-06

* Adjust `runRedis` errror message when connection not found

## [2.5.0.1] - 2021-06-21

* Fix uncaught exceptions from runClientM in callAPI

## [2.5.0.0] - 2021-04-29

* Remove `Mocked` constructors everywhere.

## [2.4.0.0] - 2021-04-28

* Remove `Serializable` module.

## [2.3.0.0] - 2021-04-26

* Remove all `lens` and `generic-lens` usage and exports.

## [2.2.0.1] - 2021-04-26

* Add a range of additional functions to `EulerHS.Extra.Language`, originally
  from `euler-webservice`.

## [2.2.0.0] - 2021-04-23

* Major version bump for `beam-mysql`.

## [2.1.1.0] - 2021-04-20

* Use RDP plugin

## [2.1.0.0] - 2021-04-12

* `ManagerSelector` is now a newtype.
* Add `CertificateRegistrationError` data type.
* Add 'withSelfSignedFlowRuntime` function for registering self-signed
  certificates in a `FlowRuntime`.

## [2.0.4.4] - 2021-04-13

* Rename `changeLoggerContext` to `withLoggerContext`
* Add `updateLoggerContext`

## [2.0.4.3] - 2021-04-12

* Add `changeLoggerContext` to EulerHS.Extra.Language

## [2.0.4.2] - 2021-04-07

* Fixed a bug with MVar blocking when disposing `Async` logger.

## [2.0.4.1] - 2021-03-03

* Remove waste entries.
* Add `rSetOpts` and `rSetOptsB`

## [2.0.4] - 2021-02-25

* Add `ShouldLogSQL(SafelyOmitSqlLogs, UnsafeLogSQL_DO_NOT_USE_IN_PRODUCTION)` to `LoggerConfig`

## [2.0.3.2] - 2021-02-12

* Add `rawRequest` to KVDB API, plus implementation for KVDB interpreter.

## [2.0.3.1] - 2021-01-28

* Add `withModifiedRuntime` method to `MonadFlow`, plus implementation for
  `Flow` interpreter.

## [2.0.3.0] - 2021-01-20

* Allow passing of a character set to a MySQL connection, along with a type
  specifying the character sets supported currently

## [1.10.0.0] - 2020-06-16
* Euler-HS 1.10.0.0: a significant update with new features and fixes.
  - New authentication service in euler-api-order
  - Rework of repository structure, each library has its own repo now.
  - Added an `eulerBuild` --  a collection of nix functions for building and developing euler-based projects.
  - See `eulerBuild` documentation [BUILD.md](BUILD.md) for upgrade and usage instructions.
  - Compatibility with GHC 8.8.
  - Updated stack resolver to 15.15.
  - Updated pinned nixpkgs to `nixos-unstable` at `0f5ce2fac0c726036ca69a5524c59a49e2973dd4` (~ 18.05.2020)
  - Added CI builds.
  - See notes on how to upgrade in [UPGRADE.md](UPGRADE.md).
  - New policy for releases -- create a new tag with the same name for all core libraries,
    i.e. `euler-hs`, `euler-db`, `euler-types`, `euler-webservice` have a tag `EulerHS-1.10.0.0` for this release.
### `euler-hs`
  - Added `run[Update/Delete]ReturningList` for Postgres.
  - Added `delOption`.
  - Added `runUntracedIO` for reading/writing sensitive data.
  - Log format temporarily changed to partly mimic euler-ps.
  - Added untyped HTTP calls to `Flow`.
  - Lots of various fixes not listed here.
  - Added `insertRowReturningMySql` function which does not use temporary tables internally.
  - Beware of `SqlBool` vs `Bool` when writing `beam` queries, and other gotchas: [see BEAM_NOTES.md](BEAM_NOTES.md),
    read this if you use database at all via `euler-hs`.
    Some of this can have a critical effect on performance, especially on MySQL.
### `euler-db`
  - Contains DB-facing types.
  - https://bitbucket.org/juspay/euler-db
### `euler-types`
  - Contains domain types.
  - https://bitbucket.org/juspay/euler-types
### `euler-webservice`
  - Contains utilities for writing servant-based web services.
  - https://bitbucket.org/juspay/euler-webservice

## [2.0.0.0] - 2020-07-01

* Use `beam-mysql` based on `mysql-haskell` instead of `mysql`

## [1.9.5.0] - 2020-04-13
* Euler-HS 1.9.5.0: fixes
  - Async logger mem consumption fixed.
  - Integration tests with MySQL disabled
  - Improved documentation [see README.md](README.md)
* Euler-backend 0.9.0.0:
  - Order Status API
  - Integration tests disabled

## [1.9.0.0] - 2020-04-13
* Euler-HS 1.9.0.0: a significant update with new features and fixes.
  - RunSafeFlow method: ability to catch exceptions thrown by throwException
    (breaking change for recordings)
  - Exceptions in forked flows are now handled (breaking change)
  - Options made a bit more effective
  - Raw SQL now is printed into recordings (breaking change for recordings)
* Euler-backend 0.9.0.0:
  - Configs updated

## [1.8.0.0] - 2020-04-03
* Euler-HS 1.8.0.0: new features and fixes.
  - Redis cluster support (switched to other hedis fork).
  - Framework and Logger performance tuned.
* Euler-backend 0.8.0.0:
  - DB model and storage types separated into own repo.

## [1.7.0.0] - 2020-03-30
* Euler-HS 1.7.0.0: new features.
  - Granular DB errors added (breaking change)
  - Test framework added

## [1.6.0.0] - 2020-03-27
* Euler-HS 1.6.0.0: a significant update with new features and fixes.
  - beam-mysql updated: temporary tables fix, autocommit fix, bytestrings encoding fix
  - MySQL transactions bug fixed
  - New feature: awaiting for results from forked flows added
  - runIO' with description added
  - KV DB hardcorded DB name fixed (breaking change)
  - More documentation on SQL subsystem usage added (see README.md)

* Euler-Backend: small updates
  - AWS-KMS encryption functionality added
  - Environment variables support added

## [1.5.0.0] - 2020-03-13
* Euler-Backend 0.5.1.0:
  - Added MerchantGatewayAccount Storage type. MerchantGatewayAccount added to DB scheme
  - Added EulerAccountDetails with HasSqlValueSyntax and FromBackEndRow instances
  - stack.yaml fixed to avoid multiple rebuilds of euler-backend code

## [1.4.0.0] - 2020-03-12
* Euler-hs 1.4.0.0:
  - Performance analysed and tuned
    N.B. Async logger has a lazy mem leak. Will be fixed in the next version.
    Use sync logger for now.
  - Pub-Sub mechanism added
  - Beam-MySQL updated (support of the `Day` type added)
  - Small fixes and additions
* Euler-Backend 0.5.0.0:
  - DB types added.
  - Order Create completely implemented
  - Integration tests & ART tests added
  - Customer & Card API types, domain types and validators added

## [1.3.0.0] - 2020-02-17
- Euler-hs: (breaking changes) Options reworked.
  Interface modified (Typeable instance required now).
  Fixed a bug with identical encoding of different keys.
- Euler-hs: added GHC options: Wcompat Widentities fhide-source-paths
- Euler-hs: added wrappers for kvdb actions
- Euler-hs: added callServantApi request | response logging
- Euler-hs: changed `Serializable` instances for ByteStrings
- Euler-hs: fixed recording forked flow with exception
- Euler-hs: fixed throwException method entry record/replay
- Improvements in business logic and tests
- console: removed from repo
- dashboard: removed from repo

## [1.2.0.0] - 2019-12-20

### Added

- beam-mysql support of insertReturning
- beam-mysql BIT / TEXT problem solved
- Transactions for SQL DB subsystem made right
- `getOrInit` methods for SQL DB & KV DB
- Improvements in business logic and tests
- Flex casing

## [1.1.0.0] - 2019-12-16

### Added

- InitKVDB method
- Shared connections
- `getOrInitKVDB` method

## [1.0.0.0] - 2019-12-09

### Added

- Shared connections `getSqlDBConnection` and  `getOrInitSqlConn` for SQL DB

## [0.9.0.0] - 2019-12-02

### 2019-12

- Added branching policy guide

### 2019-11

- Euler-hs: Added metrics base
- Euler-hs: Added strictness annotations to reduce memory consumption under heavy logger load
- Euler-hs: Tune sqlite to wait for resource in case of concurrent access
- Euler-hs: Added Load tester base app
- Euler-hs: Agnostic ART record|replay Optimized
- Euler-hs: Added ART player/recorder & art.sh script
- CI: Implement continuous deployment of console
- Added CODESTYLE guide
- console: Add Docker image building stage to Nix
- Euler-hs: Added redis tests mocked with ART
- Euler-hs: Added descriptions. Imports refactored
- Euler-backend: Validation refactored and cleaned up
- Euler-hs: Rollback to the BeamRunner instances
- Euler-hs: Fixed bugs with DB initialization

### 2019-10

- Nix: Added deriviations for broken xml libs
- Euler-backend: Added App, Server & Some Types
- Euler-hs: Add art in KVDB layer
- Euler-hs: Fixed fork recording race
- Euler-hs: Added own Serializable class
- Euler-hs: Added JSONEx as union of Serializable ans To/FromJSON
- Euler-hs: Art tests improved
- Euler-hs: Added art entries and art support for new methods.
- Euler-hs: Added connection pool
- Euler-hs: Added deinitSqlDBConn method
- CI: build: Enable tests for euler-hs
- dashboard: Add tests for SQL generation
- Euler-hs: Fork Flow logic fixes and improvements
- Euler-hs: Add art rec/rep for InitSqlDBConnection method
- Euler-hs: Added ART
- Euler-hs: Added mysql tests
- Euler-hs: Postgres support added
- console: Add setup instructions for running console
- console: Make test more robust and not dependent on external data
- dashboard: Allow providing BigQuery credentials to the backend
- Console: Conditionally parse date time fields as strings
- Euler-hs: MySQL support added
- Euler-hs: Introducing kvdb mock
- CI: Add a Jenkinsfile to enable CI
- Nix: Add basic infrastructure for using Nix
- Euler-hs: SQL DB support reworked and improved
- dashboard: Deal with null values from the database
- Euler-hs: Added KVDB transactions support
- dashboard: Reduce dependencies to the minimal set
- Console: Add a way to allow cross-origin requests for development
- Euler-backend: Added fail test cases
- Euler-backend: Added infix versions of transformations
- Euler-backend: Added transformation & validation for txns

### 2019-09

- Euler-hs: Added kvdb methods wrappers
- Euler-hs: Added KVDB sub-language
- Euler-hs: Test DB clearing added
- Euler-hs: Tests for SqlDB added
- Euler-hs: Run db in transaction
- Euler-hs: Added custom ConnectInfo (isomorphic to Beam.Postgres.ConnectInfo)
- Euler-hs: SQL DB Support Beam
- Euler-hs: Added SQL DB Support
- Console: Add a way to generate PureScript types for API
- Euler-hs: Added mocked values in test interpreter
- Console: Implement a BigQuery backend to run queries
- dashboard: Supports multiple select fields and validation for same
- dashboard: Validate filters against their field types
- dashboard: Add Query -> SQL generation for BigQuery
- dashboard: Add a field name for Query intervals
- dashboard: Add a mechanism to validate queries
- Console: Add dashboard query API
- Euler-hs: Added additional language methods `GenerateGUID`, `runSysCmd`, `forkFlow`, `throwException`
- Euler-hs: Test app implemented
- Euler-hs: Added logger logic, language and interpreter
- dashboard: Add basic query and query result types
- Euler-hs: Added `runIO` and get/setOptions tests
- Euler-hs: ServantClient types added
- Euler-hs: `runIO`, `getOption`, `setOption` methods added
- Credit platform: Added basic app facilities
- Credit platform: Types for FINVU APIs added
- Euler-hs: `interpretFlowMethodL` and `runFlowMethodL` methods added
- Euler-hs: Added basic project layout, test app layout, initial CallAPI facilities, sample API
