# What you should be aware of when using beam

`euler-hs` uses `beam` library for database access.

The `beam` library has some properties making it difficult in usage. Apart of that, the original version of the library and of its satellites seems a bit unpolished, so we had to rework parts of it. We also found some performance related issues you might want to know.

### Performance of SqlBool vs Bool

Mostly affected: *MySQL*
Less affected: *Postgres*, *SQLite*

`beam` has two different comparison operators for `NULLABLE` values. One that works with `Bool` and another for `SqlBool`. They behave quite differently, and as it's an implementation detail of the corresponding libraries, there almost no control on the generated SQL. So you might want to compare the behavior of the two for your `SELECT` and `UPDATE` queries.

*Selecting rows*

```
Bool operator:   (&&.)
Filter function: filter_

SqlBool operator: (&&?.)
Filter function: filter_'
```

Both may be used in `WHERE` clauses and for building a boolean filtering expression. However, the `(&&.)` operator seem to generate a very inefficient code for the `MySql` backend. It's better avoided. The first one may cause a generation of heavy `CASE WHEN ...` expressions which destroy the performance.

First version:

```haskell
import qualified Database.Beam as B
import qualified Database.Beam.Query as B

-- Data structure used in the sample:
data OrderT f = Order
  { id         :: B.C f (Maybe Int)
  , merchantId :: B.C f (Maybe Text)              -- nullable field
  , orderId    :: B.C f (Maybe Text)              -- nullable field
  }
  deriving (Generic, B.Beamable)


predicate orderId merchantId =
  (orderId    ==. B.just_ (B.val_ orderId')) &&.  -- Bool-related operator
  (merchantId ==. B.just_ (B.val_ merchantId'))

findOrder orderId merchantId
  = findRow
  $ B.select
  $ B.limit_ 1
  $ B.filter_ (predicate orderId merchantId)       -- filter_ function
  $ B.all_ DBSchema.orders
```

Second version:

```haskell
predicate orderId merchantId =
  (orderId    ==?. B.just_ (B.val_ orderId')) &&?.   -- SqlBool-related operator
  (merchantId ==?. B.just_ (B.val_ merchantId'))

findOrder orderId merchantId
  = findRow
  $ B.select
  $ B.limit_ 1
  $ B.filter_' (predicate orderId merchantId)        -- filter_' function
  $ B.all_ DBSchema.orders
```

Mix `Bool` and `SqlBool` is also possible. Check out these documents to know more:

* [Beam's SqlBool](https://hackage.haskell.org/package/beam-core-0.8.0.0/docs/Database-Beam-Query.html#t:SqlBool)
* [Beam's SQL-like comparisons](https://github.com/haskell-beam/beam/blob/master/docs/user-guide/expressions.md#sql-like-comparisons)

*Updating rows*

Initially, `beam` didn't have the `update` version for `SqlBool`.
We've added `update'` and `save'` functions to the [fork of beam](https://github.com/juspay/beam/tree/upstream-master). An upstream PR [is merged](https://github.com/haskell-beam/beam/pull/464).

Additionally, the problem may be workarounded. Rewrite such code:

```haskell
updateSomething neededEntityId someUpdateAssigments
  = updateRows
  $ B.update someDBTable someUpdateAssigments
      (\someEntity -> (someEntity ^. _id) ==. (B.just_ $ B.val_ neededEntityId))
```

like this:

```haskell
updateSomething neededEntityId someUpdateAssigments
  = updateRows
  $ B.update' someDBTable someUpdateAssigments
      (\someEntity -> B.unknownAs_ False $ (someEntity ^. _id) ==?. (B.just_ $ B.val_ neededEntityId))
```

If you do a change like this:

```haskell
(\someEntity -> B.unknownAs_ False $
   (someEntity ^. _id) ==?.               -- notice the operator
   (B.just_ $ B.val_ neededEntityId))
```

The generated SQL code will contain `UPDATE ... WHERE (some_id == 1) IS TRUE` which will also destroy the performance.

### Transactions

`beam` does not provide support for transactions, but the `EulerHS` provides own way to have transactions for SQL DBs (when the SQL backends allow that).

There are two methods for running a query script:
- `runDB`          - run a query without a transaction;
- `runTransaction` - treat the `SqlDB` monad as a transactional scope and run it within a transaction.

### Connectivity & pool management

`beam` does not provide a connectivity management but the framework supports several ways of handling connections and pools. See [README](./README.md) for more info.

### beam-mysql rework

We paid a certain cost to fix some issues we think we've encountered in the `beam-mysql` library. We've reworked the library in [our fork](https://github.com/juspay/beam-mysql). We'll attempt to upstream some of the changes, though `beam-mysql` repo remains in the account of original author of beam.

### MySQL lacks INSERT RETURNING

As MySQL does not support `INSERT ... RETURNING`, the `beam-mysql` library does not provide `insertReturning*` functions. We've added this functionality [in our fork](https://github.com/juspay/beam-mysql) although the implementation is not efficient and should be considered a workaround.

The framework also provides a specific method `insertRowReturningMySQL` which can insert a single row and return its value together with the generated autoincrement id column. Should be used with care.

### More value conversions enabled in beam-mysql

[In our fork](https://github.com/juspay/beam-mysql), additional parsers are implemented:
* `Blob` as `Text`
* `Bit(1)` as `Bool`
