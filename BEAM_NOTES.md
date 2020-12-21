# What you should be aware of when using beam

`euler-hs` uses `beam` library for database access.

### SqlBool vs Bool (can have critical impact on performance)

This is especially important for users of *MySQL*.
On Postgres the generated queries are somewhat better.

For filtering where we have NULLABLE values we have to use different comparison operators, which operate on `SqlBool` type, for example instead of this:

```haskell
import qualified Database.Beam as B
import qualified Database.Beam.Query as B

...

predicate orderId merchantId =
  (orderId    ==. B.just_ (B.val_ orderId')) &&.
  (merchantId ==. B.just_ (B.val_ merchantId'))

findOrder orderId merchantId
  = findRow
  $ B.select
  $ B.limit_ 1
  $ B.filter_ (predicate orderId merchantId)
  $ B.all_ DBSchema.orders
```

do this:

```haskell
predicate orderId merchantId =
  (orderId    ==?. B.just_ (B.val_ orderId')) &&?.   -- Notice a change here
  (merchantId ==?. B.just_ (B.val_ merchantId'))     -- NULLABLE values

findOrder orderId merchantId
  = findRow
  $ B.select
  $ B.limit_ 1
  $ B.filter_' (predicate orderId merchantId)        -- Notice a change here: filter_'
  $ B.all_ DBSchema.orders
```

Notice there are not only operators like `&&?.` and `==?`, but also a `filter_'` which operates on `SqlBools`
if you need to mix `Bool` and `SqlBool` in a single predicate there are functions to allow this:

[Beam's SqlBool](https://hackage.haskell.org/package/beam-core-0.8.0.0/docs/Database-Beam-Query.html#t:SqlBool)

Without that the generated SQL code consists of many `CASE WHEN ...` expressions which destroy performance.

Beam docs have an overview of that:

[Beam's SQL-like comparisons](https://github.com/haskell-beam/beam/blob/master/docs/user-guide/expressions.md#sql-like-comparisons)

With updates it's less straightforward as there is no `update'` function which supports `SqlBool`
What we can do is change:

```haskell
    updateRows
    $ B.update someDBTable someUpdateAssigments
      (\someEntity -> (someEntity ^. _id) ==. (B.just_ $ B.val_ neededEntityId))
```

like this:

```haskell
    updateRows
    $ B.update' someDBTable someUpdateAssigments
      (\someEntity -> B.unknownAs_ False $ (someEntity ^. _id) ==?. (B.just_ $ B.val_ neededEntityId))
```

If you do a change like this:

```haskell
      (\oRef -> B.unknownAs_ False $ (oRef ^. _id) ==?. (B.just_ $ B.val_ orderRefId))
```

The generated SQL code will contain `UPDATE ... WHERE (some_id == 1) IS TRUE` which will also destroy performance.
Similar with `save` function which uses similar query internally. You can use `save''` instead.

We've added an `update'` and `save'` functions to the [fork of beam](https://github.com/juspay/beam/tree/upstream-master).

An upstream PR [is merged](https://github.com/haskell-beam/beam/pull/464).

### Transactions

`beam` does not provide support for transactions.

The framework has two methods:
- `runDB` - this one doesn't use transactions,
- `runTransaction` - treats the `SQLDB` monad as a transactional scope.

### Connection pool management

`beam` does not provide a connection management, if you want to use a connection pool you have the following options:

The framework has support of pools out of box. Consider to pass an appropriate config to `initSqlDBConnection`.

There is currently a possible issue with connection pools on Postgres which is under investigation.

### MySQL specific

There are several problems with the `beam-mysql` backend described below which are currently fixed or mitigated in [our fork](https://github.com/juspay/beam-mysql) of `beam-mysql`.

We'll attempt to upstream some of the changes, though `beam-mysql` repo remains in the account of original author of beam.

#### Insert returning

As MySQL does not support `INSERT ... RETURNING` `beam-mysql` does not implement `insertReturning*` functions.
The `juspay/beam-mysql` adds an `INSERT RETURNING` emulation and support for these, but uses a temporary table.

So we added an additional function `insertRowReturningMySQL` which can insert a single row and return its value together with the generated autoincrement id column.

#### Changes to parsing

`juspay/beam-mysql` also allows to parse `Blob` as `Text`; `Bit(1)` as `Bool`.
