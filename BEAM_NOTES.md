# What you should be aware of when using beam
`euler-hs` uses `beam` library for database access.

### SqlBool vs Bool (can have critical impact on performance)
This is especially important for users of *MySQL*.
On Postgres the generated queries are somewhat better.

For filtering where we have NULLABLE values we have to use different comparison operators, which operate on SqlBool type,
for example instead of:
```haskell
      let predicate DB.OrderReference {orderId, merchantId} =
            (orderId    ==. B.just_ (B.val_ orderId')) &&.
            (merchantId ==. B.just_ (B.val_ merchantId'))
      findRow
        $ B.select
        $ B.limit_ 1
        $ B.filter_ predicate
        $ B.all_ (DB.order_reference DB.eulerDBSchema)
```
do a
```haskell
      let predicate DB.OrderReference {orderId, merchantId} =
            (orderId    ==?. B.just_ (B.val_ orderId')) &&?.
            (merchantId ==?. B.just_ (B.val_ merchantId'))
      findRow
        $ B.select
        $ B.limit_ 1
        $ B.filter_' predicate
        $ B.all_ (DB.order_reference DB.eulerDBSchema)
```
notice there are not only operators like &&?. and ==?. but also a filter_' which operates on SqlBools
if you need to mix Bool and SqlBool in a single predicate there are functions to allow this https://hackage.haskell.org/package/beam-core-0.8.0.0/docs/Database-Beam-Query.html#t:SqlBool

Without that the generated SQL code consists of many `CASE WHEN ...` expressions which destroy performance.

Beam docs have an overview of that:
https://github.com/tathougies/beam/blob/master/docs/user-guide/expressions.md#sql-like-comparisons

With updates it's less straightforward as there is no update' function which supports SqlBool
What we can do is change:
```haskell
    updateRows
    $ B.update (DB.order_reference DB.eulerDBSchema)
           (\oRef -> DBO.billingAddressId oRef <-. B.val_ mbBillingAddrId)
        <> (\oRef -> DBO.shippingAddressId oRef <-. B.val_ mbShippingAddrId)
      )
      (\oRef -> (oRef ^. _id) ==. (B.just_ $ B.val_ orderRefId))
```
to
```haskell
    updateRows
    $ B.update' (DB.order_reference DB.eulerDBSchema)
      (    (\oRef -> DBO.billingAddressId oRef <-. B.val_ mbBillingAddrId)
        <> (\oRef -> DBO.shippingAddressId oRef <-. B.val_ mbShippingAddrId)
      )
      (\oRef -> B.unknownAs_ False $ (oRef ^. _id) ==?. (B.just_ $ B.val_ orderRefId))
```      
If you do a change like this:
```haskell
      (\oRef -> B.unknownAs_ False $ (oRef ^. _id) ==?. (B.just_ $ B.val_ orderRefId))
```
The generated SQL code will contain `UPDATE ... WHERE (some_id == 1) IS TRUE` which will also destroy performance.
Similar with `save` function which uses similar query internally. You can use `save''` instead.

We've added an `update'` and `save'` functions to the fork of beam: https://github.com/juspay/beam/tree/upstream-master.
An unstream PR is waiting to be merged: https://github.com/haskell-beam/beam/pull/464

### Transactions
`beam` does not provide support for transactions.
You can rely on `euler-hs` library, but it currently (the problem is still present in `EulerHS-1.10.0.0`) wraps too much queries in a transaction, which can negatively affect performance, depending on your beam backend and database used.
Also it currently does not provide a fine-grained control over transactions.
The issue with too much wrapping is under investigation and we may add a `withTransaction` or similar function to specify where transactions should be used in the future.

If you want more control over the transactions you can implement a custom function and use it.
`beam` docs mention importing and wrapping transaction-related functions from `postgres-simple` and using them as an example.
For a similar example see: https://github.com/3noch/beam-postgres-transaction-monad

If you go with the custom option you might want to implement it in the way that is compatible with using inside the `Flow` monad and possibly a `SqlDBMethod` language. Generally it's better to ask `euler-hs` team to implement this or help them with this.

### Connection pool management
`beam` does not provide a connection management, if you want to use a connection pool you have the following options:
1. rely on `euler-hs` and don't forget to set `connectionsPerStripe` in the `PoolConfig` to a sensible value (by default it's 1 (one)).
2. implement a custom connection pooling by relying on the current `euler-hs` implementation. If you do this also try to make it compatible with `Flow` (easier than the transaction option). You can ask about it in the *#euler-haskell* slack channel.

There is currently (still present in `EulerHS-1.10.0.0`) a possible issue with connection pools on postgres which is under investigation.

### MySQL specific
There are several problems with the `beam-mysql` backend described below which are currently fixed or mitigated in our fork of `beam-mysql`: https://github.com/juspay/beam-mysql

We'll attempt to upstream some of the changes, though `beam-mysql` repo remins in the account of original author of beam.
Author did not respond yet about moving beam-mysql to the https://github.com/haskell-beam organisation.

#### insert returning
As MySQL does not support `INSERT ... RETURNING` `beam-mysql` does not implement `insertReturning*` functions.
The `juspay/beam-mysql` adds an `INSERT RETURNING` emulation and support for these, but uses a temporary table.

This turned out to be a problem during the staging tests of `euler-api-order` as

So we added an additional function `insertRowReturningMySQL` which can insert a single row and return its value together with the generated autoincrement id column.

#### changes to parsing
`juspay/beam-mysql` also allows to parse `Blob` as `Text`; `Bit(1)` as `Bool`.

### Defining table types
The order of fields is important to `beam` so you either have to match the fields of your type with the database or use a `tableModification` to explicitly match fields (this option is preferable).
For an example see the `euler-db` repo: https://bitbucket.org/juspay/euler-db
