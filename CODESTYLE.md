
# Haskell Style Guide

This is a Style Guide for all the projects within the euler-hs repository.
This is a nice-to-follow guide, still it's not pushed hardly to avoid
unnecessary conflicts and friction. You should decide on your own
about how to structure your code. Still remember that your code will be
read by other people.

### Line Length

- Recommended line length is *80 characters*.
- Maximum line length is *120 characters*.

Comments should be wrapped accordingly.
There should be no trailing whitespace anywhere in your code.
This makes git diff output look ugly and causes spurious merge
conflicts.

### Indentation

- Tabs are illegal. Use spaces for indenting.
- Recommended to use *2 spaces* for each indentation level.
- Allowed to use *4 spaces* if this seems more appropriate
  for certain situations.

The only exception is for code blocks inside
a definition, which should be indented with *4 spaces*. Indent the
`where` keyword two spaces to set it apart from the rest of the code
and indent the definitions in a `where` clause 2 spaces. Guards are
usually indented 2 spaces. Some examples:

```Haskell
sayHello :: IO ()
sayHello = do
    name <- getLine
    putStrLn $ greeting name
  where
    greeting name = "Hello, " ++ name ++ "!"

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs
```

As a general rule, *indentation of a line should not depend on the
length of any identifier in preceding lines*, only on layout
constraints.

### Hanging let-expressions

Avoid hanging let-expressions in do blocks. This makes it harder to refactor
and brings more unstructured noise into the code, as well as makes resolving
conflicts in git harder.

Avoid:

```
someFunc = do
  let a = calcA 10
      b = calcB 20
  pure (a + b)

someFunc = do
  let
     a = calcA 10
     b = calcB 20
  pure (a + b)
```
Prefer:
```
someFunc = do
  let a = calcA 10
  let b = calcB 20
  pure (a + b)
```

### Blank Lines

Two blank lines between top-level definitions, and a line of 78 "-" characters
to delineate top-level definitions from each other. No blank lines between
type signatures and function definitions. Add one blank line between functions
in a type class instance declaration if the functions bodies are large.
Use your judgment.

### Whitespace

Surround binary operators with a single space on either side. Use your
better judgement for the insertion of spaces around arithmetic
operators but always be consistent about whitespace on either side of
a binary operator. Don't insert a space after a lambda. Don't insert
space inside a parenthesized expression.

### Applications

If an application must spawn multiple lines to fit within the maximum
line length, then write one argument on each line following the head,
indented by one level:

```Haskell
let'sFold xs = do
    foldr
      (\x y -> ...)
      Nothing
      xs
```

But consider naming the arguments instead to avoid multi-line
expressions.

### Data Declarations

Align the constructors in a data type definition. Example:

```Haskell
data Tree a
  = Branch !a !(Tree a) !(Tree a)
  | Leaf
```

Format records as follows:

```Haskell
data Person = Person
  { firstName :: !String  -- ^ First name
  , lastName  :: !String  -- ^ Last name
  , age       :: !Int     -- ^ Age
  } deriving (Eq, Show)

data Person
  = Person
      { firstName :: !String  -- ^ First name
      , lastName  :: !String  -- ^ Last name
      , age       :: !Int     -- ^ Age
      } deriving (Eq, Show)
  | SomeThing
      { foo :: !String  -- ^ Foo
      , bar :: !String  -- ^ Bar
      , baz :: !String  -- ^ Baz
      }
```

### List Declarations

Align the elements in the list. Example:
```Haskell
exceptions =
    [ InvalidStatusCode
    , MissingContentHeader
    , InternalServerError
    ]
```

### Pragmas

Put pragmas immediately following the function they apply to. Example:

```Haskell
id :: a -> a
id x = x
{-# INLINE id #-}
```

In the case of data type definitions you must put the pragma before
the type it applies to. Example:

```Haskell
data Array e = Array {-# UNPACK #-} !Int !ByteArray
```

`LANGUAGE` pragmas should enable a single language extension per line,
for easy addition and deletion.

### Hanging Lambdas

You may or may not indent the code following a "hanging" lambda. Use
your judgement. Some examples:

```Haskell
bar :: IO ()
bar =
    forM_ [1, 2, 3] $ \n -> do
      putStrLn "Here comes a number!"
      print n

foo :: IO ()
foo =
    alloca 10 $ \a ->
    alloca 20 $ \b ->
    cFunction a b
```

### Export Lists

Format export lists as follows:

```Haskell
module Data.Set
  ( -- * The @Set@ type
    Set
  , empty
  , singleton
    -- * Querying
  , member
  ) where
```

### If-then-else expressions

Generally, guards should be preferred over if-then-else expressions,
where possible. if-then-else is preferred to case analysis on
a boolean. Short cases should usually be put on a single line (when
line length allows it).

When writing non-monadic code (i.e. when not using `do`) and guards
can't be used, you can align if-then-else expressions like you would
normal expressions:

```Haskell
foo =
    if ...
      then ...
      else ...
```

In monadic code, so long as you use the Haskell 2010 dialect and
above, you can use the same alignment as above. A different alignment
rule for monadic code is no longer necessary.

### Case expressions

The alternatives in a case expression can be indented using either of
the two following styles:

```Haskell
foobar = case something of
    Nothing -> foo
    Just j  -> bar
```

or as

```Haskell
foobar =
    case something of
      Nothing -> foo
      Just j  -> bar
```
Right-hand side of `->` can be moved to the next line

```Haskell
foobar =
    case something of
      Nothing -> foo
      Just j  ->
        someLongLongLong $ bar $ foo $ baz [1,2,3]
```

`->` should be aligned


## Alignment in do, case, guards
`->` in case should be aligned

```Haskell
...
    case foo of
        Bar    -> ()
        BazBaz -> ()
```


`<-` in do should be aligned
```Haskell
...
    do
        a   <- Bar
        bar <- BarBazaz
        pure ()
```


`=` in guards should be aligned

```Haskell
foo a b
    | a < b     = ()
    | otherwise = ()
```

## Imports

Imports should be listed in alphabetical order with no intervening
blank lines, except for any explicit `Prelude` import, which must
always come first. The reason for this exception is that some redundant
import warnings are sensitive to the order of the `Prelude` import.

Always use explicit import lists or `qualified` imports for standard
and third party libraries. This makes the code more robust against
changes in these libraries. Exception: the Prelude.

Use your judgement when it comes to local application/library specific
imports. On the one hand, they make for more maintainable code because
identifiers that are removed from the imported module will be caught
early and identifiers added to the imported module do not risk
clashing with local identifiers. They also serve as documentation as
to which parts of a module are actually required.

However, explicit import lists are also much more verbose, and slow
down development. Moreover, in a collaborative environment, explicit
import lists can cause spurious conflicts, since two otherwise
unrelated changes to a file may both require changes to the same
import list.

The qualifier for well known modules, such as `ByteString` can be
shortened further, eg `BS`. But in general, prefer descriptive
qualifiers rather than one letter ones. For example

```Haskell
import qualified Data.Map as Map  -- good
import qualified Data.Map as M    -- not so good
```

## Comments

Comments should be placed immediately *before* the line(s) of code
they pertain to.

### End-of-line comments

End-of-line comments should be separated from code by at least two
spaces.

### Top-Level Definitions

Comment every top-level function (particularly exported functions),
and provide a type signature; use Haddock syntax in the comments.
Comment every exported data type. Function example:

```Haskell
-- | Send a message on a socket. The socket must be in a connected
-- state. Returns the number of bytes sent. Applications are
-- responsible for ensuring that all data has been sent.
send
  :: Socket     -- ^ Connected socket
  -> ByteString -- ^ Data to send
  -> IO Int     -- ^ Bytes sent
```

For functions the documentation should give enough information to
apply the function without looking at the function's definition.

Record example:
```Haskell
-- | Bla bla bla.
data Person = Person
  { age  :: !Int    -- ^ Age
  , name :: !String -- ^ First name
  }
```
For fields that require longer comments format them like so:

```Haskell
data Record = Record
  { -- | This is a very very very long comment that is split over
    -- multiple lines.
    field1 :: !Text
    -- | This is a second very very very long comment that is split
    -- over multiple lines.
  , field2 :: !Int
  }
```

## Naming

Use camel-case when naming values (`fooBar`) and data
types (`FooBar`).

For readability reasons, don't capitalize all letters when using an
abbreviation. For example, write `HttpServer` instead of `HTTPServer`.
Exception: Two letter abbreviations, e.g. `IO`.

### Records

Where appropriate, add an unabbreviated prefix to the name of record
fields. Example:

```Haskell
-- | Messages consist of their typeRep fingerprint and their encoding
data Message = Message
  { messageFingerprint :: !Fingerprint
  , messageEncoding    :: !BSL.ByteString
  }
```

This is not necessary for modules that export only one data type *and*
are meant to be imported qualified.

### Modules

Use singular when naming modules e.g. use `Data.Map` and
`Data.ByteString.Internal` instead of `Data.Maps` and
`Data.ByteString.Internals`.

### Lenses

**N.B. This section is under construction.**

Where appropriate, define lenses for all fields in a record. Use the
`_` prefix to name fields. When using the [lens package][lens], use
`makeClassy` [where possible][lens-makeClassy] to generate lenses
rather than `makeLenses`. This is to make it easy to export all lenses
for a record all at once.

```Haskell
module Person
  ( Person(..)
  , HasPerson(..)
  ) where

data Person = Person
  { _firstName :: !String  -- ^ First name
  , _lastName  :: !String  -- ^ Last name
  , _age       :: !Int     -- ^ Age
  } deriving (Eq, Show)

makeClassy ''Person
```

For consistency, if a record has lenses defined, always use the lens
to get or set the field of a record (rather than `_fieldName`). Field
names should only be used to initialize the record.

[lens]: http://hackage.haskell.org/package/lens
[lens-makeClassy]: http://hackage.haskell.org/package/lens-4.3.3/docs/Control-Lens-TH.html#v:makeClassy
