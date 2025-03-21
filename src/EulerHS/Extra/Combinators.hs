{-# LANGUAGE TypeOperators #-}

module EulerHS.Extra.Combinators
( toDomain
, toDomainAll
, throwOnDBError
, throwOnParseError
, extractDBResult
)
where

import           Control.Exception (Exception)
import           Control.Monad (void)
import           Data.Text (Text, pack)
import           Juspay.Extra.Parsing (Parsed (Failed, Result))
import           EulerHS.Language (MonadFlow, logErrorWithCategory, throwException)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           Named (NamedF (Arg), type (:!))
import           Prelude hiding (id)
import           EulerHS.Logger.Types (ErrorL(..))

{- |
Parse a loaded DB result and throw errors.

  * In case of a database error, throws `Euler.Types.Errors.DatabaseError`.
  * In case of a domain type parsing error, throws
    `Euler.Types.Errors.DomainTypeParseError`
-}
toDomain
  :: (HasCallStack, MonadFlow m, Show err)
  => Either err a
  -> (a -> Parsed b)
  -> "function_name" :! Text -- Name of query function for error log
  -> "parser_name" :! Text
  -> m b
toDomain eitherRes parser functionName parserName = do
  res <- extractDBResult eitherRes functionName
  throwOnParseError (parser res) parserName

{- | For traversable responses.
Maybe a, [a], etc.
-}
toDomainAll
  :: (HasCallStack, MonadFlow m, Traversable f, Show err)
  => Either err (f a)
  -> (a -> Parsed b)
  -> "function_name" :! Text
  -> "parser_name" :! Text
  -> m (f b)
toDomainAll eitherRes parser functionName parserName = do
  res <- extractDBResult eitherRes functionName
  throwOnParseError (traverse parser res) parserName

{- | What the name says silly! -}
throwOnDBError
  :: (HasCallStack, MonadFlow m, Show err)
  => Either err ()
  -> "function_name" :! Text
  -> m ()
throwOnDBError res = void . extractDBResult res

throwOnParseError
  :: (HasCallStack, MonadFlow m)
  => Parsed a
  -> "parser_name" :! Text
  -> m a
throwOnParseError parseResult (Arg parserName) = case parseResult of
  Result c -> pure c
  Failed e -> do
    let errMsg = pack . show $ e
    logErrorWithCategory @Text ("Domain type parse error in " <> parserName) errMsg $ ErrorL Nothing "PARSE_ERROR" errMsg
    throwException $ DomainTypeParseError errMsg

-----------------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------------

extractDBResult
  :: (HasCallStack, MonadFlow m, Show err)
  => Either err a
  -> "function_name" :! Text
  -> m a
extractDBResult eitherResult (Arg functionName) = case eitherResult of
  Right res -> pure res
  Left err -> do
    let errMsg = pack . show $ err
    logErrorWithCategory @Text ("Database error from function " <> functionName) errMsg $ ErrorL Nothing "DATABASE_ERROR" errMsg
    throwException $ DatabaseError errMsg

-----------------------------------------------------------------------------
-- Errors
-----------------------------------------------------------------------------

newtype DatabaseError = DatabaseError
   { errorMessage :: Text
   }
  deriving (Eq, Show, Generic)

instance Exception DatabaseError

newtype DomainTypeParseError = DomainTypeParseError
   { errorMessage :: Text
   }
  deriving (Eq, Show, Generic)

instance Exception DomainTypeParseError
