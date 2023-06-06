{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EulerHS.Extra.Validation
  (
    -- * Extra Validation
    Transform(..)
  , mkValidator
  , mkTransformer
  , Transformer
  , Validator
  , V
  , Errors
  , module X
  , withField'
  , withField
  , runParser
  , extractJust
  , extractMaybeWithDefault
  , guarded
  , decode
  , insideJust
  , parValidate
  ) where

import qualified Data.Text as T
import           Data.Validation as X
import           EulerHS.Prelude hiding (or, pred)
import qualified Data.Generics.Product.Fields as GL (HasField', getField)
import           GHC.Records.Compat (HasField, getField)
import           GHC.TypeLits (KnownSymbol, Symbol)
import qualified Prelude as P
import           Type.Reflection (typeRep)

type Ctx = Text
type Errors = [Text]
type V a = Validation [Text] a

-- TODO: Looks like Profunctor. Does it hold laws?
-- | Represents Transformer from one type to another.

--- | This class represents transformation abilities between types.
class Transform a b where
  transform :: a -> Validation Errors b

type Transformer a b = a -> ReaderT Ctx (Either Errors) b

-- | Represents the value parameter validator.
type Validator a = Transformer a a

-- | Takes error message and predicate and return validation function
mkValidator :: Text -> (t -> Bool) -> Validator t
mkValidator err pred v = ReaderT (\ctx -> if not $ pred v
  then Left [ctx <> " " <> err]
  else pure v)

guarded :: Text -> Bool -> ReaderT Ctx (Either Errors) ()
guarded err pred | pred      = ReaderT (\_   -> pure ())
                 | otherwise = ReaderT (\ctx -> Left [ctx <> " " <> err])

-- | Trying to decode Text to target type
decode :: forall t . (Read t) => Transformer Text t
decode v = ReaderT (\ctx -> case readMaybe $ toString v of
  Just x -> Right x
  _      -> Left ["Can't decode " <> v <> " from field " <> ctx])

mkTransformer :: Text -> (a -> Maybe b) -> Transformer a b
mkTransformer err f v = ReaderT (\ctx -> case f v of
  Just x  -> Right x
  Nothing -> Left [ctx <> " " <> err])

insideJust :: Transformer a b -> Transformer (Maybe a) (Maybe b)
insideJust _ Nothing    = pure Nothing
insideJust val (Just a) = Just <$> val a

-- | Trying to extract the argument from Maybe type
--   if value is Nothing then raise Failure
extractJust :: Transformer (Maybe a) a
extractJust r = ReaderT (\ctx -> maybe (Left [ctx <> " not present"]) Right r)

extractMaybeWithDefault :: a -> Transformer (Maybe a) a
extractMaybeWithDefault d r = ReaderT (\_ -> maybe (Right d) Right r)

-- | Extract value and run validators on it
-- New One
withField'
  :: forall (f :: Symbol) v r a
   . (HasField f r v, KnownSymbol f)
  => r -> Transformer v a -> Validation Errors a
withField' rec pav = fromEither $ runReaderT (pav $ getField @f rec) $ fieldName_ @f

-- Old One with generic-lens
withField
  :: forall (f :: Symbol) v r a
   . (GL.HasField' f r v, KnownSymbol f)
  => r -> Transformer v a -> Validation Errors a
withField rec pav = fromEither $ runReaderT (pav $ GL.getField @f rec) $ fieldName_ @f

-- | Run parser
runParser
  :: forall a
   . ReaderT Ctx (Either Errors) a
  -> Text
  -> Validation Errors a
runParser p msg = fromEither $ runReaderT p msg

-- | Return text representation of constructors of a given type
-- showConstructors :: forall t . Data t => Text
-- showConstructors = T.pack $ show $ getConstructors @t

-- | Return list with constructors of a given type
-- getConstructors :: forall t . Data t => [Constr]
-- getConstructors = dataTypeConstrs (dataTypeOf (undefined :: t))

-- | Return given 'Symbol' as 'Text'
-- >>> fieldName @"userId"
-- "userId"
fieldName_ :: forall (f :: Symbol) . KnownSymbol f => Text
fieldName_ = T.pack $ filter (/='"') $ P.show $ typeRep @f

parValidate :: [Validator a] -> Validator a
parValidate vals a = ReaderT (\ctx -> toEither $ foldr (*>) (pure a) $ fmap (mapper ctx) vals)
  where
    mapper ctx val = fromEither $ runReaderT (val a) ctx
