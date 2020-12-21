{-# OPTIONS -fno-warn-deprecations #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module EulerHS.Extra.AltValidation
  (
    -- * Extra Validation
    Transform(..)
  , mkValidator
  , mkCustomValidator
  , mkTransformer
  , withCustomError
  , Transformer
  , Validator
  , V
  , Errors
  , VErrorPayload(..)
  , module X
  , withField
  , runParser
  , extractJust
  , extractMaybeWithDefault
  , guarded
  , guardedCustom
  , decode
  , decodeCustom
  , insideJust
  , parValidate
  ) where

import           EulerHS.Prelude hiding (or, pred)
import qualified Prelude as P

import           Data.Either.Extra (mapLeft)
import           Data.Generics.Product.Fields
import qualified Data.Text as T
import           Data.Validation
import           Data.Validation as X
import           GHC.TypeLits
import           Type.Reflection


data VErrorPayload = VErrorPayload
  { status        :: Text
  , status_id     :: Maybe Int
  , error_code    :: Maybe Text
  , error_message :: Maybe Text
  , error_field   :: Maybe Text
  }
  deriving (Eq, Show, Generic)

validationError :: VErrorPayload
validationError = VErrorPayload
  { status = "invalid_request_error"
  , status_id = Nothing
  , error_code = Just "INVALID_REQUEST"
  , error_message = Nothing
  , error_field = Nothing
  }

-- | Context contains the name of validated field
type Ctx = Text

type Errors = [VErrorPayload]
type V a = Validation [VErrorPayload] a

-- | Represents Transformer from one type to another.

--- | This class represents transformation abilities between types.
class Transform a b where
  transform :: a -> Validation Errors b

type Transformer a b = a -> ReaderT Ctx (Either Errors) b

-- | Represents the value parameter validator.
type Validator a = Transformer a a

-- | Takes error message and predicate and returns validation function
-- using default 'VErrorPayload'
mkValidator :: Text -> (t -> Bool) -> Validator t
mkValidator msg pred v = ReaderT (\ctx -> if not $ pred v
  then Left [validationError { error_message =  Just msg, error_field = Just ctx }]
  else pure v)

-- | Make a validator using a particular error message, original
-- errors are ignored
withCustomError :: VErrorPayload -> Validator a -> Validator a
withCustomError err v a = ReaderT (\ctx -> mapLeft (\_ -> [err]) $ runReaderT (v a) ctx)

-- | Takes error message and predicate and returns validation function
-- using custom error
mkCustomValidator :: VErrorPayload -> Text -> (t -> Bool) -> Validator t
mkCustomValidator err msg pred v = ReaderT (\ctx -> if not $ pred v
  then Left [err { error_message = Just msg, error_field = Just ctx }]
  else pure v)

-- | Guards computations by a validation rule
guarded :: Text -> Bool -> ReaderT Ctx (Either Errors) ()
guarded msg pred | pred      = ReaderT (\_   -> pure ())
                 | otherwise = ReaderT (\ctx -> Left [validationError {error_message = Just msg, error_field = Just ctx }])

-- | Guards computations by a validation rule with a custom error
guardedCustom :: VErrorPayload -> Bool -> ReaderT Ctx (Either Errors) ()
guardedCustom err pred | pred      = ReaderT (\_   -> pure ())
                 | otherwise = ReaderT (\ctx -> Left [err {error_field = Just ctx }])

-- | Trying to decode 'Text' into a target type
decode :: forall t . (Read t) => Transformer Text t
decode v = ReaderT (\ctx -> case (readMaybe $ toString v) of
  Just x -> Right x
  _      -> Left [ validationError { error_message = Just ("Can't decode value: " <> v)
                       , error_field = Just ctx}])

-- | Trying to decode 'Text' into a target type, use custom error
decodeCustom :: forall t . (Read t) => VErrorPayload -> Transformer Text t
decodeCustom err v = ReaderT (\_ -> case (readMaybe $ toString v) of
  Just x -> Right x
  _      -> Left [ err ])

mkTransformer :: VErrorPayload -> (a -> Maybe b) -> Transformer a b
mkTransformer err f v = ReaderT (\_ -> case f v of
  Just x  -> Right x
  Nothing -> Left [ err ])

insideJust :: Transformer a b -> Transformer (Maybe a) (Maybe b)
insideJust _ Nothing    = pure Nothing
insideJust val (Just a) = Just <$> val a

-- | Trying to extract the argument from Maybe type
--   if value is Nothing then raise Failure
extractJust :: Transformer (Maybe a) a
extractJust r = ReaderT (\ctx -> maybe (Left [err ctx]) Right r)
  where
    err ctx = validationError
      { status = "Bad Request"
      , error_message = Just "Mandatory fields are missing"
      , error_code = Just "Mandatory fields are missing"
      , error_field = Just ctx
      }

extractMaybeWithDefault :: a -> Transformer (Maybe a) a
extractMaybeWithDefault d r = ReaderT (\_ -> maybe (Right d) Right r)

-- | Extract value and run validators on it
withField
  :: forall (f :: Symbol) v r a
   . (HasField' f r v, KnownSymbol f)
  => r -> Transformer v a -> Validation Errors a
withField rec pav = fromEither $ runReaderT (pav $ getField @f rec) $ fieldName_ @f

-- | Run a custom parser
runParser
  :: forall a
   . ReaderT Ctx (Either Errors) a
  -> Text
  -> Validation Errors a
runParser p err = fromEither $ runReaderT p err

-- | Return given 'Symbol' as 'Text'
-- >>> fieldName @"userId"
-- "userId"
fieldName_ :: forall (f :: Symbol) . KnownSymbol f => Text
fieldName_ = T.pack $ ((filter (/='"'))) $ P.show $ typeRep @f

parValidate :: [Validator a] -> Validator a
parValidate vals a = ReaderT (\ctx -> toEither $ foldr (*>) (pure a) $ fmap (mapper ctx) vals)
  where
    mapper ctx val = fromEither $ runReaderT (val a) ctx
