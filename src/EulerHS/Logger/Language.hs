{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EulerHS.Logger.Language
  (
    Logger
  , LoggerMethod(..)
  , logMessage'
  ) where

import qualified EulerHS.Logger.Types as T
import           EulerHS.Prelude
import           Type.Reflection

-- | Language for logging.
data LoggerMethod next where
  -- | Log message with a predefined level.
  LogMessage :: T.LogLevel -> !T.Tag -> !T.Message -> (() -> next) -> LoggerMethod next

instance Functor LoggerMethod where
  fmap f (LogMessage lvl tag msg next) = LogMessage lvl tag msg $ f . next

type Logger = F LoggerMethod

logMessage' :: forall tag . (Typeable tag, Show tag) => T.LogLevel -> tag -> T.Message -> Logger ()
logMessage' lvl tag msg = liftFC $ LogMessage lvl textTag msg id
  where
    textTag :: Text
    textTag
      | Just HRefl <- eqTypeRep (typeRep @tag) (typeRep @Text  ) = tag
      | Just HRefl <- eqTypeRep (typeRep @tag) (typeRep @String) = toText tag
      | otherwise = show tag
