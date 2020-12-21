{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module EulerHS.Core.Logger.Language
  (
    Logger
  , LoggerMethod(..)
  , logMessage'
  ) where

import           EulerHS.Prelude

import qualified EulerHS.Core.Types.Logger as T

-- | Language for logging.
data LoggerMethod next where
  -- | Log message with a predefined level.
  LogMessage :: T.LogLevel -> !T.Tag -> !T.Message -> (() -> next) -> LoggerMethod next

instance Functor LoggerMethod where
  fmap f (LogMessage lvl tag msg next) = LogMessage lvl tag msg $ f . next

type Logger = F LoggerMethod

logMessage' :: Show tag => T.LogLevel -> tag -> T.Message -> Logger ()
logMessage' lvl tag msg = liftFC $ LogMessage lvl (show tag) msg id
-- {-# NOINLINE logMessage' #-}
-- {-# RULES
--
--     "Specialise Text Tag logMessage'" forall (tag :: Text) (lvl :: T.LogLevel) (msg :: T.Message) .
--        logMessage' lvl tag msg = liftFC $ LogMessage lvl tag msg id ;
--
--     "Specialise String Tag logMessage'" forall (tag :: String) (lvl :: T.LogLevel) (msg :: T.Message) .
--        logMessage' lvl tag msg = liftFC $ LogMessage lvl (toText tag) msg id
-- #-}
