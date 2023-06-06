{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EulerHS.Extra.EulerDB (
  EulerDbCfg(..),
  EulerDbCfgR1(..),
  EulerPsqlDbCfg(..),
  EulerPsqlDbAdditionalCfg(..),
  getEulerDbConf,
  getEulerDbConfR1,
  withEulerDB,
  withEulerDBR1,
  withEulerPsqlDB,
  withEulerDBTransaction,
  withEulerAdditionalPsqlDB
  ) where

import           EulerHS.Language (MonadFlow, SqlDB, getOption, logErrorT,
                                   throwException, withDB, withDBTransaction)
import           EulerHS.Prelude hiding (getOption)
import           EulerHS.Types (DBConfig, OptionEntity)

import           Database.Beam.MySQL (MySQLM)
import qualified Database.Beam.Postgres as BP

data EulerDbCfg = EulerDbCfg
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity EulerDbCfg (DBConfig MySQLM)

data EulerDbCfgR1 = EulerDbCfgR1
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity EulerDbCfgR1 (DBConfig MySQLM)

data EulerPsqlDbCfg = EulerPsqlDbCfg
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity EulerPsqlDbCfg (DBConfig BP.Pg)

data EulerPsqlDbAdditionalCfg = EulerPsqlDbAdditionalCfg
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity EulerPsqlDbAdditionalCfg (DBConfig BP.Pg)

-- Pass Exception argument to function ad hoc,
-- or better use prepared functions from
-- src/Euler/WebService/Database/EulerDB.hs
getEulerDbConf :: (HasCallStack, MonadFlow m, Exception e) => e -> m (DBConfig MySQLM)
getEulerDbConf = getEulerDbByConfig EulerDbCfg

getEulerDbConfR1 :: (HasCallStack, MonadFlow m, Exception e) => e -> m (DBConfig MySQLM)
getEulerDbConfR1 = getEulerDbByConfig EulerDbCfgR1

getEulerDbByConfig :: (HasCallStack, MonadFlow m, Exception e, OptionEntity k (DBConfig MySQLM))
  => k -> e -> m (DBConfig MySQLM)
getEulerDbByConfig dbConf internalError = do
  dbcfg <- getOption dbConf
  case dbcfg of
    Just cfg -> pure cfg
    Nothing -> do
      logErrorT "MissingDB identifier" "Can't find EulerDB identifier in options"
      throwException internalError

-- NOTE: Does NOT run inside a transaction
withEulerDB :: (HasCallStack, MonadFlow m, Exception e) => e -> SqlDB MySQLM a -> m a
withEulerDB internalError act = withEulerDBGeneral EulerDbCfg internalError act

-- | Runs the query against the MySQL read replica DB
-- Falls back to using default MySQL DB if replica DB connection is not initialized
-- NOTE: Does NOT run inside a transaction
withEulerDBR1 :: (HasCallStack, MonadFlow m, Exception e) => e -> SqlDB MySQLM a -> m a
withEulerDBR1 internalError act = withEulerDBGeneral EulerDbCfg internalError act

withEulerDBGeneral :: (HasCallStack, MonadFlow m, Exception e, OptionEntity k (DBConfig MySQLM))
  => k -> e -> SqlDB MySQLM a -> m a
withEulerDBGeneral key internalError act = do
  dbcfg <- getOption key
  case dbcfg of
    Just cfg -> withDB cfg act
    Nothing -> do
      logErrorT "MissingDB identifier" "Can't find EulerDB identifier in options"
      throwException internalError

-- NOTE: Does NOT run inside a transaction
withEulerDBTransaction :: (HasCallStack, MonadFlow m, Exception e)
  => e -> SqlDB MySQLM a -> m a
withEulerDBTransaction internalError act = do
  (dbcfg :: Maybe (DBConfig MySQLM)) <- getOption EulerDbCfg
  case dbcfg of
    Just cfg -> withDBTransaction cfg act
    Nothing -> do
      logErrorT "MissingDB identifier" "Can't find EulerDB identifier in options"
      throwException internalError

withEulerPsqlDB :: (HasCallStack, MonadFlow m, Exception e)
  => e -> SqlDB BP.Pg a -> m a
withEulerPsqlDB internalError act = do
  (dbcfg :: Maybe (DBConfig BP.Pg)) <- getOption EulerPsqlDbCfg
  case dbcfg of
    Just cfg -> withDB cfg act
    Nothing -> do
      logErrorT "MissingDB identifier" "Can't find EulerDB identifier in options"
      throwException internalError

withEulerAdditionalPsqlDB :: (HasCallStack, MonadFlow m, Exception e)
  => e -> SqlDB BP.Pg a -> m a
withEulerAdditionalPsqlDB internalError act = do
  (dbcfg :: Maybe (DBConfig BP.Pg)) <- getOption EulerPsqlDbAdditionalCfg
  case dbcfg of
    Just cfg -> withDB cfg act
    Nothing -> do
      logErrorT "MissingDB identifier" "Can't find EulerAdditionalPsqlDB identifier in options"
      throwException internalError