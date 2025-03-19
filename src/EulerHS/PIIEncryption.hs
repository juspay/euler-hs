{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-error=unused-top-binds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DefaultSignatures #-}

module EulerHS.PIIEncryption
  ( PII(..),
    PIIUpdate(..),
    PIIEncryptionDEK(..),
    PIIDecryptionDEKs(..),
    OrganizationDek(..),
    getEncryptionKey,
    PIIEnabledTables(..),
    PIIEncryptionKeyId(..),
    PIIEncryptionKey(..),
    PIIKeyConfig(..),
    DefaultPiiEnabledTables(..),
    PIIEnabledMerchant(..),
    PIITenantEncryptionDEK(..)
  )
where

import           Data.HashMap.Strict as HM
import           EulerHS.Prelude
import           EulerHS.SqlDB.Types (DBError(..), DBErrorType(PIIError))
import           Sequelize (Set)
import qualified Data.HashSet as HashSet
import qualified EulerHS.Framework.Language as L
import qualified EulerHS.Options as T
import qualified Streamly.Data.MutByteArray as MBA
import           EulerHS.Logger.Types (ErrorL(..))
import           EulerHS.EnvVars
import qualified EulerHS.Constants as Const

-- PII class is used for encrypting and decrypting rows for DB. Instance will be defined for each table in storage files.
class PII (table :: (Type -> Type) -> Type) where
  encryptRow :: forall m. (L.MonadFlow m) => table Identity -> PIIKeyConfig -> m (Either Text (table Identity))
  decryptRow :: forall m. (L.MonadFlow m) => table Identity -> m (Either Text (table Identity))
  setPrimaryKey :: table Identity -> table Identity -> table Identity

  default encryptRow :: forall m. (L.MonadFlow m) => table Identity -> PIIKeyConfig -> m (Either Text (table Identity))
  encryptRow model _ = pure (Right model)

  default decryptRow :: forall m. (L.MonadFlow m) => table Identity -> m (Either Text (table Identity))
  decryptRow = pure . Right 

  default setPrimaryKey :: table Identity -> table Identity -> table Identity
  setPrimaryKey _ encDBRes = encDBRes 

-- PIIUpdate class is used for encrypting column values present in setClause.
class PII table => PIIUpdate (be :: Type) table where
  transformSetClause :: forall m. (L.MonadFlow m) => [Set be table] -> PIIKeyConfig -> m (Either Text [Set be table])
  
  default transformSetClause :: forall m. (L.MonadFlow m) => [Set be table] -> PIIKeyConfig -> m (Either Text [Set be table]) 
  transformSetClause model _  = pure (Right model)

newtype PIIEncryptionKeyId = PIIEncryptionKeyId
  { encKeyId :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype PIIEncryptionKey = PIIEncryptionKey
  { encKey :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data PIIKeyConfig = PIIKeyConfig
  { keyId    :: Text
  , encKey   :: Text
  , version  :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data PIIEncryptionDEK = PIIEncryptionDEK
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

$(MBA.deriveSerialize [d|instance MBA.Serialize PIIEncryptionDEK|])
instance T.OptionEntity PIIEncryptionDEK PIIKeyConfig

data PIIDecryptionDEKs = PIIDecryptionDEKs
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

$(MBA.deriveSerialize [d|instance MBA.Serialize PIIDecryptionDEKs|])
instance T.OptionEntity PIIDecryptionDEKs (HM.HashMap Text (Text, Text))  

data OrganizationDek = OrganizationDek
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

$(MBA.deriveSerialize [d|instance MBA.Serialize OrganizationDek|])
instance T.OptionEntity OrganizationDek Text 

data PIIEnabledTables = PIIEnabledTables
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

$(MBA.deriveSerialize [d|instance MBA.Serialize PIIEnabledTables|])
instance T.OptionEntity PIIEnabledTables (HashSet.HashSet Text)

data DefaultPiiEnabledTables = DefaultPiiEnabledTables
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

$(MBA.deriveSerialize [d|instance MBA.Serialize DefaultPiiEnabledTables|])
instance T.OptionEntity DefaultPiiEnabledTables (HashSet.HashSet Text)

data PIIEnabledMerchant = PIIEnabledMerchant 
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

$(MBA.deriveSerialize [d|instance MBA.Serialize PIIEnabledMerchant|])
instance T.OptionEntity PIIEnabledMerchant Bool

data PIITenantEncryptionDEK = PIITenantEncryptionDEK
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

$(MBA.deriveSerialize [d|instance MBA.Serialize PIITenantEncryptionDEK|])
instance T.OptionEntity PIITenantEncryptionDEK PIIKeyConfig

getEncryptionKey :: (L.MonadFlow m) => Text -> m (Either DBError (Maybe PIIKeyConfig))
getEncryptionKey tName = do
  if isOrganizationConfigModel tName
    then do
      mKey <- L.getOption OrganizationDek
      return $ Right $ maybe Nothing (Just . getOrganizationPIIKeyConfig) mKey
    else do
      isMerchantEnabled <- fromMaybe True <$> L.getOptionLocal PIIEnabledMerchant
      piiEnabledTableConfig <- L.getOptionLocal PIIEnabledTables
      defaultEnabledTablesConfig <- L.getOptionLocal DefaultPiiEnabledTables
      let isPiiEnabled = maybe False (\enabledPIITables -> isMerchantEnabled && HashSet.member tName enabledPIITables) piiEnabledTableConfig
          isEnabledByDefault = maybe False (HashSet.member tName) defaultEnabledTablesConfig
      bool (return $ Right Nothing) (fetchKeyConfig isEnabledByDefault) (isPiiEnabled || isEnabledByDefault)
      where 
        fetchKeyConfig isEnabledByDefault = do 
          mbKeyConfig <- L.getOptionLocal PIIEncryptionDEK
          case mbKeyConfig of
            Nothing -> do
              if isEnabledByDefault
                then maybe handleNoConfig (return . Right . Just) =<< L.getOptionLocal PIITenantEncryptionDEK
                else handleNoConfig
            val -> return $ Right val
        handleNoConfig = bool (logError $> Right Nothing) (return $ Left $ DBError PIIError "options not set correctly") shouldEnforcePii 
        logError = L.logErrorWithCategory @Text "getEncryptionKey" "PII Decryption DEK not set properly"  $ ErrorL Nothing "PII_ENCRYPTION" ("PII Decryption DEK not set properly for " <> tName)

isOrganizationConfigModel :: Text -> Bool
isOrganizationConfigModel tName = tName == Const.organizationConfigModelName

getOrganizationPIIKeyConfig :: Text -> PIIKeyConfig
getOrganizationPIIKeyConfig key = 
  PIIKeyConfig {
    keyId    = Const.organizationConfigPIIKeyId
  , encKey   = key
  , version  = Const.organizationConfigPIIVersion
  }

--------------------------------------------------------------------------------
-- TH Instances
--------------------------------------------------------------------------------

