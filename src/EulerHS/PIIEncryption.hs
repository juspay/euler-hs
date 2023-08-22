{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-error=unused-top-binds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass      #-}

module EulerHS.PIIEncryption
  ( PII(..),
    PIIUpdate(..),
    PIIEncryptionDEK(..),
    PIIDecryptionDEKs(..),
    PIIMetadata(..),
    getEncryptionKey,
    makePIIMetadata,
    PIIEnabledMerchantAndTables (..),
    PIIEncryptionKeyId(..),
    PIIEncryptionKey(..),
    JuspayPIIKeyConfig(..)
  )
where

import           Data.HashMap.Strict as HM
import           EulerHS.Prelude
import           EulerHS.SqlDB.Types (DBError(..), DBErrorType(PIIError))
import           Sequelize (Set)
import qualified Data.HashSet as HashSet
import qualified EulerHS.Framework.Language as L
import qualified EulerHS.Options as T

-- PII class is used for encrypting and decrypting rows for DB. Instance will be defined for each table in storage files.
class PII (table :: (Type -> Type) -> Type) where
  encryptRow :: forall m. (L.MonadFlow m) => table Identity -> PIIEncryptionKeyId -> PIIEncryptionKey -> m (Either Text (table Identity))
  decryptRow :: forall m. (L.MonadFlow m) => table Identity -> Maybe (HM.HashMap Text Text) -> m (Either Text (table Identity))
  setPrimaryKey :: table Identity -> table Identity -> table Identity

-- PIIUpdate class is used for encrypting column values present in setClause.
class PII table => PIIUpdate (be :: Type) table where
  transformSetClause :: forall m. (L.MonadFlow m) => [Set be table] -> PIIEncryptionKeyId -> PIIEncryptionKey -> m (Either Text [Set be table])

data PIIMetadata = PIIMetadata
  { keyID :: Maybe Text
  , kvTag :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

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

data PIIEncryptionDEK = PIIEncryptionDEK
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance T.OptionEntity PIIEncryptionDEK (PIIEncryptionKeyId, PIIEncryptionKey)

data PIIDecryptionDEKs = PIIDecryptionDEKs
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance T.OptionEntity PIIDecryptionDEKs (HM.HashMap Text Text)

data PIIEnabledMerchantAndTables = PIIEnabledMerchantAndTables
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
instance T.OptionEntity PIIEnabledMerchantAndTables (Bool, (HashSet.HashSet Text))

data JuspayPIIKeyConfig = JuspayPIIKeyConfig
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
instance T.OptionEntity JuspayPIIKeyConfig [(PIIEncryptionKeyId, PIIEncryptionKey, Bool)]

makePIIMetadata :: (Maybe Text) -> Text -> PIIMetadata
makePIIMetadata mbKID tg = PIIMetadata
  { keyID = mbKID
  , kvTag = tg
  }

getEncryptionKey :: (L.MonadFlow m) => Text -> m (Either DBError (Maybe (PIIEncryptionKeyId, PIIEncryptionKey))) -- need here as we have to sent to kv part
getEncryptionKey tName = do
  piiEnable <- L.getOptionLocal PIIEnabledMerchantAndTables
  case piiEnable of
    Just (isMerchantEnabled, enabledPIITables) ->
        if isMerchantEnabled && (HashSet.member tName enabledPIITables)
          then do
            mbKeyConfig <- L.getOptionLocal PIIEncryptionDEK
            case mbKeyConfig of
              Nothing -> return $ Left $ DBError PIIError "options not set correctly"
              val -> return $ Right $ val
          else return $ Right Nothing
    Nothing -> return $ Right Nothing
