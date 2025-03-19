{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-error=unused-top-binds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedStrings   #-}
module EulerHS.KVConnector.PIIUtils where

import           EulerHS.PIIEncryption
import           Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import           EulerHS.Extra.Redis (rSetBWithART)
import qualified EulerHS.Framework.Language as L
import           EulerHS.Prelude
import           EulerHS.ART.V2.Types (HasArtRecOptions)
import           EulerHS.SqlDB.Types (
                                      DBError (DBError),
                                      DBErrorType (PIIError))

cacheName :: String
cacheName = "eulerKVDB"

findAllDecryptUtility ::  
  ( PII table,
    HasArtRecOptions,
    ToJSON (table Identity),
    L.MonadFlow m
  ) =>
  (Either DBError [table Identity]) ->
  Maybe Text ->
  m (Either DBError [table Identity])
findAllDecryptUtility mDBRes mbCacheKey = do 
  case mDBRes of 
    Right encResult -> do
      let emptyKeyConfig = replicate (length encResult) Nothing
      eitherDecryptedRes <- decryptOrEncryptAllUtility encResult emptyKeyConfig False
      case eitherDecryptedRes of 
        Right res -> do
          whenJust mbCacheKey (`cacheWithKey` res)
          return $ Right $ res
        Left decErr -> return $ Left $ decErr
    Left dbError -> return $ Left $ dbError

decryptOrEncryptAllUtility :: (L.MonadFlow m, PII table) => [table Identity] -> [Maybe PIIKeyConfig] -> Bool ->  m (Either DBError [table Identity])
decryptOrEncryptAllUtility [] _ _ = pure $ Right []
decryptOrEncryptAllUtility listRow mayKeyConfiglist shouldEncrypt = do
  eithResList <- foldM doEncryptDecrypt [] $ zip listRow mayKeyConfiglist
  case eithResList of
    [Left err] -> do
      L.logError @Text "PII error : decryptOrEncryptAllUtility" $ if shouldEncrypt then "encryption failed" else "decryption failed"
      pure $ Left err
    _ -> pure $ Right $ foldl (\acc row -> either (const $ acc) (\rw -> rw : acc) row) [] eithResList

  where
    -- doEncryptDecrypt : for any Left case, directly return [Left err]
    doEncryptDecrypt :: (L.MonadFlow m, PII table) => [Either DBError (table Identity)] -> ((table Identity), Maybe PIIKeyConfig) -> m [Either DBError (table Identity)]
    doEncryptDecrypt [Left e] _ = pure [Left e]
    doEncryptDecrypt acc (row, mbKeyConfig) = do
      eithRes <- case (shouldEncrypt, mbKeyConfig) of
          (True, Just keyConfig) -> encryptRow row keyConfig
          (False, _) -> decryptRow row
          (_, _) -> return $ Right row
      case eithRes of
        Left e -> pure [Left $ DBError PIIError e]
        Right rowRes -> pure (Right rowRes : acc)


findDecryptUtility ::
  ( ToJSON (table Identity),
    HasArtRecOptions,
    PII table,
    L.MonadFlow m
  ) =>
  (Either DBError (Maybe (table Identity))) ->
  Maybe Text ->
  m (Either DBError (Maybe (table Identity)))
findDecryptUtility mDBRes mbCacheKey = do 
  case mDBRes of 
    Right (Just encResult) -> do 
      decryptResult <- decryptRow encResult
      case decryptResult of 
        Left err -> return $ Left $ DBError PIIError err
        Right decryptedRes -> do
          whenJust mbCacheKey (`cacheWithKey` decryptedRes)
          return $ Right $ Just $ decryptedRes
    Right Nothing -> return $ Right $ Nothing
    Left dbError -> return $ Left $ dbError

cacheWithKey :: (HasCallStack, HasArtRecOptions, ToJSON table, L.MonadFlow m) => Text -> table -> m ()
cacheWithKey key row = do
  -- TODO: Should we log errors here?
  void $ rSetBWithART (T.pack cacheName) (encodeUtf8 key) (BSL.toStrict $ encode row)