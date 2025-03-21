{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fclear-plugins #-}

module Envs where

import           EulerHS.Prelude
import           Juspay.Extra.Env
import           System.Environment (setEnv)
import           Test.Hspec
import           EulerHS.Runtime (withFlowRuntime)
import           EulerHS.Interpreters
import           EulerHS.Language
import           Data.Time.Clock (NominalDiffTime)

spec :: Spec
spec = do
    let env = JuspayEnv
            { key = "EXAMPLE_KEY"
            , actionLeft = mkErrorEnvAction (error "Default value not implemented")
            , decryptFunc = pure
            , logWhenThrowException = Just (print ("An exception occurred" :: String))
            }
    let envM = JuspayEnv
            { key = "EXAMPLE_KEY"
            , actionLeft = mkErrorEnvAction (error "Default value not implemented")
            , decryptFunc = pure
            , logWhenThrowException = Just (EulerHS.Language.runIO $ print ("An exception occurred" :: String))
            }
    describe "Env read on IO" $ do
        it "lookupENV on [String]" $ do
            setEnvs
            result1 <- lookupEnvM (env { key = "STRING_LIST" }) :: IO [String]
            result1 `shouldBe` (["item1","item2","item3"] :: [String])
        it "lookupENV on [Text]" $ do
            setEnvs
            result2 <- lookupEnvM (env { key = "TEXT_LIST" }) :: IO [Text]
            result2 `shouldBe` (["text1","text2","text3"] :: [Text])
        it "lookupENV on Maybe [String]" $ do
            setEnvs
            result3 <- lookupEnvM (env { key = "MAYBE_STRING_LIST" }) :: IO (Maybe [String])
            result3 `shouldBe` (Just ["maybe1","maybe2"] :: Maybe [String])
        it "lookupENV on Maybe [Text]" $ do
            setEnvs
            result4 <- lookupEnvM (env { key = "MAYBE_TEXT_LIST" }) :: IO (Maybe [Text])
            result4 `shouldBe` (Just ["maybeText1","maybeText2"] :: Maybe [Text])
        it "lookupENV on String" $ do
            setEnvs
            (result5 :: String) <- lookupEnvM (env { key = "STRING" })
            result5 `shouldBe` ("HI" :: String)
        it "lookupENV on Text" $ do
            setEnvs
            result6 <- lookupEnvM (env { key = "TEXT" }) :: IO Text
            result6 `shouldBe` ("HITHERE" :: Text)
        it "lookupENV on Int" $ do
            setEnvs
            result6 <- lookupEnvM (env { key = "INT" }) :: IO Int
            result6 `shouldBe` (10 :: Int)
        it "lookupENV on NOMINALDIFFTIME" $ do
            setEnvs
            result6 <- lookupEnvM (env { key = "NOMINALDIFFTIME" }) :: IO (NominalDiffTime)
            result6 `shouldBe` (900 :: NominalDiffTime)
        it "lookupENV on Maybe Int" $ do
            setEnvs
            result6 <- lookupEnvM (env { key = "MAYBE_INT" }) :: IO (Maybe Int)
            result6 `shouldBe` (Just 10 :: Maybe Int)
        it "lookupENV on Maybe Float" $ do
            setEnvs
            result6 <- lookupEnvM (env { key = "MAYBE_FLOAT" }) :: IO (Maybe Float)
            result6 `shouldBe` (Just 10.0 :: Maybe Float)

    describe "Env read on Flow" $ do
        it "lookupENV on [String]" $ do
            setEnvs
            (result1 :: [String]) <- withFlowRuntime Nothing (\rt -> runFlow rt (lookupEnvM (envM { key = "STRING_LIST" }) :: Flow [String]))
            result1 `shouldBe` (["item1","item2","item3"] :: [String])
        it "lookupENV on [Text]" $ do
            setEnvs
            (result2 :: [Text]) <- withFlowRuntime Nothing (\rt -> runFlow rt (lookupEnvM (envM { key = "TEXT_LIST" }) :: Flow [Text]))
            result2 `shouldBe` (["text1","text2","text3"] :: [Text])
        it "lookupENV on Maybe [String]" $ do
            setEnvs
            (result3 :: Maybe [String]) <- withFlowRuntime Nothing (\rt -> runFlow rt (lookupEnvM (envM { key = "MAYBE_STRING_LIST" }) :: Flow (Maybe [String])))
            result3 `shouldBe` (Just ["maybe1","maybe2"] :: Maybe [String])
        it "lookupENV on Maybe [Text]" $ do
            setEnvs
            (result4 :: Maybe [Text]) <- withFlowRuntime Nothing (\rt -> runFlow rt (lookupEnvM (envM { key = "MAYBE_TEXT_LIST" }) :: Flow (Maybe [Text])))
            result4 `shouldBe` (Just ["maybeText1","maybeText2"] :: Maybe [Text])
        it "lookupENV on String" $ do
            setEnvs
            (result5 :: String) <- withFlowRuntime Nothing (\rt -> runFlow rt (lookupEnvM (envM { key = "STRING" }) :: Flow String))
            result5 `shouldBe` ("HI" :: String)
        it "lookupENV on Text" $ do
            setEnvs
            (result6 :: Text) <- withFlowRuntime Nothing (\rt -> runFlow rt (lookupEnvM (envM { key = "TEXT" }) :: Flow Text))
            result6 `shouldBe` ("HITHERE" :: Text)

setEnvs :: IO ()
setEnvs = do
    setEnv "STRING" "HI"
    setEnv "TEXT" "HITHERE"
    setEnv "INT" "10"
    setEnv "MAYBE_INT" "10"
    setEnv "MAYBE_FLOAT" "10.0"
    setEnv "NOMINALDIFFTIME" "900s"
    setEnv "STRING_LIST" "[\"item1\",\"item2\",\"item3\"]"
    setEnv "TEXT_LIST" "[\"text1\",\"text2\",\"text3\"]"
    setEnv "MAYBE_STRING_LIST" "[\"maybe1\",\"maybe2\"]"
    setEnv "MAYBE_TEXT_LIST" "[\"maybeText1\",\"maybeText2\"]"