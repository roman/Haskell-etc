{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Etc.Resolver.FileSpec where

import RIO

import Test.Hspec

import Data.Aeson.QQ (aesonQQ)

import qualified Data.Aeson.BetterErrors as JSON

import System.Environment (setEnv, unsetEnv)

import qualified Etc.Config as Config
import qualified Etc.Spec as Spec
import qualified Etc.Resolver as Resolver
import qualified Etc.Resolver.File as SUT

configSpecFilesEntryMissing :: Selector (Resolver.ResolverError (JSON.ParseError SUT.FileResolverError))
configSpecFilesEntryMissing (Resolver.ResolverError (JSON.BadSchema [] (JSON.CustomError (SUT.ConfigSpecFilesEntryMissing _)))) = True
configSpecFilesEntryMissing _ = False

configSpecFilesPathsEntryIsEmpty :: Selector (Resolver.ResolverError (JSON.ParseError SUT.FileResolverError))
configSpecFilesPathsEntryIsEmpty (Resolver.ResolverError (JSON.BadSchema [JSON.ObjectKey "etc/files"]
                                                  (JSON.CustomError SUT.ConfigSpecFilesPathsEntryIsEmpty))) =
  True
configSpecFilesPathsEntryIsEmpty _ = False

unsupportedFileExtensionGiven :: Text -> Selector SUT.FileResolverError
unsupportedFileExtensionGiven filename (SUT.UnsupportedFileExtensionGiven otherFilename _) = filename == otherFilename
unsupportedFileExtensionGiven _ _ = False

unknownConfigKeyFound :: [Text] -> Text -> [Text] -> Selector SUT.FileResolverError
unknownConfigKeyFound keyPath k ks (SUT.UnknownConfigKeyFound _ keyPath' looking others) =
  keyPath == keyPath' && k == looking && ks == others
unknownConfigKeyFound _ _ _ _ = False

configurationFileNotPresent :: Text -> Selector SUT.FileResolverError
configurationFileNotPresent filepath (SUT.ConfigFileNotPresent path) = filepath == path
configurationFileNotPresent _ _ = False

configFileValueTypeMismatch :: [Text] -> Selector SUT.FileResolverError
configFileValueTypeMismatch keyPath (SUT.ConfigFileValueTypeMismatch _ ks _ _) = ks == keyPath
configFileValueTypeMismatch _ _ = False

spec :: Spec
spec = do
  describe "jsonFileResolver" $ do
    it "throws exception when 'etc/files' entry is not present" $ do
      let
        configSpecValue = [aesonQQ|
          { "etc/entries": {"database": {"etc/spec": {"default": "database"}}}}
       |]
      configSpec <- Spec.parseConfigSpecValue configSpecValue
      Resolver.resolveConfig configSpec [SUT.jsonFileResolver] `shouldThrow` configSpecFilesEntryMissing

    it "throws exception when 'etc/files.path' is empty" $ do
      let
        configSpecValue = [aesonQQ|
          { "etc/files": {"paths": []}
          , "etc/entries": {"database": {"etc/spec": {"default": "database"}}}
          }
       |]
      configSpec <- Spec.parseConfigSpecValue configSpecValue
      Resolver.resolveConfig configSpec [SUT.jsonFileResolver] `shouldThrow` configSpecFilesPathsEntryIsEmpty

    it "throws exception when file in 'etc/files.path' contains key not present in spec" $ do
      let
        configSpecValue = [aesonQQ|
          { "etc/files":   {"paths": ["./test/fixtures/config1.json"]}
          , "etc/entries": {"database": {"etc/spec": {"default": "database"}}}
          }
       |]
      configSpec <- Spec.parseConfigSpecValue configSpecValue

      -- NOTE: When using getFileWarnings, it returns the error as a warning
      warnings <- SUT.getFileWarnings SUT.jsonFileParser configSpec
      case warnings of
        [warning] ->
          (throwIO warning) `shouldThrow` (unknownConfigKeyFound [] "greeting" ["database"])
        _ ->
          expectationFailure $ "Expecting exactly one warning, got " <> show (length warnings)

      -- NOTE: configuration file contains "greeting" key, which is not present in the spec above
      Resolver.resolveConfig configSpec [SUT.jsonFileResolver]
        `shouldThrow` (unknownConfigKeyFound [] "greeting" ["database"])

    it "throws exception when spec entry type differs from file entry type" $ do
      let
        configSpecValue = [aesonQQ|
          { "etc/files":   {
              "paths": [ "./test/fixtures/config_wrong_type.json" ]
            }
          , "etc/entries": {"greeting": {"etc/spec": {"default": "default greeting"}}}
          }
        |]
      configSpec <- Spec.parseConfigSpecValue configSpecValue
      Resolver.resolveConfig configSpec [SUT.jsonFileResolver]
        `shouldThrow` configFileValueTypeMismatch ["greeting"]


    it "returns the value of the configuration file with most precedence in 'etc/files.path' (the last file)" $ do
      let
        configSpecValue = [aesonQQ|
          { "etc/files":   {"paths": [ "./test/fixtures/config1.json"
                                     , "./test/fixtures/config2.json" ]}
          , "etc/entries": {"greeting": {"etc/spec": {"default": "default greeting"}}}
          }
       |]
      configSpec <- Spec.parseConfigSpecValue configSpecValue
      config <- Resolver.resolveConfig configSpec [SUT.jsonFileResolver]
      databaseValue <- Config.getConfigValue ["greeting"] config
      databaseValue `shouldBe` ("config2" :: Text)

    it "returns the value of the configuration file with most precedence in 'etc/files' (the env file)" $ do
      let
        varName = "_ETC_TEST_CONFIG"
        configSpecValue = [aesonQQ|
          { "etc/files":   {
              "env": #{varName}
            , "paths": [ "./test/fixtures/config1.json"
                       , "./test/fixtures/config2.json" ]
            }
          , "etc/entries": {"greeting": {"etc/spec": {"default": "default greeting"}}}
          }
       |]
      bracket
        (setEnv varName "./test/fixtures/config_env.json")
        (const $ unsetEnv varName)
        (\_ -> do
            configSpec <- Spec.parseConfigSpecValue configSpecValue
            config <- Resolver.resolveConfig configSpec [SUT.jsonFileResolver]
            databaseValue <- Config.getConfigValue ["greeting"] config
            databaseValue `shouldBe` ("env" :: Text))


  describe "getFileWarnings" $ do
    it "returns warning when file in 'etc/files.path' has no JSON extension" $ do
      let
        configSpecValue = [aesonQQ|
          { "etc/files":   {"paths": ["./test/fixtures/config1.yaml"]}
          , "etc/entries": {"database": {"etc/spec": {"default": "database"}}}
          }
       |]
      configSpec <- Spec.parseConfigSpecValue configSpecValue
      warnings <- SUT.getFileWarnings SUT.jsonFileParser configSpec
      case warnings of
        [warning] ->
          (throwIO warning) `shouldThrow` (unsupportedFileExtensionGiven "./test/fixtures/config1.yaml")
        _ ->
          expectationFailure $ "expecting exactly one warning, got " <> show (length warnings)

    it "returns warnings when files in 'etc/files.path' are not present" $ do
      let
        configSpecValue = [aesonQQ|
          { "etc/files":   {"paths": ["./test/fixtures/non_existing.json"]}
          , "etc/entries": {"database": {"etc/spec": {"default": "database"}}}
          }
       |]
      configSpec <- Spec.parseConfigSpecValue configSpecValue
      warnings <- SUT.getFileWarnings SUT.jsonFileParser configSpec
      case warnings of
        [warning] ->
          (throwIO warning) `shouldThrow` (configurationFileNotPresent "./test/fixtures/non_existing.json")
        _ ->
          expectationFailure $ "expecting exactly one warning, got " <> show (length warnings)
