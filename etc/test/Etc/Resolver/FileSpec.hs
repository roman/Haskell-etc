{-# LANGUAGE CPP               #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Etc.Resolver.FileSpec where

import           RIO
import qualified RIO.Text as Text

import Test.Hspec

import Data.Aeson.QQ (aesonQQ)

import qualified Data.Aeson.BetterErrors as JSON

import System.Environment (setEnv, unsetEnv)
import System.FilePath    ((</>))

import qualified Etc.Internal.Config as Config
import           Etc.Resolver        (ResolverError (..))
import qualified Etc.Resolver        as Resolver
import qualified Etc.Spec            as Spec

import qualified Etc.Internal.Resolver.File       as SUT
import qualified Etc.Internal.Resolver.File.Types as SUT

testFixturePath :: FilePath -> FilePath
testFixturePath path =
#ifdef GHCI
  "etc-spec/test/fixtures" </> path
#else
  "test/fixtures" </> path
#endif

configSpecFilesEntryMissing :: Selector (Resolver.ResolverError (JSON.ParseError SUT.FileResolverError))
configSpecFilesEntryMissing (Resolver.ResolverError (JSON.BadSchema [] (JSON.CustomError (SUT.ConfigSpecFilesEntryMissing _)))) = True
configSpecFilesEntryMissing _ = False

configSpecFilesPathsEntryIsEmpty :: Selector (Resolver.ResolverError (JSON.ParseError SUT.FileResolverError))
configSpecFilesPathsEntryIsEmpty (Resolver.ResolverError (JSON.BadSchema [JSON.ObjectKey "etc/files"]
                                                  (JSON.CustomError SUT.ConfigSpecFilesPathsEntryIsEmpty))) =
  True
configSpecFilesPathsEntryIsEmpty _ = False

unsupportedFileExtensionGiven :: FilePath -> Selector SUT.FileResolverError
unsupportedFileExtensionGiven filename (SUT.UnsupportedFileExtensionGiven otherFilename _) =
  Text.pack filename == otherFilename
unsupportedFileExtensionGiven _ _ = False

unknownConfigKeyFound :: [Text] -> Text -> [Text] -> Selector (ResolverError SUT.FileResolverError)
unknownConfigKeyFound keyPath k ks (ResolverError (SUT.UnknownConfigKeyFound _ keyPath' looking others)) =
  keyPath == keyPath' && k == looking && ks == others
unknownConfigKeyFound _ _ _ _ = False

configurationFileNotPresent :: FilePath -> Selector SUT.FileResolverError
configurationFileNotPresent filepath (SUT.ConfigFileNotPresent path) =
  Text.pack filepath == path
configurationFileNotPresent _ _                                      = False

configFileValueTypeMismatch :: [Text] -> Selector (Resolver.ResolverError SUT.FileResolverError)
configFileValueTypeMismatch keyPath (Resolver.ResolverError (SUT.ConfigFileValueTypeMismatch _ _ ks _ _)) =
  ks == keyPath
configFileValueTypeMismatch _ _ = False

config1Path :: FilePath
config1Path = testFixturePath "config1.json"

config2Path :: FilePath
config2Path = testFixturePath "config2.json"

configWrongTypePath :: FilePath
configWrongTypePath = testFixturePath "config_wrong_type.json"

configNonExistingPath :: FilePath
configNonExistingPath = testFixturePath "non_existing.json"

spec :: Spec
spec = do

  describe "fileResolver" $ do
    it "throws exception when 'etc/files' entry is not present" $ do
      let
        configSpecValue = [aesonQQ|
          { "etc/entries": {"database": {"etc/spec": {"default": "database"}}}}
       |]
      configSpec <- Spec.parseConfigSpecValue "<<string>>" [] configSpecValue
      Resolver.resolveConfigWith [] [SUT.fileResolver SUT.jsonConfig] configSpec `shouldThrow` configSpecFilesEntryMissing

    it "throws exception when 'etc/files.path' is empty" $ do
      let
        configSpecValue = [aesonQQ|
          { "etc/files": {"paths": []}
          , "etc/entries": {"database": {"etc/spec": {"default": "database"}}}
          }
       |]
      configSpec <- Spec.parseConfigSpecValue "<<string>>" [] configSpecValue
      Resolver.resolveConfigWith [] [SUT.fileResolver SUT.jsonConfig] configSpec `shouldThrow` configSpecFilesPathsEntryIsEmpty

    it "throws exception when file in 'etc/files.path' contains key not present in spec" $ do
      let
        configSpecValue = [aesonQQ|
          { "etc/files":   {"paths": [#{config1Path}]}
          , "etc/entries": {"database": {"etc/spec": {"default": "database"}}}
          }
       |]
      configSpec <- Spec.parseConfigSpecValue "<<string>>" [] configSpecValue

      -- NOTE: When using getConfigFileWarnings, it returns errors as a warning
      warnings <- SUT.getConfigFileWarnings SUT.jsonConfig configSpec
      case warnings of
        [warning] -> do
          case fromException warning of
            Just (SUT.UnknownConfigKeyFound _ _ value others) -> do
              value `shouldBe` "greeting"
              others `shouldBe` ["database"]
            _ ->
              expectationFailure $ "Expecting resolver error, got something else " <> show warning
        _ ->
          expectationFailure $ "Expecting exactly one warning, got " <> show (length warnings)

      -- NOTE: configuration file contains "greeting" key, which is not present in the spec above
      Resolver.resolveConfigWith [] [SUT.fileResolver SUT.jsonConfig] configSpec
        `shouldThrow` unknownConfigKeyFound [] "greeting" ["database"]

    it "throws exception when spec entry type differs from file entry type" $ do
      let
        configSpecValue = [aesonQQ|
          { "etc/files":   {
              "paths": [ #{configWrongTypePath} ]
            }
          , "etc/entries": {"greeting": {"etc/spec": {"default": "default greeting"}}}
          }
        |]
      configSpec <- Spec.parseConfigSpecValue "<<string>>" [] configSpecValue
      Resolver.resolveConfigWith [] [SUT.fileResolver SUT.jsonConfig] configSpec
        `shouldThrow` configFileValueTypeMismatch ["greeting"]


    it "returns the value of the configuration file with most precedence in 'etc/files.path' (the last file)" $ do
      let
        configSpecValue = [aesonQQ|
          { "etc/files":   {"paths": [ #{config1Path}
                                     , #{config2Path} ]}
          , "etc/entries": {"greeting": {"etc/spec": {"default": "default greeting"}}}
          }
       |]
      configSpec <- Spec.parseConfigSpecValue "<<string>>" [] configSpecValue
      config <- Resolver.resolveConfigWith [] [SUT.fileResolver SUT.jsonConfig] configSpec
      databaseValue <- Config.getConfigValue ["greeting"] config
      databaseValue `shouldBe` ("config2" :: Text)

    it "returns the value of the configuration file with most precedence in 'etc/files' (the env file)" $ do
      let
        varName = "_ETC_TEST_CONFIG"
        configSpecValue = [aesonQQ|
          { "etc/files":   {
              "env": #{varName}
            , "paths": [ #{config1Path}, #{config2Path} ]
            }
          , "etc/entries": {"greeting": {"etc/spec": {"default": "default greeting"}}}
          }
       |]
      bracket
        (setEnv varName (testFixturePath "config_env.json"))
        (const $ unsetEnv varName)
        (\_ -> do
            configSpec <- Spec.parseConfigSpecValue "<<string>>" [] configSpecValue
            config <- Resolver.resolveConfigWith [] [SUT.fileResolver SUT.jsonConfig] configSpec
            databaseValue <- Config.getConfigValue ["greeting"] config
            databaseValue `shouldBe` ("env" :: Text))


  describe "getConfigFileWarnings" $ do
    it "returns warning when file in 'etc/files.path' has no supported extension" $ do
      let
        yamlFilePath = testFixturePath "config1.yaml"
        configSpecValue = [aesonQQ|
          { "etc/files":   {"paths": [#{ yamlFilePath }]}
          , "etc/entries": {"database": {"etc/spec": {"default": "database"}}}
          }
       |]
      configSpec <- Spec.parseConfigSpecValue "<<string>>" [] configSpecValue
      warnings <- SUT.getConfigFileWarnings SUT.jsonConfig configSpec
      case warnings of
        [warning] ->
          throwIO warning `shouldThrow` unsupportedFileExtensionGiven yamlFilePath
        _ ->
          expectationFailure $ "expecting exactly one warning, got " <> show (length warnings)

    it "returns warnings when files in 'etc/files.path' are not present" $ do
      let
        configSpecValue = [aesonQQ|
          { "etc/files":   {"paths": [ #{configNonExistingPath} ]}
          , "etc/entries": {"database": {"etc/spec": {"default": "database"}}}
          }
       |]
      configSpec <- Spec.parseConfigSpecValue "<<string>>" [] configSpecValue
      warnings <- SUT.getConfigFileWarnings SUT.jsonConfig configSpec
      case warnings of
        [warning] ->
          throwIO warning `shouldThrow` configurationFileNotPresent configNonExistingPath
        _ ->
          expectationFailure $ "expecting exactly one warning, got " <> show (length warnings)
