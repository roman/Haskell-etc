{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Etc.Resolver.FileTest (tests) where

import           Protolude

import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.HUnit           (assertBool, assertEqual, assertFailure, testCase)

import qualified Data.Set                   as Set
import qualified Data.Text                  as Text

import           Paths_etc_spec             (getDataFileName)

import           Etc


tests :: TestTree
tests =
  testGroup "file"
  [
    testCase "supports json, yaml and yml extensions" $ do
      jsonFilepath <- getDataFileName "test/fixtures/config.json"
#ifdef WITH_YAML
      yamlFilepath <- getDataFileName "test/fixtures/config.yaml"
      ymlFilepath  <- getDataFileName "test/fixtures/config.yml"
#endif

      let
        input =
          mconcat
            [
              "{\"etc/filepaths\": ["
            , "\"" <> Text.pack jsonFilepath <> "\""
#ifdef WITH_YAML
            , ", \"" <> Text.pack yamlFilepath <> "\""
            , ", \"" <> Text.pack ymlFilepath <> "\""
#endif
            , "]}"
            ]

      (spec :: ConfigSpec ()) <- parseConfigSpec input
      (config, _) <- resolveFiles spec

      case getAllConfigSources ["greeting"] config of
        Nothing ->
          assertFailure ("expecting to get entries for greeting (check fixtures)\n"
                         <> show config)
        Just set -> do
          assertBool ("expecting to see entry from json config file " <> show set)
                     (Set.member (File 1 (Text.pack jsonFilepath) "hello json") set)

#ifdef WITH_YAML
          assertBool ("expecting to see entry from yaml config file " <> show set)
                     (Set.member (File 2 (Text.pack jsonFilepath) "hello yaml") set)

          assertBool ("expecting to see entry from yml config file " <> show set)
                     (Set.member (File 3 (Text.pack jsonFilepath) "hello yml") set)
#endif

  , testCase "does not support any other file extension" $ do
      fooFilepath <- getDataFileName "test/fixtures/config.foo"
      let
        input =
          mconcat
            [
              "{\"etc/filepaths\": ["
            , "\"" <> Text.pack fooFilepath <> "\""
            , "]}"
            ]

      (spec :: ConfigSpec ()) <- parseConfigSpec input
      (config, errs) <- resolveFiles spec

      assertEqual "config should be empty" mempty config

      case errs of
        [] ->
          assertFailure "expecting one error, got none"
        (err:_) ->
          case fromException err of
            Just (InvalidConfiguration _) ->
              return ()
            _ ->
              assertFailure ("Expecting InvalidConfigurationError; got instead "
                             <> show err)


  , testCase "does not fail if file doesn't exist" $ do
      jsonFilepath <- getDataFileName "test/fixtures/config.json"
      let
        input =
          mconcat
            [
              "{\"etc/filepaths\": ["
            , "\"" <> Text.pack jsonFilepath <> "\""
            , ", \"unknown_file.json\""
            , "]}"
            ]

      (spec :: ConfigSpec ()) <- parseConfigSpec input
      (config, errs) <- resolveFiles spec

      case getAllConfigSources ["greeting"] config of
        Nothing ->
          assertFailure ("expecting to get entries for greeting (check fixtures)\n"
                         <> show config)
        Just set -> do
          assertBool ("expecting to see entry from json config file " <> show set)
                     (Set.member (File 1 (Text.pack jsonFilepath) "hello json") set)

      case errs of
        [] ->
          assertFailure "expecting one error, got none"
        (err:_) ->
          case fromException err of
            Just (ConfigurationFileNotFound _) ->
              return ()
            _ ->
              assertFailure ("Expecting ConfigurationFileNotFound; got instead "
                             <> show err)
  ]
