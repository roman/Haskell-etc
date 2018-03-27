{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Etc.Resolver.FileTest (tests) where

import           RIO
import qualified RIO.Set    as Set
import qualified RIO.Text   as Text
import qualified RIO.Vector as Vector

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Data.Aeson as JSON
import Paths_etc (getDataFileName)

import System.Etc


tests :: TestTree
tests = testGroup
  "file"
  [ testCase "supports json, yaml and yml extensions" $ do
    jsonFilepath <- getDataFileName "test/fixtures/config.json"
#ifdef WITH_YAML
    yamlFilepath <- getDataFileName "test/fixtures/config.yaml"
    ymlFilepath  <- getDataFileName "test/fixtures/config.yml"
#endif

    let input = mconcat
          [ "{\"etc/filepaths\": ["
          , "\"" <> Text.pack jsonFilepath <> "\""
#ifdef WITH_YAML
            , ", \"" <> Text.pack yamlFilepath <> "\""
            , ", \"" <> Text.pack ymlFilepath <> "\""
#endif
          , "]}"
          ]

    (spec :: ConfigSpec ()) <- parseConfigSpec input
    (config, _)             <- resolveFiles spec

    case getAllConfigSources ["greeting"] config of
      Nothing -> assertFailure
        ("expecting to get entries for greeting (check fixtures)\n" <> show config)
      Just aSet -> assertBool
        ("expecting to see entry from json config file " <> show aSet)
        (Set.member (File 1 (Text.pack jsonFilepath) "hello json") aSet)

#ifdef WITH_YAML
       >> assertBool ("expecting to see entry from yaml config file " <> show aSet)
                     (Set.member (File 2 (Text.pack jsonFilepath) "hello yaml") aSet)

       >> assertBool ("expecting to see entry from yml config file " <> show aSet)
                     (Set.member (File 3 (Text.pack jsonFilepath) "hello yml") aSet)
#endif
  , testCase "does not support any other file extension" $ do
    fooFilepath <- getDataFileName "test/fixtures/config.foo"
    let input =
          mconcat ["{\"etc/filepaths\": [", "\"" <> Text.pack fooFilepath <> "\"", "]}"]

    (spec :: ConfigSpec ()) <- parseConfigSpec input
    (config, errs)          <- resolveFiles spec

    assertEqual "config should be empty" mempty config

    if Vector.null errs
      then assertFailure "expecting one error, got none"
      else
        let err = Vector.head errs
        in
          case fromException err of
            Just (InvalidConfiguration _ _) -> return ()
            _                               -> assertFailure
              ("Expecting InvalidConfigurationError; got instead " <> show err)
  , testCase "does not fail if file doesn't exist" $ do
    jsonFilepath <- getDataFileName "test/fixtures/config.json"
    let input = mconcat
          [ "{\"etc/filepaths\": ["
          , "\"" <> Text.pack jsonFilepath <> "\""
          , ", \"unknown_file.json\""
          , "]}"
          ]

    (spec :: ConfigSpec ()) <- parseConfigSpec input
    (config, errs)          <- resolveFiles spec

    case getAllConfigSources ["greeting"] config of
      Nothing -> assertFailure
        ("expecting to get entries for greeting (check fixtures)\n" <> show config)
      Just aSet -> assertBool
        ("expecting to see entry from json config file " <> show aSet)
        (Set.member (File 1 (Text.pack jsonFilepath) "hello json") aSet)

    if Vector.null errs
      then assertFailure "expecting one error, got none"
      else
        let err = Vector.head errs
        in
          case fromException err of
            Just (ConfigurationFileNotFound _) -> return ()
            _ -> assertFailure
              ("Expecting ConfigurationFileNotFound; got instead " <> show err)

  , testCase "null file input has no precedence over default values" $ do
    jsonFilepath <- getDataFileName "test/fixtures/config.null.json"
    let input = mconcat
          [ "{\"etc/filepaths\": ["
          , "\"" <> Text.pack jsonFilepath <> "\""
          , "]"
          , ", \"etc/entries\": {"
          , "    \"greeting\": {"
          , "      \"etc/spec\": {"
          , "        \"default\": \"hola\""
          , "}}}}"

          ]

    (spec :: ConfigSpec ()) <- parseConfigSpec input
    (configFiles, _errs)          <- resolveFiles spec
    let configDef = resolveDefault spec
        config = configDef <> configFiles

    case getAllConfigSources ["greeting"] config of
      Nothing   -> assertFailure ("expecting to get entries for greeting\n" <> show config)
      Just aSet ->
        let
          result =
               any (\entry ->
                     case entry of
                       File _ _ (Plain JSON.Null) -> True
                       _ -> False)
                  aSet
        in assertBool ("expecting to see entry from env; got " <> show aSet) result

    case getConfigValue ["greeting"] config of
      Just greeting ->
        assertEqual "Didn't take default value, despite CLI being null" ("hola" :: Text) greeting
      Nothing ->
        assertFailure "Expecting config value, but got nothing"
  ]
