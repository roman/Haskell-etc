{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Etc.Resolver.EnvTest where

import Protolude

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)


import qualified Data.Set  as Set
import qualified Data.Text as Text

import Paths_etc (getDataFileName)

import Etc


tests :: TestTree
tests =
  testGroup "env"
  [
    testCase "env entry is present when env var is defined" $ do
      let
        input =
          mconcat
            [
              "{\"etc/entries\": {"
            , " \"greeting\": { \"etc/spec\": { \"env\": \"GREETING\" }}}}"
            ]
      (spec :: ConfigSpec ()) <- parseConfigSpec input

      let
        config =
            resolveEnvPure spec [("GREETING", "hello env")]

      case getAllConfigSources ["greeting"] config of
        Nothing ->
          assertFailure ("expecting to get entries for greeting (check fixtures)\n"
                         <> show config)
        Just set ->
          assertBool ("expecting to see entry from env; got " <> show set)
                   (Set.member (Env "GREETING" "hello env") set)

  , testCase "has precedence over default and file values" $ do
      jsonFilepath <- getDataFileName "test/fixtures/config.json"
      let
        input =
          mconcat
            [
              "{\"etc/filepaths\": ["
            , "\"" <> Text.pack jsonFilepath <> "\""
            , "],"
            , " \"etc/entries\": {"
            , " \"greeting\": { \"etc/spec\": { \"env\": \"GREETING\" }}}}"
            ]
      (spec :: ConfigSpec ()) <- parseConfigSpec input
      (configFile, _) <- resolveFiles spec

      let
        configEnv =
          resolveEnvPure spec [("GREETING", "hello env")]

        config =
            configEnv <> configFile

      case getConfigValue ["greeting"] config of
        Nothing ->
          assertFailure ("expecting to get entries for greeting (check fixtures)\n"
                         <> show config)
        Just result ->
          assertEqual ("expecting to see entry from env " <> show result)
                    ("hello env" :: Text)
                    result

  , testCase "does not add entries to config if env var is not present" $ do
      let
        input =
          mconcat
            [
              "{\"etc/entries\": {"
            , " \"nested\": {\"greeting\": { \"etc/spec\": { \"env\": \"GREETING\" }}}}}"
            ]
      (spec :: ConfigSpec ()) <- parseConfigSpec input

      let
        config =
          resolveEnvPure spec []

      assertEqual "expecting to not have an entry for key"
                  (Nothing :: Maybe Text)
                  (getConfigValue ["nested"] config)

      assertEqual "expecting to not have an entry for key"
                  (Nothing :: Maybe Text)
                  (getConfigValue ["nested", "greeting"] config)
  ]
