{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Etc.Resolver.EnvTest where

import           RIO
import qualified RIO.Set  as Set
import qualified RIO.Text as Text

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)


import Paths_etc (getDataFileName)

import System.Etc
import System.Etc.Internal.Types (EnvSource (..))


tests :: TestTree
tests = testGroup
  "env"
  [ testCase "env entry is present when env var is defined" $ do
    let input = mconcat
          [ "{\"etc/entries\": {"
          , " \"greeting\":{\"etc/spec\":{\"type\":\"string\",\"env\":\"GREETING\"}}}}"
          ]
    (spec :: ConfigSpec ()) <- parseConfigSpec input

    let config = resolveEnvPure spec [("GREETING", "hello env")]

    case getAllConfigSources ["greeting"] config of
      Nothing -> assertFailure
        ("expecting to get entries for greeting (check fixtures)\n" <> show config)
      Just aSet -> assertBool
        ("expecting to see entry from env; got " <> show aSet)
        (Set.member (SomeConfigSource 2 $ EnvSource "GREETING" "hello env") aSet)
  , testCase "has precedence over default and file values" $ do
    jsonFilepath <- getDataFileName "test/fixtures/config.json"
    let input = mconcat
          [ "{\"etc/filepaths\": ["
          , "\"" <> Text.pack jsonFilepath <> "\""
          , "],"
          , " \"etc/entries\": {"
          , " \"greeting\":{\"etc/spec\":{\"type\":\"string\",\"env\": \"GREETING\"}}}}"
          ]
    (spec :: ConfigSpec ()) <- parseConfigSpec input
    (configFile, _)         <- resolveFiles spec

    let configEnv = resolveEnvPure spec [("GREETING", "hello env")]

        config    = configEnv `mappend` configFile

    case getConfigValue ["greeting"] config of
      Nothing -> assertFailure
        ("expecting to get entries for greeting (check fixtures)\n" <> show config)
      Just result -> assertEqual ("expecting to see entry from env " <> show result)
                                 ("hello env" :: Text)
                                 result
  , testCase "does not add entries to config if env var is not present" $ do
    let
      input = mconcat
        [ "{\"etc/entries\": {"
        , " \"nested\":{\"greeting\":{ \"etc/spec\":{\"type\":\"string\",\"env\": \"GREETING\" }}}}}"
        ]
    (spec :: ConfigSpec ()) <- parseConfigSpec input

    let config = resolveEnvPure spec []

    assertEqual "expecting to not have an entry for key"
                (Nothing :: Maybe Text)
                (getConfigValue ["nested"] config)

    assertEqual "expecting to not have an entry for key"
                (Nothing :: Maybe Text)
                (getConfigValue ["nested", "greeting"] config)
  , testCase "does parse numbers correctly" $ do
    let
      input = mconcat
        [ "{\"etc/entries\": {\"greeting\":{ \"etc/spec\":{\"type\":\"number\",\"env\": \"GREETING\"}}}}"
        ]
    (spec :: ConfigSpec ()) <- parseConfigSpec input

    let config = resolveEnvPure spec [("GREETING", "123")]

    assertEqual "expecting to not have an entry for key"
                (Just 123 :: Maybe Int)
                (getConfigValue ["greeting"] config)
  , testCase "does parse array of numbers correctly" $ do
    let
      input = mconcat
        [ "{\"etc/entries\": {\"greeting\":{ \"etc/spec\":{\"type\":\"[number]\",\"env\": \"GREETING\"}}}}"
        ]
    (spec :: ConfigSpec ()) <- parseConfigSpec input

    let config = resolveEnvPure spec [("GREETING", "[123, 456]")]

    assertEqual "expecting to not have an entry for key"
                (Just [123, 456] :: Maybe [Int])
                (getConfigValue ["greeting"] config)
  , testCase "does parse array of strings correctly" $ do
    let
      input = mconcat
        [ "{\"etc/entries\": {\"greeting\":{ \"etc/spec\":{\"type\":\"[string]\",\"env\": \"GREETING\"}}}}"
        ]
    (spec :: ConfigSpec ()) <- parseConfigSpec input

    let config = resolveEnvPure spec [("GREETING", "[\"hello\", \"world\"]")]

    assertEqual "expecting to not have an entry for key"
                (Just ["hello", "world"] :: Maybe [Text])
                (getConfigValue ["greeting"] config)
  , testCase "does parse array of bools correctly" $ do
    let
      input = mconcat
        [ "{\"etc/entries\": {\"greeting\":{ \"etc/spec\":{\"type\":\"[bool]\",\"env\": \"GREETING\"}}}}"
        ]
    (spec :: ConfigSpec ()) <- parseConfigSpec input

    let config = resolveEnvPure spec [("GREETING", "[false, true]")]

    assertEqual "expecting to not have an entry for key"
                (Just [False, True] :: Maybe [Bool])
                (getConfigValue ["greeting"] config)
  , testCase "does not add entries to config if env var value has invalid type" $ do
    let
      input = mconcat
        [ "{\"etc/entries\": {\"greeting\":{ \"etc/spec\":{\"type\":\"number\",\"env\": \"GREETING\"}}}}"
        ]
    (spec :: ConfigSpec ()) <- parseConfigSpec input

    let config = resolveEnvPure spec []

    assertEqual "expecting to not have an entry for key"
                (Nothing :: Maybe Int)
                (getConfigValue ["greeting"] config)
  ]
