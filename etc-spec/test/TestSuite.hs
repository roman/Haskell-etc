{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Protolude

import           Test.Tasty                   (TestTree,
                                               defaultMainWithIngredients,
                                               testGroup)
import           Test.Tasty.HUnit             (assertBool, assertEqual,
                                               assertFailure, testCase)
import           Test.Tasty.Ingredients.Rerun (rerunningTests)
import           Test.Tasty.Runners           (consoleTestReporter,
                                               listingTests)

import           Data.HashMap.Strict          (HashMap)

import qualified Data.Aeson                   as JSON
import qualified Data.HashMap.Strict          as HashMap

import           Etc.Spec.JSON                (parseConfigSpec)
import           Etc.Spec.Types


getConfigValue :: [Text] -> HashMap Text (ConfigValue cmd) -> Maybe (ConfigValue cmd)
getConfigValue keys hsh =
  case keys of
    [] ->
      Nothing
    (k:ks) -> do
      cv <- HashMap.lookup k hsh
      case cv of
        SubConfig hsh1 ->
          getConfigValue ks hsh1
        value ->
          if null ks then
            Just value
          else
            Nothing


general_tests :: TestTree
general_tests =
  testGroup "general"
  [
    testCase "entries cannot finish in an empty map" $ do
      let
        input = "{\"etc/entries\":{\"greeting\":{}}}"

      case parseConfigSpec input of
        Just (result :: ConfigSpec ()) ->
          assertFailure $ "entries should not accept empty maps as values " ++ show result
        Nothing ->
          assertBool "" True

  , testCase "entries cannot finish in an array" $ do
      let
        input = "{\"etc/entries\":{\"greeting\":[]}}"

      case parseConfigSpec input of
        Just (result :: ConfigSpec ()) ->
          assertFailure $ "entries should not accept arrays as values " ++ show result
        Nothing ->
          assertBool "" True

  , testCase "entries that finish with raw values sets them as default value" $ do
      let
        input = "{\"etc/entries\":{\"greeting\":123}}"
        keys = ["greeting"]

      config <- parseConfigSpec input

      case getConfigValue keys (specConfigValues config) of
        Nothing ->
          assertFailure (show keys ++ " should map to a config value, got sub config map instead")
        Just (value :: ConfigValue ()) ->
          assertEqual "should contain default value"
                      (Just (JSON.Number 123))
                      (defaultValue value)

  , testCase "entries can have many levels of nesting" $ do
      let
        input = "{\"etc/entries\":{\"english\":{\"greeting\":\"hello\"}}}"
        keys = ["english", "greeting"]

      config <- parseConfigSpec input

      case getConfigValue keys (specConfigValues config) of
        Nothing ->
          assertFailure (show keys ++ " should map to a config value, got sub config map instead")
        Just (value :: ConfigValue ()) ->
          assertEqual "should contain default value"
                      (Just (JSON.String "hello"))
                      (defaultValue value)

  , testCase "spec map can not be empty object" $ do
      let
        input = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":{}}}"

      case parseConfigSpec input of
        Just (result :: ConfigSpec ()) ->
          assertFailure $ "etc/spec map should not be an empty object " ++ show result
        Nothing ->
          assertBool "" True

  , testCase "spec map can not be a JSON array" $ do
      let
        input = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":[]}}"

      case parseConfigSpec input of
        Just (result :: ConfigSpec ()) ->
          assertFailure $ "etc/spec map should not be an array " ++ show result
        Nothing ->
          assertBool "" True

  , testCase "spec map can not be a JSON bool" $ do
      let
        input = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":true}}"

      case parseConfigSpec input of
        Just (result :: ConfigSpec ()) ->
          assertFailure $ "etc/spec map should not be a boolean " ++ show result
        Nothing ->
          assertBool "" True

  , testCase "spec map can not be a JSON string" $ do
      let
        input = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":\"hello\"}}}"

      case parseConfigSpec input of
        Just (result :: ConfigSpec ()) ->
          assertFailure $ "etc/spec map should not be a string " ++ show result
        Nothing ->
          assertBool "" True

  , testCase "spec map can not be a JSON number" $ do
      let
        input = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":123}}}"

      case parseConfigSpec input of
        Just (result :: ConfigSpec ()) ->
          assertFailure $ "etc/spec map should not be a number " ++ show result
        Nothing ->
          assertBool "" True

  , testCase "spec map can not any other key that is not etc/spec" $ do
      let
        input = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":{\"default\":\"hello\"},\"foobar\":123}}}"

      case parseConfigSpec input of
        Just (result :: ConfigSpec ()) ->
          assertFailure $ "etc/spec map should not contain more than one key " ++ show result
        Nothing ->
          assertBool "" True
  ]

cli_tests :: TestTree
cli_tests =
  testGroup "cli"
  [
    testCase "cli entry settings requires an input" $ do
      let
        input = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":{\"cli\":{}}}}}"

      case parseConfigSpec input of
        Just (result :: ConfigSpec ()) ->
          assertFailure $ "cli entry should require a type " ++ show result
        Nothing ->
          assertBool "" True

  , testCase "cli option entry requires either short or long" $ do
      let
        input = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":{\"cli\":{\"input\":\"option\"}}}}}"

      case parseConfigSpec input of
        Just (result :: ConfigSpec ()) ->
          assertFailure $ "cli entry should require a type " ++ show result
        Nothing ->
          assertBool "" True

  , testCase "cli option entry works when setting short and type" $ do
      let
        input = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":{\"cli\":{\"input\":\"option\",\"short\":\"g\",\"type\":\"string\"}}}}}"
        keys  = ["greeting"]

      (config :: ConfigSpec ()) <- parseConfigSpec input

      let
        result = do
          value    <- getConfigValue keys (specConfigValues config)
          PlainEntry metadata <- cliEntry (configSources value)
          short <- optShort metadata
          valueType <- pure $ optValueType metadata
          return (short, valueType)

      case result of
        Nothing ->
          assertFailure (show keys ++ " should map to a config value, got sub config map instead")
        Just (short, valueType) -> do
          assertEqual "should contain short" "g" short
          assertEqual "should contain option type" StringOpt valueType

  , testCase "cli option entry works when setting long and type" $ do
      let
        input = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":{\"cli\":{\"input\":\"option\",\"long\":\"greeting\",\"type\":\"string\"}}}}}"
        keys  = ["greeting"]

      (config :: ConfigSpec ()) <- parseConfigSpec input

      let
        result = do
          value    <- getConfigValue keys (specConfigValues config)
          PlainEntry metadata <- cliEntry (configSources value)
          long <- optLong metadata
          valueType <- pure $ optValueType metadata
          return (long, valueType)

      case result of
        Nothing ->
          assertFailure (show keys ++ " should map to a config value, got sub config map instead")
        Just (long, valueType) -> do
          assertEqual "should contain long" "greeting" long
          assertEqual "should contain option type" StringOpt valueType

  , testCase "cli entry accepts command" $ do
      let
        input = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":{\"cli\":{\"input\":\"option\",\"long\":\"greeting\",\"type\":\"string\",\"commands\":[\"foo\"]}}}}}"
        keys  = ["greeting"]

      (config :: ConfigSpec Text) <- parseConfigSpec input

      let
        result = do
          value <- getConfigValue keys (specConfigValues config)
          CmdEntry cmd metadata <- cliEntry (configSources value)
          long <- optLong metadata
          valueType <- pure $ optValueType metadata
          return (cmd, long, valueType)

      case result of
        Nothing ->
          assertFailure (show config)
        Just (cmd, long, valueType) -> do
          assertEqual "should contain cmd" ["foo"] cmd
          assertEqual "should contain long" "greeting" long
          assertEqual "should contain option type" StringOpt valueType

  , testCase "cli entry does not accept unrecognized keys" $ do
      let
        -- typo on command instead of command_s_
        input = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":{\"cli\":{\"input\":\"option\",\"long\":\"greeting\",\"type\":\"string\",\"command\":[\"foo\"]}}}}}"

      case parseConfigSpec input of
        Just (result :: ConfigSpec ()) ->
          assertFailure $ "cli entry should fail on invalid entry " ++ show result
        Nothing ->
          assertBool "" True

  ]

envvar_tests :: TestTree
envvar_tests =
  testGroup "env"
  [
    testCase "env key creates an ENV source" $ do
      let
        input = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":{\"env\":\"GREETING\"}}}}"
        keys  = ["greeting"]

      (config :: ConfigSpec ()) <- parseConfigSpec input

      case getConfigValue keys (specConfigValues config) of
        Nothing ->
          assertFailure (show keys ++ " should map to a config value, got sub config map instead")
        Just value ->
          assertEqual "should contain EnvVar value"
                      (ConfigSources (Just "GREETING") Nothing)
                      (configSources value)
  ]

main :: IO ()
main =
  defaultMainWithIngredients
    [ rerunningTests [listingTests, consoleTestReporter] ]
    (testGroup "etc-spec" [general_tests, envvar_tests, cli_tests])
