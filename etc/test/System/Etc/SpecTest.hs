{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Etc.SpecTest (tests) where

import           RIO
import qualified RIO.HashMap as HashMap
import qualified RIO.Vector  as Vector

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Data.Aeson as JSON

import           System.Etc.Internal.Spec.Types
import qualified System.Etc.Spec                as SUT

#ifdef WITH_YAML
import qualified Data.Yaml as YAML
import           Paths_etc (getDataFileName)
#endif

getConfigValue :: [Text] -> HashMap Text (ConfigValue cmd) -> Maybe (ConfigValue cmd)
getConfigValue keys hsh =
  -- NOTE: For some reason, pattern matching doesn't work on lts-6, using
  -- if/else instead
                          if null keys
  then Nothing
  else do
    let (k : ks) = keys
    cv <- HashMap.lookup k hsh
    case cv of
      SubConfig hsh1 -> getConfigValue ks hsh1
      value          -> if null ks then Just value else Nothing


general_tests :: TestTree
general_tests = testGroup
  "general"
  [ testCase "does not fail when etc/entries is not defined" $ do
    let input = "{}"

    case SUT.parseConfigSpec input of
      Nothing -> assertFailure "should not fail if no etc/entries key is present"
      Just (_ :: ConfigSpec ()) -> assertBool "" True
  , testCase "fails when default JSON value doesn't correspond to type entry" $ do
    let
      input
        = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":{\"default\":[123],\"type\":\"[string]\"}}}}"

    case SUT.parseConfigSpec input of
      Left err -> case fromException err of
        Just SpecInvalidSyntaxFound{} -> assertBool "" True

        _ ->
          assertFailure
            $  "expecting to get an SpecInvalidSyntaxFound error; but got "
            <> show err

      Right (_ :: ConfigSpec ()) ->
        assertFailure "Expecting config spec parsing to fail, but didn't"
  , testCase "entries cannot finish in an empty map" $ do
    let input = "{\"etc/entries\":{\"greeting\":{}}}"

    case SUT.parseConfigSpec input of
      Just (result :: ConfigSpec ()) ->
        assertFailure $ "entries should not accept empty maps as values " ++ show result
      Nothing -> assertBool "" True
  , testCase "entries cannot finish in an array" $ do
    let input = "{\"etc/entries\":{\"greeting\":[]}}"

    case SUT.parseConfigSpec input of
      Just (result :: ConfigSpec ()) ->
        assertFailure $ "entries should not accept arrays as values " ++ show result
      Nothing -> assertBool "" True
  , testCase "entries that finish with raw values sets them as default value" $ do
    let input = "{\"etc/entries\":{\"greeting\":123}}"
        keys  = ["greeting"]

    config <- SUT.parseConfigSpec input
    case getConfigValue keys (specConfigValues config) of
      Nothing -> assertFailure
        (show keys ++ " should map to a config value, got sub config map instead")
      Just (value :: ConfigValue ()) -> assertEqual "should contain default value"
                                                    (Just (JSON.Number 123))
                                                    (defaultValue value)
  , testCase "entries that finish with arrays sets them as default value" $ do
    let input = "{\"etc/entries\":{\"greeting\":[123]}}"
        keys  = ["greeting"]

    config <- SUT.parseConfigSpec input

    case getConfigValue keys (specConfigValues config) of
      Nothing -> assertFailure
        (show keys ++ " should map to a config value, got sub config map instead")
      Just (value :: ConfigValue ()) -> assertEqual
        "should contain default value"
        (Just (JSON.Array (Vector.fromList [JSON.Number 123])))
        (defaultValue value)
  , testCase "entries with empty arrays as values fail because type cannot be infered" $ do
    let input = "{\"etc/entries\":{\"greeting\": []}}"
    case SUT.parseConfigSpec input of
      Left err -> case fromException err of
        Just SpecInvalidSyntaxFound{} -> assertBool "" True
        _ ->
          assertFailure $ "expecting SpecInvalidSyntaxFound error; got instead " <> show err

      Right (_configSpec :: ConfigSpec ()) ->
        assertFailure "expecting config spec parse to fail, but didn't"
  , testCase "entries with empty arrays as default values and a type do not fail" $ do
    let
      input
        = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":{\"default\":[],\"type\":\"[string]\"}}}}"
      keys = ["greeting"]

    config <- SUT.parseConfigSpec input
    case getConfigValue keys (specConfigValues config) of
      Nothing -> assertFailure
        (show keys ++ " should map to an array config value, got sub config map instead")

      Just (value :: ConfigValue ()) -> assertEqual
        "should contain default array value"
        (Just (JSON.Array (Vector.fromList [])))
        (defaultValue value)
  , testCase "entries with array of objects do not fail" $ do
    let
      input
        = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":{\"default\":[{\"hello\":\"world\"}],\"type\":\"[object]\"}}}}"
      keys = ["greeting"]

    config <- SUT.parseConfigSpec input
    case getConfigValue keys (specConfigValues config) of
      Nothing -> assertFailure
        (show keys ++ " should map to an array config value, got sub config map instead")

      Just (value :: ConfigValue ()) -> assertEqual
        "should contain default array value"
        (Just
          (JSON.Array (Vector.fromList [JSON.object ["hello" JSON..= ("world" :: Text)]]))
        )
        (defaultValue value)
  , testCase "entries can have many levels of nesting" $ do
    let input = "{\"etc/entries\":{\"english\":{\"greeting\":\"hello\"}}}"
        keys  = ["english", "greeting"]

    config <- SUT.parseConfigSpec input

    case getConfigValue keys (specConfigValues config) of
      Nothing -> assertFailure
        (show keys ++ " should map to a config value, got sub config map instead")
      Just (value :: ConfigValue ()) -> assertEqual "should contain default value"
                                                    (Just (JSON.String "hello"))
                                                    (defaultValue value)
  , testCase "spec map cannot be empty object" $ do
    let input = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":{}}}"

    case SUT.parseConfigSpec input of
      Just (result :: ConfigSpec ()) ->
        assertFailure $ "etc/spec map should not be an empty object " ++ show result
      Nothing -> assertBool "" True
  , testCase "spec map cannot be a JSON array" $ do
    let input = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":[]}}"

    case SUT.parseConfigSpec input of
      Just (result :: ConfigSpec ()) ->
        assertFailure $ "etc/spec map should not be an array " ++ show result
      Nothing -> assertBool "" True
  , testCase "spec map cannot be a JSON bool" $ do
    let input = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":true}}"

    case SUT.parseConfigSpec input of
      Just (result :: ConfigSpec ()) ->
        assertFailure $ "etc/spec map should not be a boolean " ++ show result
      Nothing -> assertBool "" True
  , testCase "spec map cannot be a JSON string" $ do
    let input = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":\"hello\"}}}"

    case SUT.parseConfigSpec input of
      Just (result :: ConfigSpec ()) ->
        assertFailure $ "etc/spec map should not be a string " ++ show result
      Nothing -> assertBool "" True
  , testCase "spec map cannot be a JSON number" $ do
    let input = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":123}}}"

    case SUT.parseConfigSpec input of
      Just (result :: ConfigSpec ()) ->
        assertFailure $ "etc/spec map should not be a number " ++ show result
      Nothing -> assertBool "" True
  , testCase "spec map cannot have any other key that is not etc/spec" $ do
    let
      input
        = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":{\"default\":\"hello\"},\"foobar\":123}}}"

    case SUT.parseConfigSpec input of
      Just (result :: ConfigSpec ()) ->
        assertFailure $ "etc/spec map should not contain more than one key " ++ show result
      Nothing -> assertBool "" True
  ]

#ifdef WITH_CLI
cli_tests :: TestTree
cli_tests =
  testGroup "cli"
  [
    testCase "cli entry settings requires an input" $ do
      let
        input = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":{\"cli\":{}}}}}"

      case SUT.parseConfigSpec input of
        Just (result :: ConfigSpec ()) ->
          assertFailure $ "cli entry should require a type " ++ show result
        Nothing ->
          assertBool "" True

  , testCase "cli option entry requires either short or long" $ do
      let
        input = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":{\"type\": \"string\", \"cli\":{\"input\":\"option\"}}}}}"

      case SUT.parseConfigSpec input of
        Just (result :: ConfigSpec ()) ->
          assertFailure $ "cli entry should require a type " ++ show result
        Nothing ->
          assertBool "" True

  , testCase "cli option entry works when setting short and type" $ do
      let
        input = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":{\"type\":\"string\",\"cli\":{\"input\":\"option\",\"short\":\"g\"}}}}}"
        keys  = ["greeting"]

      (config :: ConfigSpec ()) <- SUT.parseConfigSpec input

      let
        result = do
          value    <- getConfigValue keys (specConfigValues config)
          let valueType = configValueType value
          PlainEntry metadata <- cliEntry (configSources value)
          short <- optShort metadata
          return (short, valueType)

      case result of
        Nothing ->
          assertFailure (show keys ++ " should map to a config value, got sub config map instead")
        Just (short, valueType) -> do
          assertEqual "should contain short" "g" short
          assertEqual "should contain option type" (CVTSingle CVTString) valueType

  , testCase "cli option entry works when setting long and type" $ do
      let
        input = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":{\"type\": \"string\",\"cli\":{\"input\":\"option\",\"long\":\"greeting\"}}}}}"
        keys  = ["greeting"]

      (config :: ConfigSpec ()) <- SUT.parseConfigSpec input

      let
        result = do
          value  <- getConfigValue keys (specConfigValues config)
          let valueType = configValueType value
          PlainEntry metadata <- cliEntry (configSources value)
          long <- optLong metadata
          return (long, valueType)

      case result of
        Nothing ->
          assertFailure (show keys ++ " should map to a config value, got sub config map instead")
        Just (long, valueType) -> do
          assertEqual "should contain long" "greeting" long
          assertEqual "should contain option type" (CVTSingle CVTString) valueType

  , testCase "cli entry accepts command" $ do
      let
        input = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":{\"type\": \"string\",\"cli\":{\"input\":\"option\",\"long\":\"greeting\",\"commands\":[\"foo\"]}}}}}"
        keys  = ["greeting"]

      (config :: ConfigSpec Text) <- SUT.parseConfigSpec input

      let
        result = do
          value <- getConfigValue keys (specConfigValues config)
          let valueType = configValueType value
          CmdEntry cmd metadata <- cliEntry (configSources value)
          long <- optLong metadata
          return (cmd, long, valueType)

      case result of
        Nothing ->
          assertFailure (show config)
        Just (cmd, long, valueType) -> do
          assertEqual "should contain cmd" ["foo"] cmd
          assertEqual "should contain long" "greeting" long
          assertEqual "should contain option type" (CVTSingle CVTString) valueType

  , testCase "cli entry does not accept unrecognized keys" $ do
      let
        -- error is a typo on key command (instead of command_s_)
        input = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":{\"cli\":{\"input\":\"option\",\"long\":\"greeting\",\"type\":\"string\",\"command\":[\"foo\"]}}}}}"

      case SUT.parseConfigSpec input of
        Just (result :: ConfigSpec ()) ->
          assertFailure $ "cli entry should fail on invalid entry " ++ show result
        Nothing ->
          assertBool "" True

  ]
#endif

envvar_tests :: TestTree
envvar_tests = testGroup
  "env"
  [ testCase "env key creates an ENV source" $ do
      let
        input
          = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":{\"type\": \"string\",\"env\":\"GREETING\"}}}}"
        keys = ["greeting"]

      (config :: ConfigSpec ()) <- SUT.parseConfigSpec input

      case getConfigValue keys (specConfigValues config) of
        Nothing -> assertFailure
          (show keys ++ " should map to a config value, got sub config map instead")
        Just value -> assertEqual "should contain EnvVar value"
                                  (ConfigSources (Just "GREETING") Nothing)
                                  (configSources value)
  ]

#ifdef WITH_YAML
yaml_tests :: TestTree
yaml_tests =
  testGroup "yaml parser"
  [
    testCase "should work with Aeson instances" $ do
      let
        keys  = ["greeting"]
      path   <- getDataFileName "test/fixtures/config.spec.yaml"
      econfig <- YAML.decodeFileEither path

      case econfig of
        Left err ->
          assertFailure $ "yaml file did not have a configuration spec: " <> show err

        Right (config :: ConfigSpec ()) ->
          case getConfigValue keys (specConfigValues config) of
            Nothing ->
              assertFailure (show keys ++ " should map to a config value, got sub config map instead")

            Just value ->
              assertEqual "should contain EnvVar value"
                          (ConfigSources (Just "GREETING") Nothing)
                          (configSources value)
  ]
#endif

tests :: TestTree
tests = testGroup
  "spec"
  [ general_tests
#ifdef WITH_YAML
  , yaml_tests
#endif
  , envvar_tests
#ifdef WITH_CLI
  , cli_tests
#endif
  ]
