{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Etc.Resolver.Cli.PlainTest where

import           RIO
import qualified RIO.Set as Set

import           Data.Aeson ((.:))
import qualified Data.Aeson as JSON

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified System.Etc                as SUT
import           System.Etc.Internal.Types (CliSource (..))

resolver_tests :: TestTree
resolver_tests = testGroup
  "resolver"
  [ testCase "inputs with type string should accept numbers" $ do
    let input = mconcat
          [ "{ \"etc/entries\": {"
          , "    \"greeting\": {"
          , "      \"etc/spec\": {"
          , "        \"type\": \"string\""
          , "        , \"cli\": {"
          , "            \"input\": \"option\""
          , "          , \"short\": \"g\""
          , "          , \"long\": \"greeting\""
          , "          , \"required\": true"
          , "}}}}}"
          ]
    (spec :: SUT.ConfigSpec ()) <- SUT.parseConfigSpec input
    config                      <- SUT.resolvePlainCliPure spec "program" ["-g", "1234"]
    str                         <- SUT.getConfigValue ["greeting"] config
    assertEqual "Expected String; got something else" ("1234" :: Text) str
  , testCase "throws an error when input type does not match with spec type" $ do
    let input = mconcat
          [ "{ \"etc/entries\": {"
          , "    \"greeting\": {"
          , "      \"etc/spec\": {"
          , "        \"type\": \"[number]\""
          , "        , \"cli\": {"
          , "            \"input\": \"option\""
          , "          , \"short\": \"g\""
          , "          , \"long\": \"greeting\""
          , "          , \"required\": true"
          , "}}}}}"
          ]
    (spec :: SUT.ConfigSpec ()) <- SUT.parseConfigSpec input
    eConfig <- try $ SUT.resolvePlainCliPure spec "program" ["-g", "hello world"]

    case eConfig of
      Left SUT.CliEvalExited{} -> assertBool "" True
      _ ->
        assertFailure $ "Expecting CliEvalExited error; got this instead " <> show eConfig
  , testCase "throws an error when entry is not given and is requested" $ do
    let
      input
        = "{\"etc/entries\":{\"database\":{\"username\": {\"etc/spec\": {\"type\": \"string\", \"cli\": {\"input\": \"option\", \"long\": \"username\", \"required\": false}}}, \"password\": \"abc-123\"}}}"

    (spec :: SUT.ConfigSpec ()) <- SUT.parseConfigSpec input
    config                      <- SUT.resolvePlainCliPure spec "program" []
    let parseDb = JSON.withObject "Database"
          $ \obj -> (,) <$> obj .: "username" <*> obj .: "password"

    case SUT.getConfigValueWith parseDb ["database"] config of
      Left err -> case fromException err of
        Just (SUT.ConfigValueParserFailed inputKeys _) ->
          assertEqual "expecting key to be database, but wasn't" ["database"] inputKeys
        _ ->
          assertFailure
            $  "expecting ConfigValueParserFailed; got something else: "
            <> show err
      Right (_ :: (Text, Text)) -> assertFailure "expecting error; got none"
  ]

option_tests :: TestTree
option_tests = testGroup
  "option input"
  [ testCase "entry accepts short" $ do
    let input = mconcat
          [ "{ \"etc/entries\": {"
          , "    \"greeting\": {"
          , "      \"etc/spec\": {"
          , "        \"type\": \"string\""
          , "        , \"cli\": {"
          , "            \"input\": \"option\""
          , "          , \"short\": \"g\""
          , "          , \"long\": \"greeting\""
          , "}}}}}"
          ]
    (spec :: SUT.ConfigSpec ()) <- SUT.parseConfigSpec input
    config <- SUT.resolvePlainCliPure spec "program" ["-g", "hello cli"]

    case SUT.getAllConfigSources ["greeting"] config of
      Nothing   -> assertFailure ("expecting to get entries for greeting\n" <> show config)
      Just aSet -> assertBool
        ("expecting to see entry from env; got " <> show aSet)
        (Set.member (SUT.SomeConfigSource 3 $ CliSource "hello cli") aSet)
  , testCase "entry accepts long" $ do
    let input = mconcat
          [ "{ \"etc/entries\": {"
          , "    \"greeting\": {"
          , "      \"etc/spec\": {"
          , "        \"type\": \"string\""
          , "        , \"cli\": {"
          , "            \"input\": \"option\""
          , "          , \"short\": \"g\""
          , "          , \"long\": \"greeting\""
          , "}}}}}"
          ]
    (spec :: SUT.ConfigSpec ()) <- SUT.parseConfigSpec input
    config <- SUT.resolvePlainCliPure spec "program" ["--greeting", "hello cli"]

    case SUT.getAllConfigSources ["greeting"] config of
      Nothing   -> assertFailure ("expecting to get entries for greeting\n" <> show config)
      Just aSet -> assertBool
        ("expecting to see entry from env; got " <> show aSet)
        (Set.member (SUT.SomeConfigSource 3 $ CliSource "hello cli") aSet)
  , testCase "entry gets validated with a type" $ do
    let input = mconcat
          [ "{ \"etc/entries\": {"
          , "    \"greeting\": {"
          , "      \"etc/spec\": {"
          , "        \"type\": \"number\""
          , "        , \"cli\": {"
          , "            \"input\": \"option\""
          , "          , \"short\": \"g\""
          , "          , \"long\": \"greeting\""
          , "}}}}}"
          ]
    (spec :: SUT.ConfigSpec ()) <- SUT.parseConfigSpec input

    case SUT.resolvePlainCliPure spec "program" ["--greeting", "hello cli"] of
      Left err -> case fromException err of
        Just SUT.CliEvalExited{} -> return ()

        _ -> assertFailure ("Expecting type validation to work on cli; got " <> show err)


      Right _ -> assertFailure "Expecting type validation to work on cli"
  , testCase "entry with required false does not barf" $ do
    let input = mconcat
          [ "{ \"etc/entries\": {"
          , "    \"greeting\": {"
          , "      \"etc/spec\": {"
          , "        \"type\": \"string\""
          , "        , \"cli\": {"
          , "            \"input\": \"option\""
          , "          , \"short\": \"g\""
          , "          , \"long\": \"greeting\""
          , "          , \"required\": false"
          , "}}}}}"
          ]
    (spec :: SUT.ConfigSpec ()) <- SUT.parseConfigSpec input
    config                      <- SUT.resolvePlainCliPure spec "program" []

    case SUT.getConfigValue ["greeting"] config of
      Just aSet ->
        assertFailure ("expecting to have no entry for greeting; got\n" <> show aSet)

      (_ :: Maybe ()) -> return ()
  , testCase "entry with required fails when option not given" $ do
    let input = mconcat
          [ "{ \"etc/entries\": {"
          , "    \"greeting\": {"
          , "      \"etc/spec\": {"
          , "        \"type\": \"string\""
          , "        , \"cli\": {"
          , "            \"input\": \"option\""
          , "          , \"short\": \"g\""
          , "          , \"long\": \"greeting\""
          , "          , \"required\": true"
          , "}}}}}"
          ]
    (spec :: SUT.ConfigSpec ()) <- SUT.parseConfigSpec input
    case SUT.resolvePlainCliPure spec "program" [] of
      Left err -> case fromException err of
        Just SUT.CliEvalExited{} -> return ()

        _ ->
          assertFailure ("Expecting required validation to work on cli; got " <> show err)

      Right _ -> assertFailure "Expecting required option to fail cli resolving"
  , testCase "does parse array of numbers correctly" $ do
    let input = mconcat
          [ "{ \"etc/entries\": {"
          , "    \"greeting\": {"
          , "      \"etc/spec\": {"
          , "        \"type\": \"[number]\""
          , "        , \"cli\": {"
          , "            \"input\": \"option\""
          , "          , \"short\": \"g\""
          , "          , \"long\": \"greeting\""
          , "          , \"required\": true"
          , "}}}}}"
          ]
    (spec :: SUT.ConfigSpec ()) <- SUT.parseConfigSpec input
    config                      <- SUT.resolvePlainCliPure spec "program" ["-g", "[1,2,3]"]

    case SUT.getConfigValue ["greeting"] config of
      Right arr  -> assertEqual "did not parse an array" ([1, 2, 3] :: [Int]) arr

      (Left err) -> assertFailure ("expecting to parse an array, but didn't " <> show err)
  ]

argument_tests :: TestTree
argument_tests = testGroup
  "argument input"
  [ testCase "entry gets validated with a type" $ do
    let input = mconcat
          [ "{ \"etc/entries\": {"
          , "    \"greeting\": {"
          , "      \"etc/spec\": {"
          , "        \"type\": \"number\""
          , "        , \"cli\": {"
          , "            \"input\": \"argument\""
          , "          , \"metavar\": \"GREETING\""
          , "}}}}}"
          ]
    (spec :: SUT.ConfigSpec ()) <- SUT.parseConfigSpec input

    case SUT.resolvePlainCliPure spec "program" ["hello cli"] of
      Left err -> case fromException err of
        Just SUT.CliEvalExited{} -> return ()

        _ -> assertFailure ("Expecting type validation to work on cli; got " <> show err)

      Right _ -> assertFailure "Expecting type validation to work on cli"
  , testCase "entry with required false does not barf" $ do
    let input = mconcat
          [ "{ \"etc/entries\": {"
          , "    \"greeting\": {"
          , "      \"etc/spec\": {"
          , "        \"type\": \"string\""
          , "        , \"cli\": {"
          , "            \"input\": \"argument\""
          , "          , \"metavar\": \"GREETING\""
          , "          , \"required\": false"
          , "}}}}}"
          ]
    (spec :: SUT.ConfigSpec ()) <- SUT.parseConfigSpec input
    config                      <- SUT.resolvePlainCliPure spec "program" []

    case SUT.getConfigValue ["greeting"] config of
      (Nothing :: Maybe ()) -> return ()

      Just aSet ->
        assertFailure ("expecting to have no entry for greeting; got\n" <> show aSet)
  , testCase "entry with required fails when argument not given" $ do
    let input = mconcat
          [ "{ \"etc/entries\": {"
          , "    \"greeting\": {"
          , "      \"etc/spec\": {"
          , "        \"type\": \"string\""
          , "        , \"cli\": {"
          , "            \"input\": \"argument\""
          , "          , \"metavar\": \"GREETING\""
          , "          , \"required\": true"
          , "}}}}}"
          ]
    (spec :: SUT.ConfigSpec ()) <- SUT.parseConfigSpec input
    case SUT.resolvePlainCliPure spec "program" [] of
      Left err -> case fromException err of
        Just SUT.CliEvalExited{} -> return ()

        _ ->
          assertFailure ("Expecting required validation to work on cli; got " <> show err)

      Right _ -> assertFailure "Expecting required argument to fail cli resolving"
  ]

switch_tests :: TestTree
switch_tests = testGroup
  "switch input"
  [ testCase "fails if etc/spec.type is not bool" $ do
    let input = mconcat
          [ "{ \"etc/entries\": {"
          , "    \"greeting\": {"
          , "      \"etc/spec\": {"
          , "        \"default\": false"
          , "        , \"type\": \"string\""
          , "        , \"cli\": {"
          , "            \"input\": \"switch\""
          , "          , \"long\": \"valid\""
          , "}}}}}"
          ]
    (spec :: Either SomeException (SUT.ConfigSpec ())) <- try $ SUT.parseConfigSpec input

    case spec of
      Left err -> case fromException err of
        Just SUT.SpecInvalidSyntaxFound{} -> return ()
        _ -> assertFailure ("Expecting type validation to work on cli; got " <> show err)

      Right _ -> assertFailure "Expecting type validation to work on cli"
  , testGroup
    "when etc/spec.default is false"
    [ testCase "returns false when flag not given" $ do
      let input = mconcat
            [ "{ \"etc/entries\": {"
            , "    \"greeting\": {"
            , "      \"etc/spec\": {"
            , "        \"default\": false"
            , "        , \"type\": \"bool\""
            , "        , \"cli\": {"
            , "            \"input\": \"switch\""
            , "          , \"long\": \"valid\""
            , "}}}}}"
            ]
      (spec :: SUT.ConfigSpec ()) <- SUT.parseConfigSpec input

      case SUT.resolvePlainCliPure spec "program" [] of
        Left err ->
          assertFailure ("Expecting default to work on cli; but didn't: " <> show err)

        Right config -> do
          greeting <- SUT.getConfigValue ["greeting"] config
          assertBool "Expecting default to be false, but wasn't" (not greeting)
    , testCase "returns true when flag given" $ do
      let input = mconcat
            [ "{ \"etc/entries\": {"
            , "    \"greeting\": {"
            , "      \"etc/spec\": {"
            , "        \"default\": false"
            , "        , \"type\": \"bool\""
            , "        , \"cli\": {"
            , "            \"input\": \"switch\""
            , "          , \"long\": \"valid\""
            , "}}}}}"
            ]
      (spec :: SUT.ConfigSpec ()) <- SUT.parseConfigSpec input

      case SUT.resolvePlainCliPure spec "program" ["--valid"] of
        Left err ->
          assertFailure ("Expecting default to work on cli; but didn't: " <> show err)

        Right config -> do
          greeting <- SUT.getConfigValue ["greeting"] config
          assertBool "Expecting result to be true, but wasn't" greeting
    ]
  -- TODO: This testcase is failing, and it is because the optparse-applicative
  -- API _always_ returns a value, if the flag is not present, it will return
  -- false. Once refactoring of parser is done, we need to make use of the
  -- default value to change the behavior of the optparse-applicative API to
  -- return the appropiate result
  -- , testGroup "when default is true"
  --   [
  --     testCase "entry should use default when not specified (true case)" $ do
  --     let input = mconcat
  --           [ "{ \"etc/entries\": {"
  --           , "    \"greeting\": {"
  --           , "      \"etc/spec\": {"
  --           , "        \"default\": true"
  --           , "        , \"type\": \"bool\""
  --           , "        , \"cli\": {"
  --           , "            \"input\": \"switch\""
  --           , "          , \"long\": \"invalid\""
  --           , "}}}}}"
  --           ]
  --     (spec :: SUT.ConfigSpec ()) <- SUT.parseConfigSpec input

  --     case SUT.resolvePlainCliPure spec "program" [] of
  --       Left err ->
  --         assertFailure ("Expecting default to work on cli; but didn't: " <> show err)

  --       Right config -> do
  --         greeting <- SUT.getConfigValue ["greeting"] config
  --         assertBool "Expecting default to be true, but wasn't" greeting
  --   ]
  ]

tests :: TestTree
tests = testGroup "plain" [resolver_tests, option_tests, argument_tests, switch_tests]
