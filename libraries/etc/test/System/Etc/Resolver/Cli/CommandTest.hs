{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Etc.Resolver.Cli.CommandTest where

import Protolude

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Data.Set as Set

import System.Etc

with_command_option_tests :: TestTree
with_command_option_tests =
  testGroup "option input"
  [
    testCase "entry accepts short" $ do
      let
        input =
          mconcat
            [
              "{ \"etc/cli\": {"
            , "    \"desc\": \"\""
            , "  , \"header\": \"\""
            , "  , \"commands\": {"
            , "      \"test\": {\"header\": \"\", \"desc\": \"\"}}}"
            , ", \"etc/entries\": {"
            , "    \"greeting\": {"
            , "      \"etc/spec\": {"
            , "        \"cli\": {"
            , "          \"input\": \"option\""
            , "        , \"short\": \"g\""
            , "        , \"long\": \"greeting\""
            , "        , \"type\": \"string\""
            , "        , \"commands\": [\"test\"]"
            , "}}}}}"
            ]
      (spec :: ConfigSpec Text) <- parseConfigSpec input
      (cmd, config) <- resolveCommandCliPure spec "program" ["test", "-g", "hello cli"]

      assertEqual "invalid command output" "test" cmd

      case getAllConfigSources ["greeting"] config of
        Nothing ->
          assertFailure ("expecting to get entries for greeting\n"
                         <> show config)
        Just set ->
          assertBool ("expecting to see entry from env; got " <> show set)
                   (Set.member (Cli "hello cli") set)

  , testCase "entry accepts long" $ do
      let
        input =
          mconcat
            [
              "{ \"etc/cli\": {"
            , "    \"desc\": \"\""
            , "  , \"header\": \"\""
            , "  , \"commands\": {"
            , "      \"test\": {\"header\": \"\", \"desc\": \"\"}}}"
            , ", \"etc/entries\": {"
            , "    \"greeting\": {"
            , "      \"etc/spec\": {"
            , "        \"cli\": {"
            , "          \"input\": \"option\""
            , "        , \"short\": \"g\""
            , "        , \"long\": \"greeting\""
            , "        , \"type\": \"string\""
            , "        , \"commands\": [\"test\"]"
            , "}}}}}"
            ]
      (spec :: ConfigSpec Text) <- parseConfigSpec input

      (cmd, config) <- resolveCommandCliPure spec "program" ["test", "--greeting", "hello cli"]

      assertEqual "invalid command output" "test" cmd

      case getAllConfigSources ["greeting"] config of
        Nothing ->
          assertFailure ("expecting to get entries for greeting\n"
                         <> show config)
        Just set ->
          assertBool ("expecting to see entry from env; got " <> show set)
                   (Set.member (Cli "hello cli") set)

  , testCase "entry gets validated with a type" $ do
      let
        input =
          mconcat
            [
              "{ \"etc/cli\": {"
            , "    \"desc\": \"\""
            , "  , \"header\": \"\""
            , "  , \"commands\": {"
            , "      \"test\": {\"header\": \"\", \"desc\": \"\"}}}"
            , ", \"etc/entries\": {"
            , "    \"greeting\": {"
            , "      \"etc/spec\": {"
            , "        \"cli\": {"
            , "          \"input\": \"option\""
            , "        , \"short\": \"g\""
            , "        , \"long\": \"greeting\""
            , "        , \"type\": \"number\""
            , "        , \"commands\": [\"test\"]"
            , "}}}}}"
            ]
      (spec :: ConfigSpec Text) <- parseConfigSpec input

      case resolveCommandCliPure spec "program" ["test", "--greeting", "hello cli"] of
        Left err ->
          case fromException err of
            Just CliEvalExited {} ->
              return ()

            _ ->
              assertFailure ("Expecting type validation to work on cli; got "
                             <> show err)


        Right _ ->
          assertFailure "Expecting type validation to work on cli"

  , testCase "entry with required false does not barf" $ do
      let
        input =
          mconcat
            [
              "{ \"etc/cli\": {"
            , "    \"desc\": \"\""
            , "  , \"header\": \"\""
            , "  , \"commands\": {"
            , "      \"test\": {\"header\": \"\", \"desc\": \"\"}}}"
            , ", \"etc/entries\": {"
            , "    \"greeting\": {"
            , "      \"etc/spec\": {"
            , "        \"cli\": {"
            , "          \"input\": \"option\""
            , "        , \"short\": \"g\""
            , "        , \"long\": \"greeting\""
            , "        , \"type\": \"string\""
            , "        , \"required\": false"
            , "        , \"commands\": [\"test\"]"
            , "}}}}}"
            ]
      (spec :: ConfigSpec Text) <- parseConfigSpec input
      (cmd, config) <- resolveCommandCliPure spec "program" ["test"]

      assertEqual "invalid command output" "test" cmd

      case getConfigValue ["greeting"] config of
        Just set ->
          assertFailure ("expecting to have no entry for greeting; got\n"
                       <> show set)

        (_ :: Maybe ()) ->
          return ()

  , testCase "entry with required fails when option not given" $ do
      let
        input =
          mconcat
            [
              "{ \"etc/cli\": {"
            , "    \"desc\": \"\""
            , "  , \"header\": \"\""
            , "  , \"commands\": {"
            , "      \"test\": {\"header\": \"\", \"desc\": \"\"}}}"
            , ", \"etc/entries\": {"
            , "    \"greeting\": {"
            , "      \"etc/spec\": {"
            , "        \"cli\": {"
            , "          \"input\": \"option\""
            , "        , \"short\": \"g\""
            , "        , \"long\": \"greeting\""
            , "        , \"type\": \"string\""
            , "        , \"required\": true"
            , "        , \"commands\": [\"test\"]"
            , "}}}}}"
            ]
      (spec :: ConfigSpec Text) <- parseConfigSpec input
      case resolveCommandCliPure spec "program" ["test"] of
        Left err ->
          case fromException err of
            Just CliEvalExited {} ->
              return ()

            _ ->
              assertFailure ("Expecting required validation to work on cli; got "
                             <> show err)

        Right _ ->
          assertFailure "Expecting required option to fail cli resolving"
  ]

with_command_argument_tests :: TestTree
with_command_argument_tests =
  testGroup "argument input"
  [
    testCase "entry gets validated with a type" $ do
      let
        input =
          mconcat
            [
              "{ \"etc/cli\": {"
            , "    \"desc\": \"\""
            , "  , \"header\": \"\""
            , "  , \"commands\": {"
            , "      \"test\": {\"header\": \"\", \"desc\": \"\"}}}"
            , ", \"etc/entries\": {"
            , "    \"greeting\": {"
            , "      \"etc/spec\": {"
            , "        \"cli\": {"
            , "          \"input\": \"argument\""
            , "        , \"type\": \"number\""
            , "        , \"metavar\": \"GREETING\""
            , "        , \"commands\": [\"test\"]"
            , "}}}}}"
            ]
      (spec :: ConfigSpec Text) <- parseConfigSpec input

      case resolveCommandCliPure spec "program" ["test", "hello cli"] of
        Left err ->
          case fromException err of
            Just CliEvalExited {} ->
              return ()

            _ ->
              assertFailure ("Expecting type validation to work on cli; got "
                             <> show err)

        Right _ ->
          assertFailure "Expecting type validation to work on cli"

  , testCase "entry with required false does not barf" $ do
      let
        input =
          mconcat
            [
              "{ \"etc/cli\": {"
            , "    \"desc\": \"\""
            , "  , \"header\": \"\""
            , "  , \"commands\": {"
            , "      \"test\": {\"header\": \"\", \"desc\": \"\"}}}"
            , ", \"etc/entries\": {"
            , "    \"greeting\": {"
            , "      \"etc/spec\": {"
            , "        \"cli\": {"
            , "          \"input\": \"argument\""
            , "        , \"type\": \"string\""
            , "        , \"metavar\": \"GREETING\""
            , "        , \"required\": false"
            , "        , \"commands\": [\"test\"]"
            , "}}}}}"
            ]
      (spec :: ConfigSpec Text) <- parseConfigSpec input
      (cmd, config) <- resolveCommandCliPure spec "program" ["test"]

      assertEqual "invalid command output" "test" cmd

      case getConfigValue ["greeting"] config of
        (Nothing :: Maybe ()) ->
          return ()

        Just set ->
          assertFailure ("expecting to have no entry for greeting; got\n"
                       <> show set)

  , testCase "entry with required fails when argument not given" $ do
      let
        input =
          mconcat
            [
              "{ \"etc/cli\": {"
            , "    \"desc\": \"\""
            , "  , \"header\": \"\""
            , "  , \"commands\": {"
            , "      \"test\": {\"header\": \"\", \"desc\": \"\"}}}"
            , ", \"etc/entries\": {"
            , "    \"greeting\": {"
            , "      \"etc/spec\": {"
            , "        \"cli\": {"
            , "          \"input\": \"argument\""
            , "        , \"type\": \"string\""
            , "        , \"metavar\": \"GREETING\""
            , "        , \"required\": true"
            , "        , \"commands\": [\"test\"]"
            , "}}}}}"
            ]
      (spec :: ConfigSpec Text) <- parseConfigSpec input
      case resolveCommandCliPure spec "program" ["test"] of
        Left err ->
          case fromException err of
            Just CliEvalExited {} ->
              return ()

            _ ->
              assertFailure ("Expecting required validation to work on cli; got "
                             <> show err)

        Right _ ->
          assertFailure "Expecting required argument to fail cli resolving"

  , testCase "supports same cli input on multiple arguments" $ do
      let
        input =
          mconcat
            [
              "{ \"etc/cli\": {"
            , "    \"desc\": \"\""
            , "  , \"header\": \"\""
            , "  , \"commands\": {"
            , "      \"test\": {\"header\": \"\", \"desc\": \"\"}"
            , "    , \"other\": {\"header\": \"\", \"desc\": \"\"}}}"
            , ", \"etc/entries\": {"
            , "    \"greeting\": {"
            , "      \"etc/spec\": {"
            , "        \"cli\": {"
            , "          \"input\": \"option\""
            , "        , \"short\": \"g\""
            , "        , \"type\": \"string\""
            , "        , \"metavar\": \"GREETING\""
            , "        , \"required\": false"
            , "        , \"commands\": [\"test\", \"other\"]"
            , "}}}}}"
            ]
      (spec :: ConfigSpec Text) <- parseConfigSpec input

      (cmd1, config1) <- resolveCommandCliPure spec "program" ["test", "-g", "hello"]
      (cmd2, config2) <- resolveCommandCliPure spec "program" ["other", "-g", "hello"]

      assertEqual "" "test" cmd1
      assertEqual "" "other" cmd2
      assertEqual "" config1 config2
  ]


with_command :: TestTree
with_command =
  testGroup "when command given"
  [
    with_command_option_tests
  , with_command_argument_tests
  ]

without_command :: TestTree
without_command =
  testCase "fails when command not given" $ do
    let
      input =
        mconcat
          [
            "{ \"etc/cli\": {"
          , "    \"desc\": \"\""
          , "  , \"header\": \"\""
          , "  , \"commands\": {"
          , "      \"test\": {\"header\": \"\", \"desc\": \"\"}}}"
          , ", \"etc/entries\": {"
          , "    \"greeting\": {"
          , "      \"etc/spec\": {"
          , "        \"cli\": {"
          , "          \"input\": \"option\""
          , "        , \"short\": \"g\""
          , "        , \"type\": \"string\""
          , "        , \"metavar\": \"GREETING\""
          , "        , \"required\": true"
          , "        , \"commands\": [\"test\"]"
          , "}}}}}"
          ]
    (spec :: ConfigSpec Text) <- parseConfigSpec input
    case resolveCommandCliPure spec "program" [] of
      Left err ->
        case fromException err of
          Just CliEvalExited {} ->
            return ()

          _ ->
            assertFailure ("Expecting sub-command to be required; got "
                           <> show err)
      Right _ ->
        assertFailure "Expecting sub-command to be required; it wasn't"



tests :: TestTree
tests =
  testGroup "command"
  [
    with_command
  , without_command
  ]
