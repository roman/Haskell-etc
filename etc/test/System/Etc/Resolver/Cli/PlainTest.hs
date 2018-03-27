{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Etc.Resolver.Cli.PlainTest where

import           RIO
import qualified RIO.Set as Set

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

import qualified System.Etc as SUT

option_tests :: TestTree
option_tests = testGroup
  "option input"
  [ testCase "entry accepts short" $ do
    let input = mconcat
          [ "{ \"etc/entries\": {"
          , "    \"greeting\": {"
          , "      \"etc/spec\": {"
          , "        \"cli\": {"
          , "          \"input\": \"option\""
          , "        , \"short\": \"g\""
          , "        , \"long\": \"greeting\""
          , "        , \"type\": \"string\""
          , "}}}}}"
          ]
    (spec :: SUT.ConfigSpec ()) <- SUT.parseConfigSpec input
    config                  <- SUT.resolvePlainCliPure spec "program" ["-g", "hello cli"]

    case SUT.getAllConfigSources ["greeting"] config of
      Nothing   -> assertFailure ("expecting to get entries for greeting\n" <> show config)
      Just aSet -> assertBool ("expecting to see entry from env; got " <> show aSet)
                              (Set.member (SUT.Cli "hello cli") aSet)
  , testCase "entry accepts long" $ do
    let input = mconcat
          [ "{ \"etc/entries\": {"
          , "    \"greeting\": {"
          , "      \"etc/spec\": {"
          , "        \"cli\": {"
          , "          \"input\": \"option\""
          , "        , \"short\": \"g\""
          , "        , \"long\": \"greeting\""
          , "        , \"type\": \"string\""
          , "}}}}}"
          ]
    (spec :: SUT.ConfigSpec ()) <- SUT.parseConfigSpec input
    config <- SUT.resolvePlainCliPure spec "program" ["--greeting", "hello cli"]

    case SUT.getAllConfigSources ["greeting"] config of
      Nothing   -> assertFailure ("expecting to get entries for greeting\n" <> show config)
      Just aSet -> assertBool ("expecting to see entry from env; got " <> show aSet)
                              (Set.member (SUT.Cli "hello cli") aSet)
  , testCase "entry gets validated with a type" $ do
    let input = mconcat
          [ "{ \"etc/entries\": {"
          , "    \"greeting\": {"
          , "      \"etc/spec\": {"
          , "        \"cli\": {"
          , "          \"input\": \"option\""
          , "        , \"short\": \"g\""
          , "        , \"long\": \"greeting\""
          , "        , \"type\": \"number\""
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
          , "        \"cli\": {"
          , "          \"input\": \"option\""
          , "        , \"short\": \"g\""
          , "        , \"long\": \"greeting\""
          , "        , \"type\": \"string\""
          , "        , \"required\": false"
          , "}}}}}"
          ]
    (spec :: SUT.ConfigSpec ()) <- SUT.parseConfigSpec input
    config                  <- SUT.resolvePlainCliPure spec "program" []

    case SUT.getConfigValue ["greeting"] config of
      Just aSet ->
        assertFailure ("expecting to have no entry for greeting; got\n" <> show aSet)

      (_ :: Maybe ()) -> return ()
  , testCase "entry with required fails when option not given" $ do
    let input = mconcat
          [ "{ \"etc/entries\": {"
          , "    \"greeting\": {"
          , "      \"etc/spec\": {"
          , "        \"cli\": {"
          , "          \"input\": \"option\""
          , "        , \"short\": \"g\""
          , "        , \"long\": \"greeting\""
          , "        , \"type\": \"string\""
          , "        , \"required\": true"
          , "}}}}}"
          ]
    (spec :: SUT.ConfigSpec ()) <- SUT.parseConfigSpec input
    case SUT.resolvePlainCliPure spec "program" [] of
      Left err -> case fromException err of
        Just SUT.CliEvalExited{} -> return ()

        _ ->
          assertFailure ("Expecting required validation to work on cli; got " <> show err)

      Right _ -> assertFailure "Expecting required option to fail cli resolving"
  ]

argument_tests :: TestTree
argument_tests = testGroup
  "argument input"
  [ testCase "entry gets validated with a type" $ do
    let input = mconcat
          [ "{ \"etc/entries\": {"
          , "    \"greeting\": {"
          , "      \"etc/spec\": {"
          , "        \"cli\": {"
          , "          \"input\": \"argument\""
          , "        , \"type\": \"number\""
          , "        , \"metavar\": \"GREETING\""
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
          , "        \"cli\": {"
          , "          \"input\": \"argument\""
          , "        , \"type\": \"string\""
          , "        , \"metavar\": \"GREETING\""
          , "        , \"required\": false"
          , "}}}}}"
          ]
    (spec :: SUT.ConfigSpec ()) <- SUT.parseConfigSpec input
    config                  <- SUT.resolvePlainCliPure spec "program" []

    case SUT.getConfigValue ["greeting"] config of
      (Nothing :: Maybe ()) -> return ()

      Just aSet ->
        assertFailure ("expecting to have no entry for greeting; got\n" <> show aSet)
  , testCase "entry with required fails when argument not given" $ do
    let input = mconcat
          [ "{ \"etc/entries\": {"
          , "    \"greeting\": {"
          , "      \"etc/spec\": {"
          , "        \"cli\": {"
          , "          \"input\": \"argument\""
          , "        , \"type\": \"string\""
          , "        , \"metavar\": \"GREETING\""
          , "        , \"required\": true"
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

tests :: TestTree
tests = testGroup "plain" [option_tests, argument_tests]
