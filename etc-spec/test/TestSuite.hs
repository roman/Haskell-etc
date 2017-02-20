{-# LANGUAGE NoImplicitPrelude   #-}
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

import qualified Data.HashMap.Strict          as HashMap

import           Etc.Spec.JSON                (parseConfigSpec)
import           Etc.Spec.Types


getConfigValue :: [Text] -> HashMap Text (ConfigValue cmd) -> Maybe (ConfigSources cmd)
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
            Just (configSources value)
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

  , testCase "entries that finish with raw values use them as default" $ do
      let
        input = "{\"etc/entries\":{\"greeting\":{}}}"

      case parseConfigSpec input of
        Just (result :: ConfigSpec ()) ->
          assertFailure $ "entries should not accept empty maps as values " ++ show result
        Nothing ->
          assertBool "" True

  ]

envvar_tests :: TestTree
envvar_tests =
  testGroup "env"
  [
    testCase "env key is present" $ do
      let
        input = "{\"etc/entries\":{\"greeting\":{\"etc/spec\":{\"env\":\"GREETING\"}}}}"
        keys  = ["greeting"]

      (config :: ConfigSpec ()) <- parseConfigSpec input

      case getConfigValue keys (specConfigValues config) of
        Nothing ->
          assertFailure (show keys ++ " should map to a config value, got sub config map instead")
        Just sources ->
          assertEqual "should contain EnvVar value"
                      (ConfigSources (Just "GREETING") Nothing)
                      sources
  ]

main :: IO ()
main =
  defaultMainWithIngredients
    [ rerunningTests [listingTests, consoleTestReporter] ]
    (testGroup "etc-spec" [general_tests, envvar_tests])
