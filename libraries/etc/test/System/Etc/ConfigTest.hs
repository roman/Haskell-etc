{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Etc.ConfigTest where

import Protolude

import Data.Aeson       ((.:))
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

import qualified Data.Aeson       as JSON
import qualified Data.Aeson.Types as JSON (Parser, typeMismatch)

import System.Etc.Internal.Config ()
import System.Etc.Internal.Resolver.Default (resolveDefault)
import System.Etc.Internal.Resolver.Env     (resolveEnvPure)
import System.Etc.Internal.Types            (IConfig (..))
import System.Etc.Spec                      (ConfigSpec, parseConfigSpec)

tests :: TestTree
tests =
  testGroup "config"
  [
    testCase "it parses JSON with resolved values" $ do
      let
        input =
          "{\"etc/entries\":{\"nested\":{\"this\":{\"one\":{\"etc/spec\":{\"env\":\"ONE\",\"default\":\"default_one\"}}}, \"that\":{\"two\":{\"etc/spec\":{\"env\":\"TWO\",\"default\": \"default_two\"}}}}}}"

      let
        parseThis value =
          case value of
            JSON.Object obj ->
              obj .: "one"
            _ ->
              JSON.typeMismatch "this" value

        parseThat value =
          case value of
            JSON.Object obj ->
              obj .: "two"
            _ ->
              JSON.typeMismatch "that" value

        parseNested :: JSON.Value -> JSON.Parser (Text, Text)
        parseNested value =
          case value of
            JSON.Object obj -> do
              thisVal <- obj .: "this" >>= parseThis
              thatVal <- obj .: "that" >>= parseThat
              return (thisVal, thatVal)
            _ ->
              JSON.typeMismatch "nested" value

      (spec :: ConfigSpec ()) <- parseConfigSpec input

      let
        configEnv =
          resolveEnvPure spec [("ONE", "env_one")]

        configDefault =
          resolveDefault spec

        config =
          configDefault `mappend` configEnv

      case getConfigValueWith parseNested ["nested"] config of
        Nothing ->
          assertFailure "Expected JSON parser to work, but didn't"

        Just (oneVal, twoVal) -> do
          assertEqual "Expected value to be resolved from Env source"
                      "env_one" oneVal
          assertEqual "Expected value to be resolved from Default source"
                       "default_two" twoVal

  ]
