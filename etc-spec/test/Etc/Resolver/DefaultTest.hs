{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Etc.Resolver.DefaultTest (tests) where

import Protolude

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)


import qualified Data.Set as Set

import Etc

tests :: TestTree
tests =
  testGroup "default"
  [
    testCase "default is used when defined on spec" $ do
      let
        input =
          mconcat
            [
              "{\"etc/entries\": {"
            , " \"greeting\": { \"etc/spec\": { \"default\": \"hello default\" }}}}"
            ]
      (spec :: ConfigSpec ()) <- parseConfigSpec input

      let
        config =
            resolveDefault spec

      case getAllConfigSources ["greeting"] config of
        Nothing ->
          assertFailure ("expecting to get entries for greeting\n"
                         <> show config)
        Just set ->
          assertBool ("expecting to see entry from env; got " <> show set)
                   (Set.member (Default "hello default") set)

  , testCase "default can be raw JSON value on entries spec" $ do
      let
        input =
          mconcat
            [
              "{\"etc/entries\": {"
            , " \"greeting\": \"hello default\"}}"
            ]
      (spec :: ConfigSpec ()) <- parseConfigSpec input

      let
        config =
            resolveDefault spec

      case getAllConfigSources ["greeting"] config of
        Nothing ->
          assertFailure ("expecting to get entries for greeting\n"
                         <> show config)
        Just set ->
          assertBool ("expecting to see entry from env; got " <> show set)
                   (Set.member (Default "hello default") set)

  ]
