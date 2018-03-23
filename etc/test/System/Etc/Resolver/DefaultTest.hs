{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Etc.Resolver.DefaultTest (tests) where

import RIO
import qualified RIO.Set as Set

import qualified Data.Aeson       as JSON
import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (assertBool, assertFailure, testCase)



import System.Etc

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
        Just aSet ->
          assertBool ("expecting to see entry from env; got " <> show aSet)
                   (Set.member (Default "hello default") aSet)

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
        Just aSet ->
          assertBool ("expecting to see entry from env; got " <> show aSet)
                   (Set.member (Default "hello default") aSet)

  , testCase "default can be a null JSON value" $ do
      let
        input =
          mconcat
            [
              "{\"etc/entries\": {"
            , " \"greeting\": null}}"
            ]
      (spec :: ConfigSpec ()) <- parseConfigSpec input

      let
        config =
            resolveDefault spec

      case getAllConfigSources ["greeting"] config of
        Nothing ->
          assertFailure ("expecting to get entries for greeting\n"
                         <> show config)
        Just aSet ->
          assertBool ("expecting to see entry from env; got " <> show aSet)
                     (Set.member (Default JSON.Null) aSet)


  ]
