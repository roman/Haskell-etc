{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Etc.Resolver.FileTest (tests) where

import           Protolude

import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.HUnit           (assertBool, assertEqual,
                                             assertFailure, testCase)

import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text

import           Paths_etc_spec             (getDataFileName)

import           Etc                        (getConfigSources)
import           Etc.Resolver.File          (resolveFiles)
import           Etc.Spec.Types             (ConfigSpec)
import           Etc.Types

import           Etc.Spec.JSON              (parseConfigSpec)


tests :: TestTree
tests =
  testGroup "file"
  [
    testCase "supports json, yaml and yml extensions" $ do
      jsonFilepath <- getDataFileName "test/fixtures/config.json"
      yamlFilepath <- getDataFileName "test/fixtures/config.yaml"
      ymlFilepath  <- getDataFileName "test/fixtures/config.yml"
      let
        input =
          LB8.concat
            [
              "{\"etc/filepaths\": ["
            , "\"" <> LB8.pack jsonFilepath <> "\", "
            , "\"" <> LB8.pack yamlFilepath <> "\", "
            , "\"" <> LB8.pack ymlFilepath <> "\"]"
            ]

      print input
      (spec :: ConfigSpec ()) <- parseConfigSpec input
      config <- resolveFiles spec
      case getConfigSources ["greeting"] config of
        Nothing ->
          assertFailure ("expecting to get entries for greeting (check fixtures)\n"
                         <> show config)
        Just set -> do
          assertBool "expecting to see entry from json config file"
                     (Set.member (File 0 (Text.pack jsonFilepath) "hello json") set)

          assertBool "expecting to see entry from yaml config file"
                     (Set.member (File 1 (Text.pack jsonFilepath) "hello yaml") set)

          assertBool "expecting to see entry from yaml config file"
                     (Set.member (File 2 (Text.pack jsonFilepath) "hello yml") set)

  -- , testCase "does not support any other file extension" $ do
  --     error "pending"

  -- , testCase "does not fail if file doesn't exist" $ do
  --     error "pending"
  ]
