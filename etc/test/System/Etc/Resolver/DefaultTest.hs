{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Etc.Resolver.DefaultTest (tests) where

import           RIO
import qualified RIO.Set as Set

import qualified Data.Aeson       as JSON
import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (assertBool, assertFailure, testCase)

import System.Etc
import System.Etc.Internal.Types (DefaultSource (..))

assertDefaultValue :: Config -> [Text] -> Value JSON.Value -> IO ()
assertDefaultValue config keys val = case getAllConfigSources keys config of
  Nothing   -> assertFailure ("expecting to get entries for greeting\n" <> show config)
  Just aSet -> assertBool ("expecting to see entry from env; got " <> show aSet)
                          (Set.member (SomeConfigSource 0 $ DefaultSource val) aSet)

tests :: TestTree
tests = testGroup
  "default"
  [ testCase "default is used when defined on spec" $ do
    let input = mconcat
          [ "{\"etc/entries\": {"
          , " \"greeting\": { \"etc/spec\": { \"default\": \"hello default 1\" }}}}"
          ]

    (spec :: ConfigSpec ()) <- parseConfigSpec input
    let config = resolveDefault spec
    assertDefaultValue config ["greeting"] "hello default 1"
  , testCase "default can be raw JSON value on entries spec" $ do
    let input = mconcat ["{\"etc/entries\": {\"greeting\": \"hello default 2\"}}"]
    (spec :: ConfigSpec ()) <- parseConfigSpec input

    let config = resolveDefault spec
    assertDefaultValue config ["greeting"] "hello default 2"
  , testCase "default can be a null JSON value" $ do
    let
      input = mconcat
        [ "{\"etc/entries\":{\"greeting\":{\"etc/spec\":{\"type\":\"number\",\"default\":null}}}}"
        ]
    (spec :: ConfigSpec ()) <- parseConfigSpec input

    let config = resolveDefault spec

    case getAllConfigSources ["greeting"] config of
      Nothing   -> assertFailure ("expecting to get entries for greeting\n" <> show config)
      Just aSet -> assertBool
        ("expecting to see entry from env; got " <> show aSet)
        (Set.member (SomeConfigSource 0 $ DefaultSource $ Plain JSON.Null) aSet)
  ]
