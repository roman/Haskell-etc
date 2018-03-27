{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Etc.ConfigTest where

import RIO

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

import qualified System.Etc as SUT

tests :: TestTree
tests = testGroup
  "System.Etc.Config"
  [ testCase "InvalidConfiguration error contains key when types don't match" $ do
      let input = "{\"etc/entries\":{\"greeting\":123}}"

      (spec :: SUT.ConfigSpec ()) <- SUT.parseConfigSpec input
      let config = SUT.resolveDefault spec
      case SUT.getConfigValue ["greeting"] config of
        Left err -> case fromException err of
          Just (SUT.InvalidConfiguration key _) ->
            assertEqual "expecting key to be greeting, but wasn't" (Just "greeting") key
          _ ->
            assertFailure
              $  "expecting InvalidConfiguration; got something else: "
              <> show err
        Right (_ :: Text) -> assertFailure "expecting error; got none"
  ]
