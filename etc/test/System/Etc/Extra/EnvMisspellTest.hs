{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Etc.Extra.EnvMisspellTest where

import           RIO
import qualified RIO.Vector as Vector

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

import System.Etc

tests :: TestTree
tests = testGroup
  "env misspells"
  [ testCase "it warns when misspell is present" $ do
      let
        input = mconcat
          [ "{\"etc/entries\": {"
          , " \"greeting\": { \"etc/spec\":{\"type\":\"string\",\"env\": \"GREETING\"}}}}"
          ]

      (spec :: ConfigSpec ()) <- parseConfigSpec input

      let result0 = getEnvMisspellingsPure spec ["GREEING"]

      case result0 Vector.!? 0 of
        Nothing     -> assertFailure "expecting to get a warning for typo"
        Just result -> assertEqual "expecting to get typo for key GREETING"
                                   (EnvMisspell "GREEING" "GREETING")
                                   result
  ]
