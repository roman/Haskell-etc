{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Etc.Resolver.CliTest where

import           Test.Tasty                 (TestTree, testGroup)

import qualified Etc.Resolver.Cli.PlainTest


tests :: TestTree
tests =
  testGroup "cli"
  [
    Etc.Resolver.Cli.PlainTest.tests
  ]
