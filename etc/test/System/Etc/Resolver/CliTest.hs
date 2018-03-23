{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Etc.Resolver.CliTest where

import Test.Tasty (TestTree, testGroup)

import qualified System.Etc.Resolver.Cli.CommandTest
import qualified System.Etc.Resolver.Cli.PlainTest


tests :: TestTree
tests = testGroup
  "cli"
  [ System.Etc.Resolver.Cli.CommandTest.tests
  , System.Etc.Resolver.Cli.PlainTest.tests
  ]
