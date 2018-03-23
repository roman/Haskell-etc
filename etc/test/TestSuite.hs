{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import RIO

import Test.Tasty         (defaultMainWithIngredients, testGroup)
import Test.Tasty.Runners (consoleTestReporter, listingTests)

import qualified System.Etc.Resolver.DefaultTest
import qualified System.Etc.Resolver.EnvTest
import qualified System.Etc.Resolver.FileTest
import qualified System.Etc.SpecTest

#ifdef WITH_CLI
import qualified System.Etc.Resolver.CliTest
#endif

#ifdef WITH_EXTRA
import qualified System.Etc.Extra.EnvMisspellTest
#endif

main :: IO ()
main =
  defaultMainWithIngredients
    [ listingTests, consoleTestReporter ]
    (testGroup "etc" [ System.Etc.SpecTest.tests
                     , System.Etc.Resolver.DefaultTest.tests
                     , System.Etc.Resolver.FileTest.tests
                     , System.Etc.Resolver.EnvTest.tests
#ifdef WITH_CLI
                     , System.Etc.Resolver.CliTest.tests
#endif

#ifdef WITH_EXTRA
                     , System.Etc.Extra.EnvMisspellTest.tests
#endif
                     ])
