{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Protolude

import           Test.Tasty                   (defaultMainWithIngredients,
                                               testGroup)
import           Test.Tasty.Ingredients.Rerun (rerunningTests)
import           Test.Tasty.Runners           (consoleTestReporter,
                                               listingTests)

import qualified Etc.SpecTest
import qualified Etc.Resolver.DefaultTest
import qualified Etc.Resolver.FileTest
import qualified Etc.Resolver.EnvTest

#ifdef WITH_CLI
import qualified Etc.Resolver.CliTest
#endif

main :: IO ()
main =
  defaultMainWithIngredients
    [ rerunningTests [listingTests, consoleTestReporter] ]
    (testGroup "etc" [ Etc.SpecTest.tests
                     , Etc.Resolver.DefaultTest.tests
                     , Etc.Resolver.FileTest.tests
                     , Etc.Resolver.EnvTest.tests
#ifdef WITH_CLI
                     , Etc.Resolver.CliTest.tests
#endif
                     ])
