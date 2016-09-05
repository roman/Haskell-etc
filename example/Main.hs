{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import qualified Options.Applicative as Opt
import qualified UB.Config as Config

import UB.Prelude

main :: IO ()
main = do
  config <-
    Config.readConfigFromAllSources
      "test/fixtures/spec.json"
      [ "test/fixtures/one.json"
      , "test/fixtures/two.json" ]
      (Opt.fullDesc
       `mappend` Opt.progDesc "An Example of all configurations sources"
       `mappend` Opt.header "example - a way to test all configuration sources")

  print config
