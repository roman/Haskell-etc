{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import qualified Options.Applicative as Opt
import qualified UB.Config as Config

import UB.Prelude

main :: IO ()
main = do
  spec <-
    Config.readConfigSpec "test/fixtures/spec.json"

  envConfig  <-
    Config.resolveEnvVars spec

  fileConfig <-
    Config.readConfigFromFiles [ "test/fixtures/one.json"
                               , "test/fixtures/two.json" ]

  let
    configParser =
      Config.configSpecToOptParser spec

    programParser =
      Opt.info
        (Opt.helper <*> configParser)
        (Opt.fullDesc
        `mappend` Opt.progDesc "An Example of all configurations"
        `mappend` Opt.header "example - a way to test all configuration sources")

  optConfig <- Opt.execParser programParser
  print (envConfig
         <> optConfig
         <> fileConfig)
