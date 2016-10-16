{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import qualified System.Etc as Config

import UB.Prelude

main :: IO ()
main = do
  configSpec <- Config.readConfigSpec "example/resources/spec.json"

  configFiles     <- Config.resolveFiles configSpec
  configEnv       <- Config.resolveEnvVars configSpec
  configOptParser <- Config.resolveOptParser configSpec

  let
    config =
      configFiles <> configEnv <> configOptParser

  Config.printPrettyConfig config
