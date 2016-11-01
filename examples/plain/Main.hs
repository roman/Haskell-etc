{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
module Main where

import qualified System.Etc as Config

import Protolude

--------------------------------------------------------------------------------
-- We specify the support commands for our program

main :: IO ()
main = do
  configSpec :: Config.PlainConfigSpec <-
    Config.readConfigSpec "./examples/plain/resources/spec.json"

  configFiles     <- Config.resolveFiles configSpec
  configEnv       <- Config.resolveEnvVars configSpec
  configOptParser <- Config.resolvePlainOptParser configSpec

  let
    config =
      configFiles
      <> configEnv
      <> configOptParser

  Config.printPrettyConfig config
