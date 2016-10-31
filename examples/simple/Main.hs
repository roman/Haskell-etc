{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import qualified System.Etc as Config

import Protolude

--------------------------------------------------------------------------------
-- We specify the support commands for our program

main :: IO ()
main = do
  configSpec <- Config.readConfigSpec "./examples/simple/resources/spec.json"

  configFiles <- Config.resolveFiles configSpec
  configEnv   <- Config.resolveEnvVars configSpec
  configOptParser <- Config.resolveOptParser configSpec

  let
    config =
      configFiles
      <> configEnv
      <> configOptParser

  Config.printPrettyConfig config
