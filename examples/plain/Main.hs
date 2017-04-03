{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
module Main where

import qualified System.Etc as Etc

import Protolude

main :: IO ()
main = do
  configSpec <-
    Etc.readConfigSpec "./resources/spec.json"

  configFiles     <- Etc.resolveFiles configSpec
  configEnv       <- Etc.resolveEnv configSpec
  configOptParser <- Etc.resolvePlainCli configSpec

  let
    config =
      configFiles
      <> configEnv
      <> configOptParser

  Etc.printPrettyConfig config
