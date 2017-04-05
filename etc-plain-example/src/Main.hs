{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Protolude

import qualified Data.Text  as Text
import qualified System.Etc as Etc

import Paths_etc_plain_example (getDataFileName)

--------------------------------------------------------------------------------
-- We specify the support commands for our program

main :: IO ()
main = do
  specPath <- getDataFileName "spec.yaml"
  configSpec <- Etc.readConfigSpec (Text.pack specPath)

  (configFiles, _fileWarnings) <- Etc.resolveFiles configSpec
  configEnv <- Etc.resolveEnv configSpec
  configOptParser <- Etc.resolvePlainCli configSpec

  let
    config =
      configFiles
      <> configEnv
      <> configOptParser

  Etc.printPrettyConfig config
