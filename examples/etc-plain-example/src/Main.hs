{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import RIO
import qualified RIO.Text  as Text

import qualified System.Etc as Etc

import Paths_etc_plain_example (getDataFileName)

--------------------------------------------------------------------------------
-- We specify the support commands for our program

main :: IO ()
main = do
  specPath <- getDataFileName "spec.yaml"
  configSpec <- Etc.readConfigSpec (Text.pack specPath)

  Etc.reportEnvMisspellingWarnings configSpec

  (configFiles, _fileWarnings) <- Etc.resolveFiles configSpec
  configEnv <- Etc.resolveEnv configSpec
  configOptParser <- Etc.resolvePlainCli configSpec

  let
    configDefault =
      Etc.resolveDefault configSpec

    config =
      configFiles
      `mappend` configDefault
      `mappend` configEnv
      `mappend` configOptParser

  Etc.printPrettyConfig config
