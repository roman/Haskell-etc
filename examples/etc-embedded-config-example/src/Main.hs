{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main where

import Data.FileEmbed (embedFile)
import Data.Monoid    ((<>))
import Protolude      hiding ((<>))

import qualified Data.Text.Encoding as Text
import qualified System.Etc         as Etc

--------------------------------------------------------------------------------
-- We specify the support commands for our program

configSpecData :: Text
configSpecData =
  Text.decodeUtf8 $(embedFile "resources/spec.yaml")

main :: IO ()
main = do
  configSpec <-
    Etc.parseConfigSpec configSpecData

  Etc.reportEnvMisspellingWarnings configSpec

  configEnv <- Etc.resolveEnv configSpec
  configOptParser <- Etc.resolvePlainCli configSpec

  let
    configDefault =
      Etc.resolveDefault configSpec

    config =
      configDefault
      <> configEnv
      <> configOptParser

  Etc.printPrettyConfig config
