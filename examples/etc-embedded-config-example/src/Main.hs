{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Protolude hiding ((<>))
import Data.Monoid ((<>))

import qualified Config (configSpec)
import qualified Data.ByteString.Base64 as BS
import qualified Data.Text.Encoding as Text
import qualified System.Etc as Etc


--------------------------------------------------------------------------------
-- We specify the support commands for our program

main :: IO ()
main = do
  configSpec <-
    Etc.parseConfigSpec (Text.decodeUtf8 $ BS.decodeLenient Config.configSpec)

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
