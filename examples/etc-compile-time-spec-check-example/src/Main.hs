{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main where

import RIO

import Data.Proxy (Proxy)

import qualified System.Etc as Etc

main :: IO ()
main = do
  let configSpec = $(Etc.readConfigSpecTH (undefined :: Proxy ()) "./resources/spec.yaml")

  Etc.reportEnvMisspellingWarnings configSpec

  (configFiles, _fileWarnings) <- Etc.resolveFiles configSpec
  configEnv                    <- Etc.resolveEnv configSpec
  configOptParser              <- Etc.resolvePlainCli configSpec

  let configDefault = Etc.resolveDefault configSpec

      config =
        configFiles `mappend` configDefault `mappend` configEnv `mappend` configOptParser

  Etc.printPrettyConfig config
