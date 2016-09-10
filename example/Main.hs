{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Data.List (foldl1')
import qualified System.Etc as Config

import UB.Prelude

main :: IO ()
main = do
  configSpec <- Config.readConfigSpec "test/fixtures/spec.json"

  config <-
    [ Config.resolveEnvVars
    , Config.resolveOptParser
    , Config.resolveFiles ]
    |> mapM (<| configSpec)
    |> fmap (foldl1' (<>))

  Config.printPrettyConfig config
