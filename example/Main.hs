{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON (typeMismatch)
import qualified Data.Text as Text
import qualified System.Etc as Config

import UB.Prelude

--------------------------------------------------------------------------------
-- We specify the support commands for our program

data Cmd
  = PrintConfig
  | RunMain
  deriving (Show, Eq, Generic)

instance Hashable Cmd

instance JSON.FromJSON Cmd where
  parseJSON json =
    case json of
      JSON.String cmdName ->
        if cmdName == "config" then
          return PrintConfig
        else if cmdName == "run" then
          return RunMain
        else
          JSON.typeMismatch ("Cmd (" <> Text.unpack cmdName <> ")") json
      _ ->
        JSON.typeMismatch "Cmd" json

instance JSON.ToJSON Cmd where
  toJSON cmd =
    case cmd of
      PrintConfig ->
        JSON.String "config"
      RunMain ->
        JSON.String "run"

--------------------------------------------------------------------------------

main :: IO ()
main = do
  configSpec <- Config.readConfigSpec "example/resources/spec.json"

  configFiles <- Config.resolveFiles configSpec
  configEnv   <- Config.resolveEnvVars configSpec
  (cmd, configOptParser) <- Config.resolveCommandOptParser configSpec

  let
    config =
      configFiles
      <> configEnv
      <> configOptParser

  case cmd of
    PrintConfig ->
      Config.printPrettyConfig config

    RunMain -> do
      putStrLn ("Executing main program" :: Text)
      Config.printPrettyConfig config
