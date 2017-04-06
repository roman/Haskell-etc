{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Aeson       as JSON
import qualified Data.Aeson.Types as JSON (typeMismatch)
import           Data.Hashable    (Hashable)
import qualified Data.Text        as Text
import           GHC.Generics     (Generic)
import qualified System.Etc       as Etc

import Paths_etc_command_example (getDataFileName)

import Protolude

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
      JSON.String cmdName
        | cmdName == "config" ->
          return PrintConfig

        | cmdName == "run" ->
          return RunMain

        | otherwise ->
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
  specPath <- getDataFileName "spec.yaml"
  configSpec <- Etc.readConfigSpec (Text.pack specPath)

  (configFiles, _fileWarnings) <- Etc.resolveFiles configSpec
  (cmd, configCli) <- Etc.resolveCommandCli configSpec
  configEnv <- Etc.resolveEnv configSpec

  let
    configDefault =
      Etc.resolveDefault configSpec

    config =
      configDefault
      <> configFiles
      <> configEnv
      <> configCli

  case cmd of
    PrintConfig ->
      Etc.printPrettyConfig config

    RunMain -> do
      putStrLn ("Executing main program" :: Text)
      Etc.printPrettyConfig config
