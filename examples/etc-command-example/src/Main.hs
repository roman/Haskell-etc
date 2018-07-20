{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Prelude

import           RIO
import qualified RIO.Text as Text

import qualified Data.Aeson       as JSON
import qualified Data.Aeson.Types as JSON (typeMismatch)
import           GHC.Generics     (Generic)
import qualified System.Etc       as Etc

import Paths_etc_command_example (getDataFileName)


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
  specPath   <- getDataFileName "spec.yaml"
  configSpec <- Etc.readConfigSpec (Text.pack specPath)

  Etc.reportEnvMisspellingWarnings configSpec

  -- in case source fetching fails with an IO error, you may want to fail fast (e.g. vault)
  -- config <- Etc.resolve [defaultCli, defaultVault, defaultEnv] configSpec
  -- cmd <- Etc.resolveCommandCli configSpec
  -- fileWarnings <- Etc.resolveFiles configSpec
  (configFiles, _fileWarnings) <- Etc.resolveFiles configSpec
  (cmd        , configCli    ) <- Etc.resolveCommandCli configSpec
  configEnv                    <- Etc.resolveEnv configSpec

  let configDefault = Etc.resolveDefault configSpec

      config = configDefault `mappend` configFiles `mappend` configEnv `mappend` configCli

  case cmd of
    PrintConfig -> Etc.printPrettyConfig config

    RunMain     -> do
      Prelude.putStrLn "Executing main program"
      Etc.printPrettyConfig config
