{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Etc.Internal.Cli.Command.Parser where

import RIO
import qualified RIO.Map as Map

import qualified Data.Aeson.BetterErrors as JSON

import Etc.Internal.Cli.Types
import Etc.Internal.Cli.Plain.Parser (parseCliInfoSpec)

parseCliEntryCommandsSpec :: JSON.Parse err [Text]
parseCliEntryCommandsSpec =
  JSON.key "commands" (JSON.eachInArray JSON.asText)

parseCliCommands :: JSON.Parse err (Map Text CliInfoSpec)
parseCliCommands =
  Map.fromList <$> JSON.eachInObject parseCliInfoSpec
