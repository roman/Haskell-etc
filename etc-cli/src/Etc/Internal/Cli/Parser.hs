{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Etc.Internal.Cli.Parser where

import RIO

import qualified Data.Aeson.BetterErrors as JSON
import Etc.Internal.Spec.Types
import Etc.Internal.Cli.Types

parseCliSwitchSpec ::
     ConfigValueType -> JSON.Parse CliEntryParseError CliSwitchSpec
parseCliSwitchSpec cvType =
  case cvType of
    CVTSingle CVTBool ->
      CliSwitchSpec <$> JSON.key "long" JSON.asText <*>
      JSON.keyMay "help" JSON.asText
    _ -> JSON.throwCustomError (SwitchIncompatibleType cvType)

parseCliArgSpec :: JSON.Parse err CliArgSpec
parseCliArgSpec =
  CliArgSpec
    <$> JSON.keyMay "meta" JSON.asText
    <*> JSON.keyMay "help" JSON.asText

parseCliOptSpec :: JSON.Parse CliEntryParseError CliOptSpec
parseCliOptSpec = do
  mlong <- JSON.keyMay "long" JSON.asText
  mshort <- JSON.keyMay "short" JSON.asText
  if isNothing mlong && isNothing mshort
    then JSON.throwCustomError MissingOptName -- error "TODO: InvalidOptionSettings"
    else CliOptSpec <$> pure mlong <*> pure mshort <*>
         JSON.keyMay "meta" JSON.asText <*>
         JSON.keyMay "help" JSON.asText <*>
         (fromMaybe False <$> JSON.keyMay "hidden" JSON.asBool) <*>
         (fromMaybe False <$> JSON.keyMay "internal" JSON.asBool)

parseCliEntrySpec :: ConfigValueType -> JSON.Parse CliEntryParseError CliEntrySpec
parseCliEntrySpec cvType = do
  inputName <- JSON.key "input" JSON.asText
  case inputName of
    "option" -> Opt <$> parseCliOptSpec
    "argument" -> Arg <$> parseCliArgSpec
    "switch" -> Switch <$> parseCliSwitchSpec cvType
    _ -> JSON.throwCustomError (InvalidInputName inputName)

parseCliInfoSpec :: JSON.Parse err CliInfoSpec
parseCliInfoSpec = do
  CliInfoSpec
    <$> JSON.key "desc" JSON.asText
    <*> JSON.keyMay "header" JSON.asText
    <*> JSON.keyMay "footer" JSON.asText
