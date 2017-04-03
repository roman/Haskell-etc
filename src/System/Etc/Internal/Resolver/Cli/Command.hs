{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Etc.Internal.Resolver.Cli.Command where

import Protolude

import Control.Monad.Catch (MonadThrow, throwM)
import Data.Hashable       (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Maybe          (fromMaybe)
import Data.Vector         (Vector)
import System.Environment  (getArgs, getProgName)

import qualified Data.Aeson          as JSON
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text           as Text
import qualified Options.Applicative as Opt

import System.Etc.Internal.Resolver.Cli.Common
import System.Etc.Internal.Types

import qualified System.Etc.Internal.Spec.Types as Spec

--------------------------------------------------------------------------------

entrySpecToJsonCli
  :: (MonadThrow m)
    => Spec.CliEntrySpec cmd
    -> m (Vector cmd, Opt.Parser (Maybe JSON.Value))
entrySpecToJsonCli entrySpec =
  case entrySpec of
    Spec.CmdEntry commandJsonValue specSettings ->
      return ( commandJsonValue
             , settingsToJsonCli specSettings
             )

    Spec.PlainEntry {} ->
      throwM CommandKeyMissing

configValueSpecToCli
  :: (MonadThrow m, Eq cmd, Hashable cmd)
    => HashMap cmd (Opt.Parser ConfigValue)
    -> Text
    -> Spec.ConfigSources cmd
    -> m (HashMap cmd (Opt.Parser ConfigValue))
configValueSpecToCli acc0 specEntryKey sources =
  let
    updateAccConfigOptParser configValueParser accOptParser =
      (\configValue accSubConfig ->
        case accSubConfig of
          ConfigValue {} ->
            accSubConfig

          SubConfig subConfigMap ->
            subConfigMap
              & HashMap.alter (const $ Just configValue) specEntryKey
              & SubConfig)
        <$> configValueParser
        <*> accOptParser
  in
    case Spec.cliEntry sources of
      Nothing ->
        return acc0

      Just entrySpec -> do
        (commands, jsonOptParser) <-
          entrySpecToJsonCli entrySpec

        let
          configValueParser =
            jsonToConfigValue <$> jsonOptParser

        foldM (\acc command ->
                 acc
                   & HashMap.alter
                        (\mAccParser ->
                          mAccParser
                            & fromMaybe (pure $ SubConfig HashMap.empty)
                            & updateAccConfigOptParser configValueParser
                            & Just)
                        command
                   & return)
              acc0
              commands

subConfigSpecToCli
  :: (MonadThrow m, JSON.FromJSON cmd, JSON.ToJSON cmd, Eq cmd, Hashable cmd)
    => Text
    -> HashMap.HashMap Text (Spec.ConfigValue cmd)
    -> HashMap cmd (Opt.Parser ConfigValue)
    -> m (HashMap cmd (Opt.Parser ConfigValue))
subConfigSpecToCli specEntryKey subConfigSpec acc =
  let
    updateAccConfigOptParser subConfigParser accOptParser =
      (\subConfig accSubConfig ->
          case accSubConfig of
            ConfigValue {} ->
              accSubConfig

            SubConfig subConfigMap ->
              subConfigMap
                & HashMap.alter (const $ Just subConfig) specEntryKey
                & SubConfig)
        <$> subConfigParser
        <*> accOptParser

    addSubParserCommand command subConfigParser =
      HashMap.alter
        (\mAccOptParser ->
           case mAccOptParser of
             Nothing -> do
               commandText <- commandToKey command
               throwM $ UnknownCommandKey (Text.intercalate ", " commandText)

             Just accOptParser ->
               Just
               $ updateAccConfigOptParser subConfigParser accOptParser)
        command

  in do
     parserPerCommand <-
       foldM specToConfigValueCli
             HashMap.empty
             (HashMap.toList subConfigSpec)

     parserPerCommand
       & HashMap.foldrWithKey
                  addSubParserCommand
                  acc
       & return

specToConfigValueCli
  :: (MonadThrow m, JSON.FromJSON cmd, JSON.ToJSON cmd, Eq cmd, Hashable cmd)
    => HashMap cmd (Opt.Parser ConfigValue)
    -> ( Text, Spec.ConfigValue cmd )
    -> m (HashMap cmd (Opt.Parser ConfigValue))
specToConfigValueCli acc (specEntryKey, specConfigValue) =
  case specConfigValue of
    Spec.ConfigValue _ sources ->
      configValueSpecToCli
        acc
        specEntryKey
        sources

    Spec.SubConfig subConfigSpec ->
      subConfigSpecToCli
        specEntryKey
        subConfigSpec
        acc

configValueCliAccInit
  :: (MonadThrow m, JSON.FromJSON cmd, Eq cmd, Hashable cmd)
    => Spec.ConfigSpec cmd
    -> m (HashMap cmd (Opt.Parser ConfigValue))
configValueCliAccInit spec =
  let
    zeroParser =
      pure $ SubConfig HashMap.empty

    commandsSpec = do
      programSpec <- Spec.specCliProgramSpec spec
      Spec.cliCommands programSpec

  in
    case commandsSpec of
      Nothing ->
        throwM CommandsKeyNotDefined

      Just commands ->
        foldM (\acc (commandVal, _) -> do
                command <- parseCommandJsonValue (JSON.String commandVal)
                return $ HashMap.insert command zeroParser acc)
              HashMap.empty
              (HashMap.toList commands)

joinCommandParsers
  :: (MonadThrow m, JSON.ToJSON cmd, Eq cmd, Hashable cmd)
    => HashMap cmd (Opt.Parser ConfigValue)
    -> m (Opt.Parser (cmd, Config))
joinCommandParsers parserPerCommand =
  let
    joinParser acc (command, subConfigParser) =
      let
        parser =
          fmap (\subConfig -> (command, Config subConfig))
               subConfigParser
      in do
        commandTexts <- commandToKey command

        let
          commandParsers =
            map (\commandText ->
                    Opt.command (Text.unpack commandText)
                    (Opt.info (Opt.helper <*> parser) Opt.idm))
                commandTexts

        [acc]
          & (++ commandParsers)
          & mconcat
          & return
  in do
    mergedParsers <-
      foldM joinParser Opt.idm (HashMap.toList parserPerCommand)
    return (Opt.subparser mergedParsers)

specToConfigCli
  :: (MonadThrow m, JSON.FromJSON cmd, JSON.ToJSON cmd, Eq cmd, Hashable cmd)
    => Spec.ConfigSpec cmd
    -> m (Opt.Parser (cmd, Config))
specToConfigCli spec = do
  acc <- configValueCliAccInit spec
  parsers <-
    foldM specToConfigValueCli
          acc
          (HashMap.toList $ Spec.specConfigValues spec)

  joinCommandParsers parsers

resolveCommandCliPure
  :: (MonadThrow m, JSON.FromJSON cmd, JSON.ToJSON cmd, Eq cmd, Hashable cmd)
    => Spec.ConfigSpec cmd
    -> Text
    -> [Text]
    -> m (cmd, Config)
resolveCommandCliPure configSpec progName args = do
  configParser <- specToConfigCli configSpec

  let
    programModFlags =
      case Spec.specCliProgramSpec configSpec of
           Just programSpec ->
             Opt.fullDesc
              `mappend` (programSpec
                        & Spec.cliProgramDesc
                        & Text.unpack
                        & Opt.progDesc)
              `mappend` (programSpec
                        & Spec.cliProgramHeader
                        & Text.unpack
                        & Opt.header)
           Nothing ->
             mempty

    programParser =
      Opt.info (Opt.helper <*> configParser)
               programModFlags

    programResult =
      args
        & map Text.unpack
        & Opt.execParserPure Opt.defaultPrefs programParser

  programResultToResolverResult progName programResult


resolveCommandCli
  :: (JSON.FromJSON cmd, JSON.ToJSON cmd, Eq cmd, Hashable cmd)
    => Spec.ConfigSpec cmd
    -> IO (cmd, Config)
resolveCommandCli configSpec = do
  progName <- Text.pack <$> getProgName
  args     <- map Text.pack <$> getArgs

  handleCliResult
    $ resolveCommandCliPure configSpec progName args
