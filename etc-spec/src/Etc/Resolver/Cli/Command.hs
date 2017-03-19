{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Etc.Resolver.Cli.Command where

import Protolude

import Control.Monad.Catch (MonadThrow, throwM)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import System.Environment (getArgs, getProgName)
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Options.Applicative as Opt

import Etc.Types
import Etc.Resolver.Cli.Common
import qualified Etc.Spec.Types as Spec

--------------------------------------------------------------------------------

entrySpecToJsonCli
  :: (MonadThrow m)
    => Spec.OptParseEntrySpec cmd
    -> m (Vector cmd, Opt.Parser (Maybe JSON.Value))
entrySpecToJsonCli entrySpec =
  case entrySpec of
    Spec.CmdEntry commandJsonValue specSettings ->
      return ( commandJsonValue
             , settingsToJsonOptParser specSettings
             )

    Spec.PlainEntry {} ->
      throwM CommandKeyMissing

configValueSpecToCli
  :: (MonadThrow m, Eq cmd, Hashable cmd)
    => Text
    -> Maybe JSON.Value
    -> Spec.ConfigSources cmd
    -> HashMap cmd (Opt.Parser ConfigValue)
    -> m (HashMap cmd (Opt.Parser ConfigValue))
configValueSpecToCli specEntryKey specEntryDefVal sources acc0 =
  let
    updateAccConfigOptParser configValueParser accOptParser =
      (\configValue accSubConfig ->
        accSubConfig
          &  (_SubConfig << at specEntryKey << _JustConfigValue Set.empty)
          .~ configValue)
        <$> configValueParser
        <*> accOptParser
  in
    case Spec.optParse sources of
      Nothing ->
        return acc0

      Just entrySpec -> do
        (commands, jsonOptParser) <-
          entrySpecToJsonCli entrySpec

        let
          configValueParser =
            jsonToConfigValue specEntryDefVal <$> jsonOptParser

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
          accSubConfig
            & (_SubConfig << at specEntryKey << _JustSubConfig)
            .~ subConfig)
        <$> subConfigParser
        <*> accOptParser

    addSubParserCommand command subConfigParser accOptParsers =
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
        accOptParsers

  in do
     parserPerCommand <-
       ifoldrMOf itraversed
                 specToConfigValueCli
                 HashMap.empty
                 subConfigSpec

     parserPerCommand
       & HashMap.foldrWithKey
                  addSubParserCommand
                  acc
       & return

specToConfigValueCli
  :: (MonadThrow m, JSON.FromJSON cmd, JSON.ToJSON cmd, Eq cmd, Hashable cmd)
    => Text
    -> Spec.ConfigValue cmd
    -> HashMap cmd (Opt.Parser ConfigValue)
    -> m (HashMap cmd (Opt.Parser ConfigValue))
specToConfigValueCli specEntryKey specConfigValue acc =
  case specConfigValue of
    Spec.ConfigValue mDefaultValue sources ->
      configValueSpecToCli
        specEntryKey
        mDefaultValue
        sources
        acc

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
      programSpec <- Spec.specOptParseProgramSpec spec
      Spec.optParseCommands programSpec

  in
    case commandsSpec of
      Nothing ->
        throwM CommandsKeyNotDefined

      Just commands ->
       ifoldrMOf itraversed
                 (\commandVal _ acc -> do
                   command <- parseCommandJsonValue (JSON.String commandVal)
                   return $ HashMap.insert command zeroParser acc)
                 HashMap.empty
                 commands

joinCommandParsers
  :: (MonadThrow m, JSON.ToJSON cmd, Eq cmd, Hashable cmd)
    => HashMap cmd (Opt.Parser ConfigValue)
    -> m (Opt.Parser (cmd, Config))
joinCommandParsers parserPerCommand =
  let
    joinParser command subConfigParser acc =
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
    mergedParsers <- ifoldrMOf itraversed joinParser Opt.idm parserPerCommand
    return (Opt.subparser mergedParsers)

specToConfigCli
  :: (MonadThrow m, JSON.FromJSON cmd, JSON.ToJSON cmd, Eq cmd, Hashable cmd)
    => Spec.ConfigSpec cmd
    -> m (Opt.Parser (cmd, Config))
specToConfigCli spec = do
  acc <- configValueCliAccInit spec
  parsers <-
    ifoldrMOf itraversed
              specToConfigValueCli
              acc
              (Spec.specConfigValues spec)

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
      case Spec.specOptParseProgramSpec configSpec of
           Just programSpec ->
             Opt.fullDesc
              `mappend` (programSpec
                        & Spec.optParseProgramDesc
                        & Text.unpack
                        & Opt.progDesc)
              `mappend` (programSpec
                        & Spec.optParseProgramHeader
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


resolveCommandOptParser
  :: (JSON.FromJSON cmd, JSON.ToJSON cmd, Eq cmd, Hashable cmd)
    => Spec.ConfigSpec cmd
    -> IO (cmd, Config)
resolveCommandOptParser configSpec = do
  progName <- Text.pack <$> getProgName
  args     <- map Text.pack <$> getArgs

  handleOptParseResult
    $ resolveCommandCliPure configSpec progName args
