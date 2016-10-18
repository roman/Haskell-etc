{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module System.Etc.Internal.Resolver.OptParse.Command where

import Control.Lens hiding ((<|), (|>))
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Control.Monad.Catch (MonadThrow, throwM)
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Options.Applicative as Opt

import UB.Prelude hiding ((&))
import System.Etc.Internal.Types
import System.Etc.Internal.Resolver.OptParse.Common
import qualified System.Etc.Internal.Spec as Spec

--------------------------------------------------------------------------------

entrySpecToJsonOptParser
  :: (MonadThrow m)
    => Spec.OptParseEntrySpec cmd
    -> m (cmd, Opt.Parser (Maybe JSON.Value))
entrySpecToJsonOptParser entrySpec =
  case entrySpec of
    Spec.CmdEntry commandJsonValue specSettings ->
      return ( commandJsonValue
             , settingsToJsonOptParser specSettings
             )

    Spec.PlainEntry {} ->
      throwM CommandKeyMissing

configValueSpecToOptParser
  :: (MonadThrow m, Eq cmd, Hashable cmd)
    => Text
    -> Maybe JSON.Value
    -> Spec.ConfigSources cmd
    -> HashMap cmd (Opt.Parser ConfigValue)
    -> m (HashMap cmd (Opt.Parser ConfigValue))
configValueSpecToOptParser specEntryKey specEntryDefVal sources acc =
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
        return acc

      Just entrySpec -> do
        (command, jsonOptParser) <-
          entrySpecToJsonOptParser entrySpec

        let
          configValueParser =
            jsonToConfigValue specEntryDefVal <$> jsonOptParser

        acc
          |> HashMap.alter
                (\mAccParser ->
                  mAccParser
                    |> fromMaybe (pure <| SubConfig HashMap.empty)
                    |> updateAccConfigOptParser configValueParser
                    |> Just)
                command
          |> return

subConfigSpecToOptParser
  :: (MonadThrow m, JSON.FromJSON cmd, JSON.ToJSON cmd, Eq cmd, Hashable cmd)
    => Text
    -> HashMap.HashMap Text (Spec.ConfigValue cmd)
    -> HashMap cmd (Opt.Parser ConfigValue)
    -> m (HashMap cmd (Opt.Parser ConfigValue))
subConfigSpecToOptParser specEntryKey subConfigSpec acc =
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
               throwM <| UnknownCommandKey (Text.intercalate ", " commandText)

             Just accOptParser ->
               Just
               <| updateAccConfigOptParser subConfigParser accOptParser)
        command
        accOptParsers

  in do
     parserPerCommand <-
       ifoldrMOf itraversed
                 specToConfigValueOptParser
                 HashMap.empty
                 subConfigSpec

     parserPerCommand
       |> HashMap.foldrWithKey
                  addSubParserCommand
                  acc
       |> return

specToConfigValueOptParser
  :: (MonadThrow m, JSON.FromJSON cmd, JSON.ToJSON cmd, Eq cmd, Hashable cmd)
    => Text
    -> Spec.ConfigValue cmd
    -> HashMap cmd (Opt.Parser ConfigValue)
    -> m (HashMap cmd (Opt.Parser ConfigValue))
specToConfigValueOptParser specEntryKey specConfigValue acc =
  case specConfigValue of
    Spec.ConfigValue mDefaultValue sources ->
      configValueSpecToOptParser
        specEntryKey
        mDefaultValue
        sources
        acc

    Spec.SubConfig subConfigSpec ->
      subConfigSpecToOptParser
        specEntryKey
        subConfigSpec
        acc

configValueOptParserAccInit
  :: (MonadThrow m, JSON.FromJSON cmd, Eq cmd, Hashable cmd)
    => Spec.ConfigSpec' cmd
    -> m (HashMap cmd (Opt.Parser ConfigValue))
configValueOptParserAccInit spec =
  let
    zeroParser =
      pure <| SubConfig HashMap.empty

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
                   return <| HashMap.insert command zeroParser acc)
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
          |> (++ commandParsers)
          |> mconcat
          |> return
  in do
    mergedParsers <- ifoldrMOf itraversed joinParser Opt.idm parserPerCommand
    return (Opt.subparser mergedParsers)

specToConfigOptParser
  :: (MonadThrow m, JSON.FromJSON cmd, JSON.ToJSON cmd, Eq cmd, Hashable cmd)
    => Spec.ConfigSpec' cmd
    -> m (Opt.Parser (cmd, Config))
specToConfigOptParser spec = do
  acc <- configValueOptParserAccInit spec
  parsers <-
    ifoldrMOf itraversed
              specToConfigValueOptParser
              acc
              (Spec.specConfigValues spec)

  joinCommandParsers parsers

resolveCommandOptParser
  :: (JSON.FromJSON cmd, JSON.ToJSON cmd, Eq cmd, Hashable cmd)
    => Spec.ConfigSpec' cmd
    -> IO (cmd, Config)
resolveCommandOptParser configSpec = do
  configParser <- specToConfigOptParser configSpec

  let
    programModFlags =
      case Spec.specOptParseProgramSpec configSpec of
           Just programSpec ->
             Opt.fullDesc
              `mappend` (programSpec
                        |> Spec.optParseProgramDesc
                        |> Text.unpack
                        |> Opt.progDesc)
              `mappend` (programSpec
                        |> Spec.optParseProgramHeader
                        |> Text.unpack
                        |> Opt.header)
           Nothing ->
             mempty

    programParser =
      Opt.info (Opt.helper <*> configParser)
               programModFlags

  Opt.execParser programParser
