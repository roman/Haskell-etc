{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Etc.Resolver.Cli.Plain where

import Protolude

import Control.Monad.Catch (MonadThrow, throwM)
import System.Environment (getArgs, getProgName)
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Options.Applicative as Opt

import Etc.Types
import Etc.Resolver.Cli.Common
import qualified Etc.Spec.Types as Spec

--------------------------------------------------------------------------------

type PlainConfigSpec =
  Spec.ConfigSpec ()

--------------------------------------------------------------------------------

entrySpecToConfigValueCli
  :: (MonadThrow m)
    => Spec.CliEntrySpec ()
    -> m (Opt.Parser (Maybe JSON.Value))
entrySpecToConfigValueCli entrySpec =
  case entrySpec of
    Spec.CmdEntry {} ->
      throwM CommandKeyOnPlainCli

    Spec.PlainEntry specSettings ->
      return (settingsToJsonCli specSettings)


configValueSpecToCli
  :: (MonadThrow m)
    => Text
    -> Spec.ConfigSources ()
    -> Opt.Parser ConfigValue
    -> m (Opt.Parser ConfigValue)
configValueSpecToCli specEntryKey sources acc =
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
        return acc

      Just entrySpec -> do
        jsonOptParser <- entrySpecToConfigValueCli entrySpec

        let
          configValueParser =
            jsonToConfigValue <$> jsonOptParser

        return $ updateAccConfigOptParser configValueParser acc

subConfigSpecToCli
  :: (MonadThrow m)
    => Text
    -> HashMap.HashMap Text (Spec.ConfigValue ())
    -> Opt.Parser ConfigValue
    -> m (Opt.Parser ConfigValue)
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
  in do
    configOptParser <-
       foldM specToConfigValueCli
             (pure $ SubConfig HashMap.empty)
             (HashMap.toList subConfigSpec)

    return
      $ updateAccConfigOptParser configOptParser acc

specToConfigValueCli
  :: (MonadThrow m)
    => Opt.Parser ConfigValue
    -> (Text, Spec.ConfigValue ())
    -> m (Opt.Parser ConfigValue)
specToConfigValueCli acc (specEntryKey, specConfigValue) =
  case specConfigValue of
    Spec.ConfigValue _ sources ->
      configValueSpecToCli
        specEntryKey
        sources
        acc

    Spec.SubConfig subConfigSpec ->
      subConfigSpecToCli
        specEntryKey
        subConfigSpec
        acc

configValueCliAccInit
  :: (MonadThrow m)
    => Spec.ConfigSpec ()
    -> m (Opt.Parser ConfigValue)
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
        return zeroParser

      Just _ ->
        throwM CommandKeyOnPlainCli

specToConfigCli
  :: (MonadThrow m)
    => Spec.ConfigSpec ()
    -> m (Opt.Parser Config)
specToConfigCli spec = do
  acc <- configValueCliAccInit spec
  parser <-
    foldM specToConfigValueCli
          acc
          (HashMap.toList $ Spec.specConfigValues spec)

  parser
    & (Config <$>)
    & return

resolvePlainCliPure
  :: MonadThrow m => PlainConfigSpec -> Text -> [Text] -> m Config
resolvePlainCliPure configSpec progName args = do
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


resolvePlainCli
  :: PlainConfigSpec -> IO Config
resolvePlainCli configSpec = do
  progName <- Text.pack <$> getProgName
  args     <- map Text.pack <$> getArgs

  handleCliResult
    $ resolvePlainCliPure configSpec progName args
