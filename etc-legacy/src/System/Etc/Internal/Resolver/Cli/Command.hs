{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Etc.Internal.Resolver.Cli.Command (resolveCommandCli, resolveCommandCliPure) where

import           RIO
import qualified RIO.HashMap as HashMap
import qualified RIO.Text    as Text

import System.Environment (getArgs, getProgName)

import qualified Data.Aeson          as JSON
import qualified Options.Applicative as Opt

import System.Etc.Internal.Resolver.Cli.Common
import System.Etc.Internal.Types

import qualified System.Etc.Internal.Spec.Types as Spec

--------------------------------------------------------------------------------

entrySpecToJsonCli
  :: (MonadThrow m)
  => Spec.ConfigValueType
  -> Bool
  -> Spec.CliEntrySpec cmd
  -> m (Vector cmd, Opt.Parser (Maybe (Value JSON.Value)))
entrySpecToJsonCli cvType isSensitive entrySpec = case entrySpec of
  Spec.CmdEntry commandJsonValue specSettings ->
    return (commandJsonValue, settingsToJsonCli cvType isSensitive specSettings)

  Spec.PlainEntry{} -> throwM CommandKeyMissing

configValueSpecToCli
  :: (MonadThrow m, Eq cmd, Hashable cmd)
  => HashMap cmd (Opt.Parser ConfigValue)
  -> Text
  -> Spec.ConfigValueType
  -> Bool
  -> Spec.ConfigSources cmd
  -> m (HashMap cmd (Opt.Parser ConfigValue))
configValueSpecToCli acc0 specEntryKey cvType isSensitive sources =
  let updateAccConfigOptParser configValueParser accOptParser =
        (\configValue accSubConfig -> case accSubConfig of
            ConfigValue{} -> accSubConfig

            SubConfig subConfigMap ->
              subConfigMap & HashMap.alter (const $ Just configValue) specEntryKey & SubConfig
          )
          <$> configValueParser
          <*> accOptParser
  in  case Spec.cliEntry sources of
        Nothing        -> return acc0

        Just entrySpec -> do
          (commands, jsonOptParser) <- entrySpecToJsonCli cvType isSensitive entrySpec

          let configValueParser = jsonToConfigValue <$> jsonOptParser

          foldM
            (\acc command ->
              acc
                & HashMap.alter
                    (\mAccParser ->
                      mAccParser
                        & fromMaybe (pure mempty)
                        & updateAccConfigOptParser configValueParser
                        & Just
                    )
                    command
                & return
            )
            acc0
            commands

subConfigSpecToCli
  :: (MonadThrow m, JSON.FromJSON cmd, JSON.ToJSON cmd, Eq cmd, Hashable cmd)
  => Text
  -> HashMap.HashMap Text (Spec.ConfigValue cmd)
  -> HashMap cmd (Opt.Parser ConfigValue)
  -> m (HashMap cmd (Opt.Parser ConfigValue))
subConfigSpecToCli specEntryKey subConfigSpec acc =
  let updateAccConfigOptParser subConfigParser accOptParser =
        (\subConfig accSubConfig -> case accSubConfig of
            ConfigValue{} -> accSubConfig

            SubConfig subConfigMap ->
              subConfigMap & HashMap.alter (const $ Just subConfig) specEntryKey & SubConfig
          )
          <$> subConfigParser
          <*> accOptParser

      addSubParserCommand command subConfigParser = HashMap.alter
        (\mAccOptParser -> case mAccOptParser of
          Nothing -> do
            commandText <- commandToKey command
            throwM $ UnknownCommandKey (Text.intercalate ", " commandText)

          Just accOptParser -> Just $ updateAccConfigOptParser subConfigParser accOptParser
        )
        command
  in  do
        parserPerCommand <- foldM specToConfigValueCli
                                  HashMap.empty
                                  (HashMap.toList subConfigSpec)

        parserPerCommand & HashMap.foldrWithKey addSubParserCommand acc & return

specToConfigValueCli
  :: (MonadThrow m, JSON.FromJSON cmd, JSON.ToJSON cmd, Eq cmd, Hashable cmd)
  => HashMap cmd (Opt.Parser ConfigValue)
  -> (Text, Spec.ConfigValue cmd)
  -> m (HashMap cmd (Opt.Parser ConfigValue))
specToConfigValueCli acc (specEntryKey, specConfigValue) = case specConfigValue of
  Spec.ConfigValue Spec.ConfigValueData { Spec.configValueType, Spec.isSensitive, Spec.configSources }
    -> configValueSpecToCli acc specEntryKey configValueType isSensitive configSources

  Spec.SubConfig subConfigSpec -> subConfigSpecToCli specEntryKey subConfigSpec acc

configValueCliAccInit
  :: (MonadThrow m, JSON.FromJSON cmd, Eq cmd, Hashable cmd)
  => Spec.ConfigSpec cmd
  -> m (HashMap cmd (Opt.Parser ConfigValue))
configValueCliAccInit spec =
  let zeroParser   = pure $ SubConfig HashMap.empty

      commandsSpec = do
        programSpec <- Spec.specCliProgramSpec spec
        Spec.cliCommands programSpec
  in  case commandsSpec of
        Nothing       -> throwM CommandsKeyNotDefined

        Just commands -> foldM
          (\acc (commandVal, _) -> do
            command <- parseCommandJsonValue (JSON.String commandVal)
            return $ HashMap.insert command zeroParser acc
          )
          HashMap.empty
          (HashMap.toList commands)

joinCommandParsers
  :: (MonadThrow m, JSON.ToJSON cmd)
  => HashMap cmd (Opt.Parser ConfigValue)
  -> m (Opt.Parser (cmd, Config))
joinCommandParsers parserPerCommand =
  let joinParser acc (command, subConfigParser) =
        let parser = fmap (\subConfig -> (command, Config subConfig)) subConfigParser
        in  do
              commandTexts <- commandToKey command

              let commandParsers = map
                    (\commandText -> Opt.command
                      (Text.unpack commandText)
                      (Opt.info (Opt.helper <*> parser) Opt.idm)
                    )
                    commandTexts

              [acc] & (++ commandParsers) & mconcat & return
  in  Opt.subparser <$> foldM joinParser Opt.idm (HashMap.toList parserPerCommand)

specToConfigCli
  :: (MonadThrow m, JSON.FromJSON cmd, JSON.ToJSON cmd, Eq cmd, Hashable cmd)
  => Spec.ConfigSpec cmd
  -> m (Opt.Parser (cmd, Config))
specToConfigCli spec = do
  acc     <- configValueCliAccInit spec
  parsers <- foldM specToConfigValueCli acc (HashMap.toList $ Spec.specConfigValues spec)

  joinCommandParsers parsers

{-|

Dynamically generate an OptParser CLI with sub-commands from the spec settings
declared on the @ConfigSpec@. This will process the OptParser from given input
rather than fetching it from the OS.

This will return the selected record parsed from the sub-command input and the
configuration map with keys defined for that sub-command.

-}
resolveCommandCliPure
  :: (MonadThrow m, JSON.FromJSON cmd, JSON.ToJSON cmd, Eq cmd, Hashable cmd)
  => Spec.ConfigSpec cmd  -- ^ Config Spec (normally parsed from json or yaml file)-- ^ The
  -> Text                 -- ^ Name of the program running the CLI
  -> [Text]               -- ^ Arglist for the program
  -> m (cmd, Config)      -- ^ Selected command and Configuration Map
resolveCommandCliPure configSpec progName args = do
  configParser <- specToConfigCli configSpec

  let
    programModFlags = case Spec.specCliProgramSpec configSpec of
      Just programSpec ->
        Opt.fullDesc
          `mappend` (programSpec & Spec.cliProgramDesc & Text.unpack & Opt.progDesc)
          `mappend` (programSpec & Spec.cliProgramHeader & Text.unpack & Opt.header)
      Nothing -> mempty

    programParser = Opt.info (Opt.helper <*> configParser) programModFlags

    programResult =
      args & map Text.unpack & Opt.execParserPure Opt.defaultPrefs programParser

  programResultToResolverResult progName programResult


{-|

Dynamically generate an OptParser CLI with sub-commands from the spec settings
declared on the @ConfigSpec@.

Once it generates the CLI and gathers the input, it will return the selected
record parsed from the sub-command input and the configuration map with keys
defined for that sub-command.

-}
resolveCommandCli
  :: (JSON.FromJSON cmd, JSON.ToJSON cmd, Eq cmd, Hashable cmd)
  => Spec.ConfigSpec cmd  -- ^ Config Spec (normally parsed from json or yaml file)
  -> IO (cmd, Config)     -- ^ Selected command and Configuration Map
resolveCommandCli configSpec = do
  progName <- Text.pack <$> getProgName
  args     <- map Text.pack <$> getArgs

  handleCliResult $ resolveCommandCliPure configSpec progName args
