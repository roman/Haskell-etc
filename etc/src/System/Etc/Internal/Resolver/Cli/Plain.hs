{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}

module System.Etc.Internal.Resolver.Cli.Plain (PlainConfigSpec, resolvePlainCli, resolvePlainCliPure) where

import           RIO
import qualified RIO.HashMap as HashMap
import qualified RIO.Text    as Text

import qualified Data.Aeson          as JSON
import qualified Options.Applicative as Opt
import           System.Environment  (getArgs, getProgName)

import           System.Etc.Internal.Resolver.Cli.Common
import qualified System.Etc.Internal.Spec.Types          as Spec
import           System.Etc.Internal.Types

--------------------------------------------------------------------------------

type PlainConfigSpec =
  Spec.ConfigSpec ()

--------------------------------------------------------------------------------

entrySpecToConfigValueCli
  :: (MonadThrow m)
  => Spec.ConfigValueType
  -> Bool
  -> Spec.CliEntrySpec ()
  -> m (Opt.Parser (Maybe (Value JSON.Value)))
entrySpecToConfigValueCli cvType isSensitive entrySpec = case entrySpec of
  Spec.CmdEntry{} -> throwM CommandKeyOnPlainCli

  Spec.PlainEntry specSettings ->
    return (settingsToJsonCli cvType isSensitive specSettings)


configValueSpecToCli
  :: (MonadThrow m)
  => Text
  -> Spec.ConfigValueType
  -> Bool
  -> Spec.ConfigSources ()
  -> Opt.Parser ConfigValue
  -> m (Opt.Parser ConfigValue)
configValueSpecToCli specEntryKey cvType isSensitive sources acc =
  let updateAccConfigOptParser configValueParser accOptParser =
        (\configValue accSubConfig -> case accSubConfig of
            ConfigValue{} -> accSubConfig

            SubConfig subConfigMap ->
              subConfigMap & HashMap.alter (const $ Just configValue) specEntryKey & SubConfig
          )
          <$> configValueParser
          <*> accOptParser
  in  case Spec.cliEntry sources of
        Nothing        -> return acc

        Just entrySpec -> do
          jsonOptParser <- entrySpecToConfigValueCli cvType isSensitive entrySpec

          let configValueParser = jsonToConfigValue <$> jsonOptParser

          return $ updateAccConfigOptParser configValueParser acc

subConfigSpecToCli
  :: (MonadThrow m)
  => Text
  -> HashMap.HashMap Text (Spec.ConfigValue ())
  -> Opt.Parser ConfigValue
  -> m (Opt.Parser ConfigValue)
subConfigSpecToCli specEntryKey subConfigSpec acc =
  let updateAccConfigOptParser subConfigParser accOptParser =
        (\subConfig accSubConfig -> case accSubConfig of
            ConfigValue{} -> accSubConfig

            SubConfig subConfigMap ->
              subConfigMap & HashMap.alter (const $ Just subConfig) specEntryKey & SubConfig
          )
          <$> subConfigParser
          <*> accOptParser
  in  do
        configOptParser <- foldM specToConfigValueCli
                                 (pure $ SubConfig HashMap.empty)
                                 (HashMap.toList subConfigSpec)

        return $ updateAccConfigOptParser configOptParser acc

specToConfigValueCli
  :: (MonadThrow m)
  => Opt.Parser ConfigValue
  -> (Text, Spec.ConfigValue ())
  -> m (Opt.Parser ConfigValue)
specToConfigValueCli acc (specEntryKey, specConfigValue) = case specConfigValue of
  Spec.ConfigValue Spec.ConfigValueData { Spec.configValueType, Spec.isSensitive, Spec.configSources }
    -> configValueSpecToCli specEntryKey configValueType isSensitive configSources acc

  Spec.SubConfig subConfigSpec -> subConfigSpecToCli specEntryKey subConfigSpec acc

configValueCliAccInit :: (MonadThrow m) => Spec.ConfigSpec () -> m (Opt.Parser ConfigValue)
configValueCliAccInit spec =
  let zeroParser   = pure $ SubConfig HashMap.empty

      commandsSpec = do
        programSpec <- Spec.specCliProgramSpec spec
        Spec.cliCommands programSpec
  in  case commandsSpec of
        Nothing -> return zeroParser

        Just _  -> throwM CommandKeyOnPlainCli

specToConfigCli :: (MonadThrow m) => Spec.ConfigSpec () -> m (Opt.Parser Config)
specToConfigCli spec = do
  acc    <- configValueCliAccInit spec
  parser <- foldM specToConfigValueCli acc (HashMap.toList $ Spec.specConfigValues spec)

  parser & (Config <$>) & return

{-|

Dynamically generate an OptParser CLI from the spec settings declared on the
@ConfigSpec@. This will process the OptParser from given input
rather than fetching it from the OS.

Once it generates the CLI and gathers the input, it will return the
configuration map with keys defined for the program on @ConfigSpec@.

-}
resolvePlainCliPure
  :: MonadThrow m
  => PlainConfigSpec -- ^ Plain ConfigSpec (no sub-commands)
  -> Text            -- ^ Name of the program running the CLI
  -> [Text]          -- ^ Arglist for the program
  -> m Config        -- ^ returns Configuration Map
resolvePlainCliPure configSpec progName args = do
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

Dynamically generate an OptParser CLI from the spec settings declared on the
@ConfigSpec@.

Once it generates the CLI and gathers the input, it will return the
configuration map with keys defined for the program on @ConfigSpec@.

-}
resolvePlainCli
  :: PlainConfigSpec  -- ^ Plain ConfigSpec (no sub-commands)
  -> IO Config        -- ^ returns Configuration Map
resolvePlainCli configSpec = do
  progName <- Text.pack <$> getProgName
  args     <- map Text.pack <$> getArgs

  handleCliResult $ resolvePlainCliPure configSpec progName args
