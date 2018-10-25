{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Etc.Internal.Cli.Plain.Resolver where

import           RIO
import qualified RIO.Map  as Map
import           RIO.Seq  (Seq, (|>))
import qualified RIO.Seq  as Seq
import qualified RIO.Set  as Set

import qualified Data.Aeson              as JSON
import qualified Data.Aeson.BetterErrors as JSON
import           Options.Applicative     as Opt

import           Etc.Internal.Config      (Config (..))
import qualified Etc.Internal.Config      as Config
import qualified Etc.Internal.Spec.Types  as Spec
import qualified Etc.Resolver             as Resolver

import Etc.Internal.Cli.Error ()
import Etc.Internal.Cli.Common
import Etc.Internal.Cli.Plain.Parser (parseCliEntrySpec)
import Etc.Internal.Cli.Types
--------------------------------------------------------------------------------

configValueSpecToOptParser
  :: (MonadReader FieldEnv m, MonadThrow m)
  => EntryKey
  -> Parser Config.ConfigValue
  -> m (Parser Config.ConfigValue)
configValueSpecToOptParser specEntryKey acc = do
  priorityIndex <- (envPriorityIndex . fieldBuildEnv) <$> ask
  configValueData <- fieldConfigValueSpec <$> ask
  cliSpec <- fieldCliEntrySpec <$> ask
  let Spec.ConfigValueData {Spec.configValueSensitive} = configValueData
      updateAccConfigOptParser configValueParser' accOptParser =
        (\configValue accSubConfig ->
           case accSubConfig of
             Config.ConfigValue {} -> accSubConfig
             Config.SubConfig subConfigMap ->
               Config.SubConfig $
               Map.alter (const configValue) specEntryKey subConfigMap) <$>
        configValueParser' <*>
        accOptParser
  jsonOptParser <- cliSpecToJsonOptParser
  let configValueParser =
        (fmap (jsonToSomeConfigValue priorityIndex configValueSensitive cliSpec)) <$>
        jsonOptParser
  return $ updateAccConfigOptParser configValueParser acc

subConfigSpecToOptParser ::
     (MonadReader BuilderEnv m, MonadThrow m)
  => Seq Text
  -> Text
  -> Map Text (Spec.ConfigValue)
  -> Opt.Parser Config.ConfigValue
  -> m (Opt.Parser Config.ConfigValue)
subConfigSpecToOptParser keyPath specEntryKey subConfigSpec acc = do
  configOptParser <-
    foldM
      (specToConfigValueOptParser (keyPath |> specEntryKey))
      (pure $ Config.SubConfig Map.empty)
      (Map.toList subConfigSpec)
  return $ updateAccConfigOptParser configOptParser acc
  where
    updateAccConfig subConfig accSubConfig =
      case accSubConfig of
        Config.ConfigValue {} -> accSubConfig
        Config.SubConfig subConfigMap ->
          Config.SubConfig
            (Map.alter (const $ Just subConfig) specEntryKey subConfigMap)
    updateAccConfigOptParser ::
         Opt.Parser Config.ConfigValue
      -> Opt.Parser Config.ConfigValue
      -> Opt.Parser Config.ConfigValue
    updateAccConfigOptParser subConfigParser accOptParser =
      updateAccConfig <$> subConfigParser <*> accOptParser

fromCliParseError ::
     Text
  -> Seq Text
  -> JSON.ParseError CliEntryParseError
  -> Resolver.ResolverError (JSON.ParseError CliResolverError)
fromCliParseError specFilePath keyPath =
  Resolver.ResolverError . fmap fromParseError_
  where
    fromParseError_ mkErr = mkErr specFilePath (toList keyPath)

fetchCliSpec ::
     (MonadReader BuilderEnv m, MonadThrow m)
  => Seq Text
  -> Text
  -> Spec.ConfigValueData
  -> m (Maybe CliEntrySpec)
fetchCliSpec keyPath specEntryKey specData = do
  specFilePath <- (Spec.configSpecFilePath . envConfigSpec) <$> ask
  let Spec.ConfigValueData {Spec.configValueType, Spec.configValueJSON} =
        specData
  case configValueJSON of
    JSON.Object _ -> do
      let result =
            JSON.parseValue
              (JSON.keyMay "cli" (parseCliEntrySpec configValueType))
              configValueJSON
      case result of
        Left err ->
          throwM (fromCliParseError specFilePath (keyPath |> specEntryKey) err)
        Right Nothing -> return Nothing
        Right (Just cliSpec) -> return $ Just cliSpec
    _ -> return Nothing

specToConfigValueOptParser ::
     (MonadReader BuilderEnv m, MonadThrow m)
  => Seq Text
  -> Opt.Parser Config.ConfigValue
  -> (Text, Spec.ConfigValue)
  -> m (Opt.Parser Config.ConfigValue)
specToConfigValueOptParser keyPath acc (specEntryKey, specConfigValue) = do
  case specConfigValue of
    Spec.ConfigValue configValueData -> do
      result <- fetchCliSpec keyPath specEntryKey configValueData
      case result of
        Nothing -> return acc
        Just cliSpec -> do
          env <- ask
          flip
            runReaderT
            (FieldEnv
               { fieldBuildEnv = env
               , fieldCliEntrySpec = cliSpec
               , fieldConfigValueSpec = configValueData
               }) $
            configValueSpecToOptParser specEntryKey acc
    Spec.SubConfig subConfigSpec ->
      subConfigSpecToOptParser keyPath specEntryKey subConfigSpec acc

toOptParser :: (MonadReader BuilderEnv m, MonadThrow m) => m (Opt.Parser Config)
toOptParser = do
  spec <- envConfigSpec <$> ask
  let acc = pure $ Config.SubConfig Map.empty
  parser <-
    foldM
      (specToConfigValueOptParser Seq.empty)
      acc
      (Map.toList $ Spec.configSpecEntries spec)
  return (Config <$> parser)

resolveCli ::
     (MonadIO m, MonadThrow m)
  => Int
  -> Map Text Spec.CustomType
  -> Spec.ConfigSpec
  -> m Config
resolveCli priorityIndex customTypes spec = do
  let builderEnv =
        BuilderEnv
          { envPriorityIndex = priorityIndex
          , envCustomTypes = customTypes
          , envConfigSpec = spec
          , envCommandNames = Set.empty
          }
  configParser <- runReaderT toOptParser builderEnv
  cliInfoSpecData <- fetchPlainCliInfoSpec spec
  let infoMod = toOptInfoMod cliInfoSpecData
  liftIO $ Opt.execParser (Opt.info (configParser Opt.<**> Opt.helper) infoMod)

resolveCliPure ::
     (MonadIO m, MonadThrow m)
  => [String]
  -> Int
  -> Map Text Spec.CustomType
  -> Spec.ConfigSpec
  -> m Config
resolveCliPure inputArgs priorityIndex customTypes spec = do
  let builderEnv =
        BuilderEnv
          { envPriorityIndex = priorityIndex
          , envCustomTypes = customTypes
          , envConfigSpec = spec
          , envCommandNames = Set.empty
          }
  configParser <- runReaderT toOptParser builderEnv
  cliInfoSpecData <- fetchPlainCliInfoSpec spec
  let infoMod = toOptInfoMod cliInfoSpecData
  liftIO $
    Opt.handleParseResult $
    Opt.execParserPure
      Opt.defaultPrefs
      (Opt.info (configParser Opt.<**> Opt.helper) infoMod)
      inputArgs

-- | PENDING
cliResolver :: (MonadIO m, MonadThrow m) => Resolver.Resolver m
cliResolver = Resolver.Resolver resolveCli

-- | PENDING
pureCliResolver :: (MonadIO m, MonadThrow m) => [String] -> Resolver.Resolver m
pureCliResolver args = Resolver.Resolver $ resolveCliPure args
