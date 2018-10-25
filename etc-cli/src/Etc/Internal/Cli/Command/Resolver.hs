{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Etc.Internal.Cli.Command.Resolver where

import           RIO
import qualified RIO.Map  as Map
import           RIO.Seq  (Seq, (|>))
import qualified RIO.Seq  as Seq
import qualified RIO.Set  as Set
import qualified RIO.Text as Text

import qualified Data.Aeson              as JSON
import qualified Data.Aeson.BetterErrors as JSON
import           Data.Coerce             (coerce)
import           Options.Applicative     as Opt

import           Etc.Internal.Config         (Config)
import qualified Etc.Internal.Config         as Config
import qualified Etc.Internal.Resolver.Types as Resolver
import qualified Etc.Internal.Spec.Types     as Spec
import qualified Etc.Resolver                as Resolver

import           Etc.Internal.Cli.Command.Parser (parseCliEntryCommandsSpec)
import           Etc.Internal.Cli.Common         (fetchCommandCliInfoSpec, toOptInfoMod)
import           Etc.Internal.Cli.Plain.Parser   (parseCliEntrySpec)
import qualified Etc.Internal.Cli.Plain.Resolver as Plain
    (configValueSpecToOptParser, fromCliParseError)
import           Etc.Internal.Cli.Types

fetchConfigValueCliSpec ::
     (MonadReader BuilderEnv m, MonadThrow m)
  => Seq EntryKey
  -> Spec.ConfigValueData
  -> m (Maybe (Set CmdName, CliEntrySpec))
fetchConfigValueCliSpec keyPath specData = do
  specFilePath <- (Spec.configSpecFilePath . envConfigSpec) <$> ask
  allCmdNames <- envCommandNames <$> ask
  case configValueJSON of
    JSON.Object _ -> do
      let result =
            JSON.parseValue
              (JSON.keyMay "cli" parseCliCommandEntrySpec)
              configValueJSON
      case result of
        Left err ->
          throwM (Plain.fromCliParseError specFilePath (coerce keyPath) err)
        Right Nothing -> return Nothing
        Right (Just (cmdNames, cliSpec)) -> do
          let cmdNamesSet = Set.fromList cmdNames
              unknownCmdSet = Set.difference cmdNamesSet allCmdNames
          unless (Set.null unknownCmdSet) $
            throwM $
            Spec.SpecError $
            UnknownCommandOnEntry
              specFilePath
              (toList $ keyPath)
              allCmdNames
              unknownCmdSet
          return $ Just (Set.fromList cmdNames, cliSpec)
    _ -> return Nothing
  where
    parseCliCommandEntrySpec =
      (,) <$> parseCliEntryCommandsSpec <*> parseCliEntrySpec configValueType
    Spec.ConfigValueData {Spec.configValueType, Spec.configValueJSON} = specData

subConfigSpecToOptParser ::
     (MonadReader BuilderEnv m, MonadThrow m)
  => Seq EntryKey
  -> EntryKey
  -> Map EntryKey Spec.ConfigValue
  -> Map CmdName (Opt.Parser (Config.ConfigValue))
  -> m (Map CmdName (Opt.Parser (Config.ConfigValue)))
subConfigSpecToOptParser keyPath specEntryKey subConfigSpec accParsers = do
  configOptParser <-
    foldM
      (configValueSpecToOptParser (keyPath |> specEntryKey))
      accParsers
      (Map.toList subConfigSpec)
  return $ updateAccConfigOptParser configOptParser accParsers
  where
    updateAccConfig subConfigA accSubConfigA =
      (\subConfig accSubConfig ->
         case accSubConfig of
            Config.ConfigValue {} -> accSubConfig
            Config.SubConfig subConfigMap ->
              Config.SubConfig
                (Map.alter (const $ Just subConfig) specEntryKey subConfigMap)
             ) <$> subConfigA <*> accSubConfigA

    updateAccConfigOptParser ::
         Map CmdName (Opt.Parser Config.ConfigValue)
      -> Map CmdName (Opt.Parser Config.ConfigValue)
      -> Map CmdName (Opt.Parser Config.ConfigValue)
    updateAccConfigOptParser subConfigParser accPerCmd =
      Map.unionWith updateAccConfig subConfigParser accPerCmd

configValueSpecToOptParser ::
     (MonadReader BuilderEnv m, MonadThrow m)
  => Seq EntryKey
  -> Map CmdName (Opt.Parser (Config.ConfigValue))
  -> (EntryKey, Spec.ConfigValue)
  -> m (Map CmdName (Opt.Parser (Config.ConfigValue)))
configValueSpecToOptParser keyPath configValueParserPerCmd (specEntryKey, specConfigValue) = do
  case specConfigValue of
    Spec.ConfigValue configValueData -> do
      mcliSpecPerCommand <-
        fetchConfigValueCliSpec (keyPath |> specEntryKey) configValueData
      case mcliSpecPerCommand of
        Nothing -> return configValueParserPerCmd
        Just (cmdNames, cliEntrySpec) -> do
          env <- ask
          configValueParser <-
            runReaderT
              (Plain.configValueSpecToOptParser specEntryKey (pure mempty))
              (FieldEnv
                 { fieldBuildEnv = env
                 , fieldCliEntrySpec = cliEntrySpec
                 , fieldConfigValueSpec = configValueData
                 })
          return $
            Map.mapWithKey
              (\cmdName existingConfig ->
                 if Set.member cmdName cmdNames
                   then (<>) <$> existingConfig <*> configValueParser
                   else existingConfig)
              configValueParserPerCmd
    Spec.SubConfig subConfigSpec ->
      subConfigSpecToOptParser
        keyPath
        specEntryKey
        subConfigSpec
        configValueParserPerCmd

assertCommandInputIsValid ::
     (MonadReader BuilderEnv m, MonadThrow m)
  => Map CmdName ctor
  -> Map CmdName info
  -> m ()
assertCommandInputIsValid ctorPerCmdName infoPerCmdName = do
  specFilePath <- (Spec.configSpecFilePath . envConfigSpec) <$> ask
  let missingCmdSet =
        Set.difference (Map.keysSet infoPerCmdName) (Map.keysSet ctorPerCmdName)
      unknownCmdSet =
        Set.difference (Map.keysSet ctorPerCmdName) (Map.keysSet infoPerCmdName)
  unless (Set.null unknownCmdSet && Set.null missingCmdSet) $
    throwM
      (Resolver.ResolverError $
       CommandListMismatch specFilePath missingCmdSet unknownCmdSet)

configParserToCmdParser ::
     Map CmdName (Opt.Parser (Config -> cmd))
  -> Map CmdName CliInfoSpecData
  -> Map CmdName (Opt.Parser Config)
  -> Opt.Parser cmd
configParserToCmdParser ctorPerCmdName infoPerCmdName configPerCmdName =
  Opt.hsubparser $ foldl' processCmdName mempty (Map.toList infoPerCmdName)
  where
    lookupConfigPerCmdName cmdName =
      let errorMsg = "Command keys don't match, invalid usage of API"
          cmdCtor =
            fromMaybe (error errorMsg) $ Map.lookup cmdName ctorPerCmdName
          configParser =
            fromMaybe (error errorMsg) $ Map.lookup cmdName configPerCmdName
       in cmdCtor <*> configParser
    processCmdName acc (cmdName, cliInfoData) =
      let cmdInfo = toOptInfoMod cliInfoData
       in acc <>
          Opt.command
            (Text.unpack $ fromCmdName cmdName)
            (Opt.info (lookupConfigPerCmdName cmdName) cmdInfo)

toConfigPerCmdParser ::
     (MonadReader BuilderEnv m, MonadThrow m)
  => Config
  -> [CmdName]
  -> m (Map CmdName (Opt.Parser Config))
toConfigPerCmdParser existingConfig cmdNames = do
  spec <- envConfigSpec <$> ask
  let acc =
        Map.fromList
          [(cmdName, pure $ Config.SubConfig Map.empty) | cmdName <- cmdNames]
  parser <-
    foldM
      (configValueSpecToOptParser Seq.empty)
      acc
      (Map.toList $ Spec.configSpecEntries spec)
  return (Map.map toConfig parser)
  where
    toConfig configValue =
      ((existingConfig <>) . Config.Config) <$> configValue

toCmdOptParser ::
     (MonadReader BuilderEnv m, MonadThrow m)
  => Config
  -> Map CmdName (Opt.Parser (Config -> cmd))
  -> Map CmdName CliInfoSpecData
  -> m (Opt.Parser cmd)
toCmdOptParser config ctorPerCmdName infoPerCmdName = do
  assertCommandInputIsValid ctorPerCmdName infoPerCmdName
  configPerCmdName <- toConfigPerCmdParser config (Map.keys infoPerCmdName)
  return $ configParserToCmdParser ctorPerCmdName infoPerCmdName configPerCmdName

resolveCliCommand ::
     (MonadIO m, MonadThrow m)
  => Config
  -> Map CmdName (Opt.Parser (Config -> cmd))
  -> Int
  -> Map Text Spec.CustomType
  -> Spec.ConfigSpec
  -> m cmd
resolveCliCommand config ctorPerCmdName priorityIndex customTypes spec = do
  (topLevelInfo, infoPerCmdName) <- fetchCommandCliInfoSpec spec
  let builderEnv =
        BuilderEnv
          { envPriorityIndex = priorityIndex
          , envCustomTypes = customTypes
          , envConfigSpec = spec
          , envCommandNames = Map.keysSet infoPerCmdName
          }
  cmdParser <-
    runReaderT (toCmdOptParser config ctorPerCmdName infoPerCmdName) builderEnv
  let infoMod = toOptInfoMod topLevelInfo
  liftIO $ Opt.execParser (Opt.info (cmdParser Opt.<**> Opt.helper) infoMod)

resolveCliCommandPure ::
     (MonadIO m, MonadThrow m)
  => [String]
  -> Config
  -> Map CmdName (Parser (Config -> cmd))
  -> Int
  -> Map Text Spec.CustomType
  -> Spec.ConfigSpec
  -> m cmd
resolveCliCommandPure inputArgs config ctorPerCmdName priorityIndex customTypes spec = do
  (topLevelInfo, infoPerCmdName) <- fetchCommandCliInfoSpec spec
  let builderEnv =
        BuilderEnv
          { envPriorityIndex = priorityIndex
          , envCustomTypes = customTypes
          , envConfigSpec = spec
          , envCommandNames = Map.keysSet infoPerCmdName
          }
  cmdParser <-
    runReaderT (toCmdOptParser config ctorPerCmdName infoPerCmdName) builderEnv
  let infoMod = toOptInfoMod topLevelInfo
  liftIO $
    Opt.handleParseResult $
    Opt.execParserPure
      Opt.defaultPrefs
      (Opt.info (cmdParser Opt.<**> Opt.helper) infoMod)
      inputArgs

resolveConfigWith ::
     (MonadThrow m, MonadUnliftIO m)
  => [(Text, Opt.Parser (Config -> cmd))]
  -> [(Text, Spec.CustomType)]
  -> [Resolver.Resolver m]
  -> Spec.ConfigSpec
  -> m cmd
resolveConfigWith ctorPerCmdName customTypeList resolvers spec
  -- NOTE: This code bellow is making assumptions from Etc private API, the reason
  -- we do this, is because resolveConfigWith only returns Config values, while we
  -- are interested in returning a cmd record, given this is an edge case, we
  -- didn't want to modify the core API just for this particular use case.
  -- By using emptyResolver we are making the values of the command the one
  -- that has the highest precedence
 = do
  let priorityIndex = 0
  configFromOtherSources <-
    Resolver.resolveConfigWith
      customTypeList
      (resolvers ++ [Resolver.emptyResolver])
      spec
  resolveCliCommand
    configFromOtherSources
    (Map.fromList $ map (first CmdName) ctorPerCmdName)
    priorityIndex
    (Map.fromList customTypeList)
    spec


resolveConfigWith1 ::
     (MonadThrow m, MonadUnliftIO m)
  => [String]
  -> [(Text, Opt.Parser (Config -> cmd))]
  -> [(Text, Spec.CustomType)]
  -> [Resolver.Resolver m]
  -> Spec.ConfigSpec
  -> m cmd
resolveConfigWith1 inputArgs ctorPerCmdName customTypeList resolvers spec
  -- NOTE: This code bellow is making assumptions from Etc private API, the reason
  -- we do this, is because resolveConfigWith only returns Config values, while we
  -- are interested in returning a cmd record, given this is an edge case, we
  -- didn't want to modify the core API just for this particular use case.
  -- By using emptyResolver we are making the values of the command the one
  -- that has the highest precedence
 = do
  let priorityIndex = 0
  configFromOtherSources <-
    Resolver.resolveConfigWith
      customTypeList
      (resolvers ++ [Resolver.emptyResolver])
      spec
  resolveCliCommandPure
    inputArgs
    configFromOtherSources
    (Map.fromList $ map (first CmdName) ctorPerCmdName)
    priorityIndex
    (Map.fromList customTypeList)
    spec
