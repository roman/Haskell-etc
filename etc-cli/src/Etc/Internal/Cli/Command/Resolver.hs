{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Etc.Internal.Cli.Command.Resolver where

import           RIO
import qualified RIO.List as List
import qualified RIO.Map  as Map
import           RIO.Seq  (Seq, (|>))
import qualified RIO.Seq  as Seq
import qualified RIO.Set  as Set
import qualified RIO.Text as Text

import qualified Data.Aeson              as JSON
import qualified Data.Aeson.BetterErrors as JSON
import           Options.Applicative     as Opt

import           Etc.Internal.Config         (Config)
import qualified Etc.Internal.Config         as Config
import qualified Etc.Internal.Resolver       as Resolver
import qualified Etc.Internal.Resolver.Types as Resolver
import qualified Etc.Internal.Spec.Types     as Spec

import Etc.Internal.Cli.Command.Parser (parseCliEntryCommandsSpec)
import Etc.Internal.Cli.Common         (fetchCommandCliInfoSpec, toOptInfoMod)
import Etc.Internal.Cli.Plain.Parser   (parseCliEntrySpec)
import Etc.Internal.Cli.Plain.Resolver (configValueSpecToOptParser, fromCliParseError)
import Etc.Internal.Cli.Types


type EntryKey = Text
type CmdName = Text

fetchConfigValueCliSpecPerCommand ::
     (MonadReader BuilderEnv m, MonadThrow m)
  => Seq Text
  -> Text
  -> Spec.ConfigValueData
  -> m (Map Text CliEntrySpec)
fetchConfigValueCliSpecPerCommand keyPath specEntryKey specData = do
  specFilePath <- (Spec.configSpecFilePath . envConfigSpec) <$> ask
  case configValueJSON of
    JSON.Object _ -> do
      let result =
            JSON.parseValue
              (JSON.keyMay "cli" parseCliCommandEntrySpec)
              configValueJSON
      case result of
        Left err ->
          throwM (fromCliParseError specFilePath (keyPath |> specEntryKey) err)
        Right Nothing -> return Map.empty
        Right (Just (commands, cliSpec))
          -- TODO: Validate commands are the same as the top-level ones
          -- throwM
         -> return $ Map.fromList $ zip commands (List.repeat cliSpec)
    _ -> return Map.empty
  where
    parseCliCommandEntrySpec =
      (,) <$> parseCliEntryCommandsSpec <*> parseCliEntrySpec configValueType
    Spec.ConfigValueData {Spec.configValueType, Spec.configValueJSON} = specData

configValueSpecToOptParserPerCommand ::
     (MonadReader BuilderEnv m, MonadThrow m)
  => Text
  -> Spec.ConfigValueData
  -> Opt.Parser (Map Text Config.ConfigValue)
  -> (Text, CliEntrySpec)
  -> m (Opt.Parser (Map Text Config.ConfigValue))
configValueSpecToOptParserPerCommand specEntryKey configValueData accParsers (cmdName, cliSpec) =
  buildCommandConfigParser
  where
    getCommandConfigParserAcc :: Opt.Parser Config.ConfigValue
    getCommandConfigParserAcc =
      let errMsg =
            mconcat
              [ "A cmdName that has not been validated was used. This function relies on "
              , "a caller function to validate that the commands are well defined, if for some "
              , "reason the field spec contains commands that are not defined at the top-level etc/cmdName "
              , "and we reach this function, there is a bug in the cmdName validation"
              ]
       in (fromMaybe (error errMsg) . Map.lookup cmdName) <$> accParsers

    putCommandConfigParserAcc ::
         Opt.Parser Config.ConfigValue
      -> Opt.Parser (Map Text Config.ConfigValue)
    putCommandConfigParserAcc cmdParserAcc =
      (Map.insert cmdName <$> cmdParserAcc <*> accParsers)

    -- buildCommandConfigParser :: m (Opt.Parser (Map Text Config.ConfigValue))
    buildCommandConfigParser = do
      let cmdParserAcc0 = getCommandConfigParserAcc
      env <- ask
      cmdParserAcc <-
        runReaderT
          (configValueSpecToOptParser specEntryKey cmdParserAcc0)
          (FieldEnv
             { fieldBuildEnv = env
             , fieldCliEntrySpec = cliSpec
             , fieldConfigValueSpec = configValueData
             })
      return $ putCommandConfigParserAcc cmdParserAcc

subConfigSpecToOptParser ::
     (MonadReader BuilderEnv m, MonadThrow m)
  => Seq EntryKey
  -> EntryKey
  -> Map EntryKey Spec.ConfigValue
  -> Opt.Parser (Map CmdName Config.ConfigValue)
  -> m (Opt.Parser (Map CmdName Config.ConfigValue))
subConfigSpecToOptParser keyPath specEntryKey subConfigSpec accParsers = do
  configOptParser <-
    foldM
      (specToConfigValueOptParser (keyPath |> specEntryKey))
      accParsers
      (Map.toList subConfigSpec)
  return $ updateAccConfigOptParser configOptParser accParsers
  where
    updateAccConfig subConfig accSubConfig =
      case accSubConfig of
        Config.ConfigValue {} -> accSubConfig
        Config.SubConfig subConfigMap ->
          Config.SubConfig
            (Map.alter (const $ Just subConfig) specEntryKey subConfigMap)

    updateAccConfigOptParser ::
         Opt.Parser (Map CmdName Config.ConfigValue)
      -> Opt.Parser (Map CmdName Config.ConfigValue)
      -> Opt.Parser (Map CmdName Config.ConfigValue)
    updateAccConfigOptParser subConfigParser accPerCmd =
      Map.unionWith updateAccConfig <$> subConfigParser <*> accPerCmd

specToConfigValueOptParser ::
     (MonadReader BuilderEnv m, MonadThrow m)
  => Seq EntryKey
  -> Opt.Parser (Map CmdName Config.ConfigValue)
  -> (EntryKey, Spec.ConfigValue)
  -> m (Opt.Parser (Map CmdName Config.ConfigValue))
specToConfigValueOptParser keyPath accParsers (specEntryKey, specConfigValue) = do
  case specConfigValue of
    Spec.ConfigValue configValueData -> do
      cliSpecPerCommand <-
        fetchConfigValueCliSpecPerCommand keyPath specEntryKey configValueData
      foldM
        (configValueSpecToOptParserPerCommand specEntryKey configValueData)
        accParsers
        (Map.toList cliSpecPerCommand)
    Spec.SubConfig subConfigSpec ->
      subConfigSpecToOptParser keyPath specEntryKey subConfigSpec accParsers

assertCommandInputIsValid ::
     (MonadReader BuilderEnv m, MonadThrow m)
  => Map Text ctor
  -> Map Text info
  -> m ()
assertCommandInputIsValid ctorPerCmdName infoPerCmdName = do
  specFilePath <- (Spec.configSpecFilePath . envConfigSpec) <$> ask

  let
    unknownCmdSet =
      Set.difference (Map.keysSet infoPerCmdName) (Map.keysSet ctorPerCmdName)
    missingCmdSet =
      Set.difference (Map.keysSet ctorPerCmdName) (Map.keysSet infoPerCmdName)

  unless (Set.null unknownCmdSet && Set.null missingCmdSet) $
    throwM (Resolver.ResolverError $ CommandListMismatch specFilePath unknownCmdSet missingCmdSet)

configParserToCmdParser ::
     Map CmdName (Opt.Parser (Config -> cmd))
  -> Map CmdName CliInfoSpecData
  -> Opt.Parser (Map CmdName Config)
  -> Opt.Parser cmd
configParserToCmdParser ctorPerCmdName infoPerCmdName configPerCmdName =
  Opt.hsubparser $ foldl' processCmdName mempty (Map.toList infoPerCmdName)
  where
    lookupConfigPerCmdName cmdName =
      let errorMsg = "Command keys don't match, invalid usage of API"
          cmdCtor =
            fromMaybe (error errorMsg) $ Map.lookup cmdName ctorPerCmdName
       in cmdCtor <*>
          (fromMaybe (error errorMsg) . Map.lookup cmdName <$> configPerCmdName)
    processCmdName acc (cmdName, cliInfoData) =
      let cmdInfo = toOptInfoMod cliInfoData
       in acc <>
          Opt.command
            (Text.unpack cmdName)
            (Opt.info (lookupConfigPerCmdName cmdName) cmdInfo)

toConfigPerCmdParser ::
     (MonadReader BuilderEnv m, MonadThrow m)
  => Config
  -> [CmdName]
  -> m (Opt.Parser (Map CmdName Config))
toConfigPerCmdParser existingConfig cmdNames = do
  spec <- envConfigSpec <$> ask
  let acc =
        pure $
        Map.fromList
          [(cmdName, Config.SubConfig Map.empty) | cmdName <- cmdNames]
  parser <-
    foldM
      (specToConfigValueOptParser Seq.empty)
      acc
      (Map.toList $ Spec.configSpecEntries spec)
  return (Map.map toConfig <$> parser)
  where
    toConfig configValue = existingConfig <> (Config.Config configValue)

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
  -> Map Text (Opt.Parser (Config -> cmd))
  -> Int
  -> Map Text Spec.CustomType
  -> Spec.ConfigSpec
  -> m cmd
resolveCliCommand config ctorPerCmdName priorityIndex customTypes spec = do
  let builderEnv =
        BuilderEnv
          { envPriorityIndex = priorityIndex
          , envCustomTypes = customTypes
          , envConfigSpec = spec
          }
  (topLevelInfo, infoPerCmdName) <- fetchCommandCliInfoSpec spec
  cmdParser <-
    runReaderT (toCmdOptParser config ctorPerCmdName infoPerCmdName) builderEnv
  let infoMod = toOptInfoMod topLevelInfo
  liftIO $ Opt.execParser (Opt.info (cmdParser Opt.<**> Opt.helper) infoMod)


resolveConfigWithCliCommand ::
     (MonadThrow m, MonadUnliftIO m)
  => [(Text, Opt.Parser (Config -> cmd))]
  -> [(Text, Spec.CustomType)]
  -> [Resolver.Resolver m]
  -> Spec.ConfigSpec
  -> m cmd
resolveConfigWithCliCommand ctorPerCmdName customTypeList resolvers spec
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
    (Map.fromList ctorPerCmdName)
    priorityIndex
    (Map.fromList customTypeList)
    spec
