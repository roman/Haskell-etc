{-# LANGUAGE ApplicativeDo       #-}
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

import Options.Applicative as Opt
import System.Environment  (getArgs)

import           Etc.Internal.Config         (Config)
import qualified Etc.Internal.Config         as Config
import qualified Etc.Internal.Resolver.Types as Resolver
import qualified Etc.Internal.Spec.Types     as Spec
import qualified Etc.Resolver                as Resolver

import Etc.Internal.Cli.Command.Parser (fetchConfigValueCliSpec)
import Etc.Internal.Cli.Common
import Etc.Internal.Cli.Types


parseConfigPerCmd ::
     (MonadReader BuilderEnv m, MonadThrow m)
  => Seq EntryKey
  -> Spec.ConfigValue
  -> m (Map CmdName (Opt.Parser (Maybe Config.ConfigValue)))
parseConfigPerCmd keyPath configValueSpec =
  case configValueSpec of
    Spec.SubConfig subConfigSpec -> do
      resultList <-
        forM (Map.toList subConfigSpec) $ \(entryKey, childConfigValueSpec) -> do
          configPerCmd <-
            parseConfigPerCmd (keyPath |> entryKey) childConfigValueSpec
          return $ insertEntryKey entryKey configPerCmd
      return $
        List.foldl'
          (Map.unionWith applicativeMergeSubConfigs)
          Map.empty
          resultList
    Spec.ConfigValue configValueData -> do
      result <- fetchConfigValueCliSpec keyPath configValueData
      case result of
        Nothing -> return Map.empty
        Just (entryCmdNames, cliSpec) -> do
          specFilePath <- (Spec.configSpecFilePath . envConfigSpec) <$> ask
          allCmdNames <- envCommandNames <$> ask
          let unknownCmdSet = Set.difference entryCmdNames allCmdNames
          unless (Set.null unknownCmdSet) $
            throwM $
            Spec.SpecError $
            UnknownCommandOnEntry
              specFilePath
              (toList $ keyPath)
              allCmdNames
              unknownCmdSet
          configValueParser <-
            runFieldEnv configValueData cliSpec cliSpecToConfigValueOptParser
          return $
            Map.fromList $
            [ (entryCmdName, configValueParser)
            | entryCmdName <- toList entryCmdNames
            ]
  where
    applicativeMergeSubConfigs mConfigParser1 mConfigParser2 = do
      configParser1 <- mConfigParser1
      configParser2 <- mConfigParser2
      pure $
        (mergeSubConfigs <$> configParser1 <*> configParser2) <|> configParser1 <|>
        configParser2
    mergeSubConfigs (Config.SubConfig config1) (Config.SubConfig config2) =
      Config.SubConfig $ Map.union config1 config2
    mergeSubConfigs _ _ = error "not possible"
    insertEntryKey ::
         EntryKey
      -> Map CmdName (Opt.Parser (Maybe Config.ConfigValue))
      -> Map CmdName (Opt.Parser (Maybe Config.ConfigValue))
    insertEntryKey entryKey =
      Map.map
        -- Map over Parser
        (fmap
        -- Map over Maybe
           (fmap
              (\config ->
                 Config.SubConfig $ Map.insert entryKey config Map.empty)))

assertCommandInputIsValid ::
     (MonadReader BuilderEnv m, MonadThrow m)
  => Set CmdName
  -> Set CmdName
  -> m ()
assertCommandInputIsValid givenCmds allCmds = do
  specFilePath <- (Spec.configSpecFilePath . envConfigSpec) <$> ask
  let missingCmdSet =
        Set.difference givenCmds allCmds
      unknownCmdSet =
        Set.difference allCmds givenCmds
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
  -> m (Map CmdName (Opt.Parser Config))
toConfigPerCmdParser existingConfig = do
  Spec.ConfigSpec {Spec.configSpecEntries} <- envConfigSpec <$> ask
  configParserPerCmd <-
    parseConfigPerCmd Seq.empty (Spec.SubConfig configSpecEntries)
  return (Map.map (fmap toConfig) configParserPerCmd)
  where
    toConfig =
      maybe
        existingConfig
        (\configValue -> existingConfig <> Config.Config configValue)

toCmdOptParser ::
     (MonadReader BuilderEnv m, MonadThrow m)
  => Config
  -> Map CmdName (Opt.Parser (Config -> cmd))
  -> Map CmdName CliInfoSpecData
  -> m (Opt.Parser cmd)
toCmdOptParser config ctorPerCmdName infoPerCmdName = do
  assertCommandInputIsValid
    (Map.keysSet ctorPerCmdName)
    (Map.keysSet infoPerCmdName)
  configPerCmdName <- toConfigPerCmdParser config
  return $
    configParserToCmdParser ctorPerCmdName infoPerCmdName configPerCmdName

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
  arguments <- liftIO getArgs
  resolveCliCommandPure
    arguments
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
