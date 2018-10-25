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
import qualified RIO.Text as Text

import qualified Data.Aeson              as JSON
import qualified Data.Aeson.BetterErrors as JSON
import           Options.Applicative     as Opt

import           Etc.Internal.Config      (Config (..))
import qualified Etc.Internal.Config      as Config
import qualified Etc.Internal.Renderer    as Renderer
import qualified Etc.Internal.Spec.Parser as Spec (matchesConfigValueType)
import qualified Etc.Internal.Spec.Types  as Spec
import qualified Etc.Resolver             as Resolver

import Etc.Internal.Cli.Error ()
import Etc.Internal.Cli.Common (toOptInfoMod, fetchPlainCliInfoSpec)
import Etc.Internal.Cli.Plain.Parser (parseCliEntrySpec)
import Etc.Internal.Cli.Types
--------------------------------------------------------------------------------

#if MIN_VERSION_optparse_applicative(0,14,0)
cliOptSpecToFieldMod ::
     (Opt.HasMetavar f, Opt.HasName f) => CliOptSpec -> Opt.Mod f a
#endif
cliOptSpecToFieldMod optSpec =
  maybe Opt.idm (Opt.long . Text.unpack) (optLong optSpec)
    `mappend` maybe Opt.idm Opt.short                shortOption
    `mappend` maybe Opt.idm (Opt.help . Text.unpack) (optHelp optSpec)
    `mappend` maybe Opt.idm (Opt.metavar . Text.unpack) (optMetavar optSpec)
    `mappend` if optHidden optSpec then Opt.hidden else Opt.idm
    `mappend` if optInternal optSpec then Opt.internal else Opt.idm
 where
  shortOption = do
    shortStr <- optShort optSpec
    fst <$> Text.uncons shortStr

-- Taken from stack codebase
-- | Enable/disable flags for any type, without a default (to allow chaining with '<|>')
enableDisableFlagsNoDefault ::
     Bool -- ^ Enabled value
  -> String -- ^ Name
  -> Maybe String -- ^ Help suffix
  -> Mod FlagFields Bool
  -> Parser Bool
enableDisableFlagsNoDefault defaultValue name helpSuffix mods =
  last <$>
  Opt.some
    (Opt.flag'
       (not defaultValue)
       (Opt.hidden <> Opt.internal <> Opt.long name <> Opt.help (fromMaybe ("Enable " ++ name) helpSuffix) <>
        mods) <|>
     Opt.flag'
       defaultValue
       (Opt.hidden <> Opt.internal <> Opt.long ("no-" ++ name) <>
        Opt.help (fromMaybe ("Disable " ++ name) helpSuffix) <>
        mods) <|>
     Opt.flag'
       (not defaultValue)
       (Opt.long ("[no-]" ++ name) <> Opt.help ("Enable/disable " ++ name ++ maybe "" (" - " ++) helpSuffix) <>
        mods))
  where
    last xs =
      case reverse xs of
        [] -> impureThrow $ stringException "enableDisableFlagsNoDefault.last"
        x:_ -> x

cliArgSpecToFieldMod :: Opt.HasMetavar f => CliArgSpec -> Opt.Mod f a
cliArgSpecToFieldMod argSpec =
  maybe Opt.idm (Opt.help . Text.unpack) (argHelp argSpec)
    `mappend` maybe Opt.idm (Opt.metavar . Text.unpack) (argMetavar argSpec)
    `mappend` maybe Opt.idm (Opt.metavar . Text.unpack) (argHelp argSpec)

coerceConfigValueType ::
     Text
  -> JSON.Value
  -> Spec.ConfigValueType
  -> Maybe JSON.Value
coerceConfigValueType rawValue json cvType = case (json, cvType) of
  (JSON.Null    , Spec.CVTSingle _        )      -> Just JSON.Null
  (JSON.String{}, Spec.CVTSingle Spec.CVTString) -> Just json
  (JSON.Number{}, Spec.CVTSingle Spec.CVTNumber) -> Just json
  (JSON.Bool{}  , Spec.CVTSingle Spec.CVTBool  ) -> Just json
  (JSON.Object{}, Spec.CVTSingle Spec.CVTObject) -> Just json
  (JSON.Array{} , Spec.CVTArray{}         )      -> Just json
  (JSON.Number{}, Spec.CVTSingle Spec.CVTString) -> Just (JSON.String rawValue)
  (JSON.Bool{}  , Spec.CVTSingle Spec.CVTString) -> Just (JSON.String rawValue)
  (_, Spec.CVTSingle (Spec.CVTCustom _))         -> Just json
  _                                              -> Nothing

jsonOptReader ::
     Map Text Spec.CustomType
  -> Spec.ConfigValueType
  -> String
  -> Either String JSON.Value
jsonOptReader customTypes cvType input =
  let cvTypeName = show (Renderer.renderConfigValueType cvType)
      inputText = Text.pack input
      jsonValue =
        fromMaybe
          (JSON.String inputText)
          (JSON.decodeStrict' $ Text.encodeUtf8 inputText)
   in case coerceConfigValueType inputText jsonValue cvType of
        Nothing ->
          Left $
          "\"" <> input <> "\" doesn't match expected type " <> cvTypeName
        Just jsonValue1
          | Spec.matchesConfigValueType customTypes cvType jsonValue1 ->
            Right jsonValue1
          | otherwise ->
            Left $
            "\"" <> input <> "\" doesn't match expected type " <> cvTypeName

buildSwitchOptParser ::
     MonadReader FieldEnv m => CliSwitchSpec -> m (Parser (Maybe JSON.Value))
buildSwitchOptParser switchSpec = do
  defaultValue <-
    (inferDefaultValue . Spec.configValueDefault . fieldConfigValueSpec) <$> ask
  let switchName = Text.unpack $ switchLong switchSpec
      switchFlag :: Opt.Parser Bool
      switchFlag =
        enableDisableFlagsNoDefault
          defaultValue
          switchName
          (Text.unpack <$> switchHelp switchSpec)
          Opt.idm
  return ((Just . JSON.Bool <$> switchFlag) <|> (pure Nothing))
  where
    inferDefaultValue result =
      case result of
        Nothing -> False
        Just (JSON.Bool b) -> b
        _ ->
          error
            "The `buildSwitchOptParser` function relies on validations done a priory that make sure the type of the default value is a boolean, this assumption has been broken from a likely bad usage of this API or an unforseen bug"

cliSpecToJsonOptParser :: MonadReader FieldEnv m => m (Opt.Parser (Maybe JSON.Value))
cliSpecToJsonOptParser = do
  customTypes <- (envCustomTypes . fieldBuildEnv) <$> ask
  cvType <- (Spec.configValueType . fieldConfigValueSpec) <$> ask
  specSettings <- fieldCliEntrySpec <$> ask
  case specSettings of
    Opt optSpec ->
      return $
      traceShow optSpec $
      Opt.optional $
      Opt.option
        (Opt.eitherReader $ jsonOptReader customTypes cvType)
        (cliOptSpecToFieldMod optSpec)
    Arg argSpec ->
      return $
      Opt.optional $
      Opt.argument
        (Opt.eitherReader $ jsonOptReader customTypes cvType)
        (cliArgSpecToFieldMod argSpec)
    Switch switchSpec -> buildSwitchOptParser switchSpec

jsonToSomeConfigValue ::
  Int -> Bool -> CliEntrySpec -> JSON.Value -> Config.ConfigValue
jsonToSomeConfigValue priorityIndex isSensitive cliSpec jsonValue =
  let
    fieldValue =
      Config.SomeConfigSource priorityIndex $ CliSource cliSpec jsonValue
  in
    Config.ConfigValue isSensitive (Set.singleton fieldValue)

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
               Map.alter
                 (const configValue)
                 (fromEntryKey specEntryKey)
                 subConfigMap) <$>
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
      let result = JSON.parseValue
                     (JSON.keyMay "cli" (parseCliEntrySpec configValueType))
                     configValueJSON
      case result of
        Left err ->
          throwM (fromCliParseError specFilePath (keyPath |> specEntryKey) err)
        Right Nothing -> return Nothing
        Right (Just cliSpec) -> return $ Just cliSpec
    _ -> return Nothing

specToConfigValueOptParser
  :: (MonadReader BuilderEnv m, MonadThrow m)
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
  (MonadIO m, MonadThrow m) =>
  Int -> Map Text Spec.CustomType -> Spec.ConfigSpec -> m Config
resolveCli priorityIndex customTypes spec = do
  let builderEnv = BuilderEnv { envPriorityIndex = priorityIndex
                              , envCustomTypes = customTypes
                              , envConfigSpec = spec
                              , envCommandNames = Set.empty
                              }
  configParser <- runReaderT toOptParser builderEnv
  cliInfoSpecData <- fetchPlainCliInfoSpec spec
  let infoMod = toOptInfoMod cliInfoSpecData
  liftIO $
    Opt.execParser
      (Opt.info (configParser Opt.<**> Opt.helper)
                infoMod)

resolveCliPure ::
  (MonadIO m, MonadThrow m) =>
  [String] -> Int -> Map Text Spec.CustomType -> Spec.ConfigSpec -> m Config
resolveCliPure inputArgs priorityIndex customTypes spec = do
  let builderEnv = BuilderEnv { envPriorityIndex = priorityIndex
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
      (Opt.info (configParser Opt.<**> Opt.helper)
                infoMod)
      inputArgs

-- | PENDING
cliResolver :: (MonadIO m, MonadThrow m) => Resolver.Resolver m
cliResolver = Resolver.Resolver resolveCli

-- | PENDING
pureCliResolver :: (MonadIO m, MonadThrow m) => [String] -> Resolver.Resolver m
pureCliResolver args = Resolver.Resolver $ resolveCliPure args
