{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Etc.Internal.Cli.Common where

import           RIO
import qualified RIO.Set  as Set
import qualified RIO.Text as Text

import qualified Data.Aeson              as JSON
import qualified Data.Aeson.BetterErrors as JSON
import qualified Options.Applicative     as Opt


import qualified Etc.Internal.Config         as Config
import qualified Etc.Internal.Renderer       as Renderer
import qualified Etc.Internal.Resolver.Types as Resolver
import           Etc.Internal.Spec.Error ()
import qualified Etc.Internal.Spec.Parser    as Spec (matchesConfigValueType)
import qualified Etc.Internal.Spec.Types     as Spec

import Etc.Internal.Cli.Error ()
import Etc.Internal.Cli.Plain.Parser
import Etc.Internal.Cli.Types

fetchCliInfoSpec :: MonadThrow m => Spec.ConfigSpec -> m CliInfoSpec
fetchCliInfoSpec Spec.ConfigSpec {Spec.configSpecFilePath, Spec.configSpecJSON} = do
  let result =
        JSON.parseValue
          (JSON.keyMay "etc/cli" parseCliInfoSpec)
          (JSON.Object configSpecJSON)
  case result of
    Left err ->
      throwM (Resolver.ResolverError (err :: JSON.ParseError CliResolverError))
    Right Nothing ->
      throwM (Resolver.ResolverError (InfoModMissing configSpecFilePath))
    Right (Just cliInfoSpec) -> pure cliInfoSpec


fetchPlainCliInfoSpec :: MonadThrow m => Spec.ConfigSpec -> m CliInfoSpecData
fetchPlainCliInfoSpec spec@Spec.ConfigSpec {Spec.configSpecFilePath} = do
  result <- fetchCliInfoSpec spec
  case result of
    PlainCliInfoSpec topLevelInfo ->
      return topLevelInfo
    CommandCliInfoSpec {} ->
      throwM (Resolver.ResolverError $ PlainInfoModExpected configSpecFilePath)

fetchCommandCliInfoSpec ::
     MonadThrow m
  => Spec.ConfigSpec
  -> m (CliInfoSpecData, Map CmdName CliInfoSpecData)
fetchCommandCliInfoSpec spec@Spec.ConfigSpec {Spec.configSpecFilePath} = do
  result <- fetchCliInfoSpec spec
  case result of
    PlainCliInfoSpec _ ->
      throwM
        (Resolver.ResolverError $ CommandInfoModExpected configSpecFilePath)
    CommandCliInfoSpec topLevelInfo infoPerCmdName ->
      return (topLevelInfo, infoPerCmdName)

toOptInfoMod :: CliInfoSpecData -> Opt.InfoMod a
toOptInfoMod cliInfoSpecData =
  Opt.fullDesc <>
  Opt.progDesc (Text.unpack $ cisProgDesc cliInfoSpecData) <>
  maybe Opt.idm (Opt.header . Text.unpack) (cisHeader cliInfoSpecData) <>
  maybe Opt.idm (Opt.footer . Text.unpack) (cisFooter cliInfoSpecData)

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

cliArgSpecToFieldMod :: Opt.HasMetavar f => CliArgSpec -> Opt.Mod f a
cliArgSpecToFieldMod argSpec =
  maybe Opt.idm (Opt.help . Text.unpack) (argHelp argSpec)
    `mappend` maybe Opt.idm (Opt.metavar . Text.unpack) (argMetavar argSpec)
    `mappend` maybe Opt.idm (Opt.metavar . Text.unpack) (argHelp argSpec)

-- Taken from stack codebase
-- | Enable/disable flags for any type, without a default (to allow chaining with '<|>')
enableDisableFlagsNoDefault ::
     Bool -- ^ Enabled value
  -> String -- ^ Name
  -> Maybe String -- ^ Help suffix
  -> Opt.Mod Opt.FlagFields Bool
  -> Opt.Parser Bool
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
        []  -> impureThrow $ stringException "enableDisableFlagsNoDefault.last"
        x:_ -> x


buildSwitchOptParser ::
     MonadReader FieldEnv m => CliSwitchSpec -> m (Opt.Parser (Maybe JSON.Value))
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

cliSpecToJsonOptParser ::
     MonadReader FieldEnv m => m (Opt.Parser (Maybe JSON.Value))
cliSpecToJsonOptParser = do
  customTypes <- (envCustomTypes . fieldBuildEnv) <$> ask
  cvType <- (Spec.configValueType . fieldConfigValueSpec) <$> ask
  specSettings <- fieldCliEntrySpec <$> ask
  case specSettings of
    Opt optSpec ->
      return $
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

cliSpecToConfigValueOptParser ::
     MonadReader FieldEnv m => m (Opt.Parser (Maybe Config.ConfigValue))
cliSpecToConfigValueOptParser = do
  priorityIndex <- (envPriorityIndex . fieldBuildEnv) <$> ask
  configValueData <- fieldConfigValueSpec <$> ask
  cliSpec <- fieldCliEntrySpec <$> ask
  let Spec.ConfigValueData {Spec.configValueSensitive} = configValueData
  jsonOptParser <- cliSpecToJsonOptParser
  return $
    fmap (jsonToSomeConfigValue priorityIndex configValueSensitive cliSpec) <$>
    jsonOptParser
