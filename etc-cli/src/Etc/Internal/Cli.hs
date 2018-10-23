{-# LANGUAGE CPP               #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Etc.Internal.Cli where

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

import Etc.Internal.Cli.Parser (parseCliEntrySpec, parseCliInfoSpec)
import Etc.Internal.Cli.Error ()
import Etc.Internal.Cli.Types
--------------------------------------------------------------------------------

#if MIN_VERSION_optparse_applicative(0,14,0)
cliOptSpecToFieldMod :: (Opt.HasMetavar f, Opt.HasName f) => CliOptSpec -> Opt.Mod f a
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

#if MIN_VERSION_optparse_applicative(0,14,0)
cliSwitchSpecToFieldMod :: Opt.HasName f => CliSwitchSpec -> Opt.Mod f a
#endif
cliSwitchSpecToFieldMod switchSpec =
  Opt.long (Text.unpack $ switchLong switchSpec)
    `mappend` maybe Opt.idm (Opt.help . Text.unpack) (switchHelp switchSpec)

cliArgSpecToFieldMod :: CliArgSpec -> Opt.Mod f a
cliArgSpecToFieldMod argSpec =
  maybe Opt.idm (Opt.help . Text.unpack) (argHelp argSpec)

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

jsonOptReader :: Map Text Spec.CustomType -> Spec.ConfigValueType -> String -> Either String JSON.Value
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
          "\"" <> input <> "\" doesn't match expected type " <>
          cvTypeName
        Just jsonValue1
          | Spec.matchesConfigValueType customTypes cvType jsonValue1 ->
            Right jsonValue1
          | otherwise ->
            Left $
            "\"" <> input <> "\" doesn't match expected type " <>
            cvTypeName

cliSpecToJsonOptParser
  :: Map Text Spec.CustomType
  -> Spec.ConfigValueType
  -> CliEntrySpec
  -> Opt.Parser (Maybe JSON.Value)
cliSpecToJsonOptParser customTypes cvType specSettings =
  case specSettings of
    Opt optSpec ->
      Opt.optional $
      Opt.option
        (Opt.eitherReader $ jsonOptReader customTypes cvType)
        (cliOptSpecToFieldMod optSpec)
    Arg argSpec ->
      Opt.optional $
      Opt.argument
        (Opt.eitherReader $ jsonOptReader customTypes cvType)
        (cliArgSpecToFieldMod argSpec)
    Switch switchSpec ->
      fmap (Just . JSON.Bool) (Opt.switch (cliSwitchSpecToFieldMod switchSpec)) <|>
      pure Nothing

jsonToSomeConfigValue ::
  Int -> Bool -> CliEntrySpec -> JSON.Value -> Config.ConfigValue
jsonToSomeConfigValue priorityIndex isSensitive cliSpec jsonValue =
  let
    fieldValue =
      Config.SomeConfigSource priorityIndex $ CliSource cliSpec jsonValue
  in
    Config.ConfigValue isSensitive (Set.singleton fieldValue)

configValueSpecToOptParser
  :: (MonadThrow m)
  => Int
  -> Map Text Spec.CustomType
  -> Text
  -> Spec.ConfigValueType
  -> Bool
  -> CliEntrySpec
  -> Parser Config.ConfigValue
  -> m (Parser Config.ConfigValue)
configValueSpecToOptParser priorityIndex customTypes specEntryKey cvType isSensitive cliSpec acc =
  let updateAccConfigOptParser configValueParser accOptParser =
        (\configValue accSubConfig ->
           case accSubConfig of
             Config.ConfigValue {} -> accSubConfig
             Config.SubConfig subConfigMap ->
               Config.SubConfig $
               Map.alter (const configValue) specEntryKey subConfigMap) <$>
        configValueParser <*>
        accOptParser
   in do let jsonOptParser = cliSpecToJsonOptParser customTypes cvType cliSpec
         let configValueParser =
               (fmap (jsonToSomeConfigValue priorityIndex isSensitive cliSpec)) <$>
               jsonOptParser
         return $ updateAccConfigOptParser configValueParser acc

subConfigSpecToOptParser
  :: (MonadThrow m)
  => Text
  -> Int
  -> Map Text Spec.CustomType
  -> Seq Text
  -> Text
  -> Map Text (Spec.ConfigValue)
  -> Opt.Parser Config.ConfigValue
  -> m (Opt.Parser Config.ConfigValue)
subConfigSpecToOptParser specFilePath priorityIndex customTypes keyPath specEntryKey subConfigSpec acc =
  let updateAccConfigOptParser subConfigParser accOptParser =
        (\subConfig accSubConfig ->
           case accSubConfig of
             Config.ConfigValue {} -> accSubConfig
             Config.SubConfig subConfigMap ->
               Config.SubConfig
                 (Map.alter (const $ Just subConfig) specEntryKey subConfigMap)) <$>
        subConfigParser <*>
        accOptParser
   in do configOptParser <-
           foldM
             (specToConfigValueOptParser specFilePath priorityIndex customTypes (keyPath |> specEntryKey))
             (pure $ Config.SubConfig Map.empty)
             (Map.toList subConfigSpec)
         return $ updateAccConfigOptParser configOptParser acc

fromCliParseError ::
     Text
  -> Seq Text
  -> JSON.ParseError CliEntryParseError
  -> Resolver.ResolverError (JSON.ParseError CliResolverError)
fromCliParseError specFilePath keyPath =
  Resolver.ResolverError . fmap fromParseError_
  where
    fromParseError_ mkErr = mkErr specFilePath (toList keyPath)

specToConfigValueOptParser
  :: (MonadThrow m)
  => Text
  -> Int
  -> Map Text Spec.CustomType
  -> Seq Text
  -> Opt.Parser Config.ConfigValue
  -> (Text, Spec.ConfigValue)
  -> m (Opt.Parser Config.ConfigValue)
specToConfigValueOptParser specFilePath priorityIndex customTypes keyPath acc (specEntryKey, specConfigValue) =
  case specConfigValue of
    Spec.ConfigValue Spec.ConfigValueData { Spec.configValueType
                                          , Spec.configValueSensitive
                                          , Spec.configValueJSON
                                          } ->
      case configValueJSON of
        JSON.Object _ ->
          case JSON.parseValue
                 (JSON.keyMay "cli" (parseCliEntrySpec configValueType))
                 configValueJSON of
            Left err ->
              throwM
                (fromCliParseError specFilePath (keyPath |> specEntryKey) err)
            Right Nothing -> return acc
            Right (Just cliSpec) ->
              configValueSpecToOptParser
                priorityIndex
                customTypes
                specEntryKey
                configValueType
                configValueSensitive
                cliSpec
                acc
        _ -> return acc
    Spec.SubConfig subConfigSpec ->
      subConfigSpecToOptParser
        specFilePath
        priorityIndex
        customTypes
        keyPath
        specEntryKey
        subConfigSpec
        acc

toOptInfoMod :: MonadThrow m => Spec.ConfigSpec -> m (Opt.InfoMod a)
toOptInfoMod Spec.ConfigSpec {Spec.configSpecFilePath, Spec.configSpecJSON} =
  case JSON.parseValue
         (JSON.keyMay "etc/cli" parseCliInfoSpec)
         (JSON.Object configSpecJSON) of
    Left err -> throwM (Resolver.ResolverError (err :: JSON.ParseError CliResolverError))
    Right Nothing -> throwM (Resolver.ResolverError (InfoModMissing configSpecFilePath))
    Right (Just cliInfoSpec) ->
      return $
      Opt.fullDesc <> Opt.progDesc (Text.unpack $ cisProgDesc cliInfoSpec) <>
      maybe Opt.idm (Opt.header . Text.unpack) (cisHeader cliInfoSpec) <>
      maybe Opt.idm (Opt.footer . Text.unpack) (cisFooter cliInfoSpec)

toOptParser ::
     MonadThrow m
  => Int
  -> Map Text Spec.CustomType
  -> Spec.ConfigSpec
  -> m (Opt.Parser Config)
toOptParser priorityIndex customTypes spec = do
  let acc = pure $ Config.SubConfig Map.empty
  parser <-
    foldM
      (specToConfigValueOptParser (Spec.configSpecFilePath spec) priorityIndex customTypes Seq.empty)
      acc
      (Map.toList $ Spec.configSpecEntries spec)
  return (Config <$> parser)

resolveCli ::
  (MonadIO m, MonadThrow m) =>
  Int -> Map Text Spec.CustomType -> Spec.ConfigSpec -> m Config
resolveCli priorityIndex customTypes spec = do
  configParser <- toOptParser priorityIndex customTypes spec
  infoMod <- toOptInfoMod spec
  liftIO $
    Opt.execParser
      (Opt.info (configParser Opt.<**> Opt.helper)
                infoMod)

resolveCliPure ::
  (MonadIO m, MonadThrow m) =>
  [String] -> Int -> Map Text Spec.CustomType -> Spec.ConfigSpec -> m Config
resolveCliPure inputArgs priorityIndex customTypes spec = do
  configParser <- toOptParser priorityIndex customTypes spec
  infoMod <- toOptInfoMod spec
  liftIO $
    Opt.handleParseResult $
    Opt.execParserPure
      Opt.defaultPrefs
      (Opt.info (configParser Opt.<**> Opt.helper)
                infoMod)
      inputArgs

cliResolver :: (MonadIO m, MonadThrow m) => Resolver.Resolver m
cliResolver = Resolver.Resolver resolveCli

pureCliResolver :: (MonadIO m, MonadThrow m) => [String] -> Resolver.Resolver m
pureCliResolver args = Resolver.Resolver $ resolveCliPure args
