{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module System.Etc.Internal.Resolver.OptParse.Plain where

import Control.Lens hiding ((<|), (|>))
import Control.Monad.Catch (MonadThrow, throwM)
import System.Environment (getArgs)
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Options.Applicative as Opt

import System.Etc.Internal.Prelude hiding ((&))
import System.Etc.Internal.Types
import System.Etc.Internal.Resolver.OptParse.Common
import qualified System.Etc.Internal.Spec as Spec

--------------------------------------------------------------------------------

type PlainConfigSpec =
  Spec.ConfigSpec ()

--------------------------------------------------------------------------------

entrySpecToConfigValueOptParser
  :: (MonadThrow m)
    => Spec.OptParseEntrySpec ()
    -> m (Opt.Parser (Maybe JSON.Value))
entrySpecToConfigValueOptParser entrySpec =
  case entrySpec of
    Spec.CmdEntry {} ->
      throwM CommandKeyOnPlainParser

    Spec.PlainEntry specSettings ->
      return (settingsToJsonOptParser specSettings)


configValueSpecToOptParser
  :: (MonadThrow m)
    => Text
    -> Maybe JSON.Value
    -> Spec.ConfigSources ()
    -> Opt.Parser ConfigValue
    -> m (Opt.Parser ConfigValue)
configValueSpecToOptParser specEntryKey specEntryDefVal sources acc =
  let
    updateAccConfigOptParser configValueParser accOptParser =
      (\configValue accSubConfig ->
        accSubConfig
          &  (_SubConfig << at specEntryKey << _JustConfigValue Set.empty)
          .~ configValue)
        <$> configValueParser
        <*> accOptParser
  in
    case Spec.optParse sources of
      Nothing ->
        return acc

      Just entrySpec -> do
        jsonOptParser <- entrySpecToConfigValueOptParser entrySpec

        let
          configValueParser =
            jsonToConfigValue specEntryDefVal <$> jsonOptParser

        return <| updateAccConfigOptParser configValueParser acc


subConfigSpecToOptParser
  :: (MonadThrow m)
    => Text
    -> HashMap.HashMap Text (Spec.ConfigValue ())
    -> Opt.Parser ConfigValue
    -> m (Opt.Parser ConfigValue)
subConfigSpecToOptParser specEntryKey subConfigSpec acc =
  let
    updateAccConfigOptParser subConfigParser accOptParser =
      (\subConfig accSubConfig ->
          accSubConfig
            & (_SubConfig << at specEntryKey << _JustSubConfig)
            .~ subConfig)
        <$> subConfigParser
        <*> accOptParser
  in do
    configOptParser <-
        specToConfigValueOptParser
          specEntryKey
          (Spec.SubConfig subConfigSpec)
          acc

    return
      <| updateAccConfigOptParser configOptParser acc

specToConfigValueOptParser
  :: (MonadThrow m)
    => Text
    -> Spec.ConfigValue ()
    -> Opt.Parser ConfigValue
    -> m (Opt.Parser ConfigValue)
specToConfigValueOptParser specEntryKey specConfigValue acc =
  case specConfigValue of
    Spec.ConfigValue mDefaultValue sources ->
      configValueSpecToOptParser
        specEntryKey
        mDefaultValue
        sources
        acc

    Spec.SubConfig subConfigSpec ->
      subConfigSpecToOptParser
        specEntryKey
        subConfigSpec
        acc

configValueOptParserAccInit
  :: (MonadThrow m)
    => Spec.ConfigSpec ()
    -> m (Opt.Parser ConfigValue)
configValueOptParserAccInit spec =
  let
    zeroParser =
      pure <| SubConfig HashMap.empty

    commandsSpec = do
      programSpec <- Spec.specOptParseProgramSpec spec
      Spec.optParseCommands programSpec
  in
    case commandsSpec of
      Nothing ->
        return zeroParser

      Just _ ->
        throwM CommandKeyOnPlainParser

specToConfigOptParser
  :: (MonadThrow m)
    => Spec.ConfigSpec ()
    -> m (Opt.Parser Config)
specToConfigOptParser spec = do
  acc <- configValueOptParserAccInit spec
  parser <-
    ifoldrMOf itraversed
              specToConfigValueOptParser
              acc
              (Spec.specConfigValues spec)

  parser
    |> (Config <$>)
    |> return

resolvePlainOptParserPure
  :: PlainConfigSpec -> [Text] -> IO Config
resolvePlainOptParserPure configSpec args = do
  configParser <- specToConfigOptParser configSpec

  let
    programModFlags =
      case Spec.specOptParseProgramSpec configSpec of
           Just programSpec ->
             Opt.fullDesc
              `mappend` (programSpec
                        |> Spec.optParseProgramDesc
                        |> Text.unpack
                        |> Opt.progDesc)
              `mappend` (programSpec
                        |> Spec.optParseProgramHeader
                        |> Text.unpack
                        |> Opt.header)
           Nothing ->
             mempty

    programParser =
      Opt.info (Opt.helper <*> configParser)
               programModFlags

  args
    |> map Text.unpack
    |> Opt.execParserPure Opt.defaultPrefs programParser
    |> Opt.handleParseResult

resolvePlainOptParser
  :: PlainConfigSpec -> IO Config
resolvePlainOptParser configSpec =
  getArgs
  >>= map Text.pack
  >>  resolvePlainOptParserPure configSpec
