{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module System.Etc.Internal.Resolver.OptParse where

import Control.Lens hiding ((<|), (|>))
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Control.Monad.Catch (MonadThrow, throwM)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Internal as JSON (iparse, IResult(..))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy.Char8 as BL (unpack)
import qualified Data.Text as Text
import qualified Options.Applicative as Opt

import UB.Prelude hiding ((&))
import System.Etc.Internal.Types
import qualified System.Etc.Internal.Spec as Spec

--------------------------------------------------------------------------------

data OptParseConfigError
  = InvalidOptParseCommandKey Text
  -- ^ The type of the Command Key is invalid
  | CommandsKeyNotDefined
  -- ^ Trying to use command for an entry without setting commands section
  | UnknownCommandKey Text
  -- ^ Trying to use a command that is not defined in commands section
  deriving (Show)

instance Exception OptParseConfigError

data ConfigValueOptParser cmd
  = CommandParser cmd (Opt.Parser (Maybe JSON.Value))
  | OptionParser  (Opt.Parser (Maybe JSON.Value))

data ConfigValueOptParserAcc cmd
  = CommandOptParsers (HashMap cmd (Opt.Parser ConfigValue))
  | SingleOptParser   (Opt.Parser ConfigValue)

--------------------------------------------------------------------------------

specToOptParserSwitchFieldMod specSettings =
  maybe Opt.idm
        (Text.unpack >> Opt.long)
        (Spec.optParseLong specSettings)
  `mappend` maybe Opt.idm
                  (Text.head >> Opt.short)
                  (Spec.optParseShort specSettings)
  `mappend` maybe Opt.idm
                  (Text.unpack >> Opt.help)
                  (Spec.optParseHelp specSettings)

specToOptParserVarFieldMod specSettings =
  specToOptParserSwitchFieldMod specSettings
  `mappend` maybe Opt.idm
                  (Text.unpack >> Opt.metavar)
                  (Spec.optParseMetavar specSettings)

--------------------------------------------------------------------------------

commandToKey :: JSON.ToJSON cmd => cmd -> Text
commandToKey =
  JSON.encode
  >> BL.unpack
  >> Text.pack

settingsToJsonOptParser
  :: Spec.OptParseEntrySpecSettings
    -> Opt.Parser (Maybe JSON.Value)
settingsToJsonOptParser specSettings =
  let
    requiredCombinator =
        if Spec.optParseRequired specSettings then
          (Just <$>)
        else
          Opt.optional
  in
    requiredCombinator <|
    case specSettings of
      Spec.Option {} ->
        case Spec.optParseOptionValueType specSettings of
          Spec.OptParseOptionString ->
            (Text.pack >> JSON.String)
            <$> Opt.strOption (specToOptParserVarFieldMod specSettings)

          Spec.OptParseOptionNumber ->
            (fromInteger >> JSON.Number)
            <$> Opt.option Opt.auto (specToOptParserVarFieldMod specSettings)

          Spec.OptParseOptionSwitch ->
            JSON.Bool
            <$> Opt.switch (specToOptParserSwitchFieldMod specSettings)

      Spec.Argument {} ->
        case Spec.optParseArgValueType specSettings of
          Spec.OptParseArgString ->
            (Text.pack >> JSON.String)
            <$> Opt.strArgument ( specSettings
                                  |> Spec.optParseMetavar
                                  |> maybe Opt.idm (Text.unpack >> Opt.metavar))
          Spec.OptParseArgNumber ->
            (fromInteger >> JSON.Number)
            <$> Opt.argument Opt.auto
                             ( specSettings
                                  |> Spec.optParseMetavar
                                  |> maybe Opt.idm (Text.unpack >> Opt.metavar))

parseCommandJsonValue
  :: (MonadThrow m, JSON.FromJSON a)
    => JSON.Value
    -> m a
parseCommandJsonValue commandValue =
  case JSON.iparse JSON.parseJSON commandValue of
    JSON.IError _path err ->
      throwM (InvalidOptParseCommandKey <| Text.pack err)

    JSON.ISuccess result ->
      return result

entrySpecToConfigValueOptParser
  :: (MonadThrow m, JSON.FromJSON cmd)
    => Spec.OptParseEntrySpec cmd
    -> m (ConfigValueOptParser cmd)
entrySpecToConfigValueOptParser entrySpec =
  case entrySpec of
    Spec.CmdEntry commandJsonValue specSettings ->
      return
        <| CommandParser commandJsonValue
                         (settingsToJsonOptParser specSettings)

    Spec.PlainEntry specSettings ->
      return
        <| OptionParser (settingsToJsonOptParser specSettings)

jsonToConfigValue
  :: Maybe JSON.Value
    -> Maybe JSON.Value
    -> ConfigValue
jsonToConfigValue specEntryDefVal mJsonValue =
  ConfigValue
    <| Set.fromList
    <| case (mJsonValue, specEntryDefVal) of
      (Just val, Just defValue) ->
        [OptParse val, Default defValue]

      (Just val, _) ->
        [OptParse val]

      (_, Just defValue) ->
        [Default defValue]

      _ ->
        crash "invalid spec creation"

configValueSpecToOptParser
  :: (MonadThrow m, JSON.FromJSON cmd, Eq cmd, Hashable cmd)
    => Text
    -> Maybe JSON.Value
    -> Spec.ConfigSources cmd
    -> ConfigValueOptParserAcc cmd
    -> m (ConfigValueOptParserAcc cmd)
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
        optParseResult <- entrySpecToConfigValueOptParser entrySpec
        case optParseResult of
          CommandParser command jsonOptParser -> do
            let
              configValueParser =
                jsonToConfigValue specEntryDefVal <$> jsonOptParser

            accCmdParsers <-
                case acc of
                  SingleOptParser {} ->
                    throwM CommandsKeyNotDefined

                  CommandOptParsers cmdParsers ->
                    return cmdParsers

            return
              <| CommandOptParsers
              <| HashMap.alter
                  (\mAccParser ->
                    mAccParser
                      |> fromMaybe (pure <| SubConfig HashMap.empty)
                      |> updateAccConfigOptParser configValueParser
                      |> Just)
                  command
                  accCmdParsers

          OptionParser jsonOptParser ->
            let
              configValueParser =
                jsonToConfigValue specEntryDefVal <$> jsonOptParser
            in
              case acc of
                CommandOptParsers {} ->
                  throwM CommandsKeyNotDefined

                SingleOptParser accOptParser ->
                  return
                    <| SingleOptParser
                    <| updateAccConfigOptParser configValueParser accOptParser


subConfigSpecToOptParser
  :: (MonadThrow m, JSON.FromJSON cmd, JSON.ToJSON cmd, Eq cmd, Hashable cmd)
    => Text
    -> HashMap.HashMap Text (Spec.ConfigValue cmd)
    -> ConfigValueOptParserAcc cmd
    -> m (ConfigValueOptParserAcc cmd)
subConfigSpecToOptParser specEntryKey subConfigSpec acc =
  let
    updateAccConfigOptParser subConfigParser accOptParser =
      (\subConfig accSubConfig ->
          accSubConfig
            & (_SubConfig << at specEntryKey << _JustSubConfig)
            .~ subConfig)
        <$> subConfigParser
        <*> accOptParser

    addSubParserCommand command subConfigParser accOptParsers =
      HashMap.alter
        (\mAccOptParser ->
           case mAccOptParser of
             Nothing ->
               throwM <| UnknownCommandKey (commandToKey command)

             Just accOptParser ->
               Just
               <| updateAccConfigOptParser subConfigParser accOptParser)
        command
        accOptParsers

  in
    case acc of
      SingleOptParser accOptParser -> do
        SingleOptParser configOptParser <-
            specToConfigValueOptParser
              specEntryKey
              (Spec.SubConfig subConfigSpec)
              acc
        return
          <| SingleOptParser
          <| updateAccConfigOptParser
               configOptParser
               accOptParser

      CommandOptParsers cmdParsers -> do
        CommandOptParsers parserPerCommand <-
            ifoldrMOf itraversed
                      specToConfigValueOptParser
                      (CommandOptParsers HashMap.empty)
                      subConfigSpec
        return
          <| CommandOptParsers
          <| HashMap.foldrWithKey
                addSubParserCommand
                cmdParsers
                parserPerCommand

specToConfigValueOptParser
  :: (MonadThrow m, JSON.FromJSON cmd, JSON.ToJSON cmd, Eq cmd, Hashable cmd)
    => Text
    -> Spec.ConfigValue cmd
    -> ConfigValueOptParserAcc cmd
    -> m (ConfigValueOptParserAcc cmd)
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
  :: (MonadThrow m, JSON.FromJSON cmd, Eq cmd, Hashable cmd)
    => Spec.ConfigSpec' cmd
    -> m (ConfigValueOptParserAcc cmd)
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
        return
          <| SingleOptParser zeroParser

      Just commands ->
       commands
       |> ifoldrMOf itraversed
                    (\commandVal _ acc -> do
                       command <- parseCommandJsonValue (JSON.String commandVal)
                       return <| HashMap.insert command zeroParser acc)
                    HashMap.empty
       >>= (CommandOptParsers >> return)

joinCommandParsers
  :: (JSON.FromJSON cmd, JSON.ToJSON cmd, Eq cmd, Hashable cmd)
    => HashMap cmd (Opt.Parser ConfigValue)
    -> Opt.Parser (Maybe cmd, Config)
joinCommandParsers parserPerCommand =
  let
    joinParser command subConfigParser acc =
      let
        parser =
          fmap (\subConfig -> (Just command, Config subConfig))
               subConfigParser
      in
        acc
        `mappend` Opt.command (command
                               |> commandToKey
                               |> Text.unpack)
                              (Opt.info (Opt.helper <*> parser) Opt.idm)
  in
    HashMap.foldrWithKey joinParser Opt.idm parserPerCommand
    |> Opt.subparser


specToConfigOptParser
  :: (MonadThrow m, JSON.FromJSON cmd, JSON.ToJSON cmd, Eq cmd, Hashable cmd)
    => Spec.ConfigSpec' cmd
    -> m (Opt.Parser (Maybe cmd, Config))
specToConfigOptParser spec = do
  acc <- configValueOptParserAccInit spec
  parseResult <-
    ifoldrMOf itraversed
              specToConfigValueOptParser
              acc
              (Spec.specConfigValues spec)

  case parseResult of
    CommandOptParsers parsers ->
      parsers
      |> joinCommandParsers
      |> return

    SingleOptParser parser ->
      parser
      |> ((\subConfig -> (Nothing, Config subConfig)) <$>)
      |> return

resolveOptParser
  :: (JSON.FromJSON cmd, JSON.ToJSON cmd, Eq cmd, Hashable cmd)
    => Spec.ConfigSpec' cmd
    -> IO (Maybe cmd, Config)
resolveOptParser configSpec = do
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

  Opt.execParser programParser
