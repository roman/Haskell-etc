{-# LANGUAGE NoImplicitPrelude #-}
module UB.Config
  ( Spec.ConfigSpec
  , ConfigSource (..)

  , Spec.readConfigSpec
  , Spec.parseConfigSpec
  , File.parseConfig
  , Resolver.resolveEnvVars
  , Resolver.configSpecToOptParser
  , getConfigValue
  , getConfigValueWith
  , getSelectedConfigSource
  , getConfigSources
  , Printer.renderConfig
  , Printer.printPrettyConfig
  , Printer.hPrintPrettyConfig

  , readConfigFromAllSources
  , readConfigFromEnv
  , readConfigFromOptParse
  , File.readConfigFromFiles
  ) where


import Text.PrettyPrint.ANSI.Leijen (putDoc, hPutDoc)
import Control.Monad.Catch (MonadThrow(..))
import Data.Set (Set)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Internal as JSON (iparse, formatError, IResult(..))
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Options.Applicative as Opt

import UB.Prelude
import UB.Config.Internal.Types
import qualified UB.Config.Internal.Spec as Spec
import qualified UB.Config.Internal.Resolver.EnvVar as Resolver
import qualified UB.Config.Internal.Resolver.OptParse as Resolver
import qualified UB.Config.Internal.File as File
import qualified UB.Config.Internal.Printer as Printer

--------------------------------------------------------------------------------

configValueToJsonObject :: ConfigValue -> JSON.Value
configValueToJsonObject configValue =
  case configValue of
    ConfigValue sources ->
      case Set.maxView sources of
        Nothing ->
          undefined

        Just (source, _) ->
          value source

    SubConfig configm ->
      configm
      |> HashMap.foldrWithKey
          (\key innerConfigValue acc ->
              HashMap.insert key (configValueToJsonObject innerConfigValue) acc)
          HashMap.empty
      |> JSON.Object

-- Can't add signature given JSON.Parser is not exposed ¯\_(ツ)_/¯
-- getConfigValueWith
--   :: MonadThrow m
--   => (JSON.Value -> JSON.Parser value)
--   -> [Text]
--   -> Config
--   -> m value
getConfigValueWith parser keys0 (Config configValue0) =
  let
    loop keys configValue =
      case (keys, configValue) of
        ([], ConfigValue sources) ->
          case Set.maxView sources of
            Nothing ->
              throwM <| InvalidConfigKeyPath keys0

            Just (source, _) ->
              case JSON.iparse parser (value source) of
                JSON.IError path err ->
                  JSON.formatError path err
                  |> Text.pack
                  |> InvalidConfiguration
                  |> throwM

                JSON.ISuccess result ->
                  return result

        ([], innerConfigValue) ->
          case JSON.iparse parser (configValueToJsonObject innerConfigValue) of
            JSON.IError path err ->
              JSON.formatError path err
              |> Text.pack
              |> InvalidConfiguration
              |> throwM

            JSON.ISuccess result ->
              return result

        (k:keys1, SubConfig configm) ->
          case HashMap.lookup k configm of
            Nothing ->
              throwM <| InvalidConfigKeyPath keys0
            Just configValue1 ->
              loop keys1 configValue1

        _ ->
          throwM <| InvalidConfigKeyPath keys0
  in
    loop keys0 configValue0

getSelectedConfigSource
  :: (MonadThrow m)
  => [Text]
  -> Config
  -> m ConfigSource
getSelectedConfigSource keys0 (Config configValue0) =
  let
    loop keys configValue =
      case (keys, configValue) of
        ([], ConfigValue sources) ->
          case Set.maxView sources of
            Nothing ->
              throwM <| InvalidConfigKeyPath keys0

            Just (source, _) ->
              return source

        (k:keys1, SubConfig configm) ->
          case HashMap.lookup k configm of
            Nothing ->
              throwM <| InvalidConfigKeyPath keys0
            Just configValue1 ->
              loop keys1 configValue1

        _ ->
          throwM <| InvalidConfigKeyPath keys0
  in
    loop keys0 configValue0


getConfigSources
  :: (MonadThrow m)
  => [Text]
  -> Config
  -> m (Set ConfigSource)
getConfigSources keys0 (Config configValue0) =
  let
    loop keys configValue =
      case (keys, configValue) of
        ([], ConfigValue sources) ->
          return sources

        (k:keys1, SubConfig configm) ->
          case HashMap.lookup k configm of
            Nothing ->
              throwM <| InvalidConfigKeyPath keys0
            Just configValue1 ->
              loop keys1 configValue1

        _ ->
          throwM <| InvalidConfigKeyPath keys0
  in
    loop keys0 configValue0

getConfigValue
  :: (MonadThrow m, JSON.FromJSON result)
  => [Text]
  -> Config
  -> m result
getConfigValue =
  getConfigValueWith JSON.parseJSON


readConfigFromAllSources
  :: Text
  -> [Text]
  -> Opt.InfoMod Config
  -> IO Config
readConfigFromAllSources specPath filepaths programFlags = do
  spec <- Spec.readConfigSpec specPath

  let
    optConfigParser =
      Resolver.configSpecToOptParser spec

    programParser =
      Opt.info
        (Opt.helper <*> optConfigParser)
        programFlags

  optConfig  <- Opt.execParser programParser
  envConfig  <- Resolver.resolveEnvVars spec
  fileConfig <- File.readConfigFromFiles filepaths

  return (optConfig <> envConfig <> fileConfig)

readConfigFromEnv :: Text -> IO Config
readConfigFromEnv specPath = do
  spec <- Spec.readConfigSpec specPath
  Resolver.resolveEnvVars spec

readConfigFromOptParse
  :: Text
  -> Opt.InfoMod Config
  -> IO Config
readConfigFromOptParse specPath programFlags = do
  spec <- Spec.readConfigSpec specPath

  let
    optConfigParser =
      Resolver.configSpecToOptParser spec

    programParser =
      Opt.info
        (Opt.helper <*> optConfigParser)
        programFlags

  Opt.execParser programParser
