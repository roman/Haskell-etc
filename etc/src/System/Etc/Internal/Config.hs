{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Etc.Internal.Config where

import Protolude

import Control.Monad.Catch (MonadThrow (..))

import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Internal as JSON (IResult (..), formatError, iparse)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set            as Set
import qualified Data.Text           as Text

import System.Etc.Internal.Types

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
      & HashMap.foldrWithKey
          (\key innerConfigValue acc ->
              HashMap.insert key (configValueToJsonObject innerConfigValue) acc)
          HashMap.empty
      & JSON.Object

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
              throwM $ InvalidConfigKeyPath keys0

            Just (None, _) ->
              throwM $ InvalidConfigKeyPath keys0

            Just (source, _) ->
              case JSON.iparse parser (value source) of
                JSON.IError path err ->
                  JSON.formatError path err
                  & Text.pack
                  & InvalidConfiguration
                  & throwM

                JSON.ISuccess result ->
                  return result

        ([], innerConfigValue) ->
          case JSON.iparse parser (configValueToJsonObject innerConfigValue) of
            JSON.IError path err ->
              JSON.formatError path err
              & Text.pack
              & InvalidConfiguration
              & throwM

            JSON.ISuccess result ->
              return result

        (k:keys1, SubConfig configm) ->
          case HashMap.lookup k configm of
            Nothing ->
              throwM $ InvalidConfigKeyPath keys0
            Just configValue1 ->
              loop keys1 configValue1

        _ ->
          throwM $ InvalidConfigKeyPath keys0
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
              throwM $ InvalidConfigKeyPath keys0

            Just (source, _) ->
              return source

        (k:keys1, SubConfig configm) ->
          case HashMap.lookup k configm of
            Nothing ->
              throwM $ InvalidConfigKeyPath keys0
            Just configValue1 ->
              loop keys1 configValue1

        _ ->
          throwM $ InvalidConfigKeyPath keys0
  in
    loop keys0 configValue0


getAllConfigSources
  :: (MonadThrow m)
  => [Text]
  -> Config
  -> m (Set ConfigSource)
getAllConfigSources keys0 (Config configValue0) =
  let
    loop keys configValue =
      case (keys, configValue) of
        ([], ConfigValue sources) ->
          return sources

        (k:keys1, SubConfig configm) ->
          case HashMap.lookup k configm of
            Nothing ->
              throwM $ InvalidConfigKeyPath keys0
            Just configValue1 ->
              loop keys1 configValue1

        _ ->
          throwM $ InvalidConfigKeyPath keys0
  in
    loop keys0 configValue0

getConfigValue
  :: (MonadThrow m, JSON.FromJSON result)
  => [Text]
  -> Config
  -> m result
getConfigValue =
  getConfigValueWith JSON.parseJSON
