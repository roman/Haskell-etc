{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Etc.Internal.Config where

import Protolude

import Control.Monad.Catch (MonadThrow (..))

import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Internal as JSON (IResult (..), formatError, iparse)
import qualified Data.Aeson.Types    as JSON (Parser)
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

_getConfigValueWith
  :: MonadThrow m
  => (JSON.Value -> JSON.Parser result)
  -> [Text]
  -> Config
  -> m result
_getConfigValueWith parser keys0 (Config configValue0) =
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

_getSelectedConfigSource
  :: (MonadThrow m)
  => [Text]
  -> Config
  -> m ConfigSource
_getSelectedConfigSource keys0 (Config configValue0) =
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


_getAllConfigSources
  :: (MonadThrow m)
  => [Text]
  -> Config
  -> m (Set ConfigSource)
_getAllConfigSources keys0 (Config configValue0) =
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

_getConfigValue
  :: (MonadThrow m, JSON.FromJSON result)
  => [Text]
  -> Config
  -> m result
_getConfigValue =
  _getConfigValueWith JSON.parseJSON


instance IConfig Config where
  getConfigValue =
    _getConfigValue

  getConfigValueWith =
    _getConfigValueWith

  getAllConfigSources =
    _getAllConfigSources

  getSelectedConfigSource =
    _getSelectedConfigSource
