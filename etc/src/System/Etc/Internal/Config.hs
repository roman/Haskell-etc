{-# LANGUAGE NamedFieldPuns    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
module System.Etc.Internal.Config where

import           RIO
import qualified RIO.HashMap as HashMap
import qualified RIO.Set     as Set
import qualified RIO.Text    as Text

import Data.Typeable (cast)

import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Internal as JSON (IResult (..), formatError, iparse)
import qualified Data.Aeson.Types    as JSON (Parser)

import System.Etc.Internal.Errors
import System.Etc.Internal.Types

--------------------------------------------------------------------------------

configValueToJsonObject :: ConfigValue -> JSON.Value
configValueToJsonObject configValue = case configValue of
  ConfigValue sources -> case Set.maxView sources of
    Nothing          -> JSON.Null

    Just (source, _) -> fromValue $ sourceValue source

  SubConfig configm ->
    configm
      & HashMap.foldrWithKey
          (\key innerConfigValue acc ->
            HashMap.insert key (configValueToJsonObject innerConfigValue) acc
          )
          HashMap.empty
      & JSON.Object

_getConfigValueWith
  :: (MonadThrow m) => (JSON.Value -> JSON.Parser result) -> [Text] -> Config -> m result
_getConfigValueWith parser keys0 (Config configValue0) =
  let
    loop keys configValue = case (keys, configValue) of
      ([], ConfigValue sources) -> case Set.maxView sources of
        Nothing          -> throwM $ InvalidConfigKeyPath keys0

        Just (source, _) -> case JSON.iparse parser (fromValue $ sourceValue source) of

          JSON.IError path err ->
            JSON.formatError path err & Text.pack & ConfigValueParserFailed keys0 & throwM

          JSON.ISuccess result -> return result

      ([], innerConfigValue) ->
        case JSON.iparse parser (configValueToJsonObject innerConfigValue) of
          JSON.IError path err ->
            JSON.formatError path err & Text.pack & ConfigValueParserFailed keys0 & throwM

          JSON.ISuccess result -> return result

      (k : keys1, SubConfig configm) -> case HashMap.lookup k configm of
        Nothing           -> throwM $ InvalidConfigKeyPath keys0
        Just configValue1 -> loop keys1 configValue1

      _ -> throwM $ InvalidConfigKeyPath keys0
  in  loop keys0 configValue0

_getSelectedConfigSource
  :: (MonadThrow m, Typeable result, IConfigSource result) => [Text] -> Config -> m result
_getSelectedConfigSource keys0 (Config configValue0) =
  let loop keys configValue = case (keys, configValue) of
        ([], ConfigValue sources) -> case Set.maxView sources of
          Nothing -> throwM $ InvalidConfigKeyPath keys0

          Just (SomeConfigSource _ source, _) ->
            -- TODO: Change exception from InvalidConfigKeyPath
            maybe (throwM $ InvalidConfigKeyPath keys0) return (cast source)

        (k : keys1, SubConfig configm) -> case HashMap.lookup k configm of
          Nothing           -> throwM $ InvalidConfigKeyPath keys0
          Just configValue1 -> loop keys1 configValue1

        _ -> throwM $ InvalidConfigKeyPath keys0
  in  loop keys0 configValue0


_getAllConfigSources :: (MonadThrow m) => [Text] -> Config -> m (Set SomeConfigSource)
_getAllConfigSources keys0 (Config configValue0) =
  let loop keys configValue = case (keys, configValue) of
        ([]       , ConfigValue sources) -> return sources

        (k : keys1, SubConfig configm  ) -> case HashMap.lookup k configm of
          Nothing           -> throwM $ InvalidConfigKeyPath keys0
          Just configValue1 -> loop keys1 configValue1

        _ -> throwM $ InvalidConfigKeyPath keys0
  in  loop keys0 configValue0

_getConfigValue :: (MonadThrow m, JSON.FromJSON result) => [Text] -> Config -> m result
_getConfigValue = _getConfigValueWith JSON.parseJSON

instance IConfig Config where
  getConfigValue = _getConfigValue
  getConfigValueWith = _getConfigValueWith
  getAllConfigSources = _getAllConfigSources
  getSelectedConfigSource = _getSelectedConfigSource
