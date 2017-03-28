{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Etc.Internal.Resolver.Env (resolveEnv, resolveEnvPure) where

import Protolude
import System.Environment (getEnvironment)

import Control.Arrow ((***))
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Aeson as JSON

import Etc.Internal.Types
import qualified Etc.Internal.Spec.Types as Spec

resolveEnvVarSource
  :: (Text -> Maybe Text)
  -> Spec.ConfigSources cmd
  -> Maybe ConfigSource
resolveEnvVarSource lookupEnv specSources =
  let
    toEnvSource varname envValue =
      envValue
      & JSON.String
      & Env varname
  in do
    varname <- Spec.envVar specSources
    toEnvSource varname <$> lookupEnv varname

emptySubConfig :: ConfigValue
emptySubConfig =
  SubConfig HashMap.empty

writeConfig :: Text -> ConfigValue -> ConfigValue -> ConfigValue
writeConfig key val subConfig =
  case subConfig of
    SubConfig hsh ->
      SubConfig
        $ HashMap.insert key val hsh
    _ ->
      subConfig

buildEnvVarResolver :: (Text -> Maybe Text) -> Spec.ConfigSpec cmd -> Maybe ConfigValue
buildEnvVarResolver lookupEnv spec =
  let
    resolverReducer :: Text -> Spec.ConfigValue cmd -> Maybe ConfigValue -> Maybe ConfigValue
    resolverReducer specKey specValue mConfig =
      case specValue of
        Spec.ConfigValue _ sources -> do
          envSource <- resolveEnvVarSource lookupEnv sources
          writeConfig specKey (ConfigValue $ Set.singleton envSource) <$> mConfig

        Spec.SubConfig specConfigMap ->
          let
            mSubConfig =
              HashMap.foldrWithKey
                   resolverReducer
                   (Just emptySubConfig)
                   specConfigMap
          in
            writeConfig specKey <$> mSubConfig <*> mConfig
  in
    HashMap.foldrWithKey
      resolverReducer
      (Just emptySubConfig)
      (Spec.specConfigValues spec)

resolveEnvPure ::  Spec.ConfigSpec cmd -> [(Text, Text)] -> Config
resolveEnvPure spec envMap0 =
  let
    envMap =
      HashMap.fromList envMap0

    lookupEnv key =
      HashMap.lookup key envMap
  in
    maybe (Config emptySubConfig)
          Config
          (buildEnvVarResolver lookupEnv spec)


resolveEnv :: Spec.ConfigSpec cmd -> IO Config
resolveEnv spec =
  let
    getEnvironmentTxt =
      map (Text.pack *** Text.pack)
        <$> getEnvironment
  in
    resolveEnvPure spec
      <$> getEnvironmentTxt
