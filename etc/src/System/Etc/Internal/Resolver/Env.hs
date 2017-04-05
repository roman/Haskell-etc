{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Etc.Internal.Resolver.Env (resolveEnv, resolveEnvPure) where

import Protolude
import System.Environment (getEnvironment)

import           Control.Arrow       ((***))
import qualified Data.Aeson          as JSON
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set            as Set
import qualified Data.Text           as Text

import qualified System.Etc.Internal.Spec.Types as Spec
import           System.Etc.Internal.Types

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

{-|

Gathers all OS Environment Variable values (@env@ entries) from the @etc/spec@
entries inside a @ConfigSpec@. This version of the function gathers the input
from a list of tuples rather than the OS.

-}
resolveEnvPure
  :: Spec.ConfigSpec cmd -- ^ ConfigSpec
  -> [(Text, Text)]      -- ^ Environment Variable tuples
  -> Config              -- ^ returns Configuration Map with Environment Variables values filled in
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


{-|

Gathers all OS Environment Variable values (@env@ entries) from the @etc/spec@
entries inside a @ConfigSpec@.

-}
resolveEnv
  :: Spec.ConfigSpec cmd -- ^ Config Spec
  -> IO Config           -- ^ returns Configuration Map with Environment Variables values filled in
resolveEnv spec =
  let
    getEnvironmentTxt =
      map (Text.pack *** Text.pack)
        <$> getEnvironment
  in
    resolveEnvPure spec
      <$> getEnvironmentTxt
