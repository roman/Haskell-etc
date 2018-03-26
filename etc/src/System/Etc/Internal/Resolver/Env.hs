{-# LANGUAGE NoImplicitPrelude #-}

module System.Etc.Internal.Resolver.Env (resolveEnv, resolveEnvPure) where

import           RIO
import qualified RIO.HashMap        as HashMap
import qualified RIO.Set            as Set
import qualified RIO.Text           as Text
import           System.Environment (getEnvironment)

import           Control.Arrow ((***))
import qualified Data.Aeson    as JSON

import qualified System.Etc.Internal.Spec.Types as Spec
import           System.Etc.Internal.Types

resolveEnvVarSource
  :: (Text -> Maybe Text) -> Bool -> Spec.ConfigSources cmd -> Maybe ConfigSource
resolveEnvVarSource lookupEnv sensitive specSources =
  let toEnvSource varname envValue =
        envValue & JSON.String & boolToValue sensitive & Env varname
  in  do
        varname <- Spec.envVar specSources
        toEnvSource varname <$> lookupEnv varname

buildEnvVarResolver :: (Text -> Maybe Text) -> Spec.ConfigSpec cmd -> Maybe ConfigValue
buildEnvVarResolver lookupEnv spec =
  let resolverReducer
        :: Text -> Spec.ConfigValue cmd -> Maybe ConfigValue -> Maybe ConfigValue
      resolverReducer specKey specValue mConfig = case specValue of
        Spec.ConfigValue _ sensitive sources ->
          let updateConfig = do
                envSource <- resolveEnvVarSource lookupEnv sensitive sources
                writeInSubConfig specKey (ConfigValue $ Set.singleton envSource)
                  <$> mConfig
          in  updateConfig <|> mConfig

        Spec.SubConfig specConfigMap ->
          let mSubConfig =
                specConfigMap
                  & HashMap.foldrWithKey resolverReducer (Just emptySubConfig)
                  & filterMaybe isEmptySubConfig

              updateConfig = writeInSubConfig specKey <$> mSubConfig <*> mConfig
          in  updateConfig <|> mConfig
  in  Spec.specConfigValues spec
      & HashMap.foldrWithKey resolverReducer (Just emptySubConfig)
      & filterMaybe isEmptySubConfig
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
  let envMap = HashMap.fromList envMap0

      lookupEnv key = HashMap.lookup key envMap
  in  maybe (Config emptySubConfig) Config (buildEnvVarResolver lookupEnv spec)


{-|

Gathers all OS Environment Variable values (@env@ entries) from the @etc/spec@
entries inside a @ConfigSpec@.

-}
resolveEnv
  :: Spec.ConfigSpec cmd -- ^ Config Spec
  -> IO Config           -- ^ returns Configuration Map with Environment Variables values filled in
resolveEnv spec =
  let getEnvironmentTxt = map (Text.pack *** Text.pack) <$> getEnvironment
  in  resolveEnvPure spec <$> getEnvironmentTxt
