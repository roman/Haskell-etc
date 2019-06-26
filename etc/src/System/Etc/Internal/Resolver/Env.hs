{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
module System.Etc.Internal.Resolver.Env (resolveEnv, resolveEnvPure) where

import           RIO
import qualified RIO.HashMap        as HashMap
import qualified RIO.Set            as Set
import qualified RIO.Text           as Text
import           System.Environment (getEnvironment)

import Control.Arrow ((***))

import qualified System.Etc.Internal.Spec.Parser as Spec
import qualified System.Etc.Internal.Spec.Types  as Spec
import           System.Etc.Internal.Types

resolveEnvVarSource
  :: (Text -> Maybe Text)
  -> Spec.ConfigValueType
  -> Bool
  -> Spec.ConfigSources cmd
  -> Maybe SomeConfigSource
resolveEnvVarSource lookupEnv configValueType isSensitive specSources =
  let envTextToJSON = Spec.parseBytesToConfigValueJSON configValueType

      toEnvSource varname envValue =
        envSource 2 varname . markAsSensitive isSensitive <$> envTextToJSON envValue
  in  do
        varname <- Spec.envVar specSources
        envText <- lookupEnv varname
        toEnvSource varname envText

buildEnvVarResolver :: (Text -> Maybe Text) -> Spec.ConfigSpec cmd -> Maybe ConfigValue
buildEnvVarResolver lookupEnv spec =
  let
    resolverReducer
      :: Text -> Spec.ConfigValue cmd -> Maybe ConfigValue -> Maybe ConfigValue
    resolverReducer specKey specValue mConfig = case specValue of
      Spec.ConfigValue Spec.ConfigValueData { Spec.isSensitive, Spec.configValueType, Spec.configSources }
        -> let updateConfig = do
                 envSource' <- resolveEnvVarSource lookupEnv
                                                   configValueType
                                                   isSensitive
                                                   configSources
                 writeInSubConfig specKey (ConfigValue $ Set.singleton envSource')
                   <$> mConfig
           in  updateConfig <|> mConfig

      Spec.SubConfig specConfigMap ->
        let mSubConfig =
              specConfigMap
                & HashMap.foldrWithKey resolverReducer (Just emptySubConfig)
                & filterMaybe isEmptySubConfig

            updateConfig = writeInSubConfig specKey <$> mSubConfig <*> mConfig
        in  updateConfig <|> mConfig
  in
    Spec.specConfigValues spec
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
entries inside a @ConfigSpec@

-}
resolveEnv
  :: Spec.ConfigSpec cmd -- ^ Config Spec
  -> IO Config           -- ^ returns Configuration Map with Environment Variables values filled in
resolveEnv spec =
  let getEnvironmentTxt = map (Text.pack *** Text.pack) <$> getEnvironment
  in  resolveEnvPure spec <$> getEnvironmentTxt
