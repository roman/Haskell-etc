{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
module System.Etc.Internal.Resolver.EnvVar where

import Control.Lens ((&), (%~))
import Data.Vector (Vector)
import System.Environment (lookupEnv)
import qualified Control.Lens as L
import qualified Data.Aeson as JSON
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap

import UB.Prelude hiding ((&))
import System.Etc.Internal.Types
import qualified System.Etc.Internal.Spec as Spec

--------------------------------------------------------------------------------

resolveEnvVarSource
  :: Spec.ConfigSources
  -> IO (Maybe ConfigSource)
resolveEnvVarSource specSources =
  let
    toEnvVarSource varname envValue =
      envValue
      |> Text.pack
      |> JSON.String
      |> EnvVar varname

  in
    case Spec.envVar specSources of
      Just (Spec.EnvVar varname) ->
        fmap (fmap <| toEnvVarSource varname)
             (lookupEnv <| Text.unpack varname)

      Just _ ->
        return Nothing

      Nothing ->
        return Nothing

envVarConfigResolver
  :: Maybe JSON.Value
  -> Spec.ConfigSources
  -> L.ASetter Config Config (Set ConfigSource) (Set ConfigSource)
  -> Config
  -> IO Config
envVarConfigResolver defaultValue configSpecSources configLens config = do
  mEnvVarSource <- resolveEnvVarSource configSpecSources

  let
    sources =
      [ mEnvVarSource, fmap Default defaultValue ]
      |> catMaybes
      |> Set.fromList

  return <| config & configLens %~ (Set.union sources)

buildEnvVarResolver_
  :: L.ASetter Config Config ConfigValue ConfigValue
  -> Spec.ConfigValue
  -> Vector (Config -> IO Config)
buildEnvVarResolver_ configLens configSpecValue =
  let
    configEntryReducer configKey configSpecVal acc =
      case configSpecVal of
        Spec.ConfigValue mdefaultVal sources ->
          let
            subConfigLens =
              configLens
              << _SubConfig
              << L.at configKey
              << (_JustConfigValue Set.empty)
              << _ConfigValue

            resolver =
              Vector.singleton <|
                envVarConfigResolver mdefaultVal
                                     sources
                                     subConfigLens

          in
            Vector.concat [ acc, resolver ]

        Spec.SubConfig {} ->
          let
            subConfigLens =
              configLens
              << _SubConfig
              << L.at configKey
              << _JustSubConfig

            resolver =
              buildEnvVarResolver_ subConfigLens
                                   configSpecVal
          in
            Vector.concat [ acc, resolver ]
  in
    case configSpecValue of
      Spec.SubConfig configm ->
        if HashMap.null configm then
          Vector.empty
        else
          HashMap.foldrWithKey configEntryReducer Vector.empty configm

      _ ->
        Vector.empty

buildEnvVarResolver
  :: Spec.ConfigSpec
  -> Vector (Config -> IO Config)
buildEnvVarResolver (Spec.ConfigSpec _ _ configValue) =
  buildEnvVarResolver_ _Config configValue

resolveEnvVars :: Spec.ConfigSpec -> IO Config
resolveEnvVars specConfig =
  specConfig
  |> buildEnvVarResolver
  |> foldM (|>) (Config (SubConfig HashMap.empty))
