{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
module UB.Config.Internal.Resolver where

import Control.Lens ((&), (.~), (%~))
import Control.Monad.Catch (MonadThrow(..))
import Data.Maybe (fromMaybe)
import Data.HashMap.Strict (HashMap)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Vector (Vector)
import System.Environment (getEnv, lookupEnv)
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.Aeson as JSON
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap

import UB.Prelude hiding ((&))
import UB.Config.Internal.Types
import qualified UB.Config.Internal.Spec as Spec

--------------------------------------------------------------------------------

resolveEnvVarSource
  :: Spec.ConfigSources
  -> IO (Maybe ConfigSource)
resolveEnvVarSource specSources =
  let
    toEnvVarSource varname value =
      value
      |> Text.pack
      |> JSON.String
      |> EnvVar varname

  in
    case Spec.envVar specSources of
      Just (Spec.EnvVar varname) ->
        fmap (fmap <| toEnvVarSource varname)
             (lookupEnv <| Text.unpack varname)

      Nothing ->
        return Nothing

envVarConfigResolver sources configLens config = do
  mEnvVarSource <- resolveEnvVarSource sources

  let
    sources =
      [ mEnvVarSource ]
      |> catMaybes
      |> Set.fromList

  return <| config & configLens %~ (Set.union sources)

buildEnvVarResolver_ configLens unresolvedConfigValue =
  let
    configEntryReducer configKey unresolvedConfigVal acc =
      case unresolvedConfigVal of
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
                envVarConfigResolver sources
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
                                   unresolvedConfigVal
          in
            Vector.concat [ acc, resolver ]
  in
    case unresolvedConfigValue of
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
buildEnvVarResolver (Spec.ConfigSpec configValue) =
  buildEnvVarResolver_ _Config configValue

resolveEnvVars :: Spec.ConfigSpec -> IO Config
resolveEnvVars specConfig =
  specConfig
  |> buildEnvVarResolver
  |> foldM (|>) (Config (SubConfig HashMap.empty))
