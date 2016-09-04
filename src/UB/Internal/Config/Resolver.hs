{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
module UB.Internal.Config.Resolver where

import UB.Internal.Types
import UB.Prelude hiding ((&))
import UB.Lens.EDN

import Control.Lens ((&), (.~), (%~))
import Control.Monad.Catch (MonadThrow(..))
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Vector (Vector)
import System.Environment (getEnv, lookupEnv)

import qualified Control.Lens as L
import qualified UB.Internal.Config.Unresolved as Unresolved
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.EDN as EDN
import qualified Data.EDN.Types.Class as EDN (fromEDNv, Result(..))
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Data.Vector as Vector
import qualified Data.Map as Map

type ConfigKey = Text

data ConfigSource
  = File     { filepath :: Text
             , configIndex    :: Int
             , value    :: EDN.Value }
  | EnvVar   { envVar :: Text, value :: EDN.Value }
  | OptParse { value :: EDN.Value, option :: Text }
  deriving (Show, Eq)

instance Ord ConfigSource where
  compare a b =
    case (a, b) of
      (OptParse {}, _) ->
        LT

      (_, OptParse {}) ->
        GT

      (EnvVar {}, _) ->
        LT

      (_, EnvVar {}) ->
        GT

      (File {}, File {}) ->
        comparing configIndex a b

data ConfigValue
  = ConfigValue { configSource :: Set ConfigSource }
  | SubConfig { subConfig :: Map EDN.Value ConfigValue }
  deriving (Show)

newtype Config
  = Config { fromConfig :: ConfigValue }
  deriving (Show)


$(L.makePrisms ''ConfigValue)
$(L.makePrisms ''Config)

-- Works like the _Just prism, but instead of not doing anything on Nothing, it
-- creates a ConfigValue record
_JustConfigValue
  :: Set ConfigSource
    -> L.Prism (Maybe ConfigValue) (Maybe ConfigValue) ConfigValue ConfigValue
_JustConfigValue source =
  L.prism Just <| maybe (Right <| ConfigValue source) Right


_JustSubConfig :: L.Prism (Maybe ConfigValue) (Maybe ConfigValue) ConfigValue ConfigValue
_JustSubConfig =
  L.prism Just <| maybe (Right <| SubConfig Map.empty) Right

--------------------------------------------------------------------------------

resolveEnvVarSource
  :: Unresolved.ConfigSources
  -> IO (Maybe ConfigSource)
resolveEnvVarSource unresolvedSources =
  let
    toEnvVarSource varname value =
      value
      |> Text.pack
      |> EDN.String
      |> EnvVar varname

  in
    case Unresolved.envVar unresolvedSources of
      Unresolved.Pending (Unresolved.EnvVar varname) ->
        fmap (fmap <| toEnvVarSource varname)
             (lookupEnv <| Text.unpack varname)

      Unresolved.Skip ->
        return Nothing

resolveDefaultValue
  :: Int
  -> Text
  -> EDN.Value
  -> ConfigSource
resolveDefaultValue configIndex filepath defaultValue =
  File filepath configIndex defaultValue

envVarConfigResolver configIndex filepath mDefaultValue sources configLens config = do
  mEnvVarSource <- resolveEnvVarSource sources

  let
    mFileSource =
      resolveDefaultValue configIndex filepath <$> mDefaultValue

    sources =
      [mEnvVarSource, mFileSource]
      |> catMaybes
      |> Set.fromList

  return <| config & configLens %~ (Set.union sources)

buildEnvVarResolver_ configIndex filepath configLens unresolvedConfigValue =
  let
    configEntryReducer configKey unresolvedConfigVal acc =
      case unresolvedConfigVal of
        Unresolved.ConfigValue mdefaultVal sources ->
          let
            subConfigLens =
              configLens
              << _SubConfig
              << L.at configKey
              << (_JustConfigValue Set.empty)
              << _ConfigValue

            resolver =
              Vector.singleton <|
                envVarConfigResolver configIndex
                                     filepath
                                     mdefaultVal
                                     sources
                                     subConfigLens

          in
            Vector.concat [ acc, resolver ]

        Unresolved.SubConfig {} ->
          let
            subConfigLens =
              configLens
              << _SubConfig
              << L.at configKey
              << _JustSubConfig

            resolver =
              buildEnvVarResolver_ configIndex
                                   filepath
                                   subConfigLens
                                   unresolvedConfigVal
          in
            Vector.concat [ acc, resolver ]
  in
    case unresolvedConfigValue of
      Unresolved.SubConfig configm ->
        if Map.null configm then
          Vector.empty
        else
          Map.foldWithKey configEntryReducer Vector.empty configm

      _ ->
        Vector.empty

buildEnvVarResolver
  :: Int
  -> Unresolved.Config
  -> Vector (Config -> IO Config)
buildEnvVarResolver configIndex unresolvedConfig =
  let
    filepath =
      Unresolved.configFilePath unresolvedConfig

    configValue =
      Unresolved.configValue unresolvedConfig
  in
    buildEnvVarResolver_ configIndex filepath _Config configValue

buildEnvVarResolvers :: [Unresolved.Config] -> IO (Vector (Config -> IO Config))
buildEnvVarResolvers unresolvedConfigList =
  unresolvedConfigList
  |> L.imapM (\configIndex unresolvedConfig ->
               return <| buildEnvVarResolver configIndex unresolvedConfig)
  |> (Vector.concat <$>)


resolveEnvVarConfiguration :: [Unresolved.Config] -> IO Config
resolveEnvVarConfiguration unresolvedConfigList =
  unresolvedConfigList
  |> buildEnvVarResolvers
  >>= foldM (|>) (Config (SubConfig Map.empty))


configValueToEdnMap :: ConfigValue -> EDN.TaggedValue
configValueToEdnMap configValue =
  case configValue of
    ConfigValue sources ->
      case Set.minView sources of
        Nothing ->
          error "config value is empty? impossible"
        Just (source, _) ->
          EDN.tag "" "" (value source)

    SubConfig configm ->
      configm
      |> Map.foldWithKey (\key value acc ->
                             Map.insert key (configValueToEdnMap value) acc)
                         Map.empty
      |> EDN.Map
      |> EDN.tag "" ""

getConfigValue
  :: (MonadThrow m, EDN.FromEDN result)
  => [Text]
  -> Config
  -> m result
getConfigValue keys0 config@(Config configValue0) =
  let
    loop keys configValue =
      case (keys, configValue) of
        ([], ConfigValue sources) ->
          case Set.minView sources of
            Nothing ->
              throwM <| InvalidConfigKeyPath keys0

            Just (source, _) ->
              case EDN.fromEDNv (value source) of
                EDN.Error err ->
                  throwM <| InvalidConfiguration (Text.pack err)

                EDN.Success result ->
                  return result

        ([], configValue) ->
          case EDN.fromEDN (configValueToEdnMap configValue) of
            EDN.Error err ->
              throwM <| InvalidConfiguration (Text.pack err)

            EDN.Success result ->
              return result

        (k:keys1, SubConfig configm) ->
          case Map.lookup (EDN.Keyword <| Text.encodeUtf8 k) configm of
            Nothing ->
              throwM <| InvalidConfigKeyPath keys0
            Just configValue1 ->
              loop keys1 configValue1

        _ ->
          throwM <| InvalidConfigKeyPath keys0
  in
    loop keys0 configValue0
