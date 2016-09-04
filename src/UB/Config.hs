{-# LANGUAGE NoImplicitPrelude #-}
module UB.Config
  ( Spec.ConfigSpec
  , Spec.readConfigSpec
  , Spec.parseConfigSpec
  , Plain.parseConfig
  , Plain.readConfigFromFiles
  , Resolver.resolveEnvVars
  , getConfigValue
  ) where


import Control.Monad.Catch (MonadThrow(..))
import qualified Data.Aeson as JSON
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import UB.Prelude
import UB.Config.Internal.Types
import qualified UB.Config.Internal.Spec as Spec
import qualified UB.Config.Internal.Resolver as Resolver
import qualified UB.Config.Internal.Plain as Plain

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
      |> HashMap.foldrWithKey
          (\key value acc ->
              HashMap.insert key (configValueToJsonObject value) acc)
          HashMap.empty
      |> JSON.Object


getConfigValue
  :: (MonadThrow m, JSON.FromJSON result)
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
              case JSON.fromJSON (value source) of
                JSON.Error err ->
                  throwM <| InvalidConfiguration (Text.pack err)

                JSON.Success result ->
                  return result

        ([], configValue) ->
          case JSON.fromJSON (configValueToJsonObject configValue) of
            JSON.Error err ->
              throwM <| InvalidConfiguration (Text.pack err)

            JSON.Success result ->
              return result

        (k:keys1, SubConfig configm) ->
          case HashMap.lookup k configm of
            Nothing ->
              throwM <| InvalidConfigKeyPath keys0
            Just configValue1 ->
              loop keys1 configValue1

        _ ->
          throwM <| InvalidConfigKeyPath keys0
  in
    loop keys0 configValue0
