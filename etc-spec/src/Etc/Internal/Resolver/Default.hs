{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Etc.Internal.Resolver.Default (resolveDefault) where

import Protolude

import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap

import Etc.Internal.Types
import qualified Etc.Internal.Spec.Types as Spec

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

buildDefaultResolver :: Spec.ConfigSpec cmd -> Maybe ConfigValue
buildDefaultResolver spec =
  let
    resolverReducer :: Text -> Spec.ConfigValue cmd -> Maybe ConfigValue -> Maybe ConfigValue
    resolverReducer specKey specValue mConfig =
      case specValue of
        Spec.ConfigValue def _ ->
          let
            mConfigSource =
              (ConfigValue . Set.singleton . Default) <$> def
          in
            writeConfig specKey <$> mConfigSource <*> mConfig

        Spec.SubConfig specConfigMap ->
          let
            mSubConfig =
              HashMap.foldrWithKey
                   resolverReducer
                   (Just emptySubConfig)
                   specConfigMap
          in do
            writeConfig specKey <$> mSubConfig <*> mConfig
  in
    HashMap.foldrWithKey
      resolverReducer
      (Just emptySubConfig)
      (Spec.specConfigValues spec)

resolveDefault ::  Spec.ConfigSpec cmd -> Config
resolveDefault spec =
  maybe (Config emptySubConfig)
        Config
        (buildDefaultResolver spec)
