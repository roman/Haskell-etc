{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Etc.Internal.Resolver.Default where

import           RIO
import qualified RIO.Map as Map
import qualified RIO.Set as Set

import qualified Data.Aeson as JSON

import           Etc.Internal.Config
    ( Config (..)
    , ConfigValue (..)
    , IConfigSource (..)
    , SomeConfigSource (..)
    , Value
    , emptySubConfig
    , filterMaybe
    , isEmptySubConfig
    , markAsSensitive
    , writeInSubConfig
    )
import           Etc.Internal.Resolver.Types
import qualified Etc.Internal.Spec.Types     as Spec

data DefaultSource = DefaultSource
  { dsValue       :: !(Value JSON.Value) }
  deriving (Generic, Typeable, Show, Eq)

instance NFData DefaultSource
instance IConfigSource DefaultSource where
  compareSources _ _ = EQ
  sourceValue = dsValue
  sourcePrettyDoc _ = "Default"

toSomeConfigSource :: Int -> Value JSON.Value -> SomeConfigSource
toSomeConfigSource priorityIndex val =
  SomeConfigSource priorityIndex $ DefaultSource val

toDefaultConfigValue :: Int -> Bool -> JSON.Value -> ConfigValue
toDefaultConfigValue priorityIndex sensitive =
  ConfigValue . Set.singleton . toSomeConfigSource priorityIndex . markAsSensitive sensitive

resolveDefault :: Int -> Spec.ConfigSpec -> Maybe ConfigValue
resolveDefault priorityIndex spec =
  let resolverReducer
        :: Text -> Spec.ConfigValue -> Maybe ConfigValue -> Maybe ConfigValue
      resolverReducer specKey specValue mConfig = case specValue of
        Spec.ConfigValue Spec.ConfigValueData { Spec.configValueDefault, Spec.configValueSensitive } ->
          let mConfigSource = toDefaultConfigValue priorityIndex configValueSensitive <$> configValueDefault
              updateConfig  = writeInSubConfig specKey <$> mConfigSource <*> mConfig
          in  updateConfig <|> mConfig

        Spec.SubConfig specConfigMap ->
          let mSubConfig =
                specConfigMap
                  & Map.foldrWithKey resolverReducer (Just emptySubConfig)
                  & filterMaybe isEmptySubConfig

              updateConfig = writeInSubConfig specKey <$> mSubConfig <*> mConfig
          in  updateConfig <|> mConfig
  in  Spec.configSpecEntries spec
      & Map.foldrWithKey resolverReducer (Just emptySubConfig)
      & filterMaybe isEmptySubConfig

defaultResolver :: Monad m => Resolver m
defaultResolver =
  Resolver $ \priorityIndex spec ->
    return $ maybe (Config mempty) Config (resolveDefault priorityIndex spec)
