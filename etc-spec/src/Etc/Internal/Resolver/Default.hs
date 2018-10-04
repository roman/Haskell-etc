{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Etc.Internal.Resolver.Default where

import           RIO

import qualified Data.Aeson as JSON
import qualified Data.Aeson.BetterErrors as JSON

import           Etc.Internal.Config
    (
      Config
    , IConfigSource (..)
    , SomeConfigSource (..)
    )
import qualified Etc.Internal.Spec.Types     as Spec
import           Etc.Internal.Resolver.Types
import           Etc.Internal.Resolver.Fold      (ResolverResult (..), resolveSpecEntries)

--------------------------------------------------------------------------------

data DefaultSource = DefaultSource
  { dsValue       :: !JSON.Value }
  deriving (Generic, Typeable, Show, Eq)

instance NFData DefaultSource
instance IConfigSource DefaultSource where
  compareSources _ _ = EQ
  sourceValue = dsValue
  sourcePrettyDoc _ = "Default"

toSomeConfigSource :: Int -> JSON.Value -> SomeConfigSource
toSomeConfigSource priorityIndex val =
  SomeConfigSource priorityIndex $ DefaultSource val

resolveDefault ::
  MonadThrow m =>
  Int -> Map Text Spec.CustomType -> Spec.ConfigSpec -> m Config
resolveDefault priorityIndex customTypes spec =
  resolveSpecEntries configEntryParser resolveConfigEntry customTypes spec
  where
    configEntryParser = JSON.key "default" JSON.asValue
    resolveConfigEntry defaultVal =
      return $
      Just $
      ResolverResult
        Spec.DefaultValueTypeMismatchFound
        (toSomeConfigSource priorityIndex defaultVal)

defaultResolver :: MonadThrow m => Resolver m
defaultResolver =
  Resolver resolveDefault
