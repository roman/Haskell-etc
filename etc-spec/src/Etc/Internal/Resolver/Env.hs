{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Etc.Internal.Resolver.Env (envResolver, pureEnvResolver) where

import           RIO
import qualified RIO.Map     as Map
import qualified RIO.Text    as Text

import System.Environment (getEnvironment)

import qualified Data.Aeson              as JSON
import qualified Data.Aeson.BetterErrors as JSON

import           Etc.Internal.Config
import           Etc.Internal.Resolver.Types
import           Etc.Internal.Spec.Fold      (ResolverResult (..), resolveSpecToConfig)
import qualified Etc.Internal.Spec.Types     as Spec

import Etc.Internal.Resolver.Env.Error ()
import Etc.Internal.Resolver.Env.Types


toSomeConfigSource :: Int -> Text -> JSON.Value -> SomeConfigSource
toSomeConfigSource index varName val =
  SomeConfigSource index $ EnvSource varName val

resolveEnv
  :: MonadThrow m
  => Map Text Text
  -> Int
  -> Map Text Spec.CustomType
  -> Spec.ConfigSpec
  -> m Config
resolveEnv env priorityIndex customTypes spec =
  resolveSpecToConfig resolveConfigEntry customTypes spec
  where
    getEnvVarName val =
      either
        (const Nothing)
        Just
        (JSON.parseValue (JSON.key "env" JSON.asText) val)

    resolveConfigEntry entryJson =
      return $ do
        varName <- getEnvVarName entryJson
        result <-
          toSomeConfigSource priorityIndex varName . JSON.String <$>
            Map.lookup varName env
        return (ResolverResult (EnvValueTypeMismatch varName) result)

pureEnvResolver :: (MonadThrow m, Monad m) => [(Text, Text)] -> Resolver m
pureEnvResolver env = Resolver (resolveEnv $ Map.fromList env)

envResolver :: (MonadThrow m, MonadIO m) => m (Resolver m)
envResolver = do
  env <- map (Text.pack *** Text.pack) <$> liftIO getEnvironment
  return $ pureEnvResolver env
