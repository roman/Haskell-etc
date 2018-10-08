{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}
module Etc.Internal.Resolver.Env (envResolver, pureEnvResolver) where

import           RIO
import qualified RIO.Map     as Map
import qualified RIO.Text    as Text

import System.Environment (lookupEnv)

import qualified Data.Aeson              as JSON
import qualified Data.Aeson.BetterErrors as JSON

import           Etc.Internal.Config
import           Etc.Internal.Resolver.Types
import           Etc.Internal.Resolver.Fold      (ResolverResult (..), resolveSpecEntries)
import qualified Etc.Internal.Spec.Types     as Spec

import Etc.Internal.Resolver.Env.Error ()
import Etc.Internal.Resolver.Env.Types


toSomeConfigSource :: Int -> Text -> JSON.Value -> SomeConfigSource
toSomeConfigSource index varName val =
  SomeConfigSource index $ EnvSource varName val

resolveEnv
  :: MonadThrow m
  => (Text -> m (Maybe Text))
  -> Int
  -> Map Text Spec.CustomType
  -> Spec.ConfigSpec
  -> m Config
resolveEnv lookupEnvFn priorityIndex customTypes spec =
  resolveSpecEntries configEntryParser resolveConfigEntry customTypes spec
  where
    configEntryParser =
      JSON.key "env" JSON.asText

    resolveConfigEntry varName = do
      lookupResult <- lookupEnvFn varName
      case lookupResult of
        Nothing -> return Nothing
        Just varValue -> do
          let result = toSomeConfigSource priorityIndex varName $ JSON.String varValue
          return $ Just $ ResolverResult (EnvValueTypeMismatch varName) result

pureEnvResolver :: (MonadThrow m, Monad m) => [(Text, Text)] -> Resolver m
pureEnvResolver env =
  let
    envMap = Map.fromList env
    pureLookupEnv = return . flip Map.lookup envMap
  in
    Resolver (resolveEnv pureLookupEnv)

envResolver :: (MonadThrow m, MonadIO m) => Resolver m
envResolver =
  let
    lookupText name =
      fmap Text.pack <$> liftIO (lookupEnv (Text.unpack name))
  in
    Resolver (resolveEnv lookupText)
