{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# OPTIONS_HADDOCK hide #-}
module Etc.Internal.Resolver.Types where

import           RIO

import           Etc.Internal.Config     (Config)
import qualified Etc.Internal.Spec.Types as Spec

--------------------
-- Configuration Types

-- | Responsible of parsing and interpreting the metadata found in your
-- application's 'Spec.ConfigSpec'; it gathers all the values of configuration
-- entries from a particular "input source" (e.g. configuration files,
-- environment variables, etc).
--
-- @since 1.0.0.0
newtype Resolver m
  = Resolver {
      runResolver :: Int -> Map Text Spec.CustomType -> Spec.ConfigSpec -> m Config
    }

newtype ResolverError err
  = ResolverError err
