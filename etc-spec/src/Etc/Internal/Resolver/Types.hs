{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
module Etc.Internal.Resolver.Types where

import           RIO

import           Etc.Internal.Config     (Config)
import qualified Etc.Internal.Spec.Types as Spec

--------------------
-- Configuration Types

newtype Resolver m
  = Resolver {
      runResolver :: Int -> Map Text Spec.CustomType -> Spec.ConfigSpec -> m Config
    }

newtype ResolverError err
  = ResolverError err
