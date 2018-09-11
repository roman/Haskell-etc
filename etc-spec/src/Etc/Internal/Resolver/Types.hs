{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
module Etc.Internal.Resolver.Types where

import Prelude (putStrLn)

import           RIO

import           Etc.Internal.Config     (Config)
import qualified Etc.Internal.Spec.Types as Spec

--------------------
-- Configuration Types

newtype Resolver m
  = Resolver {
      runResolver :: Int -> Spec.ConfigSpec -> m Config
    }

newtype ResolverError err
  = ResolverError err
  deriving (Show)
