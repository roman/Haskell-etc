{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
module Etc.Resolver.Internal.Types where

import           RIO

import qualified Etc.Spec as Spec
import Etc.Config (Config)

--------------------
-- Configuration Types

newtype Resolver m
  = Resolver {
      runResolver :: Int -> Spec.ConfigSpec -> m Config
    }

resolveConfig :: Monad m => Spec.ConfigSpec -> [Resolver m] -> m Config
resolveConfig spec resolvers =
  mconcat <$>
  mapM (\(priorityIndex, resolver) ->
          runResolver resolver priorityIndex spec)
    (zip [(1 :: Int)..] $ reverse resolvers)
