{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Etc.Internal.Resolver.Env.Types where

import RIO

import qualified Data.Aeson as JSON

import Data.Text.Prettyprint.Doc (pretty, (<+>))

import           Etc.Internal.Config
import qualified Etc.Internal.Spec.Types as Spec

data EnvSource = EnvSource
  { esVarName :: !Text
  , esValue   :: !JSON.Value
  }
  deriving (Generic, Typeable, Show, Eq)

instance NFData EnvSource
instance IConfigSource EnvSource where
  sourceValue = esValue
  sourcePrettyDoc (EnvSource {esVarName}) =
    "Env:" <+> pretty esVarName

data EnvResolverError
  = EnvValueTypeMismatch !Text !Spec.SpecFilePath !Spec.SpecEntryPath !Spec.ConfigValueType !JSON.Value
  deriving (Show)
