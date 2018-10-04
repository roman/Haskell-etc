{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Etc
  (
    -- * CustomType API
    Spec.CustomType
    -- ** 'CustomType' builder functions
  , CustomType.aesonCustomType
  , CustomType.textCustomType
  , CustomType.boundedIntCustomType
  , CustomType.boundedFloatCustomType

    -- * API to read a YAML configuration spec file
  , Spec.ConfigSpec
  , Spec.readConfigSpecTH

    -- * API to generate a configuration map from files and env
  , Resolver.Resolver
  , Resolver.resolveConfig

    -- * API to get values from the resolved configuration map
  , Config.IConfig
  , Config.Config
  , Config.getConfigValue
  , Config.getConfigValueWith

    -- * Other useful APIs

    -- ** API to generate a configuration map using different resolvers
    -- manually
  , Resolver.resolveConfigWith
  , File.jsonConfig
  , File.yamlConfig
  , File.fileResolver
  , Env.envResolver
  , Env.pureEnvResolver

    -- ** API to create different formats to parse configuration files and
    -- configuration spec files
  , FileFormat.FileFormat
  , FileFormat.newFileFormat
  , FileFormat.jsonFormat
  , FileFormat.yamlFormat

    -- * API to parse configuration spec files using a custom 'FileFormat'
  , Spec.readConfigSpecFormatTH
  , Spec.yamlSpec
  , Spec.jsonSpec
  ) where

import qualified Etc.Internal.Config as Config
import qualified Etc.Internal.CustomType as CustomType
import qualified Etc.Internal.FileFormat as FileFormat
import qualified Etc.Internal.Spec.Parser as Spec
import qualified Etc.Internal.Spec.Types as Spec
import qualified Etc.Internal.Resolver as Resolver
import qualified Etc.Internal.Resolver.Types as Resolver
import qualified Etc.Internal.Resolver.File as File
import qualified Etc.Internal.Resolver.Env as Env
