{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Etc
  ( Spec.ConfigSpec
  , Spec.readConfigSpecTH
  , Spec.yamlSpec
  , Spec.jsonSpec

  , Resolver.Resolver
  , Resolver.resolveConfig
  , File.jsonConfig
  , File.yamlConfig
  , File.fileResolver
  , Env.envResolver
  , Env.pureEnvResolver

  , Spec.CustomType
  , CustomType.aesonCustomType
  , CustomType.textCustomType
  , CustomType.boundedIntCustomType

  , Config.IConfig
  , Config.Config
  , Config.getConfigValue
  , Config.getConfigValueWith
  )
  where

import qualified Etc.Internal.Config as Config
import qualified Etc.Internal.CustomType as CustomType
import qualified Etc.Internal.Spec.Parser as Spec
import qualified Etc.Internal.Spec.Types as Spec
import qualified Etc.Internal.Resolver as Resolver
import qualified Etc.Internal.Resolver.Types as Resolver
import qualified Etc.Internal.Resolver.File as File
import qualified Etc.Internal.Resolver.Env as Env
