{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Etc.Spec
  ( Types.ConfigSpec
  , Types.ConfigValue (..)
  , Types.ConfigValueData(..)
  , Types.SpecError (..)
  , Types.SpecParserError (..)
  , Types.ConfigValueType (..)
  , Types.SingleConfigValueType (..)

  , Types.CustomType
  , CustomType.aesonCustomType
  , CustomType.textCustomType
  , CustomType.boundedIntCustomType
  , CustomType.boundedFloatCustomType

  , Parser.matchesConfigValueType
  , Parser.assertFieldTypeMatchesE
  , Types.getConfigSpecJSON
  , Types.getConfigSpecEntries

  , Parser.jsonSpec
  , Parser.yamlSpec
  , Parser.readConfigSpec
  , Parser.parseConfigSpec
  , Parser.parseConfigSpecValue
  , Parser.readConfigSpecTH
  )
  where

import qualified Etc.Internal.CustomType      as CustomType
import           Etc.Internal.Spec.Error ()
import qualified Etc.Internal.Spec.Parser     as Parser
import           Etc.Internal.Spec.Serializer ()
import qualified Etc.Internal.Spec.Types      as Types
