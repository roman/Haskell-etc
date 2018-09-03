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
  , Parser.matchesConfigValueType
  , Parser.assertFieldTypeMatchesE
  , Parser.readConfigSpec
  , Parser.readConfigSpecTH
  , Parser.parseConfigSpec
  , Parser.parseConfigSpecValue
  , Types.getConfigSpecJSON
  , Types.getConfigSpecEntries
  )
  where

import           Etc.Internal.Spec.Error ()
import qualified Etc.Internal.Spec.Parser     as Parser
import           Etc.Internal.Spec.Serializer ()
import qualified Etc.Internal.Spec.Types      as Types
