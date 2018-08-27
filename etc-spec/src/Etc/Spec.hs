{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Etc.Spec.Internal.Error ()
import Etc.Spec.Internal.Serializer ()
import qualified Etc.Spec.Internal.Parser as Parser
import qualified Etc.Spec.Internal.Types as Types
