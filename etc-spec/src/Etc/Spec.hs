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

import Etc.Internal.Spec.Error ()
import Etc.Internal.Spec.Serializer ()
import qualified Etc.Internal.Spec.Parser as Parser
import qualified Etc.Internal.Spec.Types as Types
