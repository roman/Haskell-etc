{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Etc.Spec
  ( Types.ConfigSpec
  , Types.SpecError (..)
  , Types.SpecParserError (..)
  , Types.ConfigValueType (..)
  , Types.SingleConfigValueType (..)
  , Parser.parseConfigSpec
  , Parser.parseConfigSpecJson
  )
  where

import qualified Etc.Spec.Internal.Parser as Parser
import qualified Etc.Spec.Internal.Types as Types
