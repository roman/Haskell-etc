{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Etc.Spec
  ( Types.ConfigSpec
  , Types.SpecError (..)
  , Types.SpecParserError (..)
  , Types.ConfigValueType (..)
  , Types.SingleConfigValueType (..)
  , Parser.readConfigSpec
  , Parser.readConfigSpecTH
  , Parser.parseConfigSpec
  , Parser.parseConfigSpecValue
  )
  where

import Etc.Spec.Internal.ErrorRender ()
import Etc.Spec.Internal.Serializer ()
import qualified Etc.Spec.Internal.Parser as Parser
import qualified Etc.Spec.Internal.Types as Types
