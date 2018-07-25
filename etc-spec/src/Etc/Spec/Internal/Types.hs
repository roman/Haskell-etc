{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Etc.Spec.Internal.Types where

import RIO

-- import Language.Haskell.TH.Syntax (Lift(..))

import qualified Data.Aeson as JSON
import qualified Data.Aeson.BetterErrors as JSON

data SpecError
  = SpecParserError !(JSON.ParseError SpecParserError)
  deriving (Show)

instance Exception SpecError

data SpecParserError
  = CannotInferTypeFromDefault ![Text] !JSON.Value
  | InferredNestedArrayOnDefault ![Text] !JSON.Value
  | InvalidConfigValueType ![Text] !Text
  | ConfigValueDefaultTypeMismatchFound ![Text] !ConfigValueType !JSON.Value
  | RedundantKeysOnValueSpec ![Text] ![Text]
  | InvalidSpecEntries !ConfigValue
  deriving (Show)

instance Exception SpecParserError

-- |
data SingleConfigValueType
  = CVTString
  | CVTNumber
  | CVTBool
  | CVTObject
  | CVTCustom Text
  deriving (Generic, Show, Read, Eq)

-- |
data ConfigValueType
  = CVTSingle !SingleConfigValueType
  | CVTArray  !SingleConfigValueType
  deriving (Generic, Show, Read, Eq)

-- |
data ConfigValueData =
  ConfigValueData {
    configValueDefault    :: !(Maybe JSON.Value)
  , configValueType       :: !ConfigValueType
  , configValueSensitive  :: !Bool
  , configValueJSON       :: !JSON.Value
  }
  deriving (Generic, Show, Eq)

-- |
data ConfigValue
  = ConfigValue !ConfigValueData
  | SubConfig   !(Map Text ConfigValue)
  deriving (Generic, Show, Eq)

-- |
data ConfigSpec
  = ConfigSpec { configSpecJSON    :: !JSON.Object
               , configSpecEntries :: !(Map Text ConfigValue)
               }
  deriving (Generic, Show, Eq)
