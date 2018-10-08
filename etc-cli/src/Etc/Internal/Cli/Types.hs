{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Etc.Internal.Cli.Types where

import RIO

import qualified Data.Aeson as JSON

import Etc.Internal.Config  (IConfigSource(..))

type CliEntryParseError = Text -> [Text] -> CliResolverError

data CliResolverError
  = MissingOptName !Text ![Text]
  | InvalidInputName !Text !Text ![Text]
  | InfoModMissing !Text
  deriving (Show)

data CliSource =
  CliSource { csCliSpec :: !CliEntrySpec
            , csValue   :: !JSON.Value
            }
  deriving (Generic, Show, Eq)

instance NFData CliSource
instance IConfigSource CliSource where
  sourceValue = csValue
  sourcePrettyDoc (CliSource {}) =
    "Cli"

data CliOptSpec
  = CliOptSpec {
    optLong     :: !(Maybe Text)
  , optShort    :: !(Maybe Text)
  , optMetavar  :: !(Maybe Text)
  , optHelp     :: !(Maybe Text)
  , optHidden   :: !Bool
  , optInternal :: !Bool
  }
  deriving (Generic, Show, Eq)

instance NFData CliOptSpec

data CliArgSpec
  = CliArgSpec {
    argMetavar :: !(Maybe Text)
  , argHelp    :: !(Maybe Text)
  }
  deriving (Generic, Show, Eq)

instance NFData CliArgSpec

data CliSwitchSpec
  = CliSwitchSpec {
    switchLong :: !Text
  , switchHelp :: !(Maybe Text)
  }
  deriving (Generic, Show, Eq)

instance NFData CliSwitchSpec

data CliInfoSpec
  = CliInfoSpec {
    cisProgDesc :: !Text
  , cisHeader   :: !(Maybe Text)
  , cisFooter   :: !(Maybe Text)
  }
  deriving (Generic, Show, Eq)

instance NFData CliInfoSpec

data CliEntrySpec
  = Opt CliOptSpec
  | Arg CliArgSpec
  | Switch CliSwitchSpec
  deriving (Generic, Show, Eq)

instance NFData CliEntrySpec
