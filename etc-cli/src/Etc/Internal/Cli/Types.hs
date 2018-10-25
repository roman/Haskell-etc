{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Etc.Internal.Cli.Types where

import RIO

import qualified Data.Aeson as JSON
import qualified Options.Applicative as Opt

import Etc.Internal.Spec.Types (ConfigSpec, CustomType, ConfigValueData, ConfigValueType)
import Etc.Internal.Config  (IConfigSource(..))

type EntryKey = Text
newtype CmdName =
  CmdName { fromCmdName :: Text }  deriving (Show, Eq, Ord)

type CliEntryParseError = Text -> [Text] -> CliResolverError

data CliResolverError
  = MissingOptName !Text ![Text]
  | InvalidInputName !Text !Text ![Text]
  | SwitchIncompatibleType !ConfigValueType !Text ![Text]
  | InfoModMissing !Text
  | PlainInfoModExpected !Text
  | CommandInfoModExpected !Text
  | CommandListMismatch !Text !(Set CmdName) !(Set CmdName)
  | UnknownCommandOnEntry !Text ![Text] !(Set CmdName) !(Set CmdName)
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

data CliInfoSpecData
  = CliInfoSpecData {
    cisProgDesc :: !Text
  , cisHeader   :: !(Maybe Text)
  , cisFooter   :: !(Maybe Text)
  }
  deriving (Generic, Show, Eq)

instance NFData CliInfoSpecData

data CliInfoSpec
  = PlainCliInfoSpec !CliInfoSpecData
  | CommandCliInfoSpec !CliInfoSpecData !(Map CmdName CliInfoSpecData)
  deriving (Generic, Show, Eq)

instance NFData CliInfoSpec

data CliEntrySpec
  = Opt CliOptSpec
  | Arg CliArgSpec
  | Switch CliSwitchSpec
  deriving (Generic, Show, Eq)

instance NFData CliEntrySpec

-- | References that come handy when building an optparse-applicative
-- 'Opt.Parser'; by using this record, we avoid threading through a bunch of
-- variables
data BuilderEnv
  = BuilderEnv {
    envPriorityIndex   :: !Int
  , envCustomTypes     :: !(Map Text CustomType)
  , envConfigSpec      :: !ConfigSpec
  , envCommandNames    :: !(Set CmdName)
  }

-- | References that come handy when building an optparse-applicative
-- 'Opt.Parser'; by using this record, we avoid threading through a bunch of
-- variables
data FieldEnv
  = FieldEnv {
      fieldBuildEnv        :: !BuilderEnv
    , fieldCliEntrySpec    :: !CliEntrySpec
    , fieldConfigValueSpec :: !ConfigValueData
    }

data CliInfoMod f
  = PlainCli !(Opt.InfoMod f)
  | CommandCli !(Opt.InfoMod f) !(Map Text (Opt.InfoMod f))
