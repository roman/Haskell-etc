{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TemplateHaskellQuotes      #-}
#else
{-# LANGUAGE TemplateHaskell            #-}
#endif
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
module System.Etc.Internal.Spec.Types where


import           RIO
import qualified RIO.HashMap        as HashMap
import qualified RIO.Text           as Text
import qualified RIO.Vector         as Vector

import Language.Haskell.TH.Syntax (Lift(..))

import qualified Data.Aeson       as JSON

--------------------------------------------------------------------------------
-- Cli

data CliOptValueType
  = StringOpt
  | NumberOpt
  | SwitchOpt
  deriving (Generic, Show, Eq, Lift)

data CliArgValueType
  = StringArg
  | NumberArg
  deriving (Generic, Show, Eq, Lift)

data CliOptMetadata
  = CliOptMetadata {
    optLong     :: !(Maybe Text)
  , optShort    :: !(Maybe Text)
  , optMetavar  :: !(Maybe Text)
  , optHelp     :: !(Maybe Text)
  , optRequired :: !Bool
  }
  deriving (Generic, Show, Eq)

instance Lift CliOptMetadata where
  lift CliOptMetadata {optLong, optShort, optMetavar, optHelp, optRequired} =
    [| CliOptMetadata
           { optLong = fmap Text.pack optLongStr
           , optShort = fmap Text.pack optShortStr
           , optMetavar = fmap Text.pack optMetavarStr
           , optHelp = fmap Text.pack optHelpStr
           , optRequired = optRequired
           } |]
    where
      optLongStr = fmap Text.unpack optLong
      optShortStr = fmap Text.unpack optShort
      optMetavarStr = fmap Text.unpack optMetavar
      optHelpStr = fmap Text.unpack optHelp

data CliArgMetadata
  = CliArgMetadata {
    argMetavar  :: !(Maybe Text)
  , argHelp     :: !(Maybe Text)
  , argRequired :: !Bool
  }
  deriving (Generic, Show, Eq)

instance Lift CliArgMetadata where
  lift CliArgMetadata {argMetavar, argHelp, argRequired} =
    [| CliArgMetadata {
          argMetavar = fmap Text.pack argMetavarStr
        , argHelp    = fmap Text.pack argHelpStr
        , argRequired = argRequired
        }
     |]
    where
      argMetavarStr = fmap Text.unpack argMetavar
      argHelpStr = fmap Text.unpack argHelp

data CliSwitchMetadata
  = CliSwitchMetadata {
    switchLong     :: !Text
  , switchHelp     :: !(Maybe Text)
  }
  deriving (Generic, Show, Eq)

instance Lift CliSwitchMetadata where
  lift CliSwitchMetadata {switchLong, switchHelp} =
    [| CliSwitchMetadata
           { switchLong = Text.pack switchLongStr
           , switchHelp = fmap Text.pack switchHelpStr
           } |]
    where
      switchLongStr = Text.unpack switchLong
      switchHelpStr = fmap Text.unpack switchHelp


data CliEntryMetadata
  = Opt CliOptMetadata
  | Arg CliArgMetadata
  | Switch CliSwitchMetadata
  deriving (Generic, Show, Eq)

instance Lift CliEntryMetadata where
  lift (Opt metadata)    = [| Opt metadata |]
  lift (Arg metadata)    = [| Arg metadata |]
  lift (Switch metadata) = [| Switch metadata |]

data CliEntrySpec cmd
  = CmdEntry {
    cliEntryCmdValue :: !(Vector cmd)
  , cliEntryMetadata :: !CliEntryMetadata
  }
  | PlainEntry {
    cliEntryMetadata :: !CliEntryMetadata
  }
  deriving (Generic, Show, Eq)

instance Lift cmd => Lift (CliEntrySpec cmd) where
  lift CmdEntry {cliEntryCmdValue, cliEntryMetadata} =
    [| CmdEntry { cliEntryCmdValue = Vector.fromList cliEntryCmdValueList
                , cliEntryMetadata = cliEntryMetadata } |]
    where
      cliEntryCmdValueList = Vector.toList cliEntryCmdValue
  lift PlainEntry {cliEntryMetadata} =
    [| PlainEntry { cliEntryMetadata = cliEntryMetadata } |]

data CliCmdSpec
  = CliCmdSpec {
    cliCmdDesc   :: !Text
  , cliCmdHeader :: !Text
  }
  deriving (Generic, Show, Eq)

instance Lift CliCmdSpec where
  lift CliCmdSpec {cliCmdDesc, cliCmdHeader} =
    [| CliCmdSpec { cliCmdDesc = Text.pack cliCmdDescStr, cliCmdHeader = Text.pack cliCmdHeaderStr } |]
    where
      cliCmdDescStr = Text.unpack cliCmdDesc
      cliCmdHeaderStr = Text.unpack cliCmdHeader

--------------------------------------------------------------------------------

data ConfigSources cmd
  = ConfigSources {
    envVar   :: !(Maybe Text)
  , cliEntry :: !(Maybe (CliEntrySpec cmd))
  }
  deriving (Generic, Show, Eq)

instance Lift cmd => Lift (ConfigSources cmd) where
  lift ConfigSources {envVar, cliEntry} =
    [| ConfigSources (fmap Text.pack envVarStr) cliEntry |]
    where
      envVarStr = fmap Text.unpack envVar

data SingleConfigValueType
  = CVTString
  | CVTNumber
  | CVTBool
  | CVTObject
  deriving (Generic, Show, Read, Eq, Lift)

instance Display SingleConfigValueType where
  display value =
    case value of
      CVTString -> "string"
      CVTNumber -> "number"
      CVTBool   -> "bool"
      CVTObject -> "object"

data ConfigValueType
  = CVTSingle !SingleConfigValueType
  | CVTArray  !SingleConfigValueType
  deriving (Generic, Show, Read, Eq, Lift)

instance Display ConfigValueType where
  display value =
    case value of
      CVTSingle singleVal -> display singleVal
      CVTArray  singleVal -> display $ "[" <> display singleVal <> "]"

data ConfigValue cmd
  = ConfigValue {
    defaultValue    :: !(Maybe JSON.Value)
  , configValueType :: !ConfigValueType
  , isSensitive     :: !Bool
  , configSources   :: !(ConfigSources cmd)
  }
  | SubConfig {
    subConfig :: !(HashMap Text (ConfigValue cmd))
  }
  deriving (Generic, Show, Eq)

instance Lift cmd => Lift (ConfigValue cmd) where
  lift ConfigValue {defaultValue, configValueType, isSensitive, configSources} =
    [| ConfigValue defaultValue configValueType isSensitive configSources |]
  lift SubConfig {subConfig} =
    [| SubConfig (HashMap.fromList $ map (first Text.pack) subConfigList) |]
    where
      subConfigList = map (first Text.unpack) $ HashMap.toList subConfig

data CliProgramSpec
  = CliProgramSpec {
    cliProgramDesc   :: !Text
  , cliProgramHeader :: !Text
  , cliCommands      :: !(Maybe (HashMap Text CliCmdSpec))
  }
  deriving (Generic, Show, Eq)

instance Lift CliProgramSpec where
  lift CliProgramSpec {cliProgramDesc, cliProgramHeader, cliCommands} =
    [| CliProgramSpec (Text.pack cliProgramDescStr)
                      (Text.pack cliProgramHeaderStr)
                      (fmap (HashMap.fromList . map (first Text.pack)) cliCommandList) |]
    where
      cliProgramDescStr   = Text.unpack cliProgramDesc
      cliProgramHeaderStr = Text.unpack cliProgramHeader
      cliCommandList      = fmap (map (first Text.unpack) . HashMap.toList) cliCommands

data FilesSpec
  = FilePathsSpec ![Text]
  | FilesSpec { fileLocationEnvVar :: !(Maybe Text), fileLocationPaths :: ![Text] }
  deriving (Generic, Show, Eq)

instance Lift FilesSpec where
  lift (FilePathsSpec pathsList) =
    [| FilePathsSpec $ map Text.pack pathsListStr |]
    where
      pathsListStr = map Text.unpack pathsList
  lift FilesSpec { fileLocationEnvVar, fileLocationPaths } =
    [| FilesSpec (fmap Text.pack fileLocationEnvVarStr)
                 (fmap Text.pack fileLocationsPathsStr)  |]
    where
      fileLocationEnvVarStr = fmap Text.unpack fileLocationEnvVar
      fileLocationsPathsStr = fmap Text.unpack fileLocationPaths

data ConfigSpec cmd
  = ConfigSpec {
    specConfigFilepaths :: !(Maybe FilesSpec)
  , specCliProgramSpec  :: !(Maybe CliProgramSpec)
  , specConfigValues    :: !(HashMap Text (ConfigValue cmd))
  }
  deriving (Generic, Show, Eq)

instance Lift cmd => Lift (ConfigSpec cmd) where
  lift ConfigSpec {specConfigFilepaths, specCliProgramSpec, specConfigValues} =
    [| ConfigSpec specConfigFilepaths
                  specCliProgramSpec
                  (HashMap.fromList $ map (first Text.pack) configValuesList) |]
    where
      configValuesList = map (first Text.unpack) $ HashMap.toList specConfigValues
