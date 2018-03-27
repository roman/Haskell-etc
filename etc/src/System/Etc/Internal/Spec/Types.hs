{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Etc.Internal.Spec.Types where

import           Prelude     (fail)
import           RIO
import qualified RIO.HashMap as HashMap

import Data.Aeson ((.:), (.:?))

import qualified Data.Aeson       as JSON
import qualified Data.Aeson.Types as JSON (Parser, typeMismatch)

--------------------------------------------------------------------------------
-- Error Types

data ConfigurationError
  = InvalidConfiguration !(Maybe Text) !Text
  | InvalidConfigKeyPath ![Text]
  | ConfigurationFileNotFound !Text
  deriving (Show)

instance Exception ConfigurationError

--------------------------------------------------------------------------------

data CliOptValueType
  = StringOpt
  | NumberOpt
  | SwitchOpt
  deriving (Show, Eq)

data CliArgValueType
  = StringArg
  | NumberArg
  deriving (Show, Eq)

data CliEntryMetadata
  = Opt {
    optLong      :: !(Maybe Text)
  , optShort     :: !(Maybe Text)
  , optMetavar   :: !(Maybe Text)
  , optHelp      :: !(Maybe Text)
  , optRequired  :: !Bool
  , optValueType :: !CliOptValueType
  }
  | Arg {
    argMetavar   :: !(Maybe Text)
  , optRequired  :: !Bool
  , argValueType :: !CliArgValueType
  }
  deriving (Show, Eq)

data CliEntrySpec cmd
  = CmdEntry {
    cliEntryCmdValue :: !(Vector cmd)
  , cliEntryMetadata :: !CliEntryMetadata
  }
  | PlainEntry {
    cliEntryMetadata :: !CliEntryMetadata
  }
  deriving (Show, Eq)

data CliCmdSpec
  = CliCmdSpec {
    cliCmdDesc   :: !Text
  , cliCmdHeader :: !Text
  }
  deriving (Show, Eq)

data ConfigSources cmd
  = ConfigSources {
    envVar   :: !(Maybe Text)
  , cliEntry :: !(Maybe (CliEntrySpec cmd))
  }
  deriving (Show, Eq)

data ConfigValue cmd
  = ConfigValue {
    defaultValue  :: !(Maybe JSON.Value)
  , isSensitive   :: !Bool
  , configSources :: !(ConfigSources cmd)
  }
  | SubConfig {
    subConfig :: !(HashMap Text (ConfigValue cmd))
  }
  deriving (Show, Eq)

data CliProgramSpec
  = CliProgramSpec {
    cliProgramDesc   :: !Text
  , cliProgramHeader :: !Text
  , cliCommands      :: !(Maybe (HashMap Text CliCmdSpec))
  }
  deriving (Show, Eq)

data ConfigSpec cmd
  = ConfigSpec {
    specConfigFilepaths :: ![Text]
  , specCliProgramSpec  :: !(Maybe CliProgramSpec)
  , specConfigValues    :: !(HashMap Text (ConfigValue cmd))
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- JSON Parsers

instance JSON.FromJSON CliCmdSpec where
  parseJSON json =
    case json of
      JSON.Object object ->
        CliCmdSpec
        <$> object .: "desc"
        <*> object .: "header"
      _ ->
        JSON.typeMismatch "CliCmdSpec" json

instance JSON.FromJSON CliProgramSpec where
  parseJSON json =
    case json of
      JSON.Object object ->
        CliProgramSpec
        <$> object .: "desc"
        <*> object .: "header"
        <*> object .:? "commands"
      _ ->
        JSON.typeMismatch "CliProgramSpec" json

cliArgTypeParser :: JSON.Object -> JSON.Parser CliArgValueType
cliArgTypeParser object = do
  value <- object .: "type"
  case value of
    JSON.String typeName
      | typeName == "string" -> return StringArg
      | typeName == "number" -> return NumberArg
      | otherwise            -> JSON.typeMismatch "CliArgValueType (string, number)" value
    _ -> JSON.typeMismatch "CliArgValueType (string, number)" value

cliArgParser :: JSON.Object -> JSON.Parser CliEntryMetadata
cliArgParser object =
  Arg
    <$> (object .:? "metavar")
    <*> (fromMaybe True <$> (object .:? "required"))
    <*> cliArgTypeParser object

cliOptTypeParser :: JSON.Object -> JSON.Parser CliOptValueType
cliOptTypeParser object = do
  mvalue <- object .:? "type"
  case mvalue of
    Just value@(JSON.String typeName)
      | typeName == "string" -> return StringOpt
      | typeName == "number" -> return NumberOpt
      | typeName == "switch" -> return SwitchOpt
      | otherwise -> JSON.typeMismatch "CliOptValueType (string, number, switch)" value

    Just value -> JSON.typeMismatch "CliOptValueType" value

    Nothing    -> fail "CLI Option type is required"

cliOptParser :: JSON.Object -> JSON.Parser CliEntryMetadata
cliOptParser object = do
  long  <- object .:? "long"
  short <- object .:? "short"
  if isNothing long && isNothing short
    then fail "'option' field input requires either 'long' or 'short' settings"
    else
      Opt
      <$> pure long
      <*> pure short
      <*> (object .:? "metavar")
      <*> (object .:? "help")
      <*> (fromMaybe True <$> (object .:? "required"))
      <*> cliOptTypeParser object

cliArgKeys :: [Text]
cliArgKeys = ["input", "commands", "metavar", "required", "type"]

cliOptKeys :: [Text]
cliOptKeys = ["short", "long", "help"] ++ cliArgKeys

instance JSON.FromJSON cmd => JSON.FromJSON (CliEntrySpec cmd) where
  parseJSON json =
      case json of
        JSON.Object object -> do
          cmdValue   <- object .:? "commands"
          value <- object .: "input"

          let
            optParseEntryCtor =
              maybe PlainEntry CmdEntry cmdValue

          case value of
            JSON.String inputName
              | inputName == "option" -> do
                forM_ (HashMap.keys object) $ \key ->
                  when (not (key `elem` cliOptKeys))
                    (fail $ "cli option contains invalid key " ++ show key)

                optParseEntryCtor <$> cliOptParser object

              | inputName == "argument" -> do
                forM_ (HashMap.keys object) $ \key ->
                  when (not (key `elem` cliArgKeys))
                    (fail $ "cli option contains invalid key " ++ show key)

                optParseEntryCtor <$> cliArgParser object

              | otherwise ->
                JSON.typeMismatch "CliEntryMetadata (invalid input)" value
            _ ->
              JSON.typeMismatch "CliEntryMetadata (invalid input)" value
        _ ->
          JSON.typeMismatch "CliEntryMetadata" json

instance JSON.FromJSON cmd => JSON.FromJSON (ConfigValue cmd) where
  parseJSON json  =
    case json of
      JSON.Array _ ->
        fail "Entries cannot have arrays as values"
      JSON.Object object ->
        case HashMap.lookup "etc/spec" object of
          -- normal object
          Nothing -> do
            result <- foldM
                        (\result (key, value) -> do
                            innerValue <- JSON.parseJSON value
                            return $ HashMap.insert key innerValue result)
                        HashMap.empty
                        (HashMap.toList object)
            if HashMap.null result then
              fail "Entries cannot have empty maps as values"
            else
              return (SubConfig result)

          -- etc spec value object
          Just (JSON.Object fieldSpec) ->
            if HashMap.size object == 1 then do
              mSensitive <- fieldSpec .:? "sensitive"
              let sensitive = fromMaybe False mSensitive
              ConfigValue
                <$> fieldSpec .:? "default"
                <*> pure sensitive
                <*> (ConfigSources <$> fieldSpec .:? "env"
                                   <*> fieldSpec .:? "cli")
            else
              fail "etc/spec object can only contain one key"

          -- any other JSON value
          Just _ ->
            fail "etc/spec value must be a JSON object"

      _ ->
        return
          ConfigValue
          {
            defaultValue = Just json
          , isSensitive = False
          , configSources = ConfigSources Nothing Nothing
          }

instance JSON.FromJSON cmd => JSON.FromJSON (ConfigSpec cmd) where
  parseJSON json  =
    case json of
      JSON.Object object ->
        ConfigSpec
        <$> (fromMaybe [] <$> (object .:?  "etc/filepaths"))
        <*> (object .:? "etc/cli")
        <*> (fromMaybe HashMap.empty <$> (object .:? "etc/entries"))
      _ ->
        JSON.typeMismatch "ConfigSpec" json
