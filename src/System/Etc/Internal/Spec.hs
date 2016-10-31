{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}
module System.Etc.Internal.Spec where

import Prelude (fail)
import Control.Lens (makePrisms)
import Control.Monad.Catch (MonadThrow(..))
import Data.Aeson ((.:), (.:?))
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe, isNothing)
import Data.Vector (Vector)

import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON (Parser, typeMismatch)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import System.Etc.Internal.Prelude
import System.Etc.Internal.Types (ConfigurationError(..))

--------------------------------------------------------------------------------
-- Types

data OptParseOptionValueType
  = OptParseOptionString
  | OptParseOptionNumber
  | OptParseOptionSwitch
  deriving (Show, Eq)

data OptParseArgValueType
  = OptParseArgString
  | OptParseArgNumber
  deriving (Show, Eq)

data OptParseEntrySpec cmd
  = CmdEntry {
    optParseEntrySpecCmdValue :: Vector cmd
  , optParseEntrySpecArgs     :: OptParseEntrySpecSettings
  }
  | PlainEntry {
    optParseEntrySpecArgs :: OptParseEntrySpecSettings
  }
  deriving (Show, Eq)

data OptParseEntrySpecSettings
  = Option {
    optParseLong            :: Maybe Text
  , optParseShort           :: Maybe Text
  , optParseMetavar         :: Maybe Text
  , optParseHelp            :: Maybe Text
  , optParseRequired        :: Bool
  , optParseOptionValueType :: OptParseOptionValueType
  }
  | Argument {
    optParseMetavar      :: Maybe Text
  , optParseRequired     :: Bool
  , optParseArgValueType :: OptParseArgValueType
  }
  deriving (Show, Eq)

data OptParseCommandSpec
  = OptParseCommandSpec {
    optParseCommandDesc   :: Text
  , optParseCommandHeader :: Text
  }
  deriving (Show, Eq)

data OptParseProgramSpec
  = OptParseProgramSpec {
    optParseProgramDesc   :: Text
  , optParseProgramHeader :: Text
  , optParseCommands      :: Maybe (HashMap Text OptParseCommandSpec)
  }
  deriving (Show, Eq)

data ConfigSources cmd
  = ConfigSources { envVar   :: Maybe Text
                  , optParse :: Maybe (OptParseEntrySpec cmd)
                  }
  deriving (Show, Eq)

data ConfigValue cmd
  = ConfigValue { defaultValue  :: Maybe JSON.Value
                , configSources :: ConfigSources cmd
                }
  | SubConfig { subConfig :: HashMap Text (ConfigValue cmd) }
  deriving (Show, Eq)

$(makePrisms ''ConfigValue)

data ConfigSpec' cmd
  = ConfigSpec {
     specConfigFilepaths     :: [Text]
   , specOptParseProgramSpec :: Maybe OptParseProgramSpec
   , specConfigValues        :: HashMap Text (ConfigValue cmd)
   }
  deriving (Show, Eq)

type ConfigSpec = ConfigSpec' Text

--------------------------------------------------------------------------------
-- JSON Parsers

instance JSON.FromJSON OptParseCommandSpec where
  parseJSON json =
    case json of
      JSON.Object object ->
        OptParseCommandSpec
        <$> object .: "desc"
        <*> object .: "header"
      _ ->
        JSON.typeMismatch "OptParseCommandSpec" json

instance JSON.FromJSON OptParseProgramSpec where
  parseJSON json =
    case json of
      JSON.Object object ->
        OptParseProgramSpec
        <$> object .: "desc"
        <*> object .: "header"
        <*> object .:? "commands"
      _ ->
        JSON.typeMismatch "OptParseProgramSpec" json

argValueTypeParser
  :: JSON.Object
    -> JSON.Parser OptParseArgValueType
argValueTypeParser object = do
  value <- object .: "type"
  case value of
    JSON.String typeName ->
      if typeName == "string" then
        return OptParseArgString
      else if typeName == "number" then
        return OptParseArgNumber
      else
        JSON.typeMismatch "OptParseArgValueType (string, number)" value
    _ ->
      JSON.typeMismatch "OptParseArgValueType (string, number)" value

argumentInputOptParser
  :: JSON.Object
    -> JSON.Parser OptParseEntrySpecSettings
argumentInputOptParser object =
  Argument
    <$> (object .:? "metavar")
    <*> (fromMaybe True <$> (object .:? "required"))
    <*> (argValueTypeParser object)

optionValueTypeParser
  :: JSON.Object
    -> JSON.Parser OptParseOptionValueType
optionValueTypeParser object = do
  mvalue <- object .:? "type"
  case mvalue of
    Just value@(JSON.String typeName) ->
      if typeName == "string" then
        return OptParseOptionString
      else if typeName == "number" then
        return OptParseOptionNumber
      else if typeName == "switch" then
        return OptParseOptionSwitch
      else
        JSON.typeMismatch "OptParseOptionValueType (string, number, switch)" value

    Just value ->
      JSON.typeMismatch "OptParseOptionValueType" value

    Nothing ->
      fail "OptParse type is required"

optionInputOptParser
  :: JSON.Object
    -> JSON.Parser OptParseEntrySpecSettings
optionInputOptParser object = do
  long  <- object .:? "long"
  short <- object .:? "short"
  if isNothing long && isNothing short then
    fail "'option' field input requires either 'long' or 'short' settings"
  else
    Option
      <$> (pure long)
      <*> (pure short)
      <*> (object .:? "metavar")
      <*> (object .:? "help")
      <*> (fromMaybe True <$> (object .:? "required"))
      <*> (optionValueTypeParser object)

instance JSON.FromJSON cmd => JSON.FromJSON (OptParseEntrySpec cmd) where
  parseJSON json =
      case json of
        JSON.Object object -> do
          cmdValue   <- object .:? "commands"
          value <- object .: "input"

          let
            optParseEntryCtor =
              maybe PlainEntry CmdEntry cmdValue

          case value of
            JSON.String inputName ->
              if inputName == "option" then
                optParseEntryCtor <$> optionInputOptParser object
              else if inputName == "argument" then
                optParseEntryCtor <$> argumentInputOptParser object
              else
                JSON.typeMismatch "OptParseEntrySpec (invalid input)" value
            _ ->
              JSON.typeMismatch "OptParseEntrySpec (invalid input)" value
        _ ->
          JSON.typeMismatch "OptParseEntrySpec" json

instance JSON.FromJSON cmd => JSON.FromJSON (ConfigValue cmd) where
  parseJSON json  =
    case json of
      JSON.Object object ->
        case HashMap.lookup "etc/spec" object of
          -- normal object
          Nothing ->
            SubConfig
            <$> foldM
                  (\result (key, value) -> do
                      innerValue <- JSON.parseJSON value
                      return <| HashMap.insert key innerValue result)
                  HashMap.empty
                  (HashMap.toList object)

          -- etc spec value object
          Just (JSON.Object spec) ->
            ConfigValue
              <$> spec .:? "default"
              <*> (ConfigSources <$> (spec .:? "env")
                                 <*> (spec .:? "optparse"))

          -- any other JSON value
          Just innerJson ->
            return <|
              ConfigValue (Just innerJson) (ConfigSources Nothing Nothing)

      _ ->
        return <|
          ConfigValue (Just json) (ConfigSources Nothing Nothing)


instance JSON.FromJSON cmd => JSON.FromJSON (ConfigSpec' cmd) where
  parseJSON json  =
    case json of
      JSON.Object object ->
        ConfigSpec
        <$> (object .:  "etc/filepaths")
        <*> (object .:? "etc/optparse")
        <*> (object .:  "etc/entries")
      _ ->
        JSON.typeMismatch "ConfigSpec" json

--------------------------------------------------------------------------------

parseConfigSpec
  :: (MonadThrow m, JSON.FromJSON cmd)
    => LB8.ByteString
    -> m (ConfigSpec' cmd)
parseConfigSpec input =
  case JSON.eitherDecode input of
    Left err ->
      throwM <| InvalidConfiguration (Text.pack err)

    Right result ->
      return result

readConfigSpec :: JSON.FromJSON cmd => Text -> IO (ConfigSpec' cmd)
readConfigSpec filepath = do
  contents <- (LB8.readFile <| Text.unpack filepath)
  parseConfigSpec contents
