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

import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON (typeMismatch)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import UB.Prelude
import System.Etc.Internal.Types (ConfigurationError(..))

--------------------------------------------------------------------------------
-- Types

data OptParseOptionType
  = OptParseString
  | OptParseNumber
  | OptParseSwitch
  deriving (Show, Eq)

data OptParseOptionSpec
  = OptParseOptionSpec {
    optParseLong     :: Maybe Text
  , optParseShort    :: Maybe Text
  , optParseMetavar  :: Maybe Text
  , optParseHelp     :: Maybe Text
  , optParseRequired :: Bool
  , optParseType     :: OptParseOptionType
  }
  deriving (Show, Eq)

data OptParseProgramSpec
  = OptParseProgramSpec {
    optParseProgramDesc   :: Text
  , optParseProgramHeader :: Text
  }
  deriving (Show, Eq)

instance JSON.FromJSON OptParseProgramSpec where
  parseJSON json =
    case json of
      JSON.Object object ->
        OptParseProgramSpec
        <$> object .: "desc"
        <*> object .: "header"
      _ ->
        JSON.typeMismatch "OptParseProgramSpec" json

data ConfigSource
  = EnvVar   { varname :: Text }
  | OptParse { option  :: OptParseOptionSpec }
  deriving (Show, Eq)

data ConfigSources
  = ConfigSources { envVar   :: Maybe ConfigSource
                  , optParse :: Maybe ConfigSource
                  }
  deriving (Show, Eq)

data ConfigValue
  = ConfigValue { defaultValue  :: Maybe JSON.Value
                , configSources :: ConfigSources
                }
  | SubConfig { subConfig :: HashMap Text ConfigValue }
  deriving (Show, Eq)

$(makePrisms ''ConfigValue)

data ConfigSpec
  = ConfigSpec {
     configFilepaths     :: [Text]
   , optParseProgramSpec :: Maybe OptParseProgramSpec
   , configValue         :: ConfigValue
   }
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Parser

instance JSON.FromJSON OptParseOptionSpec where
  parseJSON json =
    let
      typeParser object = do
        mvalue <- object .:? "type"
        case mvalue of
          Just value@(JSON.String typeName) ->
            if typeName == "string" then
              return OptParseString
            else if typeName == "number" then
              return OptParseNumber
            else if typeName == "switch" then
              return OptParseSwitch
            else
              JSON.typeMismatch "OptParseOptionType (string, number, switch)" value

          Just value ->
            JSON.typeMismatch "OptParseOptionType" value

          Nothing ->
            fail "OptParse type is required"
    in
      case json of
        JSON.Object object -> do
          long  <- object .:? "long"
          short <- object .:? "short"
          if isNothing long && isNothing short then
            JSON.typeMismatch
              "OptParseOptionSpec (require either long or short)"
              json
          else
            OptParseOptionSpec
              <$> pure long
              <*> pure short
              <*> (object .:? "metavar")
              <*> (object .:? "help")
              <*> (fromMaybe True <$> (object .:? "required"))
              <*> typeParser object

        _ ->
          JSON.typeMismatch "OptParseOptionSpec" json

instance JSON.FromJSON ConfigValue where
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
              <*> (ConfigSources <$> ((EnvVar <$>) <$> spec .:? "env")
                                 <*> ((OptParse <$>) <$> spec .:? "optparse"))

          -- any other JSON value
          Just innerJson ->
            return <|
              ConfigValue (Just innerJson) (ConfigSources Nothing Nothing)

      _ ->
        return <|
          ConfigValue (Just json) (ConfigSources Nothing Nothing)


instance JSON.FromJSON ConfigSpec where
  parseJSON json  =
    case json of
      JSON.Object object ->
        ConfigSpec
        <$> (object .:  "etc/filepaths")
        <*> (object .:? "etc/optparse")
        <*> (object .: "etc/entries")
      _ ->
        JSON.typeMismatch "ConfigSpec" json

--------------------------------------------------------------------------------

parseConfigSpec
  :: MonadThrow m
    => LB8.ByteString
    -> m ConfigSpec
parseConfigSpec input =
  case JSON.eitherDecode input of
    Left err ->
      throwM <| InvalidConfiguration (Text.pack err)

    Right result ->
      return result

readConfigSpec :: Text -> IO ConfigSpec
readConfigSpec filepath = do
  contents <- (LB8.readFile <| Text.unpack filepath)
  parseConfigSpec contents
