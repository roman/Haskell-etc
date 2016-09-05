{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}
module UB.Config.Internal.Spec where

import Prelude (fail)
import Control.Lens (makePrisms)
import Control.Monad.Catch (MonadThrow(..))
import Data.Aeson ((.:?))
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe, isNothing)

import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON (typeMismatch)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import UB.Prelude
import UB.Config.Internal.Types (ConfigurationError(..))

--------------------------------------------------------------------------------
-- Types

data OptParseType
  = OptParseString
  | OptParseNumber
  | OptParseSwitch
  deriving (Show, Eq)

data OptParseSpec =
  OptParseSpec { optParseLong     :: Maybe Text
               , optParseShort    :: Maybe Text
               , optParseMetavar  :: Maybe Text
               , optParseHelp     :: Maybe Text
               , optParseRequired :: Bool
               , optParseType     :: OptParseType
               }
  deriving (Show, Eq)

data ConfigSource
  = EnvVar   { varname :: Text }
  | OptParse { option  :: OptParseSpec }
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

newtype ConfigSpec
  = ConfigSpec ConfigValue
  deriving (Show, Eq, JSON.FromJSON)

--------------------------------------------------------------------------------
-- Parser

instance JSON.FromJSON OptParseSpec where
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
              JSON.typeMismatch "OptParseType (string, number, switch)" value

          Just value ->
            JSON.typeMismatch "OptParseType" value

          Nothing ->
            fail "OptParse type is required"
    in
      case json of
        JSON.Object object -> do
          long  <- object .:? "long"
          short <- object .:? "short"
          if isNothing long && isNothing short then
            JSON.typeMismatch
              "OptParseSpec (require either long or short)"
              json
          else
            OptParseSpec
              <$> pure long
              <*> pure short
              <*> (object .:? "metavar")
              <*> (object .:? "help")
              <*> (fromMaybe True <$> (object .:? "required"))
              <*> typeParser object

        _ ->
          JSON.typeMismatch "OptParseSpec" json

instance JSON.FromJSON ConfigValue where
  parseJSON json  =
    case json of
      JSON.Object object ->
        case HashMap.lookup "config/spec" object of
          -- normal object
          Nothing ->
            SubConfig
            <$> foldM
                  (\result (key, value) -> do
                      innerValue <- JSON.parseJSON value
                      return <| HashMap.insert key innerValue result)
                  HashMap.empty
                  (HashMap.toList object)

          -- config spec value object
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
