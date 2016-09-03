{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module UB.Internal.Config.Unresolved where

import Prelude (fail)
import UB.Prelude
import Data.EDN.Types.Class (parseMaybe)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.EDN as EDN
import qualified Data.EDN.Types.Class as EDN

--------------------------------------------------------------------------------
-- Types

data ConfigSource
  = File
  | EnvVar   { varname :: Text }
  | OptParse { option  :: Text }
  deriving (Show, Eq)

data ConfigSourceSpec
  = Pending ConfigSource
  | Skip
  deriving (Show, Eq)

data ConfigSources
  = ConfigSources { envVar   :: ConfigSourceSpec
                  , optParse :: ConfigSourceSpec
                  , file     :: ConfigSourceSpec
                  }
  deriving (Show, Eq)

data ConfigValue
  = ConfigValue { defaultValue  :: Maybe EDN.Value
                , configSources :: ConfigSources
                }
  | SubConfig { subConfig :: Map EDN.Value ConfigValue }
  deriving (Show, Eq)

newtype Config
  = Config { fromConfig :: ConfigValue }
  deriving (Show, EDN.FromEDN)

--------------------------------------------------------------------------------
-- Constants

fileConfigSource :: ConfigSources
fileConfigSource =
  ConfigSources Skip Skip (Pending File)

--------------------------------------------------------------------------------
-- EDN Parsers

parseConfigDefValue :: EDN.Value -> EDN.Parser (Maybe EDN.Value)
parseConfigDefValue val =
  case val of
    EDN.Map m ->
      case Map.lookup (EDN.stripTag <| EDN.keyword "default") m of
        Just (EDN.NoTag val) ->
          return <| Just val

        Just val ->
          EDN.typeMismatch
            "Expecting value of :default key to be String"
            (EDN.stripTag val)

        Nothing ->
          return Nothing

    _ ->
      fail ("parseConfigDefValue: Expecting map; got " <> show val <> " instead")

parseOptParseStatus :: EDN.Value -> EDN.Parser ConfigSourceSpec
parseOptParseStatus val =
  case val of
    EDN.Map m ->
      case Map.lookup (EDN.stripTag <| EDN.keyword "optparse") m of
        Just (EDN.NoTag (EDN.String optFlagName)) ->
          return <| Pending (OptParse optFlagName)

        Just val ->
          EDN.typeMismatch
            "Expecting value of :optparse key to be String"
            (EDN.stripTag val)

        Nothing ->
          return Skip

    _ ->
      fail ("parseOptParseStatus: Expecting map; got "
            <> show val
            <> " instead")

parseEnvVarStatus :: EDN.Value -> EDN.Parser ConfigSourceSpec
parseEnvVarStatus val =
  case val of
    EDN.Map m ->
      case Map.lookup (EDN.stripTag <| EDN.keyword "envvar") m of
        Just (EDN.NoTag (EDN.String envVarName)) ->
          return <| Pending (EnvVar envVarName)

        Just val ->
          EDN.typeMismatch
            "Expecting value of :envvar key to be String"
            (EDN.stripTag val)

        Nothing ->
          return Skip

    _ ->
      fail ("parseEnvVarStatus: Expecting map; got "
            <> show val
            <> " instead")

configFileSourceStatus :: Maybe a -> ConfigSourceSpec
configFileSourceStatus =
  maybe
    Skip
    (always <| Pending File)

parseConfigSources :: EDN.Value -> EDN.Parser ConfigSources
parseConfigSources val = do
  defVal   <- parseConfigDefValue val
  ConfigSources
    <$> parseEnvVarStatus val
    <*> parseOptParseStatus val
    <*> pure (configFileSourceStatus defVal)

parseConfigValueWithMeta :: EDN.TaggedValue -> EDN.Parser ConfigValue
parseConfigValueWithMeta taggedVal =
  let
     EDN.Tagged val ns tag =
       taggedVal
  in
     if ns == "config" && tag == "meta" then
       case val of
         EDN.Map {} -> do
           defVal       <- parseConfigDefValue val
           sourceStatus <- parseConfigSources val

           return
             <| ConfigValue defVal sourceStatus

         _ ->
           fail "#config/meta reader macro must be used before a map"

     else
       fail ("Invalid reader macro "
             <> show ns
             <> "/"
             <> show tag
             <> "; aborting")

instance EDN.FromEDN ConfigValue where
  parseEDN taggedVal =
    case taggedVal of
      EDN.NoTag (EDN.Map m) ->
        SubConfig
          <$> Map.traverseWithKey (\_k val -> EDN.parseEDN val) m

      EDN.NoTag val ->
        return
          <| ConfigValue (Just val) fileConfigSource
      _ ->
        parseConfigValueWithMeta taggedVal
