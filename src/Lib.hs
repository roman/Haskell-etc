{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Lib where

import Prelude (fail)
import UB.Prelude
import Data.EDN.Types.Class (parseMaybe)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.EDN as EDN
import qualified Data.EDN.Types.Class as EDN
import qualified Data.Vector as V


data FileConfigSource
  = UnresolvedFile
  | ResolvedFile { index :: Int
                 , filename :: Text }

data ConfigSource
  = File     { source  :: FileConfigSource }
  | EnvVar   { varname :: Text }
  | OptParse { option  :: Text }
  deriving (Show, Eq)

data StatusValue
  = Pending ConfigSource
  | Skipped
  deriving (Show, Eq)

data ConfigSourceStatus
  = ConfigSourceStatus { envVar :: StatusValue
                       , optCli :: StatusValue
                       , file   :: StatusValue
                       }
  deriving (Show, Eq)

newtype ConfigValueSpec
  = ConfigValueSpec (Maybe EDN.Value, ConfigSourceStatus)
  deriving (Show, Eq)

newtype ConfigValue
  = ConfigValue { fromConfigValue :: (EDN.Value, ConfigSource) }
  deriving (Show, Eq)

data UnresolvedConfigValue
  = Resolved ConfigValue
  | Spec ConfigValueSpec
  deriving (Show, Eq)


onlyFileConfigSource :: ConfigSourceStatus
onlyFileConfigSource =
  ConfigSourceStatus Skipped Skipped (PendingEval UnresolvedFile)

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

parseOptParseStatus :: EDN.Value -> EDN.Parser StatusValue
parseOptParseStatus val =
  case val of
    EDN.Map m ->
      case Map.lookup (EDN.stripTag <| EDN.keyword "cliopt") m of
        Just (EDN.NoTag (EDN.String optFlagName)) ->
          return <| PendingEval (OptParse optFlagName)

        Just val ->
          EDN.typeMismatch
            "Expecting value of :cliopt key to be String"
            (EDN.stripTag val)

        Nothing ->
          return Skipped

    _ ->
      fail ("parseOptParseStatus: Expecting map; got "
            <> show val
            <> " instead")

parseEnvVarStatus :: EDN.Value -> EDN.Parser StatusValue
parseEnvVarStatus val =
  case val of
    EDN.Map m ->
      case Map.lookup (EDN.stripTag <| EDN.keyword "envvar") m of
        Just (EDN.NoTag (EDN.String envVarName)) ->
          return <| PendingEval (EnvVar envVarName)

        Just val ->
          EDN.typeMismatch
            "Expecting value of :envvar key to be String"
            (EDN.stripTag val)

        Nothing ->
          return Skipped

    _ ->
      fail ("parseEnvVarStatus: Expecting map; got "
            <> show val
            <> " instead")

parseConfigFileStatus :: Maybe a -> StatusValue
parseConfigFileStatus =
  maybe Skipped (always (PendingEval UnknownFile))

parseConfigSourceStatus :: EDN.Value -> EDN.Parser ConfigSourceStatus
parseConfigSourceStatus val = do
  defVal   <- parseConfigDefValue val
  ConfigSourceStatus
    <$> parseEnvVarStatus val
    <*> parseOptParseStatus val
    <*> pure (parseConfigFileStatus defVal)

parseConfigValueWithMeta :: EDN.TaggedValue -> EDN.Parser UnresolvedConfigValue
parseConfigValueWithMeta taggedVal =
  let
     EDN.Tagged val ns tag =
       taggedVal
  in
     if ns == "config" && tag == "meta" then
       case val of
         EDN.Map {} -> do
           defVal       <- parseConfigDefValue val
           sourceStatus <- parseConfigSourceStatus val

           return
             <| Spec
             <| ConfigValueSpec
             <| (defVal, sourceStatus)

         _ ->
           fail "#config/meta reader macro must be used before a map"

     else
       fail ("Invalid reader macro "
             <> show ns
             <> "/"
             <> show tag
             <> "; aborting")

resolvedConfigValue :: UnresolvedConfigValue -> Maybe ConfigValue
resolvedConfigValue configVal0 =
  case configVal0 of
    Spec _ ->
      Nothing
    Resolved configVal ->
      Just configVal

isResolved :: UnresolvedConfigValue -> Bool
isResolved =
  resolvedConfigValue
    >> maybe False (always True)

getValue :: EDN.FromEDN a => ConfigValue -> Maybe a
getValue =
  fromConfigValue
    >> fst
    >> parseMaybe EDN.parseEDNv

instance EDN.FromEDN UnresolvedConfigValue where
  parseEDN taggedVal =
    case taggedVal of
      EDN.NoTag val ->
        return
          <| Spec
          <| ConfigValueSpec (Just val, onlyFileConfigSource)
      _ ->
        parseConfigValueWithMeta taggedVal
