{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Etc.Internal.Spec.Parser where

import RIO

import qualified RIO.HashMap as HashMap
import qualified RIO.Text    as Text

import Prelude (fail)

import qualified RIO.Vector.Partial as Vector (head)

import Data.Aeson ((.:), (.:?))

import qualified Data.Aeson       as JSON
import qualified Data.Aeson.Types as JSON (Parser, typeMismatch)

import System.Etc.Internal.Errors
import System.Etc.Internal.Spec.Types

--------------------------------------------------------------------------------
-- CLI

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
  Arg <$> (object .:? "metavar") <*> (fromMaybe True <$> (object .:? "required"))

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

cliArgKeys :: [Text]
cliArgKeys = ["input", "commands", "metavar", "required"]

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

--------------------------------------------------------------------------------

instance JSON.FromJSON ConfigValueType where
  parseJSON = JSON.withText "ConfigValueType (string, number, bool)" $ \tyText ->
    case Text.toLower tyText of
      "string"   -> pure $ CVTSingle CVTString
      "number"   -> pure $ CVTSingle CVTNumber
      "bool"     -> pure $ CVTSingle CVTBool
      "[string]" -> pure $ CVTArray CVTString
      "[number]" -> pure $ CVTArray CVTNumber
      "[bool]"   -> pure $ CVTArray CVTBool
      "[object]" -> pure $ CVTArray CVTObject
      _  -> JSON.typeMismatch "ConfigValueType (string, number, bool)" (JSON.String tyText)

inferErrorMsg :: String
inferErrorMsg = "could not infer type from given default value"

parseBytesToConfigValueJSON :: ConfigValueType -> Text -> Maybe JSON.Value
parseBytesToConfigValueJSON cvType content =
  case JSON.eitherDecodeStrict' (Text.encodeUtf8 content) of
    Right value | matchesConfigValueType value cvType -> return value
                | otherwise                           -> Nothing
    Left _err
      | matchesConfigValueType (JSON.String content) cvType -> return (JSON.String content)
      | otherwise -> Nothing

jsonToConfigValueType :: JSON.Value -> Either String ConfigValueType
jsonToConfigValueType json = case json of
  JSON.String{} -> Right $ CVTSingle CVTString
  JSON.Number{} -> Right $ CVTSingle CVTNumber
  JSON.Bool{}   -> Right $ CVTSingle CVTBool
  JSON.Array arr
    | null arr -> Left inferErrorMsg
    | otherwise -> case jsonToConfigValueType (Vector.head arr) of
      Right CVTArray{}     -> Left "nested arrays values are not supported"
      Right (CVTSingle ty) -> Right $ CVTArray ty
      Left  err            -> Left err
  _ -> Left inferErrorMsg

coerceConfigValueType :: Text -> JSON.Value -> ConfigValueType -> Maybe JSON.Value
coerceConfigValueType rawValue json cvType = case (json, cvType) of
  (JSON.Null    , CVTSingle _        ) -> Just JSON.Null
  (JSON.String{}, CVTSingle CVTString) -> Just json
  (JSON.Number{}, CVTSingle CVTNumber) -> Just json
  (JSON.Bool{}  , CVTSingle CVTBool  ) -> Just json
  (JSON.Object{}, CVTSingle CVTObject) -> Just json
  (JSON.Array{} , CVTArray{}         ) -> Just json
  (JSON.Number{}, CVTSingle CVTString) -> Just (JSON.String rawValue)
  (JSON.Bool{}  , CVTSingle CVTString) -> Just (JSON.String rawValue)
  _                                    -> Nothing

matchesConfigValueType :: JSON.Value -> ConfigValueType -> Bool
matchesConfigValueType json cvType = case (json, cvType) of
  (JSON.Null    , CVTSingle _        ) -> True
  (JSON.String{}, CVTSingle CVTString) -> True
  (JSON.Number{}, CVTSingle CVTNumber) -> True
  (JSON.Bool{}  , CVTSingle CVTBool  ) -> True
  (JSON.Object{}, CVTSingle CVTObject) -> True
  (JSON.Array arr, CVTArray inner) ->
    if null arr then True else all (`matchesConfigValueType` CVTSingle inner) arr
  _ -> False

assertMatchingConfigValueType :: JSON.Value -> ConfigValueType -> Either SomeException ()
assertMatchingConfigValueType json cvType
  | matchesConfigValueType json cvType = Right ()
  | otherwise = Left $ toException $ ConfigValueTypeMismatchFound "" json cvType

getConfigValueType
  :: Maybe JSON.Value -> Maybe ConfigValueType -> JSON.Parser ConfigValueType
getConfigValueType mdefValue mCvType = case (mdefValue, mCvType) of
  (Just JSON.Null, Just cvType) -> pure cvType

  (Just defValue , Just cvType) -> do
    either (fail . show) return $ assertMatchingConfigValueType defValue cvType
    return cvType

  (Nothing      , Just cvType) -> pure cvType

  (Just defValue, Nothing    ) -> either fail pure $ jsonToConfigValueType defValue

  (Nothing      , Nothing    ) -> fail inferErrorMsg

instance JSON.FromJSON cmd => JSON.FromJSON (ConfigValue cmd) where
  parseJSON json  =
    case json of
      JSON.Object object ->
        case HashMap.lookup "etc/spec" object of
          -- normal object
          Nothing -> do
            subConfigMap <- foldM
                        (\subConfigMap (key, value) -> do
                            innerValue <- JSON.parseJSON value
                            return $ HashMap.insert key innerValue subConfigMap)
                        HashMap.empty
                        (HashMap.toList object)
            if HashMap.null subConfigMap then
              fail "Entries cannot have empty maps as values"
            else
              return (SubConfig subConfigMap)

          -- etc spec value object
          Just (JSON.Object fieldSpec) ->
            if HashMap.size object == 1 then do
              -- NOTE: not using .:? here as it casts JSON.Null to Nothing, we
              -- want (Just JSON.Null) returned
              let mDefaultValue = maybe Nothing Just $ HashMap.lookup "default" fieldSpec
              mSensitive    <- fieldSpec .:? "sensitive"
              mCvType       <- fieldSpec .:? "type"
              let sensitive = fromMaybe False mSensitive
              ConfigValue
                <$> pure mDefaultValue
                <*> getConfigValueType mDefaultValue mCvType
                <*> pure sensitive
                <*> (ConfigSources <$> fieldSpec .:? "env"
                                   <*> fieldSpec .:? "cli")
            else
              fail "etc/spec object can only contain one key"

          -- any other JSON value
          Just _ ->
            fail "etc/spec value must be a JSON object"

      _ -> do
        cvType <- either fail pure $ jsonToConfigValueType json
        return
          ConfigValue
          {
            defaultValue    = Just json
          , configValueType = cvType
          , isSensitive     = False
          , configSources   = ConfigSources Nothing Nothing
          }

parseFiles :: JSON.Value -> JSON.Parser FilesSpec
parseFiles = JSON.withObject "FilesSpec" $ \object -> do
  files  <- object .: "etc/files"
  mEnv   <- files .:? "env"
  mPaths <- files .:? "paths"
  if isNothing mEnv && isNothing mPaths
    then fail "either `env` or a `paths` keys are required when using `etc/files`"
    else return $ FilesSpec mEnv (fromMaybe [] mPaths)

parseFilePaths :: JSON.Value -> JSON.Parser FilesSpec
parseFilePaths =
  JSON.withObject "FilesSpec" $ \object -> FilePathsSpec <$> object .: "etc/filepaths"

parseFileSpec :: JSON.Value -> JSON.Parser (Maybe FilesSpec)
parseFileSpec json@(JSON.Object object) = do
  mFiles     <- object .:? "etc/files"
  mFilePaths <- object .:? "etc/filepaths"
  if isJust (mFiles :: Maybe JSON.Value) && isJust (mFilePaths :: Maybe JSON.Value)
    then fail "either the `etc/files` or `etc/filepaths` key can be used; not both"
    else if isJust mFiles
      then Just <$> parseFiles json
      else if isJust mFilePaths then Just <$> parseFilePaths json else pure Nothing
parseFileSpec _ = pure Nothing

instance JSON.FromJSON cmd => JSON.FromJSON (ConfigSpec cmd) where
  parseJSON json =
    case json of
      JSON.Object object ->
        ConfigSpec
        <$> parseFileSpec json
        <*> (object .:? "etc/cli")
        <*> (fromMaybe HashMap.empty <$> (object .:? "etc/entries"))
      _ ->
        JSON.typeMismatch "ConfigSpec" json
