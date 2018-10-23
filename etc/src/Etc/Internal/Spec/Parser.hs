{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_HADDOCK hide #-}
module Etc.Internal.Spec.Parser where

import           RIO
import qualified RIO.HashMap        as HashMap
import qualified RIO.Map            as Map
import qualified RIO.Text           as Text
import qualified RIO.Text.Partial   as Text (init, tail)
import qualified RIO.Vector.Partial as Vector (head)

import qualified Data.Aeson              as JSON hiding (withText)
import qualified Data.Aeson.BetterErrors as JSON
import qualified Data.Yaml               as Yaml

import Language.Haskell.TH        (ExpQ, runIO)
import Language.Haskell.TH.Syntax (addDependentFile)

import Etc.Internal.Renderer (HumanErrorMessage)
import Etc.Internal.CustomType
import Etc.Internal.FileFormat (FileFormat(..), jsonFormat, yamlFormat)
import Etc.Internal.Spec.Error ()
import Etc.Internal.Spec.Types

--------------------------------------------------------------------------------

-- | 'FileFormat' used to parse configuration spec files in JSON
jsonSpec :: FileFormat (SpecError (JSON.ParseError SpecParserError))
jsonSpec = SpecError <$> jsonFormat

-- | 'FileFormat' used to parse configuration spec files in YAML
yamlSpec :: FileFormat (SpecError Yaml.ParseException)
yamlSpec = SpecError <$> yamlFormat

matchesConfigValueType :: Map Text CustomType -> ConfigValueType -> JSON.Value -> Bool
matchesConfigValueType customTypes cvType jsonVal = case (jsonVal, cvType) of
  (JSON.Null    , CVTSingle _        ) -> True
  (JSON.String{}, CVTSingle CVTString) -> True
  (JSON.Number{}, CVTSingle CVTNumber) -> True
  (JSON.Bool{}  , CVTSingle CVTBool  ) -> True
  (JSON.Object{}, CVTSingle CVTObject) -> True
  (_, CVTSingle (CVTCustom typeName)) ->
    case Map.lookup typeName customTypes of
      Nothing -> False
      Just (CustomType parser) ->
        isRight (JSON.parseValue parser jsonVal)
  (JSON.Array arr, CVTArray inner) ->
    if null arr then True else all (matchesConfigValueType customTypes (CVTSingle inner)) arr
  _ -> False

assertFieldTypeMatchesE
  :: (ConfigValueType -> JSON.Value -> e)
  -> Map Text CustomType
  -> ConfigValueType
  -> JSON.Value
  -> Either e ()
assertFieldTypeMatchesE errCtor customTypes cvType json
  | matchesConfigValueType customTypes cvType json = Right ()
  | otherwise = Left (errCtor cvType json)

assertFieldTypeMatches
  :: Monad m
  => (ConfigValueType -> JSON.Value -> e)
  -> Map Text CustomType
  -> ConfigValueType
  -> JSON.Value
  -> JSON.ParseT e m ()
assertFieldTypeMatches errCtor customTypes cvType json
  | matchesConfigValueType customTypes cvType json = return ()
  | otherwise = JSON.throwCustomError (errCtor cvType json)

inferConfigValueTypeFromJSON ::
     Monad m
  => Text
  -> [Text]
  -> JSON.Value
  -> JSON.ParseT SpecParserError m ConfigValueType
inferConfigValueTypeFromJSON sourceName fieldKeypath defaultJSON =
  either JSON.throwCustomError return (inferJSON defaultJSON)
  where
    inferJSON json =
      case json of
        JSON.String {} -> Right $ CVTSingle CVTString
        JSON.Number {} -> Right $ CVTSingle CVTNumber
        JSON.Bool {} -> Right $ CVTSingle CVTBool
        JSON.Array arr
          | null arr -> Left (CannotInferTypeFromDefault sourceName fieldKeypath json)
          | otherwise ->
            case inferJSON (Vector.head arr) of
              Right CVTArray {} ->
                Left (InferredNestedArrayOnDefault sourceName fieldKeypath json)
              Right (CVTSingle ty) -> Right $ CVTArray ty
              Left err -> Left err
        _ -> Left (CannotInferTypeFromDefault sourceName fieldKeypath json)

parseArrayType ::
  Text -> (SingleConfigValueType -> ConfigValueType, Text)
parseArrayType input =
  if "[" `Text.isPrefixOf` input &&
     "]" `Text.isSuffixOf` input
    then (CVTArray, Text.strip $ Text.tail $ Text.init input)
    else (CVTSingle, Text.strip input)

parseConfigValueType1 ::
     (Monad m)
  => Text
  -> Map Text CustomType
  -> [Text]
  -> JSON.ParseT SpecParserError m (ConfigValueType, Maybe CustomType)
parseConfigValueType1 sourceName customTypes fieldKeypath =
  JSON.withText $ \typeText ->
    case parseArrayType typeText of
      (ctor, "string") -> Right $ (ctor CVTString, Nothing)
      (ctor, "number") -> Right $ (ctor CVTNumber, Nothing)
      (ctor, "bool")   -> Right $ (ctor CVTBool, Nothing)
      (ctor, "object") -> Right $ (ctor CVTObject, Nothing)
      (ctor, customTypeName) ->
        case Map.lookup customTypeName customTypes of
          Nothing ->
            Left (UnknownConfigValueType sourceName fieldKeypath customTypeName)
          Just parser ->
            Right ( ctor (CVTCustom customTypeName)
                  , Just parser
                  )

parseConfigValueType ::
     (Monad m)
  => Text
  -> Map Text CustomType
  -> [Text]
  -> Maybe JSON.Value
  -> JSON.ParseT SpecParserError m ConfigValueType
parseConfigValueType sourceName customTypes fieldKeypath mdefaultValue =
  case mdefaultValue of
    Nothing ->
      fst <$>
      JSON.key
        "type"
        (parseConfigValueType1 sourceName customTypes fieldKeypath)
    Just defaultValue -> do
      mFieldType <-
        JSON.keyMay
          "type"
          (parseConfigValueType1 sourceName customTypes fieldKeypath)
      case mFieldType of
        Nothing ->
          inferConfigValueTypeFromJSON sourceName fieldKeypath defaultValue
        Just (fieldType, Nothing) -> do
          assertFieldTypeMatches
            (DefaultValueTypeMismatchFound sourceName fieldKeypath)
            customTypes
            fieldType
            defaultValue
          return fieldType
        Just (fieldType, Just customType) -> do
          let parseError =
                (DefaultValueTypeMismatchFound
                   sourceName
                   fieldKeypath
                   fieldType
                   defaultValue)
          parseCustomType
            (isCVTArray fieldType)
            parseError
            defaultValue
            customType
          return fieldType

parseConfigValueData ::
     (Monad m)
  => Text
  -> Map Text CustomType
  -> [Text]
  -> JSON.ParseT SpecParserError m ConfigValueData
parseConfigValueData sourceName customTypes fieldKeypath =
  JSON.key "etc/spec" $ do
    configValueDefault <- JSON.keyMay "default" JSON.asValue
    configValueType <-
      parseConfigValueType
        sourceName
        customTypes
        fieldKeypath
        configValueDefault
    configValueSensitive <- JSON.keyOrDefault "sensitive" False JSON.asBool
    configValueJSON <- JSON.asValue
    return $
      ConfigValueData
        { configValueDefault
        , configValueType
        , configValueSensitive
        , configValueJSON
        }

parseConfigSpecEntries ::
     Monad m
  => Text
  -> Map Text CustomType
  -> [Text]
  -> JSON.ParseT SpecParserError m ConfigValue
parseConfigSpecEntries sourceName customTypes fieldKeypath = do
  jsonValue <- JSON.asValue
  case jsonValue of
    JSON.Object object ->
      case HashMap.lookup "etc/spec" object of
        Nothing -> parseSubConfig
        Just _
        -- TODO: Test this case
        -- There is more than one entry in the map containing 'etc/spec', this is likely
        -- an error and should be reported as soon as possible
          | HashMap.size object > 1 ->
            JSON.throwCustomError
              (RedundantKeysOnValueSpec sourceName fieldKeypath $
               HashMap.keys $ HashMap.delete "etc/spec" object)
          | otherwise -> parseConfigValue
    _ -> do
      configValueType <-
        inferConfigValueTypeFromJSON sourceName fieldKeypath jsonValue
      return $
        ConfigValue $
        ConfigValueData
          { configValueDefault = Just jsonValue
          , configValueType
          , configValueSensitive = False
          , configValueJSON = jsonValue
          }
  where
    parseSubConfig =
      SubConfig . Map.fromList <$>
      JSON.forEachInObject
        (\key ->
           (,) key <$>
           parseConfigSpecEntries sourceName customTypes (key : fieldKeypath))
    parseConfigValue =
      ConfigValue <$>
      (parseConfigValueData sourceName customTypes fieldKeypath)

configSpecParser ::
     (Monad m)
  => Text
  -> Map Text CustomType
  -> JSON.ParseT SpecParserError m ConfigSpec
configSpecParser configSpecFilePath customTypes = do
  result         <- JSON.key "etc/entries" (parseConfigSpecEntries configSpecFilePath customTypes [])
  configSpecJSON <- HashMap.delete "etc/entries" <$> JSON.asObject
  case result of
    ConfigValue{} -> JSON.throwCustomError (InvalidSpecEntries configSpecFilePath result)
    SubConfig configSpecEntries ->
      return $ ConfigSpec {configSpecFilePath, configSpecJSON, configSpecEntries }

parseConfigSpecValue ::
     (Monad m, MonadThrow m)
  => Text
  -> [(Text, CustomType)]
  -> JSON.Value
  -> m ConfigSpec
parseConfigSpecValue sourceName customTypes jsonValue = do
  result <- JSON.parseValueM (configSpecParser sourceName (Map.fromList customTypes)) jsonValue
  case result of
    Left  err  -> throwM (SpecError err)
    Right spec -> return spec

-- TODO: Add source name parameter for debugging purposes
parseConfigSpecInternal ::
     (Show err, Typeable err, HumanErrorMessage err, Monad m, MonadThrow m)
  => Text
  -> FileFormat (SpecError err)
  -> [(Text, CustomType)]
  -> ByteString
  -> m ConfigSpec
parseConfigSpecInternal sourceName FileFormat { fileFormatParser } customTypes bytes = do
  let result = fileFormatParser bytes
  case result of
    Left  err     -> throwM err
    Right jsonVal -> parseConfigSpecValue sourceName customTypes jsonVal

parseConfigSpec ::
     (Show err, Typeable err, HumanErrorMessage err, Monad m, MonadThrow m)
  => FileFormat (SpecError err)
  -> [(Text, CustomType)]
  -> ByteString
  -> m ConfigSpec
parseConfigSpec = parseConfigSpecInternal "<<string>>"

readConfigSpec ::
     (Typeable err, Show err, HumanErrorMessage err, MonadIO m, MonadThrow m)
  => FileFormat (SpecError err)
  -> [(Text, CustomType)]
  -> FilePath
  -> m ConfigSpec
readConfigSpec fileFormat customTypes filepath = do
  bytes <- readFileBinary filepath
  parseConfigSpecInternal (Text.pack filepath) fileFormat customTypes bytes

-- | Reads, parses and validates a configuration spec file. This function allows
-- to use a custom 'FileFormat' in case you want to parse a configuration spec
-- file in any other format that is not YAML.
--
-- ==== Important
--
-- Given this function uses @TemplateHaskell@, you'll get the validation of the
-- configuration spec file at compilation time.
--
--
readConfigSpecFormatTH ::
     (Typeable err, Show err, HumanErrorMessage err)
  => FileFormat (SpecError err) -- ^ File Format for the configuration spec file
  -> [(Text, CustomType)]       -- ^ List of 'CustomTypes' associated to an
                                -- identifier; the identifier may be used in the
                                -- configuration spec file
  -> FilePath                   -- ^ Path of the configuration spec file
  -> ExpQ
readConfigSpecFormatTH fileFormat customTypes filepath = do
  addDependentFile filepath
  configSpec <- runIO $ do
    readConfigSpec fileFormat customTypes filepath
  [| configSpec |]

-- | Reads, parses and validates a YAML configuration spec file.
--
-- ==== Important
--
-- Given this function uses @TemplateHaskell@, you'll get the validation of the
-- configuration spec file at compilation time.
--
--
readConfigSpecTH
  :: [(Text, CustomType)] -- ^ List of 'CustomTypes' associated to an
                          -- identifier; the identifier may be used in the
                          -- configuration spec file
  -> FilePath             -- ^ Path of the configuration spec file
  -> ExpQ
readConfigSpecTH =
  readConfigSpecFormatTH yamlSpec
