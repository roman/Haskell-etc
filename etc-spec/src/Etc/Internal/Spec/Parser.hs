{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
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

import Etc.Internal.CustomType
import Etc.Internal.FileFormat (FileFormat, jsonFormat, yamlFormat)
import Etc.Internal.Spec.Error ()
import Etc.Internal.Spec.Types

--------------------------------------------------------------------------------

jsonSpec :: FileFormat (JSON.ParseError SpecParserError)
jsonSpec = jsonFormat

yamlSpec :: FileFormat Yaml.ParseException
yamlSpec = yamlFormat

matchesConfigValueType :: ConfigValueType -> JSON.Value -> Bool
matchesConfigValueType cvType json = case (json, cvType) of
  (JSON.Null    , CVTSingle _        ) -> True
  (JSON.String{}, CVTSingle CVTString) -> True
  (JSON.Number{}, CVTSingle CVTNumber) -> True
  (JSON.Bool{}  , CVTSingle CVTBool  ) -> True
  (JSON.Object{}, CVTSingle CVTObject) -> True
  (JSON.Array arr, CVTArray inner) ->
    if null arr then True else all (matchesConfigValueType (CVTSingle inner)) arr
  _ -> False

assertFieldTypeMatchesE
  :: (ConfigValueType -> JSON.Value -> e) -> ConfigValueType -> JSON.Value -> Either e ()
assertFieldTypeMatchesE errCtor cvType json | matchesConfigValueType cvType json = Right ()
                                            | otherwise = Left (errCtor cvType json)

assertFieldTypeMatches
  :: Monad m
  => (ConfigValueType -> JSON.Value -> e)
  -> ConfigValueType
  -> JSON.Value
  -> JSON.ParseT e m ()
assertFieldTypeMatches errCtor cvType json
  | matchesConfigValueType cvType json = return ()
  | otherwise                          = JSON.throwCustomError (errCtor cvType json)

inferConfigValueTypeFromJSON
  :: Monad m => [Text] -> JSON.Value -> JSON.ParseT SpecParserError m ConfigValueType
inferConfigValueTypeFromJSON fieldKeypath defaultJSON = either JSON.throwCustomError
                                                               return
                                                               (inferJSON defaultJSON)
 where
  inferJSON json = case json of
    JSON.String{} -> Right $ CVTSingle CVTString
    JSON.Number{} -> Right $ CVTSingle CVTNumber
    JSON.Bool{}   -> Right $ CVTSingle CVTBool
    JSON.Array arr
      | null arr -> Left (CannotInferTypeFromDefault fieldKeypath json)
      | otherwise -> case inferJSON (Vector.head arr) of
        Right CVTArray{}     -> Left (InferredNestedArrayOnDefault fieldKeypath json)
        Right (CVTSingle ty) -> Right $ CVTArray ty
        Left  err            -> Left err
    _ -> Left (CannotInferTypeFromDefault fieldKeypath json)

parseArrayType ::
  Text -> (SingleConfigValueType -> ConfigValueType, Text)
parseArrayType input =
  if "[" `Text.isPrefixOf` input &&
     "]" `Text.isSuffixOf` input
    then (CVTArray, Text.strip $ Text.tail $ Text.init input)
    else (CVTSingle, Text.strip input)

parseConfigValueType1 ::
     (Monad m)
  => Map Text CustomType
  -> [Text]
  -> JSON.ParseT SpecParserError m (ConfigValueType, Maybe CustomType)
parseConfigValueType1 customTypes fieldKeypath =
  JSON.withText $ \typeText ->
    case parseArrayType typeText of
      (ctor, "string") -> Right $ (ctor CVTString, Nothing)
      (ctor, "number") -> Right $ (ctor CVTNumber, Nothing)
      (ctor, "bool")   -> Right $ (ctor CVTBool, Nothing)
      (ctor, "object") -> Right $ (ctor CVTObject, Nothing)
      (ctor, customTypeName) ->
        case Map.lookup customTypeName customTypes of
          Nothing ->
            Left (UnknownConfigValueType fieldKeypath customTypeName)
          Just parser ->
            Right ( ctor (CVTCustom customTypeName)
                  , Just parser
                  )

parseConfigValueType ::
     (Monad m)
  => Map Text CustomType
  -> [Text]
  -> Maybe JSON.Value
  -> JSON.ParseT SpecParserError m ConfigValueType
parseConfigValueType customTypes fieldKeypath mdefaultValue =
  case mdefaultValue of
    Nothing ->
      fst <$> JSON.key "type" (parseConfigValueType1 customTypes fieldKeypath)
    Just defaultValue -> do
      mFieldType <-
        JSON.keyMay "type" (parseConfigValueType1 customTypes fieldKeypath)
      case mFieldType of
        Nothing -> inferConfigValueTypeFromJSON fieldKeypath defaultValue

        Just (fieldType, Nothing) -> do
          assertFieldTypeMatches
            (DefaultValueTypeMismatchFound fieldKeypath)
            fieldType
            defaultValue
          return fieldType

        Just (fieldType, Just customType) -> do
          let
            parseError =
              (DefaultValueTypeMismatchFound
                   fieldKeypath
                   fieldType
                   defaultValue)
          parseCustomType (isCVTArray fieldType) parseError defaultValue customType
          return fieldType

parseConfigValueData ::
     (Monad m)
  => Map Text CustomType
  -> [Text]
  -> JSON.ParseT SpecParserError m ConfigValueData
parseConfigValueData customTypes fieldKeypath = JSON.key "etc/spec" $ do
  configValueDefault   <- JSON.keyMay "default" JSON.asValue
  -- TODO: make tests around type inference
  configValueType      <- parseConfigValueType customTypes fieldKeypath configValueDefault
  configValueSensitive <- JSON.keyOrDefault "sensitive" False JSON.asBool
  configValueJSON      <- JSON.asValue
  return $ ConfigValueData
    { configValueDefault
    , configValueType
    , configValueSensitive
    , configValueJSON
    }

parseConfigSpecEntries :: Monad m => Map Text CustomType -> [Text] -> JSON.ParseT SpecParserError m ConfigValue
parseConfigSpecEntries customTypes fieldKeypath = do
  jsonValue <- JSON.asValue
  case jsonValue of
    JSON.Object object -> case HashMap.lookup "etc/spec" object of
      Nothing -> parseSubConfig
      Just _
        |
        -- TODO: Test this case
        -- There is more than one entry in the map containing 'etc/spec', this is likely
        -- an error and should be reported as soon as possible
          HashMap.size object > 1 -> JSON.throwCustomError
          (RedundantKeysOnValueSpec fieldKeypath $ HashMap.keys $ HashMap.delete
            "etc/spec"
            object
          )
        | otherwise -> parseConfigValue
    _ -> do
      configValueType <- inferConfigValueTypeFromJSON fieldKeypath jsonValue
      return $ ConfigValue $ ConfigValueData
        { configValueDefault   = Just jsonValue
        , configValueType
        , configValueSensitive = False
        , configValueJSON      = jsonValue
        }
 where
  parseSubConfig = SubConfig . Map.fromList <$> JSON.forEachInObject
    (\key -> (,) key <$> parseConfigSpecEntries customTypes (key : fieldKeypath))
  parseConfigValue = ConfigValue <$> parseConfigValueData customTypes fieldKeypath

configSpecParser :: (Monad m) => Map Text CustomType -> JSON.ParseT SpecParserError m ConfigSpec
configSpecParser customTypes = do
  result         <- JSON.key "etc/entries" (parseConfigSpecEntries customTypes [])
  configSpecJSON <- HashMap.delete "etc/entries" <$> JSON.asObject
  case result of
    ConfigValue{} -> JSON.throwCustomError (InvalidSpecEntries result)
    SubConfig configSpecEntries ->
      return $ ConfigSpec {configSpecJSON , configSpecEntries }

parseConfigSpec :: (Monad m, MonadThrow m) => [(Text, CustomType)] -> ByteString -> m ConfigSpec
parseConfigSpec customTypes bytes = do
  let result = Yaml.decodeEither' bytes
  case result of
    Left  err     -> throwM (SpecError err)
    Right jsonVal -> parseConfigSpecValue customTypes jsonVal

parseConfigSpecValue :: (Monad m, MonadThrow m) => [(Text, CustomType)] -> JSON.Value -> m ConfigSpec
parseConfigSpecValue customTypes jsonValue = do
  result <- JSON.parseValueM (configSpecParser (Map.fromList customTypes)) jsonValue
  case result of
    Left  err  -> throwM (SpecError err)
    Right spec -> return spec

readConfigSpec :: (MonadIO m, MonadThrow m) => [(Text, CustomType)] -> FilePath -> m ConfigSpec
readConfigSpec customTypes filepath = do
  bytes <- readFileBinary filepath
  parseConfigSpec customTypes bytes

readConfigSpecTH :: [(Text, CustomType)] -> FilePath -> ExpQ
readConfigSpecTH customTypes filepath = do
  addDependentFile filepath
  configSpec <- runIO $ readConfigSpec customTypes filepath
  [| configSpec |]
