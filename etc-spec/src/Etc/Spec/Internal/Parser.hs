{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Etc.Spec.Internal.Parser where

import RIO
import qualified RIO.Map as Map
import qualified RIO.HashMap as HashMap
import qualified RIO.Vector.Partial as Vector (head)

import qualified Data.Aeson as JSON hiding (withText)
import qualified Data.Aeson.BetterErrors as JSON

import Etc.Spec.Internal.Types

--------------------------------------------------------------------------------

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

assertFieldTypeMatches ::
     Monad m
  => [Text]
  -> ConfigValueType
  -> JSON.Value
  -> JSON.ParseT SpecParserError m ()
assertFieldTypeMatches fieldKeypath cvType json
  | matchesConfigValueType cvType json = return ()
  | otherwise = JSON.throwCustomError (ConfigValueDefaultTypeMismatchFound fieldKeypath cvType json)

inferConfigValueTypeFromJSON ::
     Monad m => [Text] -> JSON.Value -> JSON.ParseT SpecParserError m ConfigValueType
inferConfigValueTypeFromJSON fieldKeypath defaultJSON =
    either JSON.throwCustomError return (inferJSON defaultJSON)
  where
    inferJSON json =
      case json of
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

parseConfigValueType1 ::
     (Monad m) => [Text] -> JSON.ParseT SpecParserError m ConfigValueType
parseConfigValueType1 fieldKeypath = do
  JSON.withText $ \typeText ->
    case typeText of
      "string"   -> Right $ CVTSingle CVTString
      "number"   -> Right $ CVTSingle CVTNumber
      "bool"     -> Right $ CVTSingle CVTBool
      "[string]" -> Right $ CVTArray CVTString
      "[number]" -> Right $ CVTArray CVTNumber
      "[bool]"   -> Right $ CVTArray CVTBool
      "[object]" -> Right $ CVTArray CVTObject
      -- TODO: Implement tests
      _ -> Left (InvalidConfigValueType fieldKeypath typeText)

parseConfigValueType ::
     Monad m
  => [Text]
  -> Maybe JSON.Value
  -> JSON.ParseT SpecParserError m ConfigValueType
parseConfigValueType fieldKeypath mdefaultValue =
  case mdefaultValue of
    Nothing ->
      JSON.key "type" (parseConfigValueType1 fieldKeypath)
    Just defaultValue -> do
      mFieldType <- JSON.keyMay "type" (parseConfigValueType1 fieldKeypath)
      case mFieldType of
        Nothing ->
          inferConfigValueTypeFromJSON fieldKeypath defaultValue
        Just fieldType -> do
          assertFieldTypeMatches fieldKeypath fieldType defaultValue
          return fieldType

parseConfigValueData ::
     Monad m => [Text] -> JSON.ParseT SpecParserError m ConfigValueData
parseConfigValueData fieldKeypath = JSON.key "etc/spec" $ do
  configValueDefault <- JSON.keyMay "default" JSON.asValue
  -- TODO: make tests around type inference
  configValueType <- parseConfigValueType fieldKeypath configValueDefault
  configValueSensitive <- JSON.keyOrDefault "sensitive" False JSON.asBool
  configValueJSON <- JSON.asValue
  return
    $ ConfigValueData
    {
      configValueDefault
    , configValueType
    , configValueSensitive
    , configValueJSON
    }

parseConfigSpecEntries :: Monad m => [Text] -> JSON.ParseT SpecParserError m ConfigValue
parseConfigSpecEntries fieldKeypath = do
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
              (RedundantKeysOnValueSpec fieldKeypath $
               HashMap.keys $ HashMap.delete "etc/spec" object)
          | otherwise -> parseConfigValue
    _ -> do
      configValueType <- inferConfigValueTypeFromJSON fieldKeypath jsonValue
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
      (SubConfig . Map.fromList) <$>
      JSON.forEachInObject
        (\key -> (,) key <$> parseConfigSpecEntries (key : fieldKeypath))
    parseConfigValue = ConfigValue <$> parseConfigValueData fieldKeypath

configSpecParser :: Monad m => JSON.ParseT SpecParserError m ConfigSpec
configSpecParser = do
  result <- JSON.key "etc/entries" (parseConfigSpecEntries [])
  configSpecJSON <- HashMap.delete "etc/entries" <$> JSON.asObject
  case result of
    ConfigValue {} -> JSON.throwCustomError (InvalidSpecEntries result)
    SubConfig configSpecEntries ->
      return $ ConfigSpec {configSpecJSON, configSpecEntries}

parseConfigSpec :: (Monad m, MonadThrow m) => ByteString -> m ConfigSpec
parseConfigSpec bytes = do
  result <- JSON.parseStrictM configSpecParser bytes
  case result of
    Left err -> throwM (SpecParserError err)
    Right spec -> return spec

parseConfigSpecJson :: (Monad m, MonadThrow m) => JSON.Value -> m ConfigSpec
parseConfigSpecJson json = do
  result <- JSON.parseValueM configSpecParser json
  case result of
    Left err -> throwM (SpecParserError err)
    Right spec -> return spec
