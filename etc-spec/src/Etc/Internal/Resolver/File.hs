{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Etc.Internal.Resolver.File where

import           RIO
import qualified RIO.HashMap as HashMap
import qualified RIO.Map     as Map
import qualified RIO.Set     as Set
import qualified RIO.Text    as Text


import qualified Data.Aeson              as JSON
import qualified Data.Aeson.BetterErrors as JSON
import qualified Data.Yaml               as Yaml

import System.Directory   (doesFileExist)
import System.Environment (lookupEnv)

import Etc.Internal.Resolver.Types

import           Etc.Internal.Config
    (Config (..), ConfigValue (..), SomeConfigSource (..), Value, markAsSensitive)
import qualified Etc.Spec            as Spec

import Etc.Internal.Resolver.File.Error ()
import Etc.Internal.Resolver.File.Types

--------------------------------------------------------------------------------
-- Spec Parser

parseSpecFiles :: Monad m => JSON.ParseT FileResolverError m (Maybe Text, [Text])
parseSpecFiles = do
  mFileEnv  <- JSON.keyMay "env" JSON.asText
  filepaths <- JSON.key "paths" (JSON.eachInArray JSON.asText)
  when (null filepaths) (JSON.throwCustomError ConfigSpecFilesPathsEntryIsEmpty)
  return (mFileEnv, filepaths)

parseConfigSpec :: Monad m => JSON.ParseT FileResolverError m (Maybe Text, [Text])
parseConfigSpec = do
  mFiles <- JSON.keyMay "etc/files" JSON.asObject
  case mFiles of
    Nothing -> do
      otherKeys <- JSON.forEachInObject return
      JSON.throwCustomError $ ConfigSpecFilesEntryMissing otherKeys
    Just _ ->
      JSON.key "etc/files" parseSpecFiles

--------------------------------------------------------------------------------
-- Resolver

parseConfig
  :: (MonadThrow m)
  => FileFormat e
  -> Int
  -> Spec.ConfigValue
  -> Int
  -> FileValueOrigin
  -> ByteString
  -> m Config
parseConfig FileFormat { fileFormatParser } priorityIndex spec fileIndex fileOrigin bytes =
    case fileFormatParser bytes of
      Left _err -> throwM $ ConfigInvalidSyntaxFound fileOrigin
      Right jsonValue ->
        Config <$> parseConfigValue [] priorityIndex spec fileIndex fileOrigin jsonValue

toSomeConfigSource :: Int -> Int -> FileValueOrigin -> Value JSON.Value -> SomeConfigSource
toSomeConfigSource priorityIndex index origin val =
  SomeConfigSource priorityIndex $ FileSource index origin val

parseConfigValue
  :: (MonadThrow m)
  => [Text]
  -> Int
  -> Spec.ConfigValue
  -> Int
  -> FileValueOrigin
  -> JSON.Value
  -> m ConfigValue
parseConfigValue keyPath priorityIndex spec fileIndex fileOrigin jsonVal =
  case (spec, jsonVal) of
    (Spec.SubConfig currentSpec, JSON.Object object) -> SubConfig <$> foldM
      (\acc (key, subConfigValue) -> case Map.lookup key currentSpec of
        Nothing ->
          throwM $ UnknownConfigKeyFound fileOrigin keyPath key (Map.keys currentSpec)
        Just subConfigSpec -> do
          value1 <- parseConfigValue (key : keyPath)
                                     priorityIndex
                                     subConfigSpec
                                     fileIndex
                                     fileOrigin
                                     subConfigValue
          return $ Map.insert key value1 acc
      )
      Map.empty
      (HashMap.toList object)

    (Spec.SubConfig{}, _) -> throwM $ SubConfigEntryExpected fileOrigin keyPath jsonVal

    (Spec.ConfigValue Spec.ConfigValueData { Spec.configValueSensitive, Spec.configValueType }, _)
      -> do
        either throwM return $ Spec.assertFieldTypeMatchesE
          (ConfigFileValueTypeMismatch fileOrigin keyPath)
          configValueType
          jsonVal
        return $ ConfigValue
          ( Set.singleton
          $ toSomeConfigSource priorityIndex fileIndex fileOrigin
          $ markAsSensitive configValueSensitive jsonVal
          )

readConfigFile
  :: (MonadIO m) => FileFormat e -> Text -> m (Either FileResolverError ByteString)
readConfigFile FileFormat { fileFormatName } filepath =
  let filepathStr = Text.unpack filepath
  in
    do
      fileExists <- liftIO $ doesFileExist filepathStr
      if fileExists
        then do
          contents <- readFileBinary filepathStr
          if any (\formatName -> ("." <> formatName) `Text.isSuffixOf` filepath)
                 fileFormatName
          then
            return $ Right contents
          else
            return (Left $ UnsupportedFileExtensionGiven filepath fileFormatName)
        else return (Left $ ConfigFileNotPresent filepath)

readConfigFromFileSources
  :: (MonadThrow m, MonadIO m)
  => FileFormat e
  -> Bool
  -> Int
  -> Spec.ConfigSpec
  -> [FileValueOrigin]
  -> m (Config, [SomeException])
readConfigFromFileSources fileParser throwErrors priorityIndex spec fileSources =
  fileSources
    & zip [1 ..]
    & mapM
        (\(fileIndex, fileOrigin) -> do
          mContents <- readConfigFile fileParser (fileSourcePath fileOrigin)
          let result =
                either throwM return mContents
                >>= parseConfig fileParser
                                priorityIndex
                                (Spec.getConfigSpecEntries spec)
                                fileIndex
                                fileOrigin
          case result of
            Left err | throwErrors ->
                -- NOTE: This is fugly, if we happen to add more "raisable" errors, improve
                -- this code with a helper that receives the exceptions (similar to catches)
                                      case fromException err of
              Just UnknownConfigKeyFound{} -> throwM err
              Just ConfigFileValueTypeMismatch{} -> throwM err
              _                                    -> return $ Left err
            _ -> return result
        )
    & (foldl'
        (\(result, errs) eCurrent -> case eCurrent of
          Left  err     -> (result, err : errs)
          Right current -> (result `mappend` current, errs)
        )
        (mempty, []) <$>
      )

--------------------------------------------------------------------------------
-- Public API

resolveFilesInternal
  :: (MonadThrow m, MonadIO m)
  => FileFormat e
  -> Bool
  -> Int
  -> Spec.ConfigSpec
  -> m (Config, [SomeException])
resolveFilesInternal fileParser throwErrors priorityIndex spec = do
  result <- JSON.parseValueM parseConfigSpec (JSON.Object $ Spec.getConfigSpecJSON spec)
  case result of
    Left  err                  -> throwM (ResolverError err)
    Right (fileEnvVar, paths0) -> do
      let getPaths = case fileEnvVar of
            Nothing       -> return $ map ConfigFileOrigin paths0
            Just filePath -> do
              envFilePath <- liftIO $ lookupEnv (Text.unpack filePath)
              let envPath =
                    maybeToList (EnvFileOrigin filePath . Text.pack <$> envFilePath)
              return $ map ConfigFileOrigin paths0 ++ envPath
      paths <- getPaths
      readConfigFromFileSources fileParser throwErrors priorityIndex spec paths

jsonFormat :: FileFormat (JSON.ParseError FileResolverError)
jsonFormat = newFileFormat "json" (JSON.parseStrict JSON.asValue)

yamlFormat :: FileFormat Yaml.ParseException
yamlFormat = newFileFormat "yaml" Yaml.decodeEither'

getFileWarnings
  :: (MonadThrow m, MonadIO m) => FileFormat e -> Spec.ConfigSpec -> m [SomeException]
getFileWarnings fileParser spec = snd `fmap` resolveFilesInternal fileParser False 0 spec

resolveFiles
  :: (MonadThrow m, MonadIO m) => FileFormat e -> Int -> Spec.ConfigSpec -> m Config
resolveFiles fileParser priorityIndex spec =
  fst `fmap` resolveFilesInternal fileParser True priorityIndex spec

fileResolver :: (MonadThrow m, MonadIO m) => FileFormat e -> Resolver m
fileResolver fileParser = Resolver (resolveFiles fileParser)

yamlFileResolver :: (MonadThrow m, MonadIO m) => Resolver m
yamlFileResolver = fileResolver yamlFormat

jsonFileResolver :: (MonadThrow m, MonadIO m) => Resolver m
jsonFileResolver = fileResolver jsonFormat