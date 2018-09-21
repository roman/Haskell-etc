{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- TODO: Make sure to remove usage of MonadThrow in favor of m (Either
-- FileResolverError result)
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

import Etc.Internal.FileFormat     as FileFormat
import Etc.Internal.Resolver.Types

import Etc.Internal.Config
    (Config (..), ConfigValue (..), SomeConfigSource (..))

import Etc.Internal.FileFormat          (FileFormat, jsonFormat, yamlFormat)
import Etc.Internal.Resolver.File.Error ()
import Etc.Internal.Resolver.File.Types

import qualified Etc.Internal.Spec.Parser as Spec (assertFieldTypeMatchesE)
import qualified Etc.Internal.Spec.Types  as Spec

--------------------------------------------------------------------------------
-- Spec Parser

parseConfigSpecFiles ::
     Monad m => JSON.ParseT FileResolverError m (Maybe Text, [Text])
parseConfigSpecFiles = do
  mFileEnv  <- JSON.keyMay "env" JSON.asText
  filepaths <- JSON.key "paths" (JSON.eachInArray JSON.asText)
  when (null filepaths) (JSON.throwCustomError ConfigSpecFilesPathsEntryIsEmpty)
  return (mFileEnv, filepaths)

parseConfigSpec ::
     Monad m => JSON.ParseT FileResolverError m (Maybe Text, [Text])
parseConfigSpec = do
  mFiles <- JSON.keyMay "etc/files" JSON.asObject
  case mFiles of
    Nothing -> do
      otherKeys <- JSON.forEachInObject return
      JSON.throwCustomError $ ConfigSpecFilesEntryMissing otherKeys
    Just _ ->
      JSON.key "etc/files" parseConfigSpecFiles

--------------------------------------------------------------------------------
-- Resolver

parseConfig
  :: (MonadThrow m)
  => FileFormat e
  -> Int
  -> Map Text Spec.CustomType
  -> Spec.ConfigValue
  -> Int
  -> FileValueOrigin
  -> ByteString
  -> m Config
parseConfig FileFormat { fileFormatParser } priorityIndex customTypes spec fileIndex fileOrigin bytes =
    case fileFormatParser bytes of
      -- TODO: Investigate ways to avoid discarding the err message from the parser
      Left _err -> throwM $ ConfigInvalidSyntaxFound fileOrigin
      Right jsonValue ->
        Config <$> parseConfigValue [] priorityIndex customTypes spec fileIndex fileOrigin jsonValue

toSomeConfigSource ::
     Int -> Int -> FileValueOrigin -> JSON.Value -> SomeConfigSource
toSomeConfigSource priorityIndex index origin val =
  SomeConfigSource priorityIndex $ FileSource index origin val

parseConfigValue
  :: (MonadThrow m)
  => [Text]
  -> Int
  -> Map Text Spec.CustomType
  -> Spec.ConfigValue
  -> Int
  -> FileValueOrigin
  -> JSON.Value
  -> m ConfigValue
parseConfigValue keyPath priorityIndex customTypes spec fileIndex fileOrigin jsonVal =
  case (spec, jsonVal) of
    (Spec.SubConfig currentSpec, JSON.Object object) -> SubConfig <$> foldM
      (\acc (key, subConfigValue) -> case Map.lookup key currentSpec of
        Nothing ->
          throwM $ UnknownConfigKeyFound fileOrigin keyPath key (Map.keys currentSpec)
        Just subConfigSpec -> do
          value1 <- parseConfigValue (key : keyPath)
                                     priorityIndex
                                     customTypes
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
          customTypes
          configValueType
          jsonVal
        return $ ConfigValue configValueSensitive
          ( Set.singleton $ toSomeConfigSource priorityIndex fileIndex fileOrigin jsonVal )

readConfigFile ::
     (MonadIO m)
  => FileFormat e
  -> Text
  -> m (Either FileResolverError ByteString)
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
  => Bool
  -> FileFormat e
  -> Int
  -> Map Text Spec.CustomType
  -> Spec.ConfigSpec
  -> [FileValueOrigin]
  -> m (Config, [SomeException])
readConfigFromFileSources throwErrors fileContentsParser priorityIndex customTypes spec fileSources =
  fileSources
    & zip [1 ..]
    & mapM
        (\(fileIndex, fileOrigin) -> do
          mContents <- readConfigFile fileContentsParser (getFileSourcePath fileOrigin)
          let result =
                either throwM return mContents
                >>= parseConfig fileContentsParser
                                priorityIndex
                                customTypes
                                (Spec.getConfigSpecEntries spec)
                                fileIndex
                                fileOrigin
          case result of
            Left baseErr | throwErrors ->
              -- NOTE: This is fugly, if we happen to add more "raisable" errors, improve
              -- this code with a helper that receives the exceptions (similar to catches)
              case (fromException baseErr) of
                Just err@UnknownConfigKeyFound{}       -> throwM (ResolverError err)
                Just err@ConfigFileValueTypeMismatch{} -> throwM (ResolverError err)
                Just err@SubConfigEntryExpected {}     -> throwM (ResolverError err)
                _                                      -> return $ Left baseErr
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
  -> Map Text Spec.CustomType
  -> Spec.ConfigSpec
  -> m (Config, [SomeException])
resolveFilesInternal fileContentsParser throwErrors priorityIndex customTypes spec = do
  result <- JSON.parseValueM parseConfigSpec (JSON.Object $ Spec.getConfigSpecJSON spec)
  case result of
    Left  err                  -> throwM (ResolverError err)
    Right (fileEnvVar, paths0) -> do
      let getPaths = case fileEnvVar of
            Nothing       -> return $ map SpecFileOrigin paths0
            Just filePath -> do
              envFilePath <- liftIO $ lookupEnv (Text.unpack filePath)
              let envPath =
                    maybeToList (EnvFileOrigin . EnvOrigin filePath . Text.pack <$> envFilePath)
              return $ map SpecFileOrigin paths0 ++ envPath
      paths <- reverse <$> getPaths
      readConfigFromFileSources throwErrors fileContentsParser priorityIndex customTypes spec paths

getConfigFileWarnings ::
     (MonadThrow m, MonadIO m)
  => FileFormat e
  -> Spec.ConfigSpec
  -> m [SomeException]
getConfigFileWarnings fileContentsParser spec =
  let
    throwErrors = False
    priorityIndex = 0
  in
    snd `fmap`
      resolveFilesInternal
        fileContentsParser
        throwErrors
        priorityIndex
        Map.empty
        spec

resolveFiles ::
     (MonadThrow m, MonadIO m)
  => FileFormat e
  -> Int
  -> Map Text Spec.CustomType
  -> Spec.ConfigSpec
  -> m Config
resolveFiles fileContentsParser priorityIndex customTypes spec =
  fst `fmap` resolveFilesInternal fileContentsParser True priorityIndex customTypes spec

fileResolver :: (MonadThrow m, MonadIO m) => FileFormat e -> Resolver m
fileResolver fileContentsParser = Resolver (resolveFiles fileContentsParser)

jsonConfig :: FileFormat (JSON.ParseError FileResolverError)
jsonConfig = jsonFormat

yamlConfig :: FileFormat Yaml.ParseException
yamlConfig = yamlFormat
