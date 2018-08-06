{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Etc.Resolver.Internal.File where

import           RIO
import qualified RIO.HashMap as HashMap
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.Text as Text

import Data.Text.Prettyprint.Doc ((<+>))


import qualified Data.Aeson              as JSON
import qualified Data.Aeson.BetterErrors as JSON

import qualified Data.Text.Prettyprint.Doc as Doc

import System.Environment (lookupEnv)
import System.Directory (doesFileExist)

import Etc.Resolver.Internal.Types

import Etc.Config (Value, IConfigSource(..), SomeConfigSource (..), Config (..), ConfigValue(..), markAsSensitive)
import qualified Etc.Spec as Spec

--------------------------------------------------------------------------------
-- Types

data FileResolverError
  -- | The 'etc/files' entry is not present in the config spec top-level
  = ConfigSpecFilesEntryMissing ![Text]
  -- | 'etc/files.paths' is an empty array, or is an invalid type
  | ConfigSpecFilesPathsEntryIsEmpty
  -- | The 'etc/files.paths' contains an entry that doesn't have a valid extension
  | UnsupportedFileExtensionGiven !Text
  -- | The 'etc/files.paths' points to a file that does not exist
  | ConfigurationFileNotPresent !Text
  -- | When configuration files contains keyPath that are not part of the spec
  | UnknownConfigKeyFound ![Text] !Text ![Text]
  -- | When configuration files contain a text with syntax that is not recognized
  | ConfigInvalidSyntaxFound !Text
  -- | When the spec is expecting a subconfig but a value is given
  | SubConfigEntryExpected ![Text] JSON.Value
  deriving (Show)

instance Exception FileResolverError

data FileValueOrigin
  = ConfigFileOrigin { fileSourcePath :: !Text }
  | EnvFileOrigin    { fileSourceEnvVar :: !Text,  fileSourcePath :: !Text }
  deriving (Generic, Show, Eq)

instance NFData FileValueOrigin

data FileSource = FileSource
  { fsConfigIndex :: !Int
  , fsValueOrigin :: !FileValueOrigin
  , fsValue       :: !(Value JSON.Value) }
  deriving (Generic, Typeable, Show, Eq)

instance NFData FileSource
instance IConfigSource FileSource where
  compareSources = comparing fsConfigIndex
  sourceValue = fsValue
  sourcePrettyDoc (FileSource _index origin _value) =
    case origin of
      ConfigFileOrigin filepath -> "File:" <+> Doc.pretty filepath
      EnvFileOrigin envVar filepath ->
        "File:" <+> Doc.pretty envVar <> "=" <> Doc.pretty filepath

fileSource :: Int -> Int -> FileValueOrigin -> Value JSON.Value -> SomeConfigSource
fileSource priorityIndex index origin val =
  SomeConfigSource priorityIndex $ FileSource index origin val

--------------------------------------------------------------------------------
-- Spec Parser

parseSpecFiles :: Monad m => JSON.ParseT FileResolverError m (Maybe Text, [Text])
parseSpecFiles = do
  mFileEnv  <- JSON.keyMay "env" JSON.asText
  filepaths <- JSON.key "paths" (JSON.eachInArray JSON.asText)
  when (null filepaths)
    (JSON.throwCustomError ConfigSpecFilesPathsEntryIsEmpty)
  return (mFileEnv, filepaths)

parseConfigSpec :: Monad m => JSON.ParseT FileResolverError m (Maybe Text, [Text])
parseConfigSpec = do
  mFiles <- JSON.keyMay "etc/files" JSON.asObject
  case mFiles of
    Nothing -> do
      otherKeys <- JSON.forEachInObject return
      JSON.throwCustomError $ ConfigSpecFilesEntryMissing otherKeys
    Just _ -> do
      JSON.key "etc/files" parseSpecFiles

--------------------------------------------------------------------------------
-- Resolver

parseConfig
  :: (MonadThrow m)
  => Int
  -> Spec.ConfigValue
  -> Int
  -> FileValueOrigin
  -> ByteString
  -> m Config
parseConfig priorityIndex spec fileIndex fileSource' bytes = do
  case JSON.parseStrict JSON.asValue bytes of
    Left _err ->
      throwM $ ConfigInvalidSyntaxFound (fileSourcePath fileSource')
    Right jsonValue ->
      Config <$> parseConfigValue [] priorityIndex spec fileIndex fileSource' jsonValue

parseConfigValue
  :: (MonadThrow m)
  => [Text]
  -> Int
  -> Spec.ConfigValue
  -> Int
  -> FileValueOrigin
  -> JSON.Value
  -> m ConfigValue
parseConfigValue keyPath priorityIndex spec fileIndex fileSource' json =
  case (spec, json) of
    (Spec.SubConfig currentSpec, JSON.Object object) -> SubConfig <$> foldM
      (\acc (key, subConfigValue) -> case Map.lookup key currentSpec of
        Nothing ->
          throwM $ UnknownConfigKeyFound keyPath key (Map.keys currentSpec)
        Just subConfigSpec -> do
          value1 <- parseConfigValue (key : keyPath)
                                     priorityIndex
                                     subConfigSpec
                                     fileIndex
                                     fileSource'
                                     subConfigValue
          return $ Map.insert key value1 acc
      )
      Map.empty
      (HashMap.toList object)

    (Spec.SubConfig{}, _) -> throwM $ SubConfigEntryExpected keyPath json

    (Spec.ConfigValue Spec.ConfigValueData { Spec.configValueSensitive, Spec.configValueType }, _)
      -> do
        either throwM return $ Spec.assertFieldTypeMatchesE keyPath configValueType json
        return $ ConfigValue
          (Set.singleton $ fileSource priorityIndex fileIndex fileSource' $ markAsSensitive
            configValueSensitive
            json
          )

readConfigFile :: (MonadIO m, MonadThrow n) => Text -> m (n ByteString)
readConfigFile filepath =
  let filepathStr = Text.unpack filepath
  in
    do
      fileExists <- liftIO $ doesFileExist filepathStr
      if fileExists
        then do
          contents <- readFileBinary filepathStr
          if ".json" `Text.isSuffixOf` filepath
          then
            return $ return contents
          else
            return (throwM $ UnsupportedFileExtensionGiven filepath)
        else
          return (throwM $ ConfigurationFileNotPresent filepath)

readConfigFromFileSources ::
     (MonadThrow m, MonadIO m)
  => Bool
  -> Int
  -> Spec.ConfigSpec
  -> [FileValueOrigin]
  -> m (Config, [SomeException])
readConfigFromFileSources throwErrors priorityIndex spec fileSources =
  fileSources
    & zip [1 ..]
    & mapM
        (\(fileIndex, fileSource') -> do
          mContents <- readConfigFile (fileSourcePath fileSource')
          let
            result =
              (   mContents
              >>= parseConfig priorityIndex
                              (Spec.getConfigSpecEntries spec)
                              fileIndex
                              fileSource'
              )
          case result of
            Left err
              | throwErrors ->
                -- NOTE: This is fugly, if we happen to add more "raisable" errors, improve
                -- this code with a helper that receives the exceptions (similar to catches)
                case fromException err of
                  Just (UnknownConfigKeyFound {}) -> throwM err
                  _ ->
                    case fromException err of
                      Just (Spec.ConfigValueTypeMismatchFound {}) -> throwM err
                      _ -> return $ Left err
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

resolveFilesInternal ::
     (MonadThrow m, MonadIO m)
  => Bool
  -> Int
  -> Spec.ConfigSpec
  -> m (Config, [SomeException])
resolveFilesInternal throwErrors priorityIndex spec = do
  result <-
    JSON.parseValueM parseConfigSpec (JSON.Object $ Spec.getConfigSpecJSON spec)
  case result of
    Left err -> throwM (Spec.SpecError err)
    Right (fileEnvVar, paths0) -> do
      let getPaths =
            case fileEnvVar of
              Nothing -> return $ map ConfigFileOrigin paths0
              Just filePath -> do
                envFilePath <- liftIO $ lookupEnv (Text.unpack filePath)
                let envPath =
                      maybeToList
                        (EnvFileOrigin filePath . Text.pack <$> envFilePath)
                return $ map ConfigFileOrigin paths0 ++ envPath
      paths <- getPaths
      readConfigFromFileSources throwErrors priorityIndex spec paths

getFileWarnings ::
     (MonadThrow m, MonadIO m) => Spec.ConfigSpec -> m [SomeException]
getFileWarnings spec = snd `fmap` resolveFilesInternal False 0 spec

resolveFiles :: (MonadThrow m, MonadIO m) => Int -> Spec.ConfigSpec -> m Config
resolveFiles priorityIndex spec =
  fst `fmap` resolveFilesInternal True priorityIndex spec

fileResolver :: (MonadThrow m, MonadIO m) => Resolver m
fileResolver = Resolver resolveFiles
