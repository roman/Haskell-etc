{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Etc.Internal.Resolver.File (resolveFiles) where

import           RIO
import           RIO.Directory (doesFileExist)
import qualified RIO.HashMap   as HashMap
import qualified RIO.Set       as Set
import qualified RIO.Text      as Text
import qualified RIO.Vector    as Vector

#ifdef WITH_YAML
import System.Etc.Internal.Spec.YAML (decodeYaml)
#endif

import qualified Data.Aeson as JSON
-- import qualified Data.Aeson.Internal as JSON (IResult (..), iparse)
import qualified RIO.ByteString.Lazy as LB8

import System.Environment (lookupEnv)

import           System.Etc.Internal.Errors
import qualified System.Etc.Internal.Spec.Parser as Spec
import qualified System.Etc.Internal.Spec.Types  as Spec
import           System.Etc.Internal.Types

--------------------------------------------------------------------------------

data ConfigFile
  = JsonFile Text LB8.ByteString
  | YamlFile Text LB8.ByteString
  deriving (Show, Eq)

--------------------------------------------------------------------------------

parseConfigValue
  :: (MonadThrow m)
  => [Text]
  -> Spec.ConfigValue cmd
  -> Int
  -> FileValueOrigin
  -> JSON.Value
  -> m ConfigValue
parseConfigValue keys spec fileIndex fileSource' json =
  let
    parentKeys = reverse keys
    currentKey = Text.intercalate "." parentKeys
  in
    case (spec, json) of
      (Spec.SubConfig currentSpec, JSON.Object object) -> SubConfig <$> foldM
        (\acc (key, subConfigValue) -> case HashMap.lookup key currentSpec of
          Nothing ->
            throwM $ UnknownConfigKeyFound parentKeys key (HashMap.keys currentSpec)
          Just subConfigSpec -> do
            value1 <- parseConfigValue (key : keys)
                                       subConfigSpec
                                       fileIndex
                                       fileSource'
                                       subConfigValue
            return $ HashMap.insert key value1 acc
        )
        HashMap.empty
        (HashMap.toList object)

      (Spec.SubConfig{}, _) -> throwM $ SubConfigEntryExpected currentKey json

      (Spec.ConfigValue { Spec.isSensitive, Spec.configValueType }, _) -> do
        either throwM return $ Spec.assertMatchingConfigValueType json configValueType
        return $ ConfigValue
          (Set.singleton $ fileSource 1 fileIndex fileSource' $ markAsSensitive
            isSensitive
            json
          )



eitherDecode :: ConfigFile -> Either String JSON.Value
#ifdef WITH_YAML
eitherDecode contents0 =
  case contents0 of
    JsonFile _ contents ->
      JSON.eitherDecode contents
    YamlFile _ contents ->
      decodeYaml (LB8.toStrict contents)
#else
eitherDecode contents0 = case contents0 of
  JsonFile _        contents -> JSON.eitherDecode contents
  YamlFile filepath _        -> Left ("Unsupported yaml file: " <> Text.unpack filepath)
#endif


parseConfig
  :: MonadThrow m
  => Spec.ConfigValue cmd
  -> Int
  -> FileValueOrigin
  -> ConfigFile
  -> m Config
parseConfig spec fileIndex fileSource' contents = case eitherDecode contents of
  Left err ->
    throwM $ ConfigInvalidSyntaxFound (fileSourcePath fileSource') (Text.pack err)
  -- Right json ->
  --   case JSON.iparse (parseConfigValue [] spec fileIndex fileSource) json of
  --     JSON.IError _ err    ->
  --       case readMaybe err of
  --         Just (e :: ConfigError) ->
  --           throwM e
  --         _ ->
  --           throwM $ InvalidConfiguration Nothing (Text.pack err)
  --     JSON.ISuccess result -> return (Config result)
  Right json -> Config <$> parseConfigValue [] spec fileIndex fileSource' json

readConfigFile :: MonadThrow m => Text -> IO (m ConfigFile)
readConfigFile filepath =
  let filepathStr = Text.unpack filepath
  in
    do
      fileExists <- doesFileExist filepathStr
      if fileExists
        then do
          contents <- LB8.readFile filepathStr
          if ".json" `Text.isSuffixOf` filepath
          then
            return $ return (JsonFile filepath contents)
          else
            if (".yaml" `Text.isSuffixOf` filepath) || (".yml" `Text.isSuffixOf` filepath)
              then return $ return (YamlFile filepath contents)
              else return (throwM $ UnsupportedFileExtensionGiven filepath)
        else return $ throwM $ ConfigurationFileNotFound filepath

readConfigFromFileSources
  :: Spec.ConfigSpec cmd -> [FileValueOrigin] -> IO (Config, [SomeException])
readConfigFromFileSources spec fileSources =
  fileSources
    & zip [1 ..]
    & mapM
        (\(fileIndex, fileSource') -> do
          mContents <- readConfigFile (fileSourcePath fileSource')
          return
            (   mContents
            >>= parseConfig (Spec.SubConfig $ Spec.specConfigValues spec)
                            fileIndex
                            fileSource'
            )
        )
    & (foldl'
        (\(result, errs) eCurrent -> case eCurrent of
          Left  err     -> (result, err : errs)
          Right current -> (result `mappend` current, errs)
        )
        (mempty, []) <$>
      )

processFilesSpec :: Spec.ConfigSpec cmd -> IO (Config, [SomeException])
processFilesSpec spec = case Spec.specConfigFilepaths spec of
  Nothing -> readConfigFromFileSources spec []
  Just (Spec.FilePathsSpec paths) ->
    readConfigFromFileSources spec (map ConfigFileOrigin paths)
  Just (Spec.FilesSpec fileEnvVar paths0) -> do
    let getPaths = case fileEnvVar of
          Nothing       -> return $ map ConfigFileOrigin paths0
          Just filePath -> do
            envFilePath <- lookupEnv (Text.unpack filePath)
            let envPath = maybeToList (EnvFileOrigin filePath . Text.pack <$> envFilePath)
            return $ map ConfigFileOrigin paths0 ++ envPath

    paths <- getPaths
    readConfigFromFileSources spec paths

{-|

Gathers configuration values from a list of files specified on the
@etc/filepaths@ entry of a Config Spec. This will return a Configuration Map
with values from all filepaths merged in, and a list of errors in case there was
an error reading one of the filepaths.

-}
resolveFiles
  :: Spec.ConfigSpec cmd -- ^ Config Spec
  -> IO (Config, Vector SomeException) -- ^ Configuration Map with all values from files filled in and a list of warnings
resolveFiles spec = do
  (config, exceptions) <- processFilesSpec spec
  return (config, Vector.fromList exceptions)


-- validateConfigFiles :: Spec.ConfigSpec cmd -> Config -> IO ()
-- validateConfigFiles Spec.ConfigSpec {Spec.specConfigFilepaths, Spec.specConfigValues} =
--   undefined
