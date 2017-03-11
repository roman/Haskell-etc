{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Etc.Resolver.File (resolveFiles) where

import Protolude

import Control.Monad.Catch (MonadThrow(..))
import Data.Maybe (catMaybes)
import System.Directory (doesFileExist)

#ifdef WITH_YAML
import qualified Data.Yaml as YAML
#endif

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Internal as JSON (iparse, IResult(..))
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Etc.Spec.Types as Spec
import Etc.Types hiding (filepath)

--------------------------------------------------------------------------------

data ConfigFile
  = JsonFile LB8.ByteString
  | YamlFile LB8.ByteString
  deriving (Show, Eq)

--------------------------------------------------------------------------------

parseConfigValue
  :: Monad m
  => Int
  -> Text
  -> JSON.Value
  -> m ConfigValue
parseConfigValue fileIndex filepath json =
  case json of
    JSON.Object object ->
      SubConfig
        <$> foldM
              (\acc (key, subconfigValue) -> do
                  value1 <- parseConfigValue fileIndex filepath subconfigValue
                  return $ HashMap.insert key value1 acc)
              HashMap.empty
              (HashMap.toList object)

    _ ->
      return $
        ConfigValue (Set.singleton $ File fileIndex filepath json)


eitherDecode :: ConfigFile -> Either [Char] JSON.Value
#ifdef WITH_YAML
eitherDecode contents0 =
  case contents0 of
    JsonFile contents ->
      JSON.eitherDecode contents
    YamlFile contents ->
      YAML.eitherDecode contents
#else
eitherDecode contents0 =
  case contents0 of
    JsonFile contents ->
      JSON.eitherDecode contents
    YamlFile _ ->
      Left "Unsupported yaml file"
#endif


parseConfig
  :: MonadThrow m
  => Int
  -> Text
  -> ConfigFile
  -> m Config
parseConfig fileIndex filepath contents =
  case eitherDecode contents of
    Left err ->
      throwM $ InvalidConfiguration (Text.pack err)

    Right json ->
      case JSON.iparse (parseConfigValue fileIndex filepath) json of
        JSON.IError _ err ->
          throwM $ InvalidConfiguration (Text.pack err)

        JSON.ISuccess result ->
          return (Config result)

readConfigFile :: Text -> IO (Maybe ConfigFile)
readConfigFile filepath =
  let
    filepathStr = Text.unpack filepath
  in do
    fileExists <- doesFileExist filepathStr
    putStrLn $ show filepath <> (" exists? " :: Text.Text) <> show fileExists
    if fileExists then do
      contents <- LB8.readFile filepathStr
      if ".json" `Text.isSuffixOf` filepath then
        return $ Just (JsonFile contents)
      else if ".yaml" `Text.isSuffixOf` filepath then
        return $ Just (YamlFile contents)
      else if ".yml" `Text.isSuffixOf` filepath then
        return $ Just (YamlFile contents)
      else
        return Nothing
    else
      return Nothing

readConfigFromFiles :: [Text] -> IO Config
readConfigFromFiles files =
  files
  & zip [1..]
  & mapM (\(fileIndex, filepath) -> do
                 mContents <- readConfigFile filepath
                 return (mContents >>= parseConfig fileIndex filepath))
  & (catMaybes <$>)
  & (mconcat <$>)

resolveFiles :: Spec.ConfigSpec cmd -> IO Config
resolveFiles =
  readConfigFromFiles . Spec.specConfigFilepaths
