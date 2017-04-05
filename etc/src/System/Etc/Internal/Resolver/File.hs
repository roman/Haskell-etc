{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Etc.Internal.Resolver.File (resolveFiles) where

import Protolude

import Control.Monad.Catch (MonadThrow (..))
import Data.Vector         (Vector)
import System.Directory    (doesFileExist)


#ifdef WITH_YAML
import qualified Data.Yaml as YAML
#endif

import qualified Data.Aeson                 as JSON
import qualified Data.Aeson.Internal        as JSON (IResult (..), iparse)
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text
import qualified Data.Vector                as Vector

import qualified System.Etc.Internal.Spec.Types as Spec
import           System.Etc.Internal.Types      hiding (filepath)

--------------------------------------------------------------------------------

data ConfigFile
  = JsonFile Text LB8.ByteString
  | YamlFile Text LB8.ByteString
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
    JsonFile _ contents ->
      JSON.eitherDecode contents
    YamlFile _ contents ->
      YAML.decodeEither (LB8.toStrict contents)
#else
eitherDecode contents0 =
  case contents0 of
    JsonFile _ contents ->
      JSON.eitherDecode contents
    YamlFile filepath _ ->
      Left ("Unsupported yaml file: " <> Text.unpack filepath)
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

readConfigFile :: MonadThrow m => Text -> IO (m ConfigFile)
readConfigFile filepath =
  let
    filepathStr = Text.unpack filepath
  in do
    fileExists <- doesFileExist filepathStr
    if fileExists then do
      contents <- LB8.readFile filepathStr
      if ".json" `Text.isSuffixOf` filepath then
        return $ return (JsonFile filepath contents)
      else if (".yaml" `Text.isSuffixOf` filepath) ||
              (".yml" `Text.isSuffixOf` filepath) then
        return $ return (YamlFile filepath contents) else
        return (throwM $ InvalidConfiguration "Unsupported file extension")
    else
      return $ throwM $ ConfigurationFileNotFound filepath

readConfigFromFiles :: [Text] -> IO (Config, [SomeException])
readConfigFromFiles files =
  files
  & zip [1..]
  & mapM (\(fileIndex, filepath) -> do
                 mContents <- readConfigFile filepath
                 return (mContents >>= parseConfig fileIndex filepath))
  & (foldl' (\(result, errs) eCurrent ->
               case eCurrent of
                 Left err ->
                   (result, err:errs)
                 Right current ->
                   (result <> current, errs))
             (mempty, [])
             <$>)

resolveFiles :: Spec.ConfigSpec cmd -> IO (Config, Vector SomeException)
resolveFiles spec = do
  (config, exceptions) <- readConfigFromFiles (Spec.specConfigFilepaths spec)
  return (config, Vector.fromList exceptions)
