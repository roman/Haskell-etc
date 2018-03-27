{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Etc.Internal.Resolver.File (resolveFiles) where

import           RIO
import           RIO.Directory (doesFileExist)
import qualified RIO.HashMap   as HashMap
import qualified RIO.Set       as Set
import qualified RIO.Text      as Text
import qualified RIO.Vector    as Vector

#ifdef WITH_YAML
import qualified Data.Yaml as YAML
#endif

import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Internal as JSON (IResult (..), iparse)
import qualified RIO.ByteString.Lazy as LB8

import qualified System.Etc.Internal.Spec.Types as Spec
import           System.Etc.Internal.Types      hiding (filepath)

--------------------------------------------------------------------------------

data ConfigFile
  = JsonFile Text LB8.ByteString
  | YamlFile Text LB8.ByteString
  deriving (Show, Eq)

--------------------------------------------------------------------------------

parseConfigValue
  :: Monad m => Maybe (Spec.ConfigValue cmd) -> Int -> Text -> JSON.Value -> m ConfigValue
parseConfigValue mSpec fileIndex filepath json = case json of
  JSON.Object object -> SubConfig <$> foldM
    (\acc (key, subConfigValue) -> do
      let msubConfigSpec = do
            spec <- mSpec
            case spec of
              Spec.SubConfig hsh -> HashMap.lookup key hsh
              _ ->
                -- TODO: This should be an error given the config doesn't match spec
                fail "configuration spec and configuration value are different"

      value1 <- parseConfigValue msubConfigSpec fileIndex filepath subConfigValue
      return $ HashMap.insert key value1 acc
    )
    HashMap.empty
    (HashMap.toList object)

  _ ->
    let mToValue = do
          spec <- mSpec
          case spec of
            Spec.ConfigValue{} -> return $ boolToValue (Spec.isSensitive spec)
            _ -> fail "configuration spec and configuration value are different"

        toValue = fromMaybe Plain mToValue
    in  return $ ConfigValue (Set.singleton $ File fileIndex filepath (toValue json))


eitherDecode :: ConfigFile -> Either String JSON.Value
#ifdef WITH_YAML
eitherDecode contents0 =
  case contents0 of
    JsonFile _ contents ->
      JSON.eitherDecode contents
    YamlFile _ contents ->
      YAML.decodeEither (LB8.toStrict contents)
#else
eitherDecode contents0 = case contents0 of
  JsonFile _        contents -> JSON.eitherDecode contents
  YamlFile filepath _        -> Left ("Unsupported yaml file: " <> Text.unpack filepath)
#endif


parseConfig :: MonadThrow m => Spec.ConfigValue cmd -> Int -> Text -> ConfigFile -> m Config
parseConfig spec fileIndex filepath contents = case eitherDecode contents of
  Left  err  -> throwM $ InvalidConfiguration Nothing (Text.pack err)

  Right json -> case JSON.iparse (parseConfigValue (Just spec) fileIndex filepath) json of
    JSON.IError _ err    -> throwM $ InvalidConfiguration Nothing (Text.pack err)

    JSON.ISuccess result -> return (Config result)

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
              else return
                (throwM $ InvalidConfiguration Nothing "Unsupported file extension")
        else return $ throwM $ ConfigurationFileNotFound filepath

readConfigFromFiles :: Spec.ConfigSpec cmd -> IO (Config, [SomeException])
readConfigFromFiles spec =
  Spec.specConfigFilepaths spec
    & zip [1 ..]
    & mapM
        (\(fileIndex, filepath) -> do
          mContents <- readConfigFile filepath
          return
            (   mContents
            >>= parseConfig (Spec.SubConfig $ Spec.specConfigValues spec)
                            fileIndex
                            filepath
            )
        )
    & (foldl'
        (\(result, errs) eCurrent -> case eCurrent of
          Left  err     -> (result, err : errs)
          Right current -> (result `mappend` current, errs)
        )
        (mempty, []) <$>
      )

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
  (config, exceptions) <- readConfigFromFiles spec
  return (config, Vector.fromList exceptions)
