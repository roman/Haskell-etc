{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}

module System.Etc.Internal.Spec.YAML where

import           RIO
import qualified RIO.Text as Text

import qualified Data.Aeson         as JSON
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Data.Text.IO       as Text (readFile)
import qualified Data.Yaml          as YAML

import System.Etc.Internal.Spec.Types

decodeYaml :: JSON.FromJSON a => ByteString -> Either String a
decodeYaml =
#if MIN_VERSION_yaml(0,8,3)
  mapLeft show . YAML.decodeEither'
#else
  YAML.decodeEither
#endif

parseConfigSpec_ :: (MonadThrow m, JSON.FromJSON cmd) => Maybe Text -> Text -> m (ConfigSpec cmd)
parseConfigSpec_ mFilepath input = case decodeYaml (Text.encodeUtf8 input) of
  Left  err    -> throwM $ SpecInvalidSyntaxFound mFilepath (Text.pack err)
  Right result -> return result

parseConfigSpec :: (MonadThrow m, JSON.FromJSON cmd) => Text -> m (ConfigSpec cmd)
parseConfigSpec = parseConfigSpec_ Nothing

readConfigSpec :: JSON.FromJSON cmd => Text -> IO (ConfigSpec cmd)
readConfigSpec filepath = do
  contents <- Text.readFile $ Text.unpack filepath
  parseConfigSpec_ (Just filepath) contents
