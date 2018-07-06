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

parseConfigSpec :: (MonadThrow m, JSON.FromJSON cmd) => Text -> m (ConfigSpec cmd)
parseConfigSpec input = case decodeYaml (Text.encodeUtf8 input) of
  Left  err    -> throwM $ InvalidConfiguration Nothing (Text.pack err)

  Right result -> return result

readConfigSpec :: JSON.FromJSON cmd => Text -> IO (ConfigSpec cmd)
readConfigSpec filepath = do
  contents <- Text.readFile $ Text.unpack filepath
  parseConfigSpec contents
