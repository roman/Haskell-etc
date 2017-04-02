{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Etc.Internal.Spec.YAML where

import Protolude

import Control.Monad.Catch (MonadThrow (..))

import qualified Data.Aeson         as JSON
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Data.Text.IO       as Text (readFile)
import qualified Data.Yaml          as YAML

import Etc.Internal.Spec.Types

parseConfigSpec
  :: (MonadThrow m, JSON.FromJSON cmd)
    => Text
    -> m (ConfigSpec cmd)
parseConfigSpec input =
  case YAML.decodeEither (Text.encodeUtf8 input) of
    Left err ->
      throwM $ InvalidConfiguration (Text.pack err)

    Right result ->
      return result

readConfigSpec :: JSON.FromJSON cmd => Text -> IO (ConfigSpec cmd)
readConfigSpec filepath = do
  contents <- Text.readFile $ Text.unpack filepath
  parseConfigSpec contents
