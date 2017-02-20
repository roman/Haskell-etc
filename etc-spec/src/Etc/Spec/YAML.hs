{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Etc.Spec.YAML where

import           Protolude

import           Control.Monad.Catch   (MonadThrow (..))

import qualified Data.Aeson            as JSON
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text             as Text
import qualified Data.Yaml             as YAML

import           Etc.Spec.Types

parseConfigSpec
  :: (MonadThrow m, JSON.FromJSON cmd)
    => B8.ByteString
    -> m (ConfigSpec cmd)
parseConfigSpec input =
  case YAML.decodeEither input of
    Left err ->
      throwM $ InvalidConfiguration (Text.pack err)

    Right result ->
      return result

readConfigSpec :: JSON.FromJSON cmd => Text -> IO (ConfigSpec cmd)
readConfigSpec filepath = do
  contents <- (B8.readFile $ Text.unpack filepath)
  parseConfigSpec contents
