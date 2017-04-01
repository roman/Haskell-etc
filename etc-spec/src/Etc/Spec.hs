{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Etc.Spec (
    module Types
  , parseConfigSpec
  , readConfigSpec
  ) where

import Protolude

import Etc.Internal.Spec.Types as Types (ConfigSpec, ConfigValue, ConfigurationError (..))

import Control.Monad.Catch (MonadThrow (..))

import qualified Data.Aeson   as JSON
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text (readFile)

import qualified Etc.Internal.Spec.JSON as JSON
#ifdef WITH_YAML
import qualified Etc.Internal.Spec.YAML as YAML
#endif

parseConfigSpec
  :: (Alternative m, MonadThrow m, JSON.FromJSON cmd)
    => Text
    -> m (ConfigSpec cmd)
#ifdef WITH_YAML
parseConfigSpec input =
  JSON.parseConfigSpec input <|> YAML.parseConfigSpec input
#else
parseConfigSpec =
  JSON.parseConfigSpec
#endif

readConfigSpec :: JSON.FromJSON cmd => Text -> IO (ConfigSpec cmd)
readConfigSpec filepath = do
  contents <- Text.readFile $ Text.unpack filepath
  parseConfigSpec contents
