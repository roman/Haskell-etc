{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Etc.Spec.JSON where

import           Protolude

import           Control.Monad.Catch        (MonadThrow (..))

import qualified Data.Aeson                 as JSON
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.Text                  as Text

import           Etc.Spec.Types

parseConfigSpec
  :: (MonadThrow m, JSON.FromJSON cmd)
    => LB8.ByteString
    -> m (ConfigSpec cmd)
parseConfigSpec input =
  case JSON.eitherDecode input of
    Left err ->
      throwM $ InvalidConfiguration (Text.pack err)

    Right result ->
      return result

readConfigSpec :: JSON.FromJSON cmd => Text -> IO (ConfigSpec cmd)
readConfigSpec filepath = do
  contents <- (LB8.readFile $ Text.unpack filepath)
  parseConfigSpec contents
