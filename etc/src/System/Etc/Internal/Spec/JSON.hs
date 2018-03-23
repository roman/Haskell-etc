{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Etc.Internal.Spec.JSON where

import RIO
import qualified RIO.Text as Text
import qualified RIO.ByteString.Lazy as LBS
import qualified Data.Text.IO as Text (readFile)

import qualified Data.Aeson              as JSON

import System.Etc.Internal.Spec.Types

parseConfigSpec
  :: (MonadThrow m, JSON.FromJSON cmd)
    => Text
    -> m (ConfigSpec cmd)
parseConfigSpec input =
  case JSON.eitherDecode (LBS.fromStrict $ encodeUtf8 input) of
    Left err ->
      throwM $ InvalidConfiguration (Text.pack err)

    Right result ->
      return result

readConfigSpec :: JSON.FromJSON cmd => Text -> IO (ConfigSpec cmd)
readConfigSpec filepath = do
  contents <- Text.readFile (Text.unpack filepath)
  parseConfigSpec contents
