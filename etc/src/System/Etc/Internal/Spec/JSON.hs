{-# LANGUAGE NoImplicitPrelude #-}

module System.Etc.Internal.Spec.JSON where

import qualified Data.Text.IO        as Text (readFile)
import           RIO
import qualified RIO.ByteString.Lazy as LBS
import qualified RIO.Text            as Text

import qualified Data.Aeson as JSON

import System.Etc.Internal.Errors
import System.Etc.Internal.Spec.Parser ()
import System.Etc.Internal.Spec.Types

parseConfigSpec_
  :: (MonadThrow m, JSON.FromJSON cmd) => Maybe Text -> Text -> m (ConfigSpec cmd)
parseConfigSpec_ mFilePath input =
  case JSON.eitherDecode (LBS.fromStrict $ encodeUtf8 input) of
    Left  err    -> throwM $ SpecInvalidSyntaxFound mFilePath (Text.pack err)
    Right result -> return result

parseConfigSpec :: (MonadThrow m, JSON.FromJSON cmd) => Text -> m (ConfigSpec cmd)
parseConfigSpec = parseConfigSpec_ Nothing

readConfigSpec :: JSON.FromJSON cmd => Text -> IO (ConfigSpec cmd)
readConfigSpec filepath = do
  contents <- Text.readFile (Text.unpack filepath)
  parseConfigSpec_ (Just filepath) contents
