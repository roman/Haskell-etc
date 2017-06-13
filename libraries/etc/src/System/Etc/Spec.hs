{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Etc.Spec (
    module Types
  , parseConfigSpec
  , readConfigSpec
  ) where

import Protolude hiding (catch)

import System.Etc.Internal.Spec.Types as Types
    (ConfigSpec, ConfigValue, ConfigurationError (..))

import Control.Monad.Catch (MonadCatch (..))

#ifdef WITH_CLI
import qualified Data.Aeson as JSON
#endif
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text (readFile)

import qualified System.Etc.Internal.Spec.JSON as JSON
#ifdef WITH_YAML
import qualified System.Etc.Internal.Spec.YAML as YAML
#endif

{-|

Parses a text input into a @ConfigSpec@, input can be JSON or YAML (if cabal
flag is set).

-}
#ifdef WITH_CLI
parseConfigSpec
  :: (MonadCatch m, JSON.FromJSON cmd)
    => Text               -- ^ Text to be parsed
    -> m (ConfigSpec cmd) -- ^ returns ConfigSpec
#else
parseConfigSpec
  :: (MonadCatch m)
    => Text               -- ^ Text to be parsed
    -> m (ConfigSpec ()) -- ^ returns ConfigSpec
#endif

#ifdef WITH_YAML
parseConfigSpec input =
   catch (JSON.parseConfigSpec input)
         (\(_ :: SomeException) ->  YAML.parseConfigSpec input)
#else
parseConfigSpec =
  JSON.parseConfigSpec
#endif

{-|

Reads contents of a file and parses into a @ConfigSpec@, file contents can be
either JSON or YAML (if cabal flag is set).

-}
#ifdef WITH_CLI
readConfigSpec
  :: JSON.FromJSON cmd
  => Text -- ^ Filepath where contents are going to be read from and parsed
  -> IO (ConfigSpec cmd) -- ^ returns ConfigSpec
#else
readConfigSpec
  :: Text -- ^ Filepath where contents are going to be read from and parsed
  -> IO (ConfigSpec ()) -- ^ returns ConfigSpec
#endif
readConfigSpec filepath = do
  contents <- Text.readFile $ Text.unpack filepath
  parseConfigSpec contents
