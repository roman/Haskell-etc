{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NoImplicitPrelude   #-}

{-# LANGUAGE ScopedTypeVariables #-}
module System.Etc.Spec (
    module Types
  , parseConfigSpec
  , readConfigSpec
  ) where

import qualified Data.Text.IO as Text (readFile)
import           RIO
import qualified RIO.Text     as Text

import System.Etc.Internal.Spec.Types as Types
    (ConfigSpec, ConfigValue, ConfigurationError (..))

#ifdef WITH_CLI
import qualified Data.Aeson as JSON
#endif

#ifdef WITH_YAML
import qualified System.Etc.Internal.Spec.YAML as YAML
#else
import qualified System.Etc.Internal.Spec.JSON as JSONSpec
#endif

{-|

Parses a text input into a @ConfigSpec@, input can be JSON or YAML (if cabal
flag is set).

-}
#ifdef WITH_CLI
parseConfigSpec
  :: (MonadThrow m, JSON.FromJSON cmd)
    => Text               -- ^ Text to be parsed
    -> m (ConfigSpec cmd) -- ^ returns ConfigSpec
#else
parseConfigSpec
  :: (MonadThrow m)
  => Text               -- ^ Text to be parsed
  -> m (ConfigSpec ()) -- ^ returns ConfigSpec
#endif

#ifdef WITH_YAML
parseConfigSpec = YAML.parseConfigSpec
#else
parseConfigSpec = JSONSpec.parseConfigSpec
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
