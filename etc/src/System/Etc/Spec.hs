{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NoImplicitPrelude   #-}

{-# LANGUAGE ScopedTypeVariables #-}
module System.Etc.Spec (
    module Types
  , InvalidConfigKeyPath (..)
  , ConfigValueParserFailed (..)
  , UnknownConfigKeyFound (..)
  , SubConfigEntryExpected (..)
  , ConfigValueTypeMismatchFound (..)
  , ConfigurationFileNotFound (..)
  , UnsupportedFileExtensionGiven (..)
  , ConfigInvalidSyntaxFound (..)
  , SpecInvalidSyntaxFound (..)
  , parseConfigSpec
  , readConfigSpecTH
  , readConfigSpec
  ) where

import qualified Data.Text.IO as Text (readFile)
import           RIO
import qualified RIO.Text     as Text

import Data.Proxy (Proxy)

import Language.Haskell.TH        (ExpQ)
import Language.Haskell.TH.Syntax (Lift)

import System.Etc.Internal.Spec.Types as Types
    ( ConfigInvalidSyntaxFound (..)
    , ConfigSpec
    , ConfigValue
    , ConfigValueParserFailed (..)
    , ConfigValueTypeMismatchFound (..)
    , ConfigurationFileNotFound (..)
    , InvalidConfigKeyPath (..)
    , SpecInvalidSyntaxFound (..)
    , SubConfigEntryExpected (..)
    , UnknownConfigKeyFound (..)
    , UnsupportedFileExtensionGiven (..)
    )

#ifdef WITH_CLI
import qualified Data.Aeson as JSON
#endif

#ifdef WITH_YAML
import qualified System.Etc.Internal.Spec.YAML    as YAML
import qualified System.Etc.Internal.Spec.YAML.TH as YAML
#else
import qualified System.Etc.Internal.Spec.JSON    as JSONSpec
import qualified System.Etc.Internal.Spec.JSON.TH as JSONSpec
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


-- | Reads a specified 'FilePath' and parses a 'ConfigSpec' at compilation time.
readConfigSpecTH :: (Lift k, JSON.FromJSON k) => Proxy k -> Text -> ExpQ
#ifdef WITH_YAML
readConfigSpecTH = YAML.readConfigSpecTH
#else
readConfigSpecTH = JSONSpec.readConfigSpecTH
#endif
