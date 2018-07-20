{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
module System.Etc (
  -- * Config
  -- $config
    Config
  , IConfig
  , Value(..)
  , getConfigValue
  , getConfigValueWith
  , getSelectedConfigSource
  , getAllConfigSources

  -- * ConfigSpec
  -- $config_spec
  , SomeConfigSource (..)
  , ConfigValue
  , ConfigSpec
  , parseConfigSpec
  , readConfigSpec
  , readConfigSpecTH

  -- * Exceptions
  , InvalidConfigKeyPath (..)
  , ConfigValueParserFailed (..)
  , UnknownConfigKeyFound (..)
  , SubConfigEntryExpected (..)
  , ConfigValueTypeMismatchFound (..)
  , ConfigurationFileNotFound (..)
  , UnsupportedFileExtensionGiven (..)
  , ConfigInvalidSyntaxFound (..)
  , SpecInvalidSyntaxFound (..)



  -- ** Resolvers
  -- $resolvers
  , resolveDefault
  , resolveFiles
  , resolveEnvPure
  , resolveEnv

#ifdef WITH_CLI

  , resolvePlainCliPure
  , resolveCommandCliPure
  , resolvePlainCli
  , resolveCommandCli

  -- ** CLI Resolver Error type
  , getErrorMessage
  , CliConfigError(..)
#endif

#ifdef WITH_EXTRA
  -- * Extra utilities
  -- $extra
  , renderConfig
  , renderConfigColor
  , printPrettyConfig
  , hPrintPrettyConfig

  , EnvMisspell(..)
  , getEnvMisspellings
  , getEnvMisspellingsPure
  , renderEnvMisspellings
  , hPrintEnvMisspellings
  , reportEnvMisspellingWarnings
#endif
  ) where

import System.Etc.Internal.Resolver.Default (resolveDefault)
import System.Etc.Internal.Types
    (Config, ConfigValue, IConfig (..), SomeConfigSource (..), Value (..))
import System.Etc.Spec
    ( ConfigInvalidSyntaxFound (..)
    , ConfigSpec
    , ConfigValueParserFailed (..)
    , ConfigValueTypeMismatchFound (..)
    , ConfigurationFileNotFound (..)
    , InvalidConfigKeyPath (..)
    , SpecInvalidSyntaxFound (..)
    , SubConfigEntryExpected (..)
    , UnknownConfigKeyFound (..)
    , UnsupportedFileExtensionGiven (..)
    , parseConfigSpec
    , readConfigSpec
    , readConfigSpecTH
    )

#ifdef WITH_CLI
import System.Etc.Internal.Resolver.Cli.Command (resolveCommandCli, resolveCommandCliPure)
import System.Etc.Internal.Resolver.Cli.Common  (CliConfigError (..), getErrorMessage)
import System.Etc.Internal.Resolver.Cli.Plain   (resolvePlainCli, resolvePlainCliPure)
#endif

#ifdef WITH_EXTRA
import System.Etc.Internal.Extra.EnvMisspell
    ( EnvMisspell (..)
    , getEnvMisspellings
    , getEnvMisspellingsPure
    , hPrintEnvMisspellings
    , renderEnvMisspellings
    , reportEnvMisspellingWarnings
    )
import System.Etc.Internal.Extra.Printer
    (hPrintPrettyConfig, printPrettyConfig, renderConfig, renderConfigColor)
#endif

import System.Etc.Internal.Config ()
import System.Etc.Internal.Resolver.Env  (resolveEnv, resolveEnvPure)
import System.Etc.Internal.Resolver.File (resolveFiles)

{- $config

   Use this functions to fetch values from the Etc.Config and cast them to types
   that make sense in your program
-}

{- $config_spec

   Use this functions to read the configuration spec. Remember you can
   use JSON or YAML(*) filepaths

   * The yaml cabal flag must be used to support yaml syntax
-}

{- $resolvers

   Use this functions to gather configuration values from different sources
   (environment variables, command lines or files). Then compose results
   together using the mappend function
-}

{- $extra

   Some extra utilities that are great for debugging (miss)-configurations.
-}
