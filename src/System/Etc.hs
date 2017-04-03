{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Etc (
    module Types

  , resolveDefault
  , resolveFiles
  , resolveEnvPure
  , resolveEnv

#ifdef WITH_CLI
  , getErrorMessage
  , CliConfigError(..)
  , resolvePlainCliPure
  , resolveCommandCliPure
  , resolvePlainCli
  , resolveCommandCli
#endif

#ifdef WITH_PRINTER
  , renderConfig
  , printPrettyConfig
  , hPrintPrettyConfig
#endif

  , getConfigValue
  , getConfigValueWith
  , getSelectedConfigSource
  , getAllConfigSources
  ) where

import System.Etc.Internal.Resolver.Default (resolveDefault)
import System.Etc.Internal.Types            as Types
    (Config, ConfigSource (..), ConfigValue)
import System.Etc.Spec                      as Types
    (ConfigSpec, ConfigurationError (..), parseConfigSpec, readConfigSpec)

#ifdef WITH_CLI
import System.Etc.Internal.Resolver.Cli.Command (resolveCommandCli, resolveCommandCliPure)
import System.Etc.Internal.Resolver.Cli.Common  (CliConfigError (..), getErrorMessage)
import System.Etc.Internal.Resolver.Cli.Plain   (resolvePlainCli, resolvePlainCliPure)
#endif

#ifdef WITH_PRINTER
import System.Etc.Internal.Printer (hPrintPrettyConfig, printPrettyConfig, renderConfig)
#endif

import System.Etc.Internal.Config
    (getAllConfigSources, getConfigValue, getConfigValueWith, getSelectedConfigSource)
import System.Etc.Internal.Resolver.Env  (resolveEnv, resolveEnvPure)
import System.Etc.Internal.Resolver.File (resolveFiles)
