{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Etc (
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

import Etc.Internal.Resolver.Default (resolveDefault)
import Etc.Internal.Types            as Types (Config, ConfigSource (..), ConfigValue)
import Etc.Spec                      as Types
    (ConfigSpec, ConfigurationError (..), parseConfigSpec, readConfigSpec)

#ifdef WITH_CLI
import Etc.Internal.Resolver.Cli.Command (resolveCommandCli, resolveCommandCliPure)
import Etc.Internal.Resolver.Cli.Common  (CliConfigError (..), getErrorMessage)
import Etc.Internal.Resolver.Cli.Plain   (resolvePlainCli, resolvePlainCliPure)
#endif

#ifdef WITH_PRINTER
import Etc.Internal.Printer (hPrintPrettyConfig, printPrettyConfig, renderConfig)
#endif

import Etc.Internal.Config
    (getAllConfigSources, getConfigValue, getConfigValueWith, getSelectedConfigSource)
import Etc.Internal.Resolver.Env  (resolveEnv, resolveEnvPure)
import Etc.Internal.Resolver.File (resolveFiles)
