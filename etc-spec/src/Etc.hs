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

import Etc.Spec as Types (
    ConfigurationError(..)
  , ConfigSpec
  , parseConfigSpec
  , readConfigSpec
  )
import Etc.Internal.Types as Types (
    ConfigSource(..)
  , ConfigValue
  , Config
  )
import Etc.Internal.Resolver.Default (resolveDefault)

#ifdef WITH_CLI
import Etc.Internal.Resolver.Cli.Common (getErrorMessage, CliConfigError(..))
import Etc.Internal.Resolver.Cli.Plain (resolvePlainCliPure, resolvePlainCli)
import Etc.Internal.Resolver.Cli.Command (resolveCommandCliPure, resolveCommandCli)
#endif

#ifdef WITH_PRINTER
import Etc.Internal.Printer (
    renderConfig
  , printPrettyConfig
  , hPrintPrettyConfig
  )
#endif

import Etc.Internal.Resolver.Env (resolveEnvPure, resolveEnv)
import Etc.Internal.Resolver.File (resolveFiles)
import Etc.Internal.Config (getConfigValueWith, getSelectedConfigSource, getAllConfigSources, getConfigValue)
