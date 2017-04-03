{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Etc.Internal.Resolver.Cli
  ( PlainConfigSpec
  -- , resolveCommandCli
  -- , resolveCommandCliPure
  , resolvePlainCli
  , resolvePlainCliPure
  ) where

-- import System.Etc.Internal.Resolver.Cli.Command (resolveCommandCli, resolveCommandCliPure)
import System.Etc.Internal.Resolver.Cli.Plain
    (PlainConfigSpec, resolvePlainCli, resolvePlainCliPure)
