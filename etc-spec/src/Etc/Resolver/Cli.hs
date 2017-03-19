{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Etc.Resolver.Cli
  ( PlainConfigSpec
  -- , resolveCommandCli
  -- , resolveCommandCliPure
  , resolvePlainCli
  , resolvePlainCliPure
  ) where

-- import Etc.Resolver.Cli.Command (resolveCommandCli, resolveCommandCliPure)
import Etc.Resolver.Cli.Plain (PlainConfigSpec, resolvePlainCli, resolvePlainCliPure)
