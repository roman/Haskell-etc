{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Etc.Internal.Resolver.Cli
  ( PlainConfigSpec
  -- , resolveCommandCli
  -- , resolveCommandCliPure
  , resolvePlainCli
  , resolvePlainCliPure
  ) where

-- import Etc.Internal.Resolver.Cli.Command (resolveCommandCli, resolveCommandCliPure)
import Etc.Internal.Resolver.Cli.Plain (PlainConfigSpec, resolvePlainCli, resolvePlainCliPure)
