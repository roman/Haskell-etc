{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Etc.Cli
  ( cliResolver
  , pureCliResolver
  , resolveConfigWith
  , resolveConfigWith1
  ) where

import Etc.Internal.Cli.Plain.Resolver
import Etc.Internal.Cli.Command.Resolver
