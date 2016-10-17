{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module System.Etc.Internal.Resolver.OptParse
  ( resolveOptParser
  , resolveCommandOptParser
  ) where

import UB.Prelude
import System.Etc.Internal.Types
import System.Etc.Internal.Resolver.OptParse.Command (resolveCommandOptParser)
import System.Etc.Internal.Resolver.OptParse.Plain (resolvePlainOptParser)

import qualified System.Etc.Internal.Spec as Spec

resolveOptParser :: Spec.ConfigSpec -> IO Config
resolveOptParser = resolvePlainOptParser
