{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module System.Etc.Internal.Resolver.OptParse
  ( PlainConfigSpec
  , resolveCommandOptParser
  , resolveCommandOptParserPure
  , resolvePlainOptParser
  , resolvePlainOptParserPure
  ) where

import System.Etc.Internal.Resolver.OptParse.Command (resolveCommandOptParser, resolveCommandOptParserPure)
import System.Etc.Internal.Resolver.OptParse.Plain (PlainConfigSpec, resolvePlainOptParser, resolvePlainOptParserPure)
