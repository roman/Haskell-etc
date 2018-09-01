{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import RIO
import Test.Hspec

import qualified Etc.Resolver.FileSpec
import qualified Etc.SpecSpec

main :: IO ()
main = hspec $ do
  Etc.SpecSpec.spec
  Etc.Resolver.FileSpec.spec
