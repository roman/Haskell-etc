{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import RIO
import Test.Hspec

import qualified Etc.Resolver.FileSpec
import qualified Etc.SpecSpec
import qualified Etc.CustomTypeSpec
import qualified Etc.FileFormatSpec

main :: IO ()
main = hspec $ do
  Etc.CustomTypeSpec.spec
  Etc.FileFormatSpec.spec
  Etc.SpecSpec.spec
  describe "Etc.Resolver" $ do
    describe "File" Etc.Resolver.FileSpec.spec
