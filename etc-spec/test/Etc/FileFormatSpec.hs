{-# LANGUAGE CPP               #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
module Etc.FileFormatSpec where

import RIO

import Data.Aeson.QQ (aesonQQ)

import Test.Hspec

import System.FilePath ((</>))

import qualified Etc.Resolver as Resolver
import qualified Etc.Spec     as Spec

import qualified Etc.Internal.Config     as Config
import qualified Etc.Internal.FileFormat as SUT

testFixturePath :: FilePath -> FilePath
testFixturePath path =
#ifdef GHCI
  "etc-spec/test/fixtures" </> path
#else
  "test/fixtures" </> path
#endif

spec :: Spec
spec =
  describe "FileFormat" $ do
    describe "Semigroup and Functor instance" $ do
      it "allows to compose different file format parsers" $ do
        let
          configSpecValue =
            [aesonQQ|
                   { "etc/files":
                     {
                       "paths": [
                         #{testFixturePath "config1.json"},
                         #{testFixturePath "config1.yaml"}
                       ]
                     },
                     "etc/entries": {"greeting": {"etc/spec": {"default": "default greeting"}}}
                   }
         |]
        configSpec <- Spec.parseConfigSpecValue "<<string>>" [] configSpecValue
        -- NOTE: therr error types for jsonFormat and yamlFormat are not the same, using
        -- the Functor instance of FileFormat, we can compose them with a high level ADT
        let newFileFormat = fmap Left (SUT.jsonFormat @Resolver.FileResolverError)  <> fmap Right SUT.yamlFormat

        config <- Resolver.resolveConfigWith [] configSpec [Resolver.fileResolver newFileFormat]

        databaseValue <- Config.getConfigValue ["greeting"] config
        databaseValue `shouldBe` ("config1.yaml" :: Text)
