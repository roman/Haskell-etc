{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Etc.SpecSpec (spec) where

import RIO

import qualified Data.Aeson              as JSON
import qualified Data.Aeson.BetterErrors as JSON hiding (withScientific)
import qualified Data.Yaml               as Yaml
import           Data.Yaml.TH            (yamlQQ)

import Text.RawString.QQ (r)

import Test.Hspec
import Test.Hspec.QuickCheck

import           Etc.Generators ()
import           Etc.Internal.Spec.Types (blankConfigValueJSON)
import qualified Etc.Spec                as SUT

invalidYamlError :: SUT.SpecError Yaml.ParseException -> Bool
invalidYamlError (SUT.SpecError (Yaml.InvalidYaml {})) = True
invalidYamlError _                                     = False

invalidJsonError :: SUT.SpecError (JSON.ParseError SUT.SpecParserError) -> Bool
invalidJsonError _ = True

spec :: Spec
spec = do
  describe "Etc.Spec" $ do

    describe "readConfigSpec" $ do
      describe "yaml format" $ do
        it "handles invalid syntax" $ do
          let
            input =
              [r|
                hello: world:
              |]
          SUT.parseConfigSpec SUT.yamlSpec [] input `shouldThrow` invalidYamlError

      describe "json format" $ do
        it "handles invalid syntax" $ do
          let
            input =
              [r|
                {"hello": "world":}
              |]
          SUT.parseConfigSpec SUT.jsonSpec [] input `shouldThrow` invalidJsonError

    describe "configSpecParser" $ do
      prop "handles encoding/parsing roundtrip" $ \configSpec -> do
        let jsonVal = JSON.toJSON configSpec
        case SUT.parseConfigSpecValue "<<string>>" [] jsonVal of
          Left err -> error (show err)
          Right configSpec' ->
              configSpec == blankConfigValueJSON configSpec'

      it "reports error on unknown type" $ do
        let specJSON =
              [yamlQQ|
                  etc/entries:
                      greeting:
                        etc/spec:
                          type: foobar
                  |]
        case SUT.parseConfigSpecValue "<<string>>" [] specJSON of
          Left err ->
            case fromException err of
              Just (SUT.SpecError specErr) ->
                case specErr of
                  JSON.BadSchema _ (JSON.CustomError (SUT.UnknownConfigValueType _sourceName keyPath typeName))  -> do
                    typeName `shouldBe` "foobar"
                    keyPath `shouldBe` ["greeting"]
                  _ ->
                   expectationFailure $
                    "Expecting spec error; got something else " <> show err
              _ ->
                expectationFailure $
                "Expecting spec error; got something else " <> show err
          Right _ -> expectationFailure "Expecting spec to fail; but didn't"

      it "reports error when 'etc/entries' is not an object" $ do
        let specJSON =
              [yamlQQ|
                     etc/entries:
                     - hello
                     - world
                  |]
        case SUT.parseConfigSpecValue "<<string>>" [] specJSON of
          Left err ->
            case fromException err of
              Just (SUT.SpecError specErr) ->
                case specErr of
                  JSON.BadSchema _ (JSON.CustomError (SUT.InvalidSpecEntries {}))  ->
                    return ()
                  _ ->
                    expectationFailure $
                    "Expecting spec error; got something else " <> show err
              _ ->
                expectationFailure $
                "Expecting spec error; got something else " <> show err
          Right _ -> expectationFailure "Expecting spec to fail; but didn't"


      it "reports error when default value and type don't match" $ do
        let specJSON =
              [yamlQQ|
                     etc/entries:
                       greeting:
                         etc/spec:
                           default: "one"
                           type: "number"
                  |]
        case SUT.parseConfigSpecValue "<<string>>" [] specJSON of
          Left err ->
            case fromException err of
              Just (SUT.SpecError specErr) ->
                case specErr of
                  JSON.BadSchema _ (JSON.CustomError (SUT.DefaultValueTypeMismatchFound _ keyPath cvType json))  -> do
                    keyPath `shouldBe` ["greeting"]
                    cvType `shouldBe` SUT.CVTSingle SUT.CVTNumber
                    json `shouldBe` JSON.String "one"
                  _ ->
                    expectationFailure $
                    "Expecting spec error; got something else " <> show err
              _ ->
                expectationFailure $
                "Expecting spec error; got something else " <> show err
          Right _ -> expectationFailure "Expecting spec to fail; but didn't"

      it
        "reports error when 'etc/spec' is not the only key in the field metadata object" $ do
        let specJSON =
              [yamlQQ|
                     etc/entries:
                       greeting:
                         etc/spec:
                           default: one
                           type: string
                         other: field
                  |]
        case SUT.parseConfigSpecValue "<<string>>" [] specJSON of
          Left err ->
            case fromException err of
              Just (SUT.SpecError specErr) ->
                case specErr of
                  JSON.BadSchema _ (JSON.CustomError (SUT.RedundantKeysOnValueSpec _ keyPath redundantKeys)) -> do
                    keyPath `shouldBe` ["greeting"]
                    redundantKeys `shouldBe` ["other"]
                  _ ->
                    expectationFailure $
                    "Expecting spec error; got something else:\n\t" <> show err
              _ ->
                expectationFailure $
                "Expecting spec error; got something else " <> show err
          Right _ -> expectationFailure "Expecting spec to fail; but didn't"

      it "reports when custom type errors detect invalid inputs" $ do
        let greetingChecker = SUT.textCustomType $ \input -> input == ("hello" :: Text)
        let specJSON =
              [yamlQQ|
                     etc/entries:
                       greeting:
                         etc/spec:
                           default: other
                           type: greeting
                  |]
        case SUT.parseConfigSpecValue "<<string>>" [("greeting", greetingChecker)] specJSON of
          Left err ->
            case fromException err of
              Just (SUT.SpecError specErr) ->
                case specErr of
                  JSON.BadSchema _ (JSON.CustomError (SUT.DefaultValueTypeMismatchFound _ keyPath cvType json))  -> do
                    keyPath `shouldBe` ["greeting"]
                    cvType `shouldBe` SUT.CVTSingle (SUT.CVTCustom "greeting")
                    json `shouldBe` JSON.String "other"
                  _ ->
                    expectationFailure $
                    "Expecting spec error; got something else " <> show err
              _ ->
                expectationFailure $
                "Expecting spec error; got something else " <> show err
          Right _ -> expectationFailure "Expecting spec to fail; but didn't"

      it "allows custom types for config values" $ do
        let greetingChecker = SUT.textCustomType $ \input -> input == ("hello" :: Text)
        let specJSON =
              [yamlQQ|
                     etc/entries:
                       greeting:
                         etc/spec:
                           default: hello
                           type: greeting
                  |]
        case SUT.parseConfigSpecValue "<<string>>" [("greeting", greetingChecker)] specJSON of
          Left err ->
            expectationFailure $ "Expecting valid config spec, got error instead " <> show err
          Right _ ->
            return ()
