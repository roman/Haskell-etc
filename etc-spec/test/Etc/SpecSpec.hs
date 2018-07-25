{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Etc.SpecSpec where

import RIO

import qualified Data.Aeson              as JSON
import qualified Data.Aeson.BetterErrors as JSON
import           Data.Aeson.QQ

import Test.Hspec

import qualified Etc.Spec as SUT

spec :: Spec
spec =
  describe "Etc.Spec.configSpecParser" $ do
    it "reports error on unknown type" $ do
      let specJSON =
            [aesonQQ|
                {
                  "etc/entries": {
                    "greeting": {
                      "etc/spec": {
                        "type": "foobar"
                      }
                    }
                  }
                }
                |]
      case SUT.parseConfigSpecJson specJSON of
        Left err ->
          case fromException err of
            Just (SUT.SpecParserError specErr) ->
              case specErr of
                JSON.BadSchema _ (JSON.CustomError (SUT.InvalidConfigValueType keyPath typeName))  -> do
                  typeName `shouldBe` "foobar"
                  keyPath `shouldBe` ["greeting"]
                _ ->
                  expectationFailure $
                  "Expecting spec error; got something else " <> show err
            Nothing ->
              expectationFailure $
              "Expecting spec error; got something else " <> show err
        Right _ -> expectationFailure "Expecting spec to fail; but didn't"

    it "reports error when 'etc/entries' is not an object" $ do
      let specJSON =
            [aesonQQ|
                {
                  "etc/entries": ["hello", "world"]
                }
                |]
      case SUT.parseConfigSpecJson specJSON of
        Left err ->
          case fromException err of
            Just (SUT.SpecParserError specErr) ->
              case specErr of
                JSON.BadSchema _ (JSON.CustomError (SUT.InvalidSpecEntries _))  -> do
                  return ()
                _ ->
                  expectationFailure $
                  "Expecting spec error; got something else " <> show err
            Nothing ->
              expectationFailure $
              "Expecting spec error; got something else " <> show err
        Right _ -> expectationFailure "Expecting spec to fail; but didn't"


    it "reports error when default value and type don't match" $ do
      let specJSON =
            [aesonQQ|
                {
                  "etc/entries": {
                    "greeting": {
                      "etc/spec": {
                        "default": "one"
                      , "type": "number"
                      }
                    }
                  }
                }
                |]
      case SUT.parseConfigSpecJson specJSON of
        Left err ->
          case fromException err of
            Just (SUT.SpecParserError specErr) ->
              case specErr of
                JSON.BadSchema _ (JSON.CustomError (SUT.ConfigValueDefaultTypeMismatchFound keyPath cvType json))  -> do
                  keyPath `shouldBe` ["greeting"]
                  cvType `shouldBe` (SUT.CVTSingle SUT.CVTNumber)
                  json `shouldBe` (JSON.String "one")
                _ ->
                  expectationFailure $
                  "Expecting spec error; got something else " <> show err
            Nothing ->
              expectationFailure $
              "Expecting spec error; got something else " <> show err
        Right _ -> expectationFailure "Expecting spec to fail; but didn't"
    it
      "reports error when 'etc/spec' is not the only key in the field metadata object" $ do
      let specJSON =
            [aesonQQ|
                {
                  "etc/entries": {
                    "greeting": {
                      "etc/spec": {
                        "default": "one"
                      , "type": "string"
                      }
                    , "other": "field"
                    }
                  }
                }
                |]
      case SUT.parseConfigSpecJson specJSON of
        Left err ->
          case fromException err of
            Just (SUT.SpecParserError specErr) ->
              case specErr of
                JSON.BadSchema _ (JSON.CustomError (SUT.RedundantKeysOnValueSpec keyPath redundantKeys)) -> do
                  keyPath `shouldBe` ["greeting"]
                  redundantKeys `shouldBe` ["other"]
                _ ->
                  expectationFailure $
                  "Expecting spec error; got something else:\n\t" <> show err
            Nothing ->
              expectationFailure $
              "Expecting spec error; got something else " <> show err
        Right _ -> expectationFailure "Expecting spec to fail; but didn't"
