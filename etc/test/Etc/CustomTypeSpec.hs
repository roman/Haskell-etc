{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Etc.CustomTypeSpec where

import RIO

import Data.Scientific (scientific, toBoundedInteger)

import Test.Hspec

import qualified Data.Aeson              as JSON
import qualified Data.Aeson.BetterErrors as JSON hiding (withScientific)

import qualified Etc.Internal.CustomType as SUT

newtype OddNumber = OddNumber Int
instance JSON.FromJSON OddNumber where
  parseJSON = JSON.withScientific "OddNumber" $ \number ->
    case toBoundedInteger number of
      Just intNumber
        | intNumber `mod` 2 == 0 -> return (OddNumber intNumber)
      _ ->
        fail "Expecting odd number"

spec :: Spec
spec =
  describe "CustomType" $ do
    describe "textCustomType" $ do
      it "parses a custom type succintly" $ do
        let customType = SUT.textCustomType $ \input -> input == "hello"
        let jsonValue = JSON.String "hello"
        let parsed =
              JSON.parseValue
                (SUT.runJsonParser customType)
                jsonValue

        case parsed of
          Left err ->
            expectationFailure $ "Expecting valid parsing, got error instead: " <> show err
          Right result ->
            result `shouldBe` ()

      it "returns error on invalid input" $ do
        let customType = SUT.textCustomType $ \input -> input == "hello"
        let jsonValue = JSON.String "world"
        let parsed =
              JSON.parseValue
                (SUT.runJsonParser customType)
                jsonValue

        case parsed of
          Left (JSON.BadSchema _ (JSON.CustomError ())) ->
            return ()
          Left err ->
            expectationFailure $ "Expecting mismatch error, got something else instead: " <> show err
          Right _ ->
            expectationFailure $ "Expecting error, but didn't happen"

    describe "boundedIntCustomType" $ do
      it "parses a custom type succintly" $ do
        let customType = SUT.boundedIntCustomType $ \number -> (number :: Int) `mod` 2 == 0
        let jsonValue = JSON.Number (fromIntegral (10 :: Int))
        let parsed =
              JSON.parseValue
                (SUT.runJsonParser customType)
                jsonValue

        case parsed of
          Left err ->
            expectationFailure $ "Expecting valid parsing, got error instead: " <> show err
          Right result ->
            result `shouldBe` ()

      it "returns error on invalid input" $ do
        let customType = SUT.boundedIntCustomType $ \number -> (number :: Int) `mod` 2 == 0
        let jsonValue = JSON.Number (fromIntegral (7 :: Int))
        let parsed =
              JSON.parseValue
                (SUT.runJsonParser customType)
                jsonValue
        case parsed of
          Left (JSON.BadSchema _ (JSON.CustomError ())) ->
            return ()
          Left err ->
            expectationFailure $ "Expecting mismatch error, got something else instead: " <> show err
          Right _ ->
            expectationFailure $ "Expecting error, but didn't happen"

    describe "boundedFloatCustomType" $ do
      it "parses a custom type succintly" $ do
        let customType = SUT.boundedFloatCustomType $ \number -> (number :: Double) == 3.1416
        let jsonValue = JSON.Number $ scientific 31416 (-4)
        let parsed =
              JSON.parseValue
                (SUT.runJsonParser customType)
                jsonValue
        case parsed of
          Left err ->
            expectationFailure $ "Expecting valid parsing, got error instead: " <> show err
          Right result ->
            result `shouldBe` ()

      it "returns error on invalid input" $ do
        let customType = SUT.boundedFloatCustomType $ \number -> (number :: Double) == 3.1416
        let jsonValue = JSON.Number $ scientific 5 1
        let parsed =
              JSON.parseValue
                (SUT.runJsonParser customType)
                jsonValue
        case parsed of
          Left (JSON.BadSchema _ (JSON.CustomError ())) ->
            return ()
          Left err ->
            expectationFailure $ "Expecting mismatch error, got something else instead: " <> show err
          Right _ ->
            expectationFailure $ "Expecting error, but didn't happen"

    describe "aesonCustomType" $ do
      it "parses a custom type succintly" $ do
        let jsonValue = JSON.Number (fromInteger 20)
        let parsed =
              JSON.parseValue
                (SUT.runJsonParser (SUT.aesonCustomType @OddNumber))
                jsonValue

        case parsed of
          Left err ->
            expectationFailure $ "Expecting valid parsing, got error instead: " <> show err
          Right result ->
            result `shouldBe` ()

      it "returns error on invalid input" $ do
       let customType = SUT.aesonCustomType @OddNumber
       let jsonValue = JSON.Number (fromInteger 35)
       let parsed =
             JSON.parseValue
               (SUT.runJsonParser customType)
               jsonValue


       case parsed of
         Left err ->
           case err of
             JSON.BadSchema _ (JSON.FromAeson msg) -> do
               msg `shouldBe` "Expecting odd number"
             _ ->
               expectationFailure $
               "Expecting spec error; got something else:\n\t" <> show err
         Right result ->
           result `shouldBe` ()
