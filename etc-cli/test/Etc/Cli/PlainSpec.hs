{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Etc.Cli.PlainSpec where

import RIO

import Data.Aeson ((.:), withObject)
import qualified Data.Aeson.BetterErrors as JSON
import Data.Aeson.QQ
import Test.Hspec

import           Etc
import qualified Etc.Cli                  as SUT
import qualified Etc.Internal.Cli.Types   as SUT
import           Etc.Internal.Spec.Parser (parseConfigSpecValue)
import           Etc.Internal.Resolver.Types (ResolverError(..))

resolver_spec :: Spec
resolver_spec =
  describe "resolver" $ do
    it "inputs with type string should accept numbers" $ do
      let input =
            [aesonQQ|
          { "etc/cli": {
              "desc": "Some Description"
            }
          , "etc/entries": {
              "greeting": {
                "etc/spec": {
                  "default": "default"
                , "type": "string"
                , "cli": {
                    "input": "option"
                  , "short": "g"
                  , "long": "greeting"
                  , "required": true
                  }
                }
              }
            }
          }
       |]
      configSpec <- parseConfigSpecValue "<<input>>" [] input
      config <-
        resolveConfigWith [] [SUT.pureCliResolver ["-g", "1234"]] configSpec
      str <- getConfigValue ["greeting"] config
      ("1234" :: Text) `shouldBe` str
    it "throws an error when input type does not match with spec type" $ do
      let input =
            [aesonQQ|
          { "etc/cli": {
              "desc": "Some Description"
            }
          , "etc/entries": {
              "greeting": {
                "etc/spec": {
                  "default": [123]
                , "type": "[number]"
                , "cli": {
                    "input": "option"
                  , "short": "g"
                  , "long": "greeting"
                  , "required": true
                  }
                }
              }
            }
          }
       |]
      configSpec <- parseConfigSpecValue "<<input>>" [] input
      let configResolver =
            resolveConfigWith
              []
              [SUT.pureCliResolver ["-g", "hello world"]]
              configSpec
      configResolver `shouldThrow` \(ExitFailure n) -> n > 0
    it "throws an error when entry is not given and is requested" $ do
      let input =
            [aesonQQ|
          {
            "etc/cli": {
              "desc": "Some Description"
            }
          , "etc/entries": {
              "database": {
                "username": {
                  "etc/spec": {
                    "default": [123]
                  , "type": "[number]"
                  , "cli": {
                      "input": "option"
                    , "long": "username"
                    , "required": false
                    }
                  }
                }
              , "password": "abc-123"
              }
            }
          }
       |]
      configSpec <- parseConfigSpecValue "<<input>>" [] input
      config <- resolveConfigWith [] [SUT.cliResolver] configSpec
      let parseDb =
            withObject "Database" $ \obj ->
              (,) <$> obj .: "username" <*> obj .: "password"
      case getConfigValueWith parseDb ["database"] config of
        Left err ->
          case fromException err of
            Just (ConfigValueParserFailed keyPath _) ->
              keyPath `shouldBe` ["database"]
            _ ->
              expectationFailure $
              "expecting ConfigValueParserFailed; got something else: " <>
              show err
        Right (_ :: ([Int], Text)) ->
          expectationFailure "expecting error; got none"

option_spec :: Spec
option_spec =
  describe "option input" $ do
    it "entry accepts short" $ do
      let input =
            [aesonQQ|
          {
            "etc/cli": {
              "desc": "Some Description"
            }
          , "etc/entries": {
              "greeting": {
                  "etc/spec": {
                    "type": "string"
                  , "cli": {
                      "input": "option"
                    , "short": "g"
                    , "long": "greeting"
                  }
                }
              }
            }
          }
       |]
      configSpec <- parseConfigSpecValue "<<input>>" [] input
      config <-
        resolveConfigWith
          []
          [SUT.pureCliResolver ["-g", "hello cli"]]
          configSpec
      case getConfigValue ["greeting"] config of
        Nothing ->
          expectationFailure
            ("expecting to get entries for greeting\n" <> show config)
        Just result -> result `shouldBe` ("hello cli" :: Text)
    it "entry accepts long" $ do
      let input =
            [aesonQQ|
          {
            "etc/cli": {
              "desc": "Some Description"
            }
          , "etc/entries": {
              "greeting": {
                  "etc/spec": {
                    "type": "string"
                  , "cli": {
                      "input": "option"
                    , "short": "g"
                    , "long": "greeting"
                  }
                }
              }
            }
          }
       |]
      configSpec <- parseConfigSpecValue "<<input>>" [] input
      config <-
        resolveConfigWith
          []
          [SUT.pureCliResolver ["--greeting", "hello cli"]]
          configSpec
      case getConfigValue ["greeting"] config of
        Nothing ->
          expectationFailure
            ("expecting to get entries for greeting\n" <> show config)
        Just result -> result `shouldBe` ("hello cli" :: Text)
    it "entry gets validated with a type" $ do
      let input =
            [aesonQQ|
          {
            "etc/cli": {
              "desc": "Some Description"
            }
          , "etc/entries": {
              "greeting": {
                  "etc/spec": {
                    "type": "number"
                  , "cli": {
                      "input": "option"
                    , "short": "g"
                    , "long": "greeting"
                  }
                }
              }
            }
          }
       |]
      configSpec <- parseConfigSpecValue "<<input>>" [] input
      result <-
        try $
        resolveConfigWith
          []
          [SUT.pureCliResolver ["--greeting", "hello cli"]]
          configSpec
      case result of
        Left err ->
          case fromException err of
            Just (ExitFailure _) -> return ()
            _ ->
              expectationFailure
                ("Expecting type validation to work on cli; got " <> show err)
        Right _ -> expectationFailure "Expecting type validation to work on cli"

    it "does parse array of numbers correctly" $ do
      let input =
            [aesonQQ|
          {
            "etc/cli": {
              "desc": "Some Description"
            }
          , "etc/entries": {
              "greeting": {
                  "etc/spec": {
                    "type": "[number]"
                  , "cli": {
                      "input": "option"
                    , "short": "g"
                    , "long": "greeting"
                  }
                }
              }
            }
          }
       |]
      configSpec <- parseConfigSpecValue "<<input>>" [] input
      config <-
        resolveConfigWith [] [SUT.pureCliResolver ["-g", "[1,2,3]"]] configSpec
      case getConfigValue ["greeting"] config of
        Right arr -> ([1, 2, 3] :: [Int]) `shouldBe` arr
        (Left err) ->
          expectationFailure
            ("expecting to parse an array, but didn't " <> show err)

argument_spec :: Spec
argument_spec =
  describe "argument input" $ do
    it "entry gets validated with a type" $ do
      let input =
            [aesonQQ|
        {
          "etc/cli": {
            "desc": "Some Description"
          }
        , "etc/entries": {
            "greeting": {
                "etc/spec": {
                  "type": "number"
                , "cli": {
                    "input": "argument"
                  , "metavar": "GREETING"
                }
              }
            }
          }
        }
     |]
      configSpec <- parseConfigSpecValue "<<input>>" [] input
      result <-
        try $
        resolveConfigWith [] [SUT.pureCliResolver ["hello cli"]] configSpec
      case result of
        Left err ->
          case fromException err of
            Just (ExitFailure status) -> status `shouldSatisfy` (> 0)
            _ ->
              expectationFailure
                ("Expecting type validation to work on cli; got " <> show err)
        Right _ -> expectationFailure "Expecting type validation to work on cli"
    it "entry with required false does not barf" $ do
      let input =
            [aesonQQ|
        {
          "etc/cli": {
            "desc": "Some Description"
          }
        , "etc/entries": {
            "greeting": {
                "etc/spec": {
                  "type": "number"
                , "cli": {
                    "input": "argument"
                  , "metavar": "GREETING"
                }
              }
            }
          }
        }
     |]
      configSpec <- parseConfigSpecValue "<<input>>" [] input
      config <-
        resolveConfigWith [] [SUT.pureCliResolver []] configSpec
      case getConfigValue ["greeting"] config of
        (Nothing :: Maybe ()) -> return ()
        Just aSet ->
          expectationFailure
            ("expecting to have no entry for greeting; got\n" <> show aSet)

switch_spec :: Spec
switch_spec =
  describe "switch input" $ do
    it "fails if etc/spec.type is not bool" $ do
      let input =
            [aesonQQ|
      {
        "etc/cli": {
          "desc": "Some Description"
        }
      , "etc/entries": {
          "greeting": {
              "etc/spec": {
                "type": "number"
              , "cli": {
                  "input": "switch"
                , "long": "valid"
              }
            }
          }
        }
      }
     |]
      configSpec <- parseConfigSpecValue "<<input>>" [] input
      result <- try $ resolveConfigWith [] [SUT.cliResolver] configSpec
      case result of
        Left err ->
          case fromException err of
            Just (ResolverError (JSON.BadSchema _ (JSON.CustomError (SUT.SwitchIncompatibleType {})))) ->
              return ()
            _ ->
              expectationFailure
                ("Expecting type validation to work on cli; got " <> show err)
        Right _ -> expectationFailure "Expecting type validation to work on cli"
    context "when etc/spec.default is false" $ do
      it "returns false when flag is not given" $ do
        let input =
              [aesonQQ|
        {
          "etc/cli": {
            "desc": "Some Description"
          }
        , "etc/entries": {
            "greeting": {
                "etc/spec": {
                  "type": "bool"
                , "default": false
                , "cli": {
                    "input": "switch"
                  , "long": "valid"
                }
              }
            }
          }
        }
        |]
        configSpec <- parseConfigSpecValue "<<input>>" [] input
        result <- try $ resolveConfigWith [] [SUT.cliResolver] configSpec
        case result of
          Left (err :: SomeException) ->
            expectationFailure
              ("Expecting default to work on cli; but didn't: " <> show err)
          Right config -> do
            greeting <- getConfigValue ["greeting"] config
            greeting `shouldBe` False
      it "returns true when flag is given" $ do
        let input =
              [aesonQQ|
        {
          "etc/cli": {
            "desc": "Some Description"
          }
        , "etc/entries": {
            "greeting": {
                "etc/spec": {
                  "type": "bool"
                , "default": false
                , "cli": {
                    "input": "switch"
                  , "long": "disabled"
                }
              }
            }
          }
        }
        |]
        configSpec <- parseConfigSpecValue "<<input>>" [] input
        result <- try $ resolveConfigWith [] [SUT.pureCliResolver ["--disabled"]] configSpec
        case result of
          Left (err :: SomeException) ->
            expectationFailure
              ("Expecting default to work on cli; but didn't: " <> show err)
          Right config -> do
            greeting <- getConfigValue ["greeting"] config
            greeting `shouldBe` True

    context "when etc/spec.default is true" $ do
      it "returns true when flag is not given" $ do
        let input =
              [aesonQQ|
        {
          "etc/cli": {
            "desc": "Some Description"
          }
        , "etc/entries": {
            "greeting": {
                "etc/spec": {
                  "type": "bool"
                , "default": true
                , "cli": {
                    "input": "switch"
                  , "long": "invalid"
                  , "help": "This is a description"
                }
              }
            }
          }
        }
        |]
        configSpec <- parseConfigSpecValue "<<input>>" [] input
        config <- resolveConfigWith [] [SUT.pureCliResolver []] configSpec
        greeting <- getConfigValue ["greeting"] config
        greeting `shouldBe` True
      it "returns false when flag is given" $ do
        let input =
              [aesonQQ|
        {
          "etc/cli": {
            "desc": "Some Description"
          }
        , "etc/entries": {
            "greeting": {
                "etc/spec": {
                  "type": "bool"
                , "default": false
                , "cli": {
                    "input": "switch"
                  , "long": "invalid"
                  , "help": "This is a description"
                }
              }
            }
          }
        }
        |]
        configSpec <- parseConfigSpecValue "<<input>>" [] input
        config <- resolveConfigWith [] [SUT.pureCliResolver []] configSpec
        greeting <- getConfigValue ["greeting"] config
        greeting `shouldBe` False

    it "creates a no- flag" $ do
      let input =
            [aesonQQ|
      {
        "etc/cli": {
          "desc": "Some Description"
        }
      , "etc/entries": {
          "disabled": {
              "etc/spec": {
                "type": "bool"
              , "default": false
              , "cli": {
                  "input": "switch"
                , "long": "disabled"
                , "help": "This is a description"
              }
            }
          }
        }
      }
      |]
      configSpec <- parseConfigSpecValue "<<input>>" [] input
      config <- resolveConfigWith [] [SUT.pureCliResolver ["--no-disabled"]] configSpec
      isDisabled <- getConfigValue ["disabled"] config
      isDisabled `shouldBe` False



spec :: Spec
spec = do
  resolver_spec
  option_spec
  argument_spec
  switch_spec


main :: IO ()
main = hspec spec
