{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Etc.Cli.CommandSpec where

import           RIO
import qualified RIO.Set as Set

-- import Data.Aeson ((.:), withObject)
-- import qualified Data.Aeson.BetterErrors as JSON
import qualified Data.Aeson          as JSON
import           Data.Aeson.QQ
import qualified Options.Applicative as Opt
import           Test.Hspec

import           Etc
import qualified Etc.Cli                     as SUT
import           Etc.Internal.Cli.Types      (CliResolverError (..))
import           Etc.Internal.Resolver.Types (ResolverError (..))
import           Etc.Internal.Spec.Types     (SpecError (..))

import Etc.Internal.Spec.Parser (parseConfigSpecValue)
-- import           Etc.Internal.Resolver.Types (ResolverError(..))

data MyCmd
  = Foo Config
  | Bar Config

data MyRecord
  = MyRecord !Int

transform_spec :: Spec
transform_spec = error "pending"

simpleExample :: JSON.Value
simpleExample =
  [aesonQQ|
    {
      "etc/cli": {
        "desc": "Some Description"
      , "commands": {
          "foobar": {
            "desc": "Running some foos"
          }
        }
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
            , "commands": ["foobar"]
            }
          }
        }
      }
    }
  |]

invalidCmdOnEntry :: JSON.Value
invalidCmdOnEntry =
  [aesonQQ|
    {
      "etc/cli": {
        "desc": "Some Description"
      , "commands": {
          "foobar": {
            "desc": "Running some foos"
          }
        }
      }
    , "etc/entries": {
        "greeting": {
          "etc/spec": {
            "type": "string"
          , "cli": {
              "input": "option"
            , "short": "g"
            , "long": "greeting"
            , "commands": ["invalid"]
            }
          }
        }
      }
    }
  |]

multipleCommands :: JSON.Value
multipleCommands =
  [aesonQQ|
    { "etc/cli": {
        "desc": "Some Description"
      , "commands": {
          "foo": {
            "desc": "Runs a bunch of foos"
          },
          "bar": {
            "desc": "Runs a bunch of bars"
          }
        }
      }
    , "etc/entries": {
        "foo": {
          "etc/spec": {
            "type": "string"
          , "cli": {
              "input": "option"
            , "short": "g"
            , "meta": "STR"
            , "long": "foo"
            , "commands": ["foo"]
            }
          }
        },
        "bar": {
          "etc/spec": {
            "type": "string"
          , "cli": {
              "input": "option"
            , "short": "g"
            , "meta": "STR"
            , "long": "bar"
            , "commands": ["bar"]
            }
          }
        }
      }
    }
  |]

sameOptionOnSameCommand :: JSON.Value
sameOptionOnSameCommand =
  [aesonQQ|
    { "etc/cli": {
        "desc": "Some Description"
      , "commands": {
          "foo": {
            "desc": "Runs a bunch of foos"
          },
          "bar": {
            "desc": "Runs a bunch of bars"
          }
        }
      }
    , "etc/entries": {
        "foo1": {
          "etc/spec": {
            "type": "string"
          , "cli": {
              "input": "option"
            , "short": "g"
            , "meta": "STR"
            , "long": "my-option"
            , "commands": ["foo"]
            }
          }
        },
        "foo2": {
          "etc/spec": {
            "type": "string"
          , "cli": {
              "input": "option"
            , "short": "g"
            , "meta": "STR"
            , "long": "my-option"
            , "commands": ["foo"]
            }
          }
        }
      }
    }
  |]

sameOptionOnDifferentCommands :: JSON.Value
sameOptionOnDifferentCommands =
  [aesonQQ|
    { "etc/cli": {
        "desc": "Some Description"
      , "commands": {
          "foo": {
            "desc": "Runs a bunch of foos"
          },
          "bar": {
            "desc": "Runs a bunch of bars"
          }
        }
      }
    , "etc/entries": {
        "foo": {
          "etc/spec": {
            "type": "string"
          , "cli": {
              "input": "option"
            , "short": "g"
            , "meta": "STR"
            , "long": "my-option"
            , "commands": ["foo"]
            }
          }
        },
        "bar": {
          "etc/spec": {
            "type": "string"
          , "cli": {
              "input": "option"
            , "short": "g"
            , "meta": "STR"
            , "long": "my-option"
            , "commands": ["bar"]
            }
          }
        }
      }
    }
  |]

foobarCommands :: [(Text, Opt.Parser (Config -> MyCmd))]
foobarCommands = [("foo", pure Foo), ("bar", pure Bar)]

spec :: Spec
spec =
  describe "resolveConfigWith1" $ do
    it "detects when a command name is missing in the resolve call" $ do
      configSpec <- parseConfigSpecValue "<<input>>" [] simpleExample
      let resolveAction =
            SUT.resolveConfigWith1
              ["foobar", "--greeting", "this input"]
              []
              []
              []
              configSpec
      resolveAction `shouldThrow`
        (\case
           ResolverError (CommandListMismatch _ _unknown missing) ->
             missing == Set.singleton "foobar"
           _ -> False)
    it "detects when an invalid command is given in the resolve call" $ do
      configSpec <- parseConfigSpecValue "<<input>>" [] simpleExample
      let resolveAction =
            SUT.resolveConfigWith1
              ["foobar", "--greeting", "this input"]
              [("invalid", pure Foo)]
              []
              []
              configSpec
      resolveAction `shouldThrow`
        (\case
           ResolverError (CommandListMismatch _ unknown _missing) ->
             unknown == Set.singleton "invalid"
           _ -> False)
    it "detects when an invalid command is used in a field spec" $ do
      configSpec <- parseConfigSpecValue "<<input>>" [] invalidCmdOnEntry
      let resolveAction =
            SUT.resolveConfigWith1
              ["foobar", "--greeting", "this input"]
              [("foobar", pure Foo)]
              []
              []
              configSpec
      resolveAction `shouldThrow`
        (\case
           SpecError (UnknownCommandOnEntry _ _ _valid unknown) ->
             unknown == Set.singleton "invalid"
           _ -> False)
    it "allows to integrate with existing optparse-applicative parsers" $ do
      let myRecordParser =
            MyRecord <$> Opt.option Opt.auto (Opt.long "my-number")
      configSpec <- parseConfigSpecValue "<<input>>" [] simpleExample
      (MyRecord n, config) <-
        SUT.resolveConfigWith1
          ["foobar", "--my-number", "42", "--greeting", "this input"]
          [("foobar", (,) <$> myRecordParser)]
          []
          []
          configSpec
      n `shouldBe` 42
      str <- getConfigValue ["greeting"] config
      ("this input" :: Text) `shouldBe` str

    it "simple use case works" $ do
      configSpec <- parseConfigSpecValue "<<input>>" [] simpleExample
      config <-
        SUT.resolveConfigWith1
          ["foobar", "--greeting", "this input"]
          [("foobar", pure id)]
          []
          []
          configSpec
      str <- getConfigValue ["greeting"] config
      ("this input" :: Text) `shouldBe` str

    context "multiple commands" $ do
      it "simple case works" $ do
        configSpec <- parseConfigSpecValue "<<input>>" [] multipleCommands

        Foo fooConfig <-
          SUT.resolveConfigWith1
            ["foo", "--foo", "from foo"]
            foobarCommands
            []
            []
            configSpec
        fooGreet <- getConfigValue ["foo"] fooConfig
        fooGreet `shouldBe` ("from foo" :: Text)

        Bar barConfig <-
          SUT.resolveConfigWith1
            ["bar", "--bar", "from bar"]
            foobarCommands
            []
            []
            configSpec
        barGreet <- getConfigValue ["bar"] barConfig
        barGreet `shouldBe` ("from bar" :: Text)

      it
        "works correctly with same option name as long as commands are different" $ do
        configSpec <- parseConfigSpecValue "<<input>>" [] sameOptionOnDifferentCommands
        Foo fooConfig <-
          SUT.resolveConfigWith1
            ["foo", "--my-option", "from foo"]
            foobarCommands
            []
            []
            configSpec
        fooGreet <- getConfigValue ["foo"] fooConfig
        fooGreet `shouldBe` ("from foo" :: Text)
        getConfigValue ["bar"] fooConfig `shouldBe` (Nothing :: Maybe Text)
        Bar barConfig <-
          SUT.resolveConfigWith1
            ["bar", "--my-option", "from bar"]
            foobarCommands
            []
            []
            configSpec
        barGreet <- getConfigValue ["bar"] barConfig
        barGreet `shouldBe` ("from bar" :: Text)
        getConfigValue ["foo"] barConfig `shouldBe` (Nothing :: Maybe Text)
      -- NOTE: This should throw an error instead, pending ticket
      it "works incorrectly with same option on same command but different entries" $ do
        configSpec <- parseConfigSpecValue "<<input>>" [] sameOptionOnSameCommand
        Foo fooConfig <-
          SUT.resolveConfigWith1
            ["foo", "--my-option", "from foo"]
            foobarCommands
            []
            []
            configSpec

        fooGreet <- getConfigValue ["foo1"] fooConfig
        fooGreet `shouldBe` ("from foo" :: Text)
        getConfigValue ["foo2"] fooConfig `shouldBe` (Nothing :: Maybe Text)
