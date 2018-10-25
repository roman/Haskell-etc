{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Etc.Cli.CommandSpec where

import RIO

-- import Data.Aeson ((.:), withObject)
-- import qualified Data.Aeson.BetterErrors as JSON
import Data.Aeson.QQ
import Test.Hspec

import           Etc
import qualified Etc.Cli                  as SUT
-- import qualified Etc.Internal.Cli.Types   as SUT
import           Etc.Internal.Spec.Parser (parseConfigSpecValue)
-- import           Etc.Internal.Resolver.Types (ResolverError(..))

data MyCmd
  = Foo Config
  | Bar Config

resolver_spec :: Spec
resolver_spec =
  describe "resolveConfigWith1" $ do
    -- xit "simple use case works" $ do
    --   let input =
    --         [aesonQQ|
    --           { "etc/cli": {
    --               "desc": "Some Description"
    --             , "commands": {
    --                 "foobar": {
    --                   "desc": "Running some foos"
    --                 }
    --               }
    --             }
    --           , "etc/entries": {
    --               "greeting": {
    --                 "etc/spec": {
    --                   "default": "default"
    --                 , "type": "string"
    --                 , "cli": {
    --                     "input": "option"
    --                   , "short": "g"
    --                   , "long": "greeting"
    --                   , "commands": ["foobar"]
    --                   }
    --                 }
    --               }
    --             }
    --           }
    --    |]
    --   configSpec <- parseConfigSpecValue "<<input>>" [] input
    --   config <-
    --     SUT.resolveConfigWith1
    --       ["foobar", "--greeting", "this input"]
    --       [("foobar", pure id)]
    --       []
    --       []
    --       configSpec
    --   str <- getConfigValue ["greeting"] config
    --   ("this input" :: Text) `shouldBe` str
    context "multiple commands" $ do
      it "allows the same option name as long as the commands are different" $ do
        let
          input =
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
                    "canada": {
                      "greeting": {
                        "etc/spec": {
                          "default": "default"
                        , "type": "string"
                        , "cli": {
                            "input": "option"
                          , "short": "g"
                          , "meta": "STR"
                          , "long": "greeting"
                          , "commands": ["bar"]
                          }
                        }
                      }
                    },
                    "top": {
                      "etc/spec": {
                        "default": "default"
                      , "type": "string"
                      , "cli": {
                          "input": "option"
                        , "short": "g"
                        , "meta": "STR"
                        , "long": "another"
                        , "commands": ["foo"]
                        }
                      }
                    },
                    "mexico": {
                      "bar": {
                        "etc/spec": {
                          "default": "default"
                        , "type": "string"
                        , "cli": {
                            "input": "option"
                          , "long": "gonzo"
                          , "commands": ["bar"]
                          }
                        }
                      }
                    }
                  }
                }
              |]
          commands =
            [ ("foo", pure Foo)
            , ("bar", pure Bar)
            ]

        configSpec <- parseConfigSpecValue "<<input>>" [] input
        -- Foo fooConfig <-
        --   SUT.resolveConfigWith1
        --     ["foo", "--help"]
        --     -- ["foo", "--greeting", "this foo"]
        --     commands
        --     []
        --     []
        --     configSpec
        -- fooGreet <- getConfigValue ["foo", "greeting"] fooConfig
        -- fooGreet `shouldBe` ("this foo" :: Text)

        Bar barConfig <-
          SUT.resolveConfigWith1
            ["bar", "--help"]
            -- ["bar", "--greeting", "this bar"]
            commands
            []
            []
            configSpec
        barGreet <- getConfigValue ["greeting"] barConfig
        barGreet `shouldBe` ("this bar" :: Text)

      -- xit "allows a single entry to be in multiple commands" $ do
      --   let
      --     input =
      --         [aesonQQ|
      --           { "etc/cli": {
      --               "desc": "Some Description"
      --             , "commands": {
      --                 "foo": {
      --                   "desc": "Runs a bunch of foos"
      --                 },
      --                 "bar": {
      --                   "desc": "Runs a bunch of bars"
      --                 }
      --               }
      --             }
      --           , "etc/entries": {
      --               "greeting": {
      --                 "etc/spec": {
      --                   "default": "default"
      --                 , "type": "string"
      --                 , "cli": {
      --                     "input": "option"
      --                   , "short": "g"
      --                   , "meta": "STR"
      --                   , "long": "greeting"
      --                   , "commands": ["foo", "bar"]
      --                   }
      --                 }
      --               },
      --               "only_bar": {
      --                 "etc/spec": {
      --                   "default": "only_bar",
      --                   "type": "string",
      --                   "cli": {
      --                     "input": "option"
      --                   , "long": "only-bar"
      --                   , "commands": ["bar"]
      --                   }
      --                 }
      --               }
      --             }
      --           }
      --         |]
      --     commands =
      --       [ ("foo", pure Foo)
      --       , ("bar", pure Bar)
      --       ]

      --   configSpec <- parseConfigSpecValue "<<input>>" [] input

      --   Foo fooConfig <-
      --     SUT.resolveConfigWith1
      --       ["foo", "--greeting", "this foo"]
      --       commands
      --       []
      --       []
      --       configSpec
      --   fooGreet <- getConfigValue ["greeting"] fooConfig
      --   fooGreet `shouldBe` ("this foo" :: Text)

      --   Bar barConfig <-
      --     SUT.resolveConfigWith1
      --       -- ["bar", "--greeting", "this bar"]
      --       ["bar", "--help"]
      --       commands
      --       []
      --       []
      --       configSpec
      --   barGreet <- getConfigValue ["greeting"] barConfig
      --   barGreet `shouldBe` ("this bar" :: Text)

    -- it "throws an error when input type does not match with spec type" $ do
    --   let input =
    --         [aesonQQ|
    --       { "etc/cli": {
    --           "desc": "Some Description"
    --         }
    --       , "etc/entries": {
    --           "greeting": {
    --             "etc/spec": {
    --               "default": [123]
    --             , "type": "[number]"
    --             , "cli": {
    --                 "input": "option"
    --               , "short": "g"
    --               , "long": "greeting"
    --               , "required": true
    --               }
    --             }
    --           }
    --         }
    --       }
    --    |]
    --   configSpec <- parseConfigSpecValue "<<input>>" [] input
    --   let configResolver =
    --         resolveConfigWith
    --           []
    --           [SUT.pureCliResolver ["-g", "hello world"]]
    --           configSpec
    --   configResolver `shouldThrow` \(ExitFailure n) -> n > 0
    -- it "throws an error when entry is not given and is requested" $ do
    --   let input =
    --         [aesonQQ|
    --       {
    --         "etc/cli": {
    --           "desc": "Some Description"
    --         }
    --       , "etc/entries": {
    --           "database": {
    --             "username": {
    --               "etc/spec": {
    --                 "default": [123]
    --               , "type": "[number]"
    --               , "cli": {
    --                   "input": "option"
    --                 , "long": "username"
    --                 , "required": false
    --                 }
    --               }
    --             }
    --           , "password": "abc-123"
    --           }
    --         }
    --       }
    --    |]
    --   configSpec <- parseConfigSpecValue "<<input>>" [] input
    --   config <- resolveConfigWith [] [SUT.cliResolver] configSpec
    --   let parseDb =
    --         withObject "Database" $ \obj ->
    --           (,) <$> obj .: "username" <*> obj .: "password"
    --   case getConfigValueWith parseDb ["database"] config of
    --     Left err ->
    --       case fromException err of
    --         Just (ConfigValueParserFailed keyPath _) ->
    --           keyPath `shouldBe` ["database"]
    --         _ ->
    --           expectationFailure $
    --           "expecting ConfigValueParserFailed; got something else: " <>
    --           show err
    --     Right (_ :: ([Int], Text)) ->
    --       expectationFailure "expecting error; got none"
