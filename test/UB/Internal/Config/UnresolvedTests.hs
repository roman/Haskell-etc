{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module UB.Internal.Config.UnresolvedTests (tests) where

import UB.Prelude
import UB.Test.Util (assertFromEDN)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit  (testCase)
import qualified Data.Map as Map

import qualified UB.Internal.Config.Unresolved as SUT

configValueEDNParserTests :: TestTree
configValueEDNParserTests =
  testGroup
    "Unresolved.ConfigValue EDN Parser"
    [ testCase "only :default key" <|
         assertFromEDN
           "does not return default config value"
           "#config/meta {:default \"world\"}"
           (SUT.ConfigValue (Just "world") SUT.fileConfigSource)

    , testCase "only :envvar key" <|
        let
          configSource =
            SUT.ConfigSources { SUT.envVar = SUT.Pending (SUT.EnvVar "HELLO")
                              , SUT.optParse = SUT.Skip
                              , SUT.file   = SUT.Skip }
        in
           assertFromEDN
             "does not include env-var config source"
             "#config/meta {:envvar \"HELLO\"}"
             (SUT.ConfigValue Nothing configSource)

    , testCase "only :optparse key" <|
        let
          configSource =
            SUT.ConfigSources { SUT.envVar   = SUT.Skip
                              , SUT.optParse = SUT.Pending (SUT.OptParse "hello")
                              , SUT.file     = SUT.Skip }
        in
           assertFromEDN
             "does not include opt parser config source"
             "#config/meta {:optparse \"hello\"}"
             (SUT.ConfigValue Nothing configSource)

    , testCase "combination of the three" <|
        let
          configSource =
            SUT.ConfigSources { SUT.envVar   = SUT.Pending (SUT.EnvVar "HELLO")
                              , SUT.optParse = SUT.Pending (SUT.OptParse "hello")
                              , SUT.file     = SUT.Pending SUT.File }
        in
           assertFromEDN
             "does not include opt parser config source"
             "#config/meta {:optparse \"hello\" :envvar \"HELLO\" :default \"hello\"}"
             (SUT.ConfigValue (Just "hello") configSource)

    , testCase "when having a sub-configuration" <|
        let
          configSource =
            SUT.ConfigSources { SUT.envVar   = SUT.Pending (SUT.EnvVar "HELLO")
                              , SUT.optParse = SUT.Pending (SUT.OptParse "hello")
                              , SUT.file     = SUT.Pending SUT.File }
          configValue =
            SUT.ConfigValue (Just "hello") configSource

          subConfig =
            SUT.SubConfig <| Map.fromList [(":hello", configValue)]
        in
          assertFromEDN
            "does not work with sub-map"
            "{:hello #config/meta {:optparse \"hello\" :envvar \"HELLO\" :default \"hello\"}}"
            subConfig
    ]

tests :: TestTree
tests = configValueEDNParserTests
