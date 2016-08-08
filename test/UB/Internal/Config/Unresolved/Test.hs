{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module UB.Internal.Config.Unresolved.Test (tests) where

import UB.Prelude
import UB.Test.Util (assertFromEDN)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit  (testCase)

import qualified UB.Internal.Config.Unresolved as SUT

configValueEDNParserTests :: TestTree
configValueEDNParserTests =
  testGroup
    "ConfigValue EDN Parser"
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
    ]

tests :: TestTree
tests = configValueEDNParserTests
