{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Etc.Resolver.FileTest (tests) where

import           RIO
import qualified RIO.Set            as Set
import qualified RIO.Text           as Text
import qualified RIO.Vector         as Vector
import qualified RIO.Vector.Partial as Vector (head)

import Data.Typeable (cast)

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Data.Aeson as JSON
import           Paths_etc  (getDataFileName)

import System.Environment (setEnv)

import System.Etc
import System.Etc.Internal.Types (FileSource (..), FileValueOrigin (..))

tests :: TestTree
tests = testGroup
  "files"
  [ testCase "fail if using both `etc/files` and `etc/filepaths`" $ do
    let input = "{\"etc/files\": {}, \"etc/filepaths\": []}"

    (espec :: Either SpecInvalidSyntaxFound (ConfigSpec ())) <- try $ parseConfigSpec input
    case espec of
      Left _ -> return ()
      _ ->
        assertFailure ("Expecting InvalidConfigurationError; got instead " <> show espec)
  , testCase "fails when file has key not defined in spec" $ do
    jsonFilepath <- getDataFileName "test/fixtures/config.json"
    let input = "{\"etc/filepaths\": [\"" <> Text.pack jsonFilepath <> "\"]}"
    (spec :: ConfigSpec ()) <- parseConfigSpec input
    (_config, warnings)     <- resolveFiles spec
    assertBool "There should be warnings" (not $ null warnings)
    case fromException (Vector.head warnings) of
      Just (UnknownConfigKeyFound _ keyName _) ->
        assertEqual "Expecting key not present in spec error" "greeting" keyName

      err ->
        assertFailure
          $  "Expecting InvalidConfigurationError; got other error instead: "
          <> show err
  , testCase "fails when file has key with value different from spec" $ do
    jsonFilepath <- getDataFileName "test/fixtures/config.json"
    let
      input =
        "{\"etc/filepaths\": [\""
          <> Text.pack jsonFilepath
          <> "\"], \"etc/entries\": { \"greeting\": {\"etc/spec\": { \"type\": \"number\" }}}}"
    (spec :: ConfigSpec ()) <- parseConfigSpec input
    (_config, warnings)     <- resolveFiles spec
    assertBool "Expecting warnings in the resolveFiles call" (Vector.length warnings > 0)
    let err = Vector.head warnings
    case fromException err of
      Just (ConfigValueTypeMismatchFound keyName _ _) ->
        -- TODO: This should return "greeting" instead
        assertEqual "Expecting config value type mismatch error" "" keyName

      _ ->
        assertFailure
          $  "Expecting InvalidConfigurationError; got other error instead: "
          <> show err
  , filePathsTests
  , filesTest
  ]


filePathsTests :: TestTree
filePathsTests = testGroup
  "etc/filepaths"
  [ testCase "supports json, yaml and yml extensions" $ do
    jsonFilepath <- getDataFileName "test/fixtures/config.json"
#ifdef WITH_YAML
    yamlFilepath <- getDataFileName "test/fixtures/config.yaml"
    ymlFilepath  <- getDataFileName "test/fixtures/config.yml"
#endif

    let input = mconcat
          [ "{\"etc/filepaths\": ["
          , "\"" <> Text.pack jsonFilepath <> "\""
#ifdef WITH_YAML
            , ", \"" <> Text.pack yamlFilepath <> "\""
            , ", \"" <> Text.pack ymlFilepath <> "\""
#endif
          , "]"
          , ", \"etc/entries\": {\"greeting\": \"hello default\"}}"
          ]

    (spec :: ConfigSpec ()) <- parseConfigSpec input
    (config, _)             <- resolveFiles spec

    case getAllConfigSources ["greeting"] config of
      Nothing -> assertFailure
        ("expecting to get entries for greeting (check fixtures)\n" <> show config)
      Just aSet -> assertBool
        ("expecting to see entry from json config file " <> show aSet)
        (Set.member
          ( SomeConfigSource 1
          $ FileSource 1 (ConfigFileOrigin $ Text.pack jsonFilepath) "hello json"
          )
          aSet
        )

#ifdef WITH_YAML
       >> assertBool ("expecting to see entry from yaml config file " <> show aSet)
                     (Set.member (SomeConfigSource 1 $ FileSource 2 (ConfigFileOrigin $ Text.pack jsonFilepath) "hello yaml") aSet)

       >> assertBool ("expecting to see entry from yml config file " <> show aSet)
                     (Set.member (SomeConfigSource 1 $ FileSource 3 (ConfigFileOrigin $ Text.pack jsonFilepath) "hello yml") aSet)
#endif
  , testCase "does not support any other file extension" $ do
    fooFilepath <- getDataFileName "test/fixtures/config.foo"
    let input =
          mconcat ["{\"etc/filepaths\": [", "\"" <> Text.pack fooFilepath <> "\"", "]}"]

    (spec :: ConfigSpec ()) <- parseConfigSpec input
    (config, errs)          <- resolveFiles spec

    assertEqual "config should be empty" mempty config

    if Vector.null errs
      then assertFailure "expecting one error, got none"
      else
        let err = do
              e <- errs Vector.!? 0
              fromException e
        in
          case err of
            Just UnsupportedFileExtensionGiven{} -> return ()
            _ -> assertFailure
              ("Expecting UnsupportedFileExtensionGiven; got instead " <> show err)
  , testCase "does not fail if file doesn't exist" $ do
    jsonFilepath <- getDataFileName "test/fixtures/config.json"
    let input = mconcat
          [ "{\"etc/filepaths\": ["
          , "\"" <> Text.pack jsonFilepath <> "\""
          , ", \"unknown_file.json\""
          , "]"
          , ", \"etc/entries\":{\"greeting\":\"hello default\"}"
          , "}"
          ]

    (spec :: ConfigSpec ()) <- parseConfigSpec input
    (config, errs)          <- resolveFiles spec

    case getAllConfigSources ["greeting"] config of
      Nothing -> assertFailure
        ("expecting to get entries for greeting (check fixtures)\n" <> show config)
      Just aSet -> assertBool
        ("expecting to see entry from json config file " <> show aSet)
        (Set.member
          ( SomeConfigSource 1
          $ FileSource 1 (ConfigFileOrigin $ Text.pack jsonFilepath) "hello json"
          )
          aSet
        )

    if Vector.null errs
      then assertFailure "expecting one error, got none"
      else
        let err = do
              e <- errs Vector.!? 0
              fromException e
        in
          case err of
            Just (ConfigurationFileNotFound _) -> return ()
            _ -> assertFailure
              ("Expecting ConfigurationFileNotFound; got instead " <> show err)
  , testCase "null file input has no precedence over default values" $ do
    jsonFilepath <- getDataFileName "test/fixtures/config.null.json"
    let input = mconcat
          [ "{\"etc/filepaths\": ["
          , "\"" <> Text.pack jsonFilepath <> "\""
          , "]"
          , ", \"etc/entries\": {"
          , "    \"greeting\": {"
          , "      \"etc/spec\": {"
          , "        \"default\": \"hola\""
          , "}}}}"
          ]

    (spec :: ConfigSpec ()) <- parseConfigSpec input
    (configFiles, _errs)    <- resolveFiles spec
    let configDef = resolveDefault spec
        config    = configDef <> configFiles

    case getAllConfigSources ["greeting"] config of
      Nothing -> assertFailure ("expecting to get entries for greeting\n" <> show config)
      Just aSet ->
        let result = any
              (\(SomeConfigSource _ source) -> case cast source of
                Just (FileSource _ _ (Plain JSON.Null)) -> True
                _                                       -> False
              )
              aSet
        in  assertBool ("expecting to see entry from env; got " <> show aSet) result

    case getConfigValue ["greeting"] config of
      Just greeting -> assertEqual "Didn't take default value, despite CLI being null"
                                   ("hola" :: Text)
                                   greeting
      Nothing -> assertFailure "Expecting config value, but got nothing"
  ]

filesTest :: TestTree
filesTest = testGroup
  "etc/files"
  [ testCase "at least the `env` or `paths` keys must be present" $ do
    let input = "{\"etc/files\": {}}"

    (espec :: Either SpecInvalidSyntaxFound (ConfigSpec ())) <- try $ parseConfigSpec input
    case espec of
      Left _ -> return ()
      _ -> assertFailure ("Expecting SpecInvalidSyntaxFound; got instead " <> show espec)
  , testCase "environment variable has precedence over all others" $ do
    jsonFilepath <- getDataFileName "test/fixtures/config.json"
    envFilePath  <- getDataFileName "test/fixtures/config.env.json"
    let input = mconcat
          [ "{\"etc/files\":{"
          , "  \"env\": \"ENV_FILE_TEST\","
          , "  \"paths\": [\"" <> Text.pack jsonFilepath <> "\"]"
          , "},"
          , "\"etc/entries\":{\"greeting\":\"hello default\"}}"
          ]

        envFileTest = "ENV_FILE_TEST"

    setEnv (Text.unpack envFileTest) envFilePath

    (spec :: ConfigSpec ()) <- parseConfigSpec input
    (config, _)             <- resolveFiles spec

    case getAllConfigSources ["greeting"] config of
      Nothing -> assertFailure
        ("expecting to get entries for greeting (check fixtures)\n" <> show config)
      Just aSet -> do
        assertBool
          ("expecting to see entry from env config file " <> show aSet)
          (Set.member
            (SomeConfigSource 1 $ FileSource
              1
              (EnvFileOrigin envFileTest $ Text.pack envFilePath)
              "hello environment"
            )
            aSet
          )
        assertBool
          ("expecting to see entry from json config file " <> show aSet)
          (Set.member
            ( SomeConfigSource 1
            $ FileSource 2 (ConfigFileOrigin $ Text.pack jsonFilepath) "hello json"
            )
            aSet
          )
  ]
