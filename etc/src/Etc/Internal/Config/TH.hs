{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_HADDOCK hide #-}
module Etc.Internal.Config.TH where

import RIO
import qualified RIO.Map as Map
import qualified RIO.Text as Text

import Language.Haskell.TH        (ExpQ, runIO)

import qualified  Data.Text.Prettyprint.Doc as Doc
import qualified  Data.Text.Prettyprint.Doc.Util as Doc (reflow)

import Etc.Internal.Renderer
import Etc.Internal.Spec.Types as Spec
import Etc.Internal.Config as Config

data CheckedConfigValueError
  = UnexpectedSubConfigEntry
      !Spec.SpecFilePath
      !Spec.SpecEntryPath
      ![Text] -- Valid keys after the given SpecEntryPath
  | UnexpectedConfigValueEntry
      !Spec.SpecFilePath
      ![Text] -- Expected valid key path
      ![Text] -- Extra invalid keys
  | FoundUnknownKey
       !Spec.SpecFilePath
       ![Text] -- Correct Path
       ![Text] -- Expected valid keys
       !Text   -- Unknown key used instead
  deriving (Show)

instance HumanErrorMessage CheckedConfigValueError where
  humanErrorMessage err =
    case err of
      UnexpectedSubConfigEntry specFilepath keyPath childKeys ->
        foundError3
          "application error"
          (Doc.vsep
             [ Doc.reflow "I've found an invalid query in your configuration."
             , mempty
             , Doc.reflow "The given path:"
             , mempty
             , Doc.indent 2 $ Doc.prettyList keyPath
             , mempty
             , Doc.reflow
                 "Is declared as a map entry in the configuration spec file located at" Doc.<+>
               filePath specFilepath
             ])
          [ Doc.vsep
              [ "Try to use one of the following key paths in your config query call instead:"
              , Doc.indent 2 $
                Doc.vsep
                  (map
                     (\key -> "- " <> Doc.prettyList (keyPath <> [key]))
                     childKeys)
              ]
          , Doc.reflow "Modify entry definition to only contain the keys" Doc.<+>
            Doc.dquotes (Doc.pretty (Text.intercalate "." keyPath)) Doc.<+>
            Doc.reflow "in the configuration spec file"
          ]
      UnexpectedConfigValueEntry specFilepath expectedKeys extraInvalidKeys ->
        foundError3
          "application error"
          (Doc.vsep
             [ Doc.reflow "I've found an invalid query in your configuration."
             , mempty
             , Doc.reflow "The given path:"
             , mempty
             , Doc.indent 2 $ Doc.viaShow (expectedKeys <> extraInvalidKeys)
             , mempty
             , Doc.reflow "Is not defined in your configuration spec located at" Doc.<+>
               filePath specFilepath
             ])
          [ Doc.reflow "Remove the extra invalid keys" Doc.<+>
            Doc.viaShow extraInvalidKeys Doc.<+>
            "from your call"
          , Doc.vsep
              [ Doc.reflow
                  "Modify your entry definition to contain the extra keys like so:"
              , mempty
              , Doc.indent 2 $
                renderSpecKeyPath
                  (map Doc.pretty $ expectedKeys <> extraInvalidKeys)
                  (newlineBody "...")
              , mempty
              , Doc.reflow "in the configuration spec file"
              ]
          ]
      FoundUnknownKey specFilepath correctPath expectedKeys unknownKey ->
        foundError3
          "application error"
          (Doc.vsep
             [ Doc.reflow "I've found an invalid query in your configuration."
             , mempty
             , Doc.reflow "The given path:"
             , mempty
             , Doc.indent 2 $ Doc.viaShow (correctPath <> [unknownKey])
             , mempty
             , Doc.reflow "Is not defined in your configuration spec located at" Doc.<+>
               filePath specFilepath
             ])
          [ Doc.reflow "Remove the unknown key" Doc.<+>
            Doc.dquotes (Doc.pretty unknownKey) Doc.<+>
            "from your call"
          , Doc.vsep
              [ "Try to use one of the following key paths in your config query call instead:"
              , Doc.indent 2 $
                Doc.vsep
                  (map
                     (\key -> "- " <> Doc.viaShow (correctPath <> [key]))
                     expectedKeys)
              ]
          , Doc.reflow "Modify entry definition to only contain the keys" Doc.<+>
            Doc.dquotes
              (Doc.pretty (Text.intercalate "." (correctPath <> [unknownKey]))) Doc.<+>
            Doc.reflow "in the configuration spec file"
          ]

assertKeyPath :: [Text] -> ConfigSpec -> IO ()
assertKeyPath keyPath (ConfigSpec {configSpecFilePath, configSpecEntries}) =
  loop keyPath (Spec.SubConfig configSpecEntries)
  where
    loop [] (Spec.SubConfig hsh) =
      throwM
        (Spec.SpecError $
         UnexpectedSubConfigEntry configSpecFilePath keyPath (Map.keys hsh))
    loop [] _ = return ()
    loop ks (Spec.ConfigValue {}) =
      let expectedOnly = take (length keyPath - length ks) keyPath
          gotExtra = ks
       in throwM
            (Spec.SpecError $
             UnexpectedConfigValueEntry configSpecFilePath expectedOnly gotExtra)
    loop kss@(k:ks) (Spec.SubConfig hsh) =
      case Map.lookup k hsh of
        Just inner -> loop ks inner
        Nothing ->
          let correctPath = take (length keyPath - length kss) keyPath
              knownKeys = Map.keys hsh
              invalidKey = k
           in throwM
                (Spec.SpecError $
                 FoundUnknownKey
                   configSpecFilePath
                   correctPath
                   knownKeys
                   invalidKey)

-- | An enhanced version of 'Config.getConfigValue' which checks the given keys
-- are present in the configuration spec entries.
--
-- This invocation will fail at /compilation time/ if the given key path is not
-- present in the configuration spec.
--
-- ==== To keep in mind
--
-- This function requires the @TemplateHaskell@ GHC extension to work.
--
-- ==== Example
--
-- @
-- main = do
--   ...
--   configValue - $(checkedConfigValue configSpec [\"db\", \"password\"]) config
--   ...
-- @
--
-- @since 1.0.0.0
checkedConfigValue :: Spec.ConfigSpec -> [Text] -> ExpQ
checkedConfigValue spec keyPath = do
  runIO $ assertKeyPath keyPath spec
  let keyPathStr = map Text.unpack keyPath
  [|Config.getConfigValue (map Text.pack keyPathStr)|]
