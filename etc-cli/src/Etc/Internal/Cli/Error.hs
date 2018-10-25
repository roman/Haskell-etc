{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Etc.Internal.Cli.Error where

import RIO

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util (reflow)

import Etc.Internal.Renderer
import Etc.Resolver (HumanErrorMessage(..))
import Etc.Internal.Cli.Types
import Etc.Internal.Spec.Types (ConfigValueType)

renderMissingOptName :: Text -> [Text] -> Doc Ann
renderMissingOptName specFilePath keyPath =
  foundError3
    "cli resolver"
    (vsep
       [ reflow "I detected an invalid CLI configuration for an entry"
       , mempty
       , reflow "The configuration spec file located at" <+>
         filePath specFilePath <+> reflow "has the following entry:"
       , mempty
       , indent 2 $
         renderSpecKeyPath (map pretty keyPath) $
         newlineBody $
         hang 2 $ vsep [annotate Current $ "cli:", "input: option", "..."]
       , mempty
       , reflow "The" <+>
         annotate Current "cli" <+>
         reflow "entry must have at least one" <+>
         annotate Expected "short" <+>
         "or" <+> annotate Expected "long" <+> "attribute"
       ])
    [ reflow "Add a \"short\" or a \"long\" attribute to the" <+>
      annotate Current "cli" <+> "map above"
    ]

invalidInputName :: Text -> [Text] -> Text -> Doc Ann
invalidInputName specFilePath keyPath givenInputName =
  foundError3
    "cli resolver"
    (vsep
       [ reflow "I detected an invalid CLI configuration for an entry"
       , mempty
       , reflow "The configuration spec file located at" <+>
         filePath specFilePath <+> reflow "has the following entry:"
       , mempty
       , indent 2 $
         renderSpecKeyPath (map pretty keyPath) $
         newlineBody $
         hang 2 $
         vsep
           [ "cli:"
           , annotate Error $ "input:" <+> pointed (pretty givenInputName)
           ]
       , mempty
       , reflow "The" <+>
         dquotes "input" <+> reflow "attribute of the cli entry is not valid"
       ])
    [ vsep
        [ reflow "Add one of the following values to the" <+> dquotes "input" <+> "attribute:"
        , indent 2 (vsep ["- option", "- argument", "- switch"])
        ]
    ]

infoModMissing :: Text -> Doc Ann
infoModMissing specFilePath =
  foundError3
    "cli resolver"
    (vsep
       [ reflow "I detected an error in the configuration spec file located at" <+>
         filePath specFilePath
       , mempty
       , "The required" <+> dquotes "etc/cli" <+> "entry is missing"
       ])
    [ reflow "Add a top-level entry" <+>
      dquotes "etc/cli" <+> "in the configuration spec file"
    , reflow "Drop the usage of cliResolver or pureCliResolver from the code"
    ]

switchIncompatibleType :: Text -> [Text] -> ConfigValueType -> Doc Ann
switchIncompatibleType specFilePath keyPath cvType =
  foundError3
    "cli resolver"
    (vsep
       [ reflow "I detected an invalid CLI configuration for an entry"
       , mempty
       , reflow "The configuration spec file located at" <+>
         filePath specFilePath <+> reflow "has the following entry:"
       , mempty
       , indent 2 $
         renderSpecKeyPath (map pretty keyPath) $
         newlineBody $
         hang 2 $
         vsep
           [ "cli:"
           , annotate Error $ "type:" <+> pointed (renderConfigValueType cvType)
           ]
       , mempty
       , reflow "The" <+>
         dquotes "type" <+>
         reflow "attribute of the cli entry should be" <+> dquotes "boolean"
       ])
    [reflow "Replace the entry's type to boolean"]


instance HumanErrorMessage CliResolverError where
  humanErrorMessage err =
    case err of
      MissingOptName specFilePath keyPath ->
        renderMissingOptName specFilePath keyPath
      InvalidInputName givenInputName specFilePath keyPath ->
        invalidInputName specFilePath keyPath givenInputName
      SwitchIncompatibleType cvType specFilePath keyPath ->
        switchIncompatibleType specFilePath keyPath cvType
      InfoModMissing specFilePath -> infoModMissing specFilePath
      PlainInfoModExpected _specFilePath -> pretty $ show err
      CommandInfoModExpected _specFilePath -> pretty $ show err
      CommandListMismatch _specFilePath _missingCommands _unknownCommands ->
        pretty $ show err
      UnknownCommandOnEntry _specFilePath _keyPath _validCommands _unknownCommands ->
        pretty $ show err
