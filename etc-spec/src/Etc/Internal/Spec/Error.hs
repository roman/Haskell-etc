{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Etc.Internal.Spec.Error where

import           RIO
import qualified RIO.List.Partial as List (last)
import qualified RIO.Text         as Text

import qualified Data.Aeson              as JSON
import qualified Data.Aeson.BetterErrors as JSON
import qualified Data.Yaml               as Yaml

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util (reflow)

import Etc.Internal.Renderer
import Etc.Internal.Spec.Types

--------------------------------------------------------------------------------
-- CannotInferTypeFromDefault

cannotInferTypeFromDefaultBody :: [Doc Ann] -> JSON.Value -> Doc Ann
cannotInferTypeFromDefaultBody keyPath jsonVal = vsep
  [ reflow "Cannot infer the type of an entry from its default value"
  , mempty
  , reflow "In the entry of the configuration spec file"
  , mempty
  , indent 2 $ renderSpecKeyPath keyPath $ newlineBody $ vsep
    [hsep ["default:", annotate Error $ pointed $ renderJsonValue jsonVal]]
  ]

renderCannotInferTypeFromDefault :: [Text] -> JSON.Value -> Doc Ann
renderCannotInferTypeFromDefault keyPath jsonVal = foundError3
  "configuration spec"
  (cannotInferTypeFromDefaultBody (map pretty keyPath) jsonVal)
  [ reflow "Add a \"type\" field in the configuration map of the"
    <+> dquotes (pretty (List.last keyPath))
    <+> "entry"
  ]

--------------------------------------------------------------------------------
-- InferredNestedArrayOnDefault

inferredNestedArrayOnDefaultBody :: [Doc Ann] -> JSON.Value -> Doc Ann
inferredNestedArrayOnDefaultBody keyPath jsonVal = vsep
  [ reflow "Detected a deeply nested array in the default value of an entry; this is not supported by the library"
  , mempty
  , reflow "In the entry of the configuration spec file"
  , mempty
  , indent 2 $ renderSpecKeyPath keyPath $ newlineBody $ vsep
    [hsep ["default:", newlineBody (annotate Error $ pointed $ renderJsonValue jsonVal)]]
  ]

renderInferredNestedArrayOnDefault :: [Text] -> JSON.Value -> Doc Ann
renderInferredNestedArrayOnDefault keyPath jsonVal = foundError3
  "configuration spec"
  (inferredNestedArrayOnDefaultBody (map pretty keyPath) jsonVal)
  [ reflow "Use a one level array value in the \"default\" field of the"
    <+> dquotes (pretty (List.last keyPath))
    <+> "entry"
  ]


--------------------------------------------------------------------------------
-- UnknownConfigValueType

unknownConfigValueTypeBody :: [Doc Ann] -> Doc Ann -> Doc Ann
unknownConfigValueTypeBody keyPath typeName = vsep
  [ reflow "Detected an entry with an unknown type"
  , mempty
  , reflow "In the entry of the configuration spec file"
  , mempty
  , indent 2 $ renderSpecKeyPath keyPath $ newlineBody $ vsep
    [hsep ["type:", annotate Error $ pointed typeName]]
  ]

renderUnknownConfigValueType :: [Text] -> Text -> Doc Ann
renderUnknownConfigValueType keyPath typeName = foundError3
  "configuration spec"
  (unknownConfigValueTypeBody (map pretty keyPath) (pretty typeName))
  [ reflow
      "Use one of the known types of the etc library; more information at: <PENDING_URL>"
  ]

--------------------------------------------------------------------------------
-- DefaultValueTypeMismatchFoundBody

defaultValueTypeMismatchFoundBody :: [Doc Ann] -> ConfigValueType -> JSON.Value -> Doc Ann
defaultValueTypeMismatchFoundBody keyPath cvType jsonVal = vsep
  [ reflow "There is a mistmach between the default value and the type of an entry"
  , mempty
  , reflow "In the entry of the configuration spec file"
  , indent 2 $ renderSpecKeyPath keyPath $ newlineBody $ vsep
    [ hsep ["type:", annotate Expected $ pointed $ renderConfigValueType cvType]
    , hsep ["default:", annotate Current $ pointed $ renderJsonValue jsonVal]
    ]
  , mempty
  , "The"
  <+> annotate Current "default value"
  <+> "(in"
  <+> annotate CurrentColorName mempty
  <>  ")"
  <+> "does not match the"
  <+> annotate Expected "expected type"
  <+> "(in"
  <+> annotate ExpectedColorName mempty
  <>  ")"
  ]

renderDefaultValueTypeMismatchFound :: [Text] -> ConfigValueType -> JSON.Value -> Doc Ann
renderDefaultValueTypeMismatchFound keyPath cvType jsonVal = foundError3
  "configuration spec"
  (defaultValueTypeMismatchFoundBody (map pretty keyPath) cvType jsonVal)
  [ reflow "Change the default value to match the given type"
    <+> dquotes (renderConfigValueType cvType)
  , case renderJsonType jsonVal of
    Just jsonTyDoc ->
      "Change the type to" <+> dquotes jsonTyDoc <+> "to match the given default value"
    Nothing -> mempty
  ]

--------------------------------------------------------------------------------
-- RedundantKeysOnValueSpec

redundantKeysOnValueSpecBody :: [Doc Ann] -> [Doc Ann] -> Doc Ann
redundantKeysOnValueSpecBody keyPath siblingKeys = vsep
  [ reflow "Detected an entry with extra keys at the \"etc/spec\" level"
  , mempty
  , reflow "In the entry of the configuration spec file"
  , mempty
  , indent 2
  $  renderKeyPathBody ("etc/entries" : keyPath)
  $  newlineBody
  $  vsep
  $  "etc/spec: ..."
  : map (\k -> annotate Error (k <> ": ...")) siblingKeys
  , mempty
  ]

renderRedundantKeysOnValueSpec :: [Text] -> [Text] -> Doc Ann
renderRedundantKeysOnValueSpec keyPath siblingKeys =
  let siblingKeyDocs = map pretty siblingKeys
  in  foundError3
        "configuration spec"
        (redundantKeysOnValueSpecBody (map pretty keyPath) siblingKeyDocs)
        [ reflow "Remove all keys that are siblings of the \"etc/spec\" (e.g."
          <+> hsep (punctuate comma $ map dquotes siblingKeyDocs)
          <>  ")"
        ]

--------------------------------------------------------------------------------
-- InvalidSpecEntriesBody

renderInvalidSpecEntries :: ConfigValue -> Doc Ann
renderInvalidSpecEntries _configValue = foundError3
  "configuration spec"
  (reflow "Detected an invalid \"etc/entries\" value in your configuration spec file")
  [ reflow
      "You can find more information about what values \"etc/entries\" can have at <PENDING_URL>"
  ]

--------------------------------------------------------------------------------

instance HumanErrorMessage SpecParserError where
  humanErrorMessage err =
    case err of
      CannotInferTypeFromDefault ks val -> renderCannotInferTypeFromDefault ks val
      UnknownConfigValueType ks tyName -> renderUnknownConfigValueType ks tyName
      RedundantKeysOnValueSpec ks other -> renderRedundantKeysOnValueSpec ks other
      InvalidSpecEntries configValue -> renderInvalidSpecEntries configValue
      InferredNestedArrayOnDefault ks val ->
        renderInferredNestedArrayOnDefault ks val
      DefaultValueTypeMismatchFound ks cvt val ->
        renderDefaultValueTypeMismatchFound ks cvt val

instance (Show ex, HumanErrorMessage ex) => HumanErrorMessage (JSON.ParseError ex) where
  humanErrorMessage parseErr =
    case parseErr of
       JSON.InvalidJSON msg -> renderInvalidJsonError msg
       JSON.BadSchema ks errSpecifics ->
         case errSpecifics of
           JSON.CustomError err -> humanErrorMessage err
           _ ->
             foundError3
               "JSON Parser"
               (vsep ["In the following entry:"
                     , mempty
                     , indent 2 $ renderPathPieces ks
                     , mempty
                     , "The aeson library returned reported the following error:"
                     , mempty
                     , indent 2 $ reflow $ Text.pack $ show errSpecifics
                     ]
               )
               []

instance HumanErrorMessage Yaml.ParseException where
  humanErrorMessage yamlErr =
    foundError2
      "YAML Parser"
      (vsep
         [ reflow "Got an error from the yaml library, it reported the following error:"
         , mempty
         , indent 4 $ pretty $ Yaml.prettyPrintParseException yamlErr
         ])
