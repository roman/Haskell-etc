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

cannotInferTypeFromDefaultBody :: Doc Ann -> [Doc Ann] -> JSON.Value -> Doc Ann
cannotInferTypeFromDefaultBody sourceName keyPath jsonVal =
  vsep
    [ reflow "Cannot infer the type of an entry from its default value"
    , mempty
    , reflow "In the entry of the configuration spec file located at" <+>
      sourceName
    , mempty
    , indent 2 $
      renderSpecKeyPath keyPath $
      newlineBody $
      vsep
        [hsep ["default:", annotate Error $ pointed $ renderJsonValue jsonVal]]
    ]

renderCannotInferTypeFromDefault :: Text -> [Text] -> JSON.Value -> Doc Ann
renderCannotInferTypeFromDefault sourceName keyPath jsonVal = foundError3
  "configuration spec"
  (cannotInferTypeFromDefaultBody (pretty sourceName) (map pretty keyPath) jsonVal)
  [ reflow "Add a \"type\" field in the configuration map of the"
    <+> dquotes (pretty (List.last keyPath))
    <+> "entry"
  ]

--------------------------------------------------------------------------------
-- InferredNestedArrayOnDefault

inferredNestedArrayOnDefaultBody :: Doc Ann -> [Doc Ann] -> JSON.Value -> Doc Ann
inferredNestedArrayOnDefaultBody sourceName keyPath jsonVal = vsep
  [ reflow "Detected a deeply nested array in the default value of an entry; this is not supported by the library"
  , mempty
  , reflow "In the entry of the configuration spec file located at" <+> sourceName
  , mempty
  , indent 2 $ renderSpecKeyPath keyPath $ newlineBody $ vsep
    [hsep ["default:", newlineBody (annotate Error $ pointed $ renderJsonValue jsonVal)]]
  ]

renderInferredNestedArrayOnDefault :: Text -> [Text] -> JSON.Value -> Doc Ann
renderInferredNestedArrayOnDefault sourceName keyPath jsonVal = foundError3
  "configuration spec"
  (inferredNestedArrayOnDefaultBody (pretty sourceName) (map pretty keyPath) jsonVal)
  [ reflow "Use a one level array value in the \"default\" field of the"
    <+> dquotes (pretty (List.last keyPath))
    <+> "entry"
  ]


--------------------------------------------------------------------------------
-- UnknownConfigValueType

unknownConfigValueTypeBody :: Doc Ann -> [Doc Ann] -> Doc Ann -> Doc Ann
unknownConfigValueTypeBody sourceName keyPath typeName =
  vsep
    [ reflow "Detected an entry with an unknown type"
    , mempty
    , reflow "In the entry of the configuration spec file located at" <+>
      sourceName
    , mempty
    , indent 2 $
      renderSpecKeyPath keyPath $
      newlineBody $ vsep [hsep ["type:", annotate Error $ pointed typeName]]
    ]

renderUnknownConfigValueType :: Text -> [Text] -> Text -> Doc Ann
renderUnknownConfigValueType sourceName keyPath typeName = foundError3
  "configuration spec"
  (unknownConfigValueTypeBody (pretty sourceName) (map pretty keyPath) (pretty typeName))
  [ reflow
      "Use one of the known types of the etc library; more information at: <PENDING_URL>"
  ]

--------------------------------------------------------------------------------
-- DefaultValueTypeMismatchFoundBody

defaultValueDoc :: Doc Ann
defaultValueDoc = annotate Current "default value"

expectedTypeDoc :: Doc Ann
expectedTypeDoc = annotate Expected "expected type"

defaultValueTypeMismatchFoundBody :: Doc Ann -> [Doc Ann] -> ConfigValueType -> JSON.Value -> Doc Ann
defaultValueTypeMismatchFoundBody sourceName keyPath cvType jsonVal =
  vsep
    [ reflow "I detected a mistmach between the" <+>
      expectedTypeDoc <+> "and the" <+> defaultValueDoc <+> reflow "of an entry"
    , mempty
    , reflow "In the entry of the configuration spec file located at" <+> sourceName
    , mempty
    , indent 2 $
      renderSpecKeyPath keyPath $
      newlineBody $
      vsep
        [ hsep
            [ "type:"
            , annotate Expected $ pointed $ renderConfigValueType cvType
            ]
        , hsep
            ["default:", annotate Current $ pointed $ renderJsonValue jsonVal]
        ]
    ]

renderDefaultValueTypeMismatchFound :: Text -> [Text] -> ConfigValueType -> JSON.Value -> Doc Ann
renderDefaultValueTypeMismatchFound sourceName keyPath cvType jsonVal =
  foundError3
    "configuration spec"
    (defaultValueTypeMismatchFoundBody (pretty sourceName) (map pretty keyPath) cvType jsonVal)
    [ reflow "Change the" <+>
      defaultValueDoc <+>
      "to match the given type" <+>
      annotate Expected (dquotes (renderConfigValueType cvType))
    , case renderJsonType jsonVal of
        Just jsonTyDoc ->
          reflow "Change the" <+>
          expectedTypeDoc <+>
          "to" <+>
          annotate Expected (dquotes jsonTyDoc) <+>
          reflow "to match the given default value"
        Nothing -> mempty
    ]

--------------------------------------------------------------------------------
-- RedundantKeysOnValueSpec

redundantKeysOnValueSpecBody :: Doc Ann -> [Doc Ann] -> [Doc Ann] -> Doc Ann
redundantKeysOnValueSpecBody sourceName keyPath siblingKeys =
  vsep
    [ reflow "Detected an entry with extra keys at the \"etc/spec\" level"
    , mempty
    , reflow "In the entry of the configuration spec file located at" <+>
      sourceName
    , mempty
    , indent 2 $
      renderKeyPathBody ("etc/entries" : keyPath) $
      newlineBody $
      vsep $
      "etc/spec: ..." : map (\k -> annotate Error (k <> ": ...")) siblingKeys
    , mempty
    ]

renderRedundantKeysOnValueSpec :: Text -> [Text] -> [Text] -> Doc Ann
renderRedundantKeysOnValueSpec sourceName keyPath siblingKeys =
  let siblingKeyDocs = map pretty siblingKeys
   in foundError3
        "configuration spec"
        (redundantKeysOnValueSpecBody
           (pretty sourceName)
           (map pretty keyPath)
           siblingKeyDocs)
        [ reflow "Remove all keys that are siblings of the \"etc/spec\" (e.g." <+>
          hsep (punctuate comma $ map dquotes siblingKeyDocs) <> ")" <+>
          "in the configuration spec file"
        ]

--------------------------------------------------------------------------------
-- InvalidSpecEntriesBody

renderInvalidSpecEntries :: Text -> ConfigValue -> Doc Ann
renderInvalidSpecEntries sourceName _configValue =
  foundError3
    "configuration spec"
    (reflow
       "Detected an invalid \"etc/entries\" value in the configuration spec file located at" <+>
     pretty sourceName)
    [ reflow
        "You can find more information about what values \"etc/entries\" can have at <PENDING_URL>"
    ]

--------------------------------------------------------------------------------

instance HumanErrorMessage SpecParserError where
  humanErrorMessage err =
    case err of
      CannotInferTypeFromDefault sourceName ks val -> renderCannotInferTypeFromDefault sourceName ks val
      UnknownConfigValueType sourceName ks tyName -> renderUnknownConfigValueType sourceName ks tyName
      RedundantKeysOnValueSpec sourceName ks other -> renderRedundantKeysOnValueSpec sourceName ks other
      InvalidSpecEntries sourceName configValue -> renderInvalidSpecEntries sourceName configValue
      InferredNestedArrayOnDefault sourceName ks val ->
        renderInferredNestedArrayOnDefault sourceName ks val
      DefaultValueTypeMismatchFound sourceName ks cvt val ->
        renderDefaultValueTypeMismatchFound sourceName ks cvt val

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
