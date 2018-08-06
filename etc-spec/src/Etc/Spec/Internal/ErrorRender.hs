{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Etc.Spec.Internal.ErrorRender where

import RIO
import qualified RIO.List as List

import qualified Data.Aeson.BetterErrors as JSON (PathPiece(..), ParseError(..), ErrorSpecifics(..))
import qualified Data.Aeson as JSON

import Data.Text.Prettyprint.Doc ((<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.String as Pretty (renderString)
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)

import Etc.Spec.Internal.Types
import Etc.Spec.Internal.Serializer ()

docToString :: Int -> Pretty.Doc ann -> String
docToString w doc = Pretty.renderString (Pretty.layoutPretty layoutOptions (Pretty.unAnnotate doc))
  where
        layoutOptions = Pretty.LayoutOptions { Pretty.layoutPageWidth = Pretty.AvailablePerLine w 1 }

renderInEtcSpecSiblings :: Pretty.Pretty a => [a] -> Pretty.Doc ann -> Pretty.Doc ann
renderInEtcSpecSiblings ks etcSpecDoc =
  case ks of
    [] ->
      Pretty.vsep [ "etc/spec:" <+> "..."
                  , etcSpecDoc
                  ]
    (k:ks1) ->
      Pretty.hang
        2
        (Pretty.vsep [Pretty.pretty k <> ":", renderInEtcSpecSiblings ks1 etcSpecDoc])

renderInEtcSpecBody :: Pretty.Pretty a => [a] -> Pretty.Doc ann -> Pretty.Doc ann
renderInEtcSpecBody ks etcSpecDoc =
  case ks of
    [] -> Pretty.hang 2 $ "etc/spec:" <> Pretty.hardline <> etcSpecDoc
    (k:ks1) ->
      Pretty.hang
        2
        (Pretty.vsep [Pretty.pretty k <> ":", renderInEtcSpecBody ks1 etcSpecDoc])


renderErrorFormat2 ::
  Pretty.Doc ann -> Pretty.Doc ann -> Pretty.Doc ann
renderErrorFormat2 header body =
  Pretty.vsep $ [header, "", body]

renderErrorFormat3 ::
  Pretty.Doc ann
  -> Pretty.Doc ann -> Pretty.Doc ann -> Pretty.Doc ann
renderErrorFormat3 header body bottom =
  Pretty.vsep $ [header, "", body, "", bottom]

renderErrorMessage_ ::
  (a -> b -> Pretty.Doc ann)
  -> a -> Pretty.Doc ann -> b -> Pretty.Doc ann -> Pretty.Doc ann
renderErrorMessage_ renderer keyPath errorMessage bodyMessage solutionMessage =
  renderErrorFormat3 topMsg middleMsg solutionMsg
  where
    topMsg =
      Pretty.hang 1 $
      Pretty.vsep
        ["Found error:" <> Pretty.hardline, errorMessage]

    middleMsg =
      Pretty.hang 2 $
      Pretty.vsep
        [ "In the configuration:" <> Pretty.hardline
        , Pretty.hang
            2
          ("etc/entries:" <> Pretty.hardline <>
             renderer keyPath bodyMessage)
        ]

    solutionMsg =
      Pretty.hang 1 $
      Pretty.vsep ["Possible solution:" <> Pretty.hardline, solutionMessage, ""]

renderErrorMessage ::
     Pretty.Pretty a
  => [a]
  -> Pretty.Doc ann
  -> Pretty.Doc ann
  -> Pretty.Doc ann
  -> Pretty.Doc ann
renderErrorMessage = renderErrorMessage_ renderInEtcSpecBody

cannotInferTypeFromDefault ::
     (Pretty.Pretty a) => [a] -> JSON.Value -> Pretty.Doc ann
cannotInferTypeFromDefault keyPath defaultValue =
  renderErrorMessage
    keyPath
    (Pretty.reflow "Could not infer the type of an entry")
    ("default:" Pretty.<+>
     Pretty.pretty
       (decodeUtf8Lenient $ toStrictBytes $ JSON.encode defaultValue))
    (if defaultValue == JSON.Null
       then Pretty.reflow
              "I cannot infer the type of your configuration entry when the \"default\" value is null, consider changing the \"default\" value or adding an explicit \"type\" attribute to the \"etc/spec\" map"
       else Pretty.reflow
              "Add an explicit \"type\" entry in the \"etc/spec\" map")

inferredNestedArrayOnDefault ::
  (JSON.ToJSON def) => [Text] -> def -> Pretty.Doc ann
inferredNestedArrayOnDefault keyPath defaultValue =
  renderErrorMessage
    keyPath
    (Pretty.reflow "Detected a default value with a double nested array type")
    ("default:" Pretty.<+> Pretty.pretty
       (decodeUtf8Lenient $ toStrictBytes $ JSON.encode defaultValue))
    (Pretty.reflow
       "Don't have a value wrapped in a two square brackets as the \"default\" value")

invalidConfigValueType ::
  (Show def) => [Text] -> def -> Pretty.Doc ann
invalidConfigValueType keyPath typeName =
  renderErrorMessage
    keyPath
    (Pretty.reflow "Detected an unrecognized type")
    ("type:" Pretty.<+> Pretty.pretty (show typeName))
    (Pretty.reflow
       "Don't have a value wrapped in a two square brackets as the \"default\" value")

configValueDefaultTypeMismatchFound ::
     (JSON.ToJSON ty, JSON.ToJSON def) => [Text] -> ty -> def -> Pretty.Doc ann
configValueDefaultTypeMismatchFound keyPath cvType defValue =
  renderErrorMessage
    keyPath
    (Pretty.reflow
       "Detected a mismatch between a given type and a default value")
    (Pretty.vsep
       [ "type:" Pretty.<+>
         Pretty.pretty (decodeUtf8Lenient $ toStrictBytes $ JSON.encode cvType)
       , "default:" Pretty.<+>
         Pretty.pretty
           (decodeUtf8Lenient $ toStrictBytes $ JSON.encode defValue)
       ])
    (Pretty.reflow
       "Make sure the given default value and the specified type match")

redundantKeysOnValueSpec :: (Pretty.Pretty k) => [Text] -> [k] -> Pretty.Doc ann
redundantKeysOnValueSpec keyPath unknownKeys =
  renderErrorMessage_
    renderInEtcSpecSiblings
    keyPath
    (Pretty.reflow "Detected invalid sibling fields to map containing etc/spec key")
    (Pretty.vsep $ map (\k -> Pretty.pretty k <> ":" <+> "...") unknownKeys)
    (Pretty.reflow "Make sure you remove all the keys that are not etc/spec")

invalidSpecEntries :: ConfigValue -> Pretty.Doc ann
invalidSpecEntries _val =
  renderErrorFormat2
    "Found error: etc/entries does not contain map"
    "Solution: only use a map in the \"etc/entries\" attribute of the configuration"

invalidJsonError :: Pretty.Pretty a => a -> Pretty.Doc ann
invalidJsonError msg =
  renderErrorFormat2 "Found error: JSON Syntax is invalid" (Pretty.pretty msg)

pathPieces :: [JSON.PathPiece] -> Pretty.Doc ann
pathPieces ks =
    mconcat $ List.intercalate ["."] (map (return . pathPiece) ks)
  where
    pathPiece (JSON.ObjectKey k) = Pretty.pretty k
    pathPiece (JSON.ArrayIndex i) = "[" <> Pretty.pretty i <> "]"

badSchemaError ::
  [JSON.PathPiece]
  -> JSON.ErrorSpecifics SpecParserError -> Pretty.Doc ann
badSchemaError ks0 errSpecifics =
  case errSpecifics of
    JSON.CustomError err ->
      case err of
        CannotInferTypeFromDefault ks val -> cannotInferTypeFromDefault ks val
        InvalidConfigValueType ks tyName -> invalidConfigValueType ks tyName
        RedundantKeysOnValueSpec ks other -> redundantKeysOnValueSpec ks other
        InvalidSpecEntries configVal -> invalidSpecEntries configVal
        InferredNestedArrayOnDefault ks val ->
          inferredNestedArrayOnDefault ks val
        ConfigValueTypeMismatchFound ks cvt val ->
          configValueDefaultTypeMismatchFound ks cvt val
    _ ->
      renderErrorFormat2
        ("Found error: JSON parser failed in " <> pathPieces ks0)
        (Pretty.pretty $ show errSpecifics)

instance Exception SpecParserError where
  displayException err =
    docToString 80 $
      case err of
        CannotInferTypeFromDefault ks val -> cannotInferTypeFromDefault ks val
        InvalidConfigValueType ks tyName -> invalidConfigValueType ks tyName
        RedundantKeysOnValueSpec ks other -> redundantKeysOnValueSpec ks other
        InvalidSpecEntries configVal -> invalidSpecEntries configVal
        InferredNestedArrayOnDefault ks val ->
          inferredNestedArrayOnDefault ks val
        ConfigValueTypeMismatchFound ks cvt val ->
          configValueDefaultTypeMismatchFound ks cvt val

instance Exception err => Exception (SpecError err) where
  displayException (SpecError specErr) =
    "\n\n" <>
    (case specErr of
       JSON.InvalidJSON msg -> docToString 80 $ invalidJsonError msg
       JSON.BadSchema ks errSpecifics ->
         case errSpecifics of
           JSON.CustomError err -> displayException err
           _ ->
             docToString 80 $
             renderErrorFormat2
               ("Found error: JSON parser failed in " <> pathPieces ks)
               (Pretty.pretty $ show errSpecifics))
