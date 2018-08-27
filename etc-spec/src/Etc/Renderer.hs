{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Etc.Renderer where

import RIO
import qualified RIO.HashMap as HashMap
import qualified RIO.List as List
import qualified RIO.Text as Text
import qualified RIO.Vector as Vector
import qualified RIO.Vector.Partial as Vector (head)

import qualified Data.Aeson as JSON
import qualified Data.Aeson.BetterErrors as JSON

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Symbols.Unicode (bullet)
import Data.Text.Prettyprint.Doc.Util (reflow)
import Data.Text.Prettyprint.Doc.Render.Util.StackMachine

import Etc.Spec.Internal.Types

--------------------------------------------------------------------------------

data Ann
  = ErrorTitle
  | SolutionsTitle
  | Current
  | CurrentColorName
  | Expected
  | ExpectedColorName
  | Error
  | Filepath
  | Envvar

--------------------------------------------------------------------------------
-- General Purpose

renderKeyPathBody :: [Doc ann] -> Doc ann -> Doc ann
renderKeyPathBody keyPath body =
  case keyPath of
    [] ->
      mempty
    [k] ->
      hsep [k <> ":", body]
    (k:ks) ->
      hang 2 (vsep [k <> ":", renderKeyPathBody ks body])

newlineBody :: Doc ann -> Doc ann
newlineBody body = hardline <> indent 2 body

highlighted :: Char -> Doc ann -> Doc ann
highlighted highlighSym doc =
  column
    (\columnStart ->
       align $
       doc <>
       column
         (\columnEnd ->
            line <> (pretty $ replicate (columnEnd - columnStart) highlighSym)))

underlined :: Doc ann -> Doc ann
underlined = highlighted '─'

pointed :: Doc ann -> Doc ann
pointed = highlighted '^'

bulletList :: Doc ann -> Doc ann -> Doc ann -> [Doc ann] -> Doc ann
bulletList emptyList singularTitle pluralTitle docs =
  case docs of
    [] -> vsep [pluralTitle, emptyList]
    [doc] -> vsep [singularTitle, indent 2 doc]
    _ ->
      vsep $
      [pluralTitle] ++ map (\doc -> indent 2 (bullet <> indent 1 doc <> line)) docs

renderSolutions :: [Doc Ann] -> Doc Ann
renderSolutions =
  bulletList "Can't recommend any solutions"
             (annotate SolutionsTitle $ underlined "Possible Solution" <> line)
             (annotate SolutionsTitle $ underlined "Possible Solutions" <> line)

renderKeyPath :: [Doc ann] -> Doc ann
renderKeyPath = flip renderKeyPathBody mempty

foundError3 :: Doc Ann -> Doc Ann -> [Doc Ann] -> Doc Ann
foundError3 errorHeader errorDescription errorSolutions =
  vsep [ annotate ErrorTitle (underlined "Error Found")
       , mempty
       , indent 2 errorHeader
       , mempty
       , indent 2 errorDescription
       , mempty
       , mempty
       , renderSolutions errorSolutions
       , mempty
       ]

foundError2 :: Doc Ann -> [Doc Ann] -> Doc Ann
foundError2 errorHeader errorSolutions =
  vsep [ annotate ErrorTitle (underlined "Error Found")
       , mempty
       , indent 2 errorHeader
       , mempty
       , renderSolutions errorSolutions
       , mempty
       ]


--------------------------------------------------------------------------------
-- JSON

renderJsonType :: JSON.Value -> Maybe (Doc ann)
renderJsonType value =
  case value of
    JSON.Object {} -> Just "object"
    JSON.Array arr ->
      if Vector.null arr
        then Nothing
        else do
          inner <- renderJsonType (Vector.head arr)
          return $ "[" <> inner <> "]"
    JSON.String {} -> Just "string"
    JSON.Bool   {} -> Just "boolean"
    JSON.Number {} -> Just "number"
    JSON.Null   {} -> Nothing

renderJsonValue :: JSON.Value -> Doc ann
renderJsonValue value =
  case value of
    JSON.Object obj ->
      vsep $ map (\(k, v) -> pretty k <> ":" <+>  renderJsonValue v)
                 (HashMap.toList obj)
    JSON.Array arr ->
      vsep $ Vector.toList $ Vector.map (\v -> "-" <+> renderJsonValue v) arr

    JSON.String str ->
      "\"" <> pretty str <> "\""

    JSON.Bool boolean ->
      pretty $ Text.toLower $ tshow boolean

    JSON.Number scientific ->
      pretty $ show scientific

    JSON.Null ->
      "null"

renderInvalidJsonError :: Pretty a => a -> Doc Ann
renderInvalidJsonError msg =
  foundError3
    "Detected invalid JSON syntax"
    (pretty msg)
    [ reflow "Fix invalid syntax" ]


renderPathPieces :: [JSON.PathPiece] -> Doc ann
renderPathPieces ks =
    mconcat $ List.intercalate ["."] (map (return . pathPiece) ks)
  where
    pathPiece (JSON.ObjectKey k)  = pretty k
    pathPiece (JSON.ArrayIndex i) = "[" <> pretty i <> "]"

--------------------------------------------------------------------------------
-- Etc Spec Types

renderSingleConfigValueType :: SingleConfigValueType -> Doc ann
renderSingleConfigValueType cvType =
  case cvType of
    CVTString      -> "string"
    CVTNumber      -> "number"
    CVTBool        -> "boolean"
    CVTObject      -> "object"
    CVTCustom name -> pretty name

renderSpecKeyPath :: [Doc ann] -> Doc ann -> Doc ann
renderSpecKeyPath keyPath =
  renderKeyPathBody (("etc/entries" : keyPath) ++ ["etc/spec"])

renderConfigValueType :: ConfigValueType -> Doc ann
renderConfigValueType cvType =
  case cvType of
    CVTSingle ty -> renderSingleConfigValueType ty
    CVTArray  ty -> "[" <> renderSingleConfigValueType ty <> "]"

--------------------------------------------------------------------------------
-- Error Renderer

renderErrorDoc :: Doc Ann -> String
renderErrorDoc =
  renderSimplyDecorated
    Text.unpack
    (\ann ->
       case ann of
         Current -> currentColor
         CurrentColorName -> currentColor <> currentColorName
         Expected -> expectedColor
         ExpectedColorName -> expectedColor <> expectedColorName
         Error -> errorColor
         ErrorTitle -> errorTitleColor
         SolutionsTitle -> solutionsTitleColor
         Filepath -> filepathColor
         Envvar -> envVarColor
       )
    (const resetColor)
  . layoutSmart defaultLayoutOptions
  where
    currentColorName = "cyan"
    currentColor = "\x1b[36m"
    expectedColorName = "purple"
    expectedColor = "\x1b[35m"
    errorColor = "\x1b[31m"
    errorTitleColor = "\x1b[31;1m"
    solutionsTitleColor = "\x1b[32;1m"
    filepathColor = "\x1B[32m"
    envVarColor = "\x1B[35m"
    resetColor = "\x1b[0m"
