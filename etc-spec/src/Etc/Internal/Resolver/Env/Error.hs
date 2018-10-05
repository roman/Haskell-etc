{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}
module Etc.Internal.Resolver.Env.Error where

import RIO

import qualified Data.Aeson as JSON

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util (reflow)

import Etc.Internal.Spec.Types
import Etc.Internal.Renderer
import Etc.Internal.Resolver.Env.Types

envValueTypeMismatchFoundBody
  :: Doc Ann -> Doc Ann -> ([Doc Ann]) -> ConfigValueType -> JSON.Value -> Doc Ann
envValueTypeMismatchFoundBody varName sourcePath keyPath cvType jsonVal =
  vsep
    [ reflow
        "I detected a mistmach between an environment variable value and the expected type specified in the configuration spec"
    , mempty
    , reflow "The configuration spec file located at" <+> sourcePath <+> reflow "has the following entry:"
    , mempty
    , indent 2 $ renderSpecKeyPath keyPath $
      newlineBody $
      vsep
        [ hsep ["env:", varName]
        , hsep ["type:", annotate Expected $ pointed $ (renderConfigValueType cvType)]]
    , mempty
    , "But the recognized environment variable has this value:"
    , mempty
    , indent 2 $
      varName <> "=" <> annotate Current (pointed (renderJsonValue jsonVal))
    , mempty
    , "The" <+>
      annotate Current "current value" <+>
      "does not match the" <+>
      annotate Expected "expected type"
    ]

renderEnvValueTypeMismatchFound
  :: Text -> Text -> [Text] -> ConfigValueType -> JSON.Value -> Doc Ann
renderEnvValueTypeMismatchFound varName sourcePath keyPath cvType jsonVal =
  foundError3
    "env resolver"
    (envValueTypeMismatchFoundBody (pretty varName) (pretty sourcePath) (map pretty keyPath) cvType jsonVal)
    [ reflow "Change the value of the environment variable" <+> pretty varName <+> reflow "to match the expected type" <+>
      dquotes (renderConfigValueType cvType)
    , case renderJsonType jsonVal of
        Just jsonTyDoc ->
          reflow "In the configuration spec file, change the entry \"type\" to" <+>
          dquotes jsonTyDoc <+> reflow "to match the value of the environment variable" <+> pretty varName
        Nothing -> mempty
    , case stripArrayWrapper cvType of
        CVTCustom customName ->
          reflow
            "Environment variables are parsed as JSON strings, make sure the parser for the expected type" <+>
          dquotes (pretty customName) <+>
          "supports JSON strings as inputs"
        _ -> mempty
    ]

instance HumanErrorMessage EnvResolverError where
  humanErrorMessage err =
    case err of
      EnvValueTypeMismatch varName sourcePath keyPath cvType jsonVal ->
        renderEnvValueTypeMismatchFound varName sourcePath keyPath cvType jsonVal
