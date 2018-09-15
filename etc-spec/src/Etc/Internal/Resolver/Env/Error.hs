{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Etc.Internal.Resolver.Env.Error where

import RIO

import qualified Data.Aeson as JSON

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util (reflow)

import Etc.Internal.Spec.Types
import Etc.Internal.Renderer
import Etc.Internal.Resolver.Env.Types

envValueTypeMismatchFoundBody
  :: Text -> ([Doc Ann]) -> ConfigValueType -> JSON.Value -> Doc Ann
envValueTypeMismatchFoundBody varName keyPath cvType jsonVal = vsep
  [ reflow "There is a mistmach between an environment variable value and the type specified in the configuration spec file"
  , mempty
  , "The environment variable"
  , mempty
  , indent 4 $ pretty varName <> "=" <> annotate Current (renderJsonValue jsonVal)
  , mempty
  , "The"
  <+> annotate Current "current value"
  <+> "does not match the"
  <+> "expected type"
  <+> annotate Expected (renderConfigValueType cvType)
  ]

renderEnvValueTypeMismatchFound
  :: Text -> [Text] -> ConfigValueType -> JSON.Value -> Doc Ann
renderEnvValueTypeMismatchFound varName keyPath cvType jsonVal =
  foundError3
    "env resolver"
    (envValueTypeMismatchFoundBody varName (map pretty keyPath) cvType jsonVal)
    [ reflow "Change the value to match the given type" <+>
      dquotes (renderConfigValueType cvType)
    , case renderJsonType jsonVal of
        Just jsonTyDoc ->
          reflow "In the configuration spec file, change the entry \"type\" to" <+>
          dquotes jsonTyDoc <+> reflow "to match the environment variable value"
        Nothing -> mempty
    , case stripArrayWrapper cvType of
        CVTCustom customName ->
          reflow
            "Environment variables are parsed from JSON strings, make sure the parser for the type" <+>
          dquotes (pretty customName) <+>
          "supports parsing JSON strings as inputs"
        _ -> mempty
    ]

instance HumanErrorMessage EnvResolverError where
  humanErrorMessage err =
    case err of
      EnvValueTypeMismatch varName keyPath cvType jsonVal ->
        renderEnvValueTypeMismatchFound varName keyPath cvType jsonVal
