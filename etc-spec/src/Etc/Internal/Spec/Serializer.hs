{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Etc.Internal.Spec.Serializer where

import           RIO
import qualified RIO.HashMap as HashMap
import qualified RIO.Map     as Map

import           Data.Aeson ((.=))
import qualified Data.Aeson as JSON

import Etc.Internal.Spec.Types

singleConfigValueTypeName :: SingleConfigValueType -> Text
singleConfigValueTypeName valueTy = case valueTy of
  CVTString       -> "string"
  CVTNumber       -> "number"
  CVTBool         -> "bool"
  CVTObject       -> "object"
  CVTCustom other -> other

instance JSON.ToJSON SingleConfigValueType where
  toJSON valueTy =
    JSON.String $ singleConfigValueTypeName valueTy

instance JSON.ToJSON ConfigValueType where
  toJSON valueTy =
    case valueTy of
      CVTSingle innerValTy -> JSON.toJSON innerValTy
      CVTArray innerValTy -> JSON.String $ "[" <> singleConfigValueTypeName innerValTy <> "]"

instance JSON.ToJSON ConfigValueData where
  toJSON ConfigValueData {..} =
    case configValueJSON of
      JSON.Object obj ->
        let otherFields =
              obj
              & HashMap.delete "default"
              & HashMap.delete "type"
              & HashMap.delete "sensitive"
              & HashMap.toList
         in JSON.object
              (maybe [] (\defVal -> ["default" .= defVal]) configValueDefault <>
               ["type" .= configValueType, "sensitive" .= configValueSensitive] <>
               otherFields)
      _ ->
        configValueJSON

instance JSON.ToJSON ConfigValue where
  toJSON val =
    case val of
      ConfigValue valData -> JSON.object ["etc/spec" .= valData]
      SubConfig subConfig ->
        JSON.object (Map.toList $ Map.map JSON.toJSON subConfig)

instance JSON.ToJSON ConfigSpec where
  toJSON ConfigSpec {..} =
    let
      otherKeys = configSpecJSON & HashMap.delete "etc/entries" & HashMap.toList
    in
      JSON.object $ ["etc/entries" .= configSpecEntries] <> otherKeys
