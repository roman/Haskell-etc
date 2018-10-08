{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}
-- | @since 1.0.0.0
module Etc.Internal.FileFormat where

import RIO

import qualified Data.Aeson              as JSON
import qualified Data.Aeson.BetterErrors as JSON
import qualified Data.Yaml               as Yaml

type FormatName = Text

data FileFormat e
  = FileFormat
  {
    fileFormatName   :: ![FormatName]
  , fileFormatParser :: !(ByteString -> Either e JSON.Value)
  }

instance Functor FileFormat where
  fmap f format@FileFormat {fileFormatParser} =
    format {fileFormatParser = mapLeft f . fileFormatParser}

instance Semigroup (FileFormat e) where
  (<>) (FileFormat fn1 fp1) (FileFormat fn2 fp2) =
    FileFormat (fn1 <> fn2) (\bytes ->
                               case fp1 bytes of
                                 Left _e      -> fp2 bytes
                                 Right result -> Right result)


newFileFormat :: FormatName -> (ByteString -> Either e JSON.Value) -> FileFormat e
newFileFormat formatName fileFormatParser =
  FileFormat {fileFormatName = [formatName], fileFormatParser }

jsonFormat :: forall err. Exception err => FileFormat (JSON.ParseError err)
jsonFormat = newFileFormat "json" (JSON.parseStrict JSON.asValue)

yamlFormat :: FileFormat Yaml.ParseException
yamlFormat = newFileFormat "yaml" Yaml.decodeEither'
