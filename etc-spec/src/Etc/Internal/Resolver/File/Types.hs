{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Etc.Internal.Resolver.File.Types where

import RIO

import qualified Data.Aeson as JSON

import           Data.Text.Prettyprint.Doc ((<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty


import           Etc.Internal.Config
import qualified Etc.Internal.Spec.Types as Spec

type FormatName = Text

data FileResolverError
  -- | The 'etc/files' entry is not present in the config spec top-level
  = ConfigSpecFilesEntryMissing ![Text]
  -- | 'etc/files.paths' is an empty array
  | ConfigSpecFilesPathsEntryIsEmpty
  -- | The 'etc/files.paths' contains an entry that doesn't have a valid extension
  | UnsupportedFileExtensionGiven !Text ![Text]
  -- | An entry in a file in 'etc/files.paths' contains a value that does not match the spec
  | ConfigFileValueTypeMismatch !FileValueOrigin ![Text] !Spec.ConfigValueType !JSON.Value
  -- | The 'etc/files.paths' points to a file that does not exist
  | ConfigFileNotPresent !Text
  -- | When configuration files contains keyPath that are not part of the spec
  | UnknownConfigKeyFound !FileValueOrigin ![Text] !Text ![Text]
  -- | When configuration files contain a text with syntax that is not recognized
  | ConfigInvalidSyntaxFound !FileValueOrigin
  -- | When the spec is expecting a subconfig but a value is given
  | SubConfigEntryExpected !FileValueOrigin ![Text] JSON.Value
  deriving (Show)

--------------------------------------------------------------------------------

data FileValueOrigin
  = ConfigFileOrigin { fileSourcePath :: !Text }
  | EnvFileOrigin    { fileSourceEnvVar :: !Text,  fileSourcePath :: !Text }
  deriving (Generic, Show, Eq)

instance NFData FileValueOrigin

data FileSource = FileSource
  { fsConfigIndex :: !Int
  , fsValueOrigin :: !FileValueOrigin
  , fsValue       :: !(Value JSON.Value) }
  deriving (Generic, Typeable, Show, Eq)

instance NFData FileSource
instance IConfigSource FileSource where
  compareSources = comparing fsConfigIndex
  sourceValue = fsValue
  sourcePrettyDoc (FileSource _index origin _value) =
    case origin of
      ConfigFileOrigin filepath -> "File:" <+> Pretty.pretty filepath
      EnvFileOrigin envVar filepath ->
        "File:" <+> Pretty.pretty envVar <> "=" <> Pretty.pretty filepath
