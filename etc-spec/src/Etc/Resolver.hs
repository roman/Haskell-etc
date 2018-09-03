module Etc.Resolver
  ( Types.ResolverError (..)
  , Types.resolveConfig

  -- * File Resolver Utilities
  , Types.FileFormat
  , Types.newFileFormat
  , File.jsonFormat
  , File.yamlFormat
  , File.fileResolver
  , File.jsonFileResolver
  , File.yamlFileResolver
  ) where

import qualified Etc.Internal.Resolver.File       as File
import qualified Etc.Internal.Resolver.File.Types as Types
import qualified Etc.Internal.Resolver.Types      as Types
