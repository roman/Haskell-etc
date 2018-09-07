module Etc.Resolver
  ( Types.ResolverError (..)
  , Types.resolveConfig

  -- * File Resolver Utilities
  , File.FileResolverError (..)
  , File.jsonConfig
  , File.yamlConfig
  , File.fileResolver
  ) where

import qualified Etc.Internal.Resolver.File       as File
import qualified Etc.Internal.Resolver.File.Types as File
import qualified Etc.Internal.Resolver.Types      as Types
