module Etc.Resolver
  ( Types.ResolverError (..)
  , Types.resolveConfig
  , File.fileResolver
  , File.jsonFileResolver
  , File.yamlFileResolver
  ) where

import qualified Etc.Resolver.Internal.Types as Types
import qualified Etc.Resolver.Internal.File as File
