module Etc.Resolver
  ( Types.ResolverError (..)
  , Resolver.resolveConfigWith

  -- * File Resolver Utilities
  , File.FileResolverError (..)
  , File.jsonConfig
  , File.yamlConfig
  , File.fileResolver
  ) where

import qualified Etc.Internal.Resolver            as Resolver
import qualified Etc.Internal.Resolver.File       as File
import qualified Etc.Internal.Resolver.File.Types as File
import qualified Etc.Internal.Resolver.Types      as Types
