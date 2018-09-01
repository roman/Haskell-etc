module Etc.Resolver.File
  ( FileParser(..)
  , jsonFileParser
  , yamlFileParser
  , FileResolverError(..)
  , fileResolver
  , jsonFileResolver
  , yamlFileResolver
  , getFileWarnings
  ) where

import Etc.Resolver.Internal.File.Types (FileParser(..), FileResolverError(..))
import Etc.Resolver.Internal.File
  ( fileResolver
  , getFileWarnings
  , jsonFileParser
  , jsonFileResolver
  , yamlFileParser
  , yamlFileResolver
  )
