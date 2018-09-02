module Etc.Resolver.File
  ( FileFormat(..)
  , jsonFormat
  , yamlFormat
  , FileResolverError(..)
  , fileResolver
  , jsonFileResolver
  , yamlFileResolver
  , getFileWarnings
  ) where

import Etc.Resolver.Internal.File.Types (FileFormat(..), FileResolverError(..))
import Etc.Resolver.Internal.File
  ( fileResolver
  , getFileWarnings
  , jsonFormat
  , jsonFileResolver
  , yamlFormat
  , yamlFileResolver
  )
