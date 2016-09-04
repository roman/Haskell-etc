module UB.Config
  ( UnresolvedConfig
  , Unresolved.readConfigurationFile
  , Resolver.Config
  , Resolver.resolveEnvVarConfiguration
  , Resolver.getConfigValue
  ) where

import qualified UB.Internal.Config.Unresolved as Unresolved
import qualified UB.Internal.Config.Resolver as Resolver

type UnresolvedConfig = Unresolved.Config
