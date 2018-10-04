{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Etc.Internal.Resolver where

import           RIO
import qualified RIO.Map as Map

import           Etc.Internal.Config           (Config)
import           Etc.Internal.FileFormat       (FileFormat)
import           Etc.Internal.Resolver.Default (defaultResolver)
import           Etc.Internal.Resolver.Types
import qualified Etc.Internal.Spec.Types       as Spec

import Etc.Internal.Resolver.Env  (envResolver)
import Etc.Internal.Resolver.File (fileResolverInternal, yamlConfig)

-- | Creates a 'Config' from the 'Spec.ConfigSpec' using the given 'Resolver's
--
-- NOTE: The order of the input 'Resolver' list matters, as the precedence of
-- a value coming from the resolver source is going to be higher if it is found
-- in the latter positions of the list
--
-- Example:
--
-- > resolveConfigWith [("address", aesonCustomType @Address)] spec [envResolver, fileResolver yamlConfig]
--
-- In the above example, values coming from Environment Variables are going to
-- have less precedence than files coming from configuration files
resolveConfigWith ::
     (MonadThrow m, MonadUnliftIO m)
  => [(Text, Spec.CustomType)] -- ^ List of keys indetifying a 'CustomType' that
                               -- can be used in the configuration spec
  -> Spec.ConfigSpec           -- ^ The parsed configuration spec
  -> [Resolver m]              -- ^ Resolvers used to resolve values for the configuration spec
  -> m Config                  -- ^ Config where values can be taken from
resolveConfigWith customTypesList spec resolvers =
    resolveAll
  where
    customTypes =
      Map.fromList customTypesList

    indexedResolvers =
      -- defaultResolver will always be the one that has the least precedence
      zip [(0 :: Int) ..] $ reverse (defaultResolver : resolvers)

    runIndexedResolver priorityIndex resolver =
      runResolver resolver priorityIndex customTypes spec

    resolveAll =
      mconcat <$> mapM (uncurry runIndexedResolver) indexedResolvers


-- | Creates a 'Config' from the 'Spec.ConfigSpec' using only the 'fileResolver'
--   (with yaml format) and 'envResolver'. This function is the most common use
--   case of the library; for this reason, this particular combination of
--   resolvers is bundled in an API function.
--
-- NOTE: Values coming from environment variables have precedence over values in
-- the configuration files and defaults in the configuration spec file
--
-- If you need to customize the precedence of the sources of the configuration
-- values or if you need to add other resolvers (e.g. CLI, Vault, etc.), use
-- 'resolveConfigWith' instead
--
resolveConfig ::
     (MonadUnliftIO m, MonadThrow m)
  => [(Text, Spec.CustomType)] -- ^ List of keys indetifying a 'CustomType' that
                               -- can be used in the configuration spec
  -> Spec.ConfigSpec           -- ^ The parsed configuration spec
  -> m Config                  -- ^ Config where values can be taken from
resolveConfig customTypesList spec =
  resolveConfigWith
    customTypesList
    spec
    [fileResolverInternal False yamlConfig, envResolver]
