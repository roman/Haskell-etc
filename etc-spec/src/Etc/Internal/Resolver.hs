{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}
module Etc.Internal.Resolver where

import           RIO
import qualified RIO.Map as Map

import           Etc.Internal.Config           (Config)
import           Etc.Internal.Resolver.Default (defaultResolver)
import           Etc.Internal.Resolver.Types
import qualified Etc.Internal.Spec.Types       as Spec

import Etc.Internal.Resolver.Env  (envResolver)
import qualified Etc.Internal.Resolver.File as File

-- | Use each input 'Resolver' to gather configuration values, all
-- runtime configuration values are stored in the returned 'Config' record.
--
-- Each 'Resolver' relies on the metadata the 'Spec.ConfigSpec' value has to
-- gather configuration values.
--
-- ===== To keep in mind
--
-- The order of the input 'Resolver' list matters, as the precedence of a value
-- coming from the resolver source is going to be higher if it is found in the
-- latter positions of the list
--
-- ===== Example
--
-- @
-- resolveConfigWith [("ip", aesonCustomType @IpAddress)] [envResolver, fileResolver yamlConfig] spec
-- @
--
-- In the above invocation, values coming from environment variables are going
-- to have less precedence than values coming from configuration files, this is
-- because 'File.fileResolver' is __after__ 'envResolver'.
--
-- @since 1.0.0.0
resolveConfigWith ::
     (MonadThrow m, MonadUnliftIO m)
  => [(Text, Spec.CustomType)] -- ^ List of keys indetifying a 'CustomType' that
                               -- can be used in the configuration spec
  -> [Resolver m]              -- ^ Resolvers used to resolve values for the configuration spec
  -> Spec.ConfigSpec           -- ^ The parsed configuration spec
  -> m Config                  -- ^ Config where values can be taken from
resolveConfigWith customTypesList resolvers spec  =
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


-- | Gathers values from configuration files and environment variables to create
-- a 'Config' record; it makes use of the metadata found in 'Spec.ConfigSpec'.
--
-- Note this function only resolves values coming from configuration files or
-- environment variables. Check the documentation about the configuration spec
-- file syntax for more details.
--
-- ==== To keep in mind
--
-- Values coming from environment variables have precedence over values coming
-- from configuration files.
--
-- If you need to customize the precedence of the sources of the configuration
-- values or if you need to add other resolvers (e.g. CLI, Vault, etc.), use
-- 'resolveConfigWith' instead
--
-- @since 1.0.0.0
resolveConfig ::
     (MonadUnliftIO m, MonadThrow m)
  => [(Text, Spec.CustomType)] -- ^ List of keys indetifying a 'CustomType' that
                               -- can be used in the configuration spec
  -> Spec.ConfigSpec           -- ^ The parsed configuration spec
  -> m Config                  -- ^ Config where values can be taken from
resolveConfig customTypesList spec =
  resolveConfigWith
    customTypesList
    [File.fileResolverInternal False File.yamlConfig, envResolver]
    spec
