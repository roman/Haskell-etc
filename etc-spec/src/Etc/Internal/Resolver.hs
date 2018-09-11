{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Etc.Internal.Resolver where

import Prelude (putStrLn)

import RIO

import           Etc.Internal.Config           (Config)
import           Etc.Internal.Resolver.Default (defaultResolver)
import           Etc.Internal.Resolver.Types
import qualified Etc.Internal.Spec.Types       as Spec

resolveConfig :: (MonadUnliftIO m) => Spec.ConfigSpec -> [Resolver m] -> m Config
resolveConfig spec resolvers = do
    result <- try resolveAll
    case result of
      Left err -> liftIO (putStrLn (displayException err)) >> throwIO (err :: SomeException)
      Right config -> return config
  where
    indexedResolvers =
      -- defaultResolver will always be the one that has the least precedence
      zip [(0 :: Int) ..] $ reverse (defaultResolver : resolvers)

    runIndexedResolver priorityIndex resolver =
      runResolver resolver priorityIndex spec

    resolveAll =
      mconcat <$> mapM (uncurry runIndexedResolver) indexedResolvers
