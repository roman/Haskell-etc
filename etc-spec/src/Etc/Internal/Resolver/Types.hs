{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
module Etc.Internal.Resolver.Types where

import Prelude (putStrLn)

import           RIO
import qualified RIO.Text as Text

import qualified Data.Aeson.BetterErrors as JSON

import qualified Data.Text.Prettyprint.Doc      as Pretty
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)

import           Etc.Internal.Config     (Config)
import qualified Etc.Internal.Spec.Types as Spec

--------------------
-- Configuration Types

newtype Resolver m
  = Resolver {
      runResolver :: Int -> Spec.ConfigSpec -> m Config
    }

newtype ResolverError err
  = ResolverError err
  deriving (Show)

resolveConfig :: (MonadUnliftIO m) => Spec.ConfigSpec -> [Resolver m] -> m Config
resolveConfig spec resolvers = do
    result <- try resolveAll
    case result of
      Left err -> liftIO (putStrLn (displayException err)) >> throwIO (err :: SomeException)
      Right config -> return config
  where
    resolveAll =
      mconcat <$> mapM
      (\(priorityIndex, resolver) -> runResolver resolver priorityIndex spec)
      (zip [(1 :: Int) ..] $ reverse resolvers)
