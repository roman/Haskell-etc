{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
module Etc.Resolver.Internal.Types where

import           RIO
import qualified RIO.Text as Text

import qualified Data.Aeson.BetterErrors as JSON

import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Util as Pretty (reflow)

import qualified Etc.Spec as Spec
import Etc.Config (Config)

import Etc.Renderer

--------------------
-- Configuration Types

newtype Resolver m
  = Resolver {
      runResolver :: Int -> Spec.ConfigSpec -> m Config
    }

newtype ResolverError err
  = ResolverError err
  deriving (Show)

instance Exception err => Exception (ResolverError (JSON.ParseError err)) where
  displayException (ResolverError resolverErr) =
    "\n\n" <>
    (case resolverErr of
       JSON.InvalidJSON msg -> renderErrorDoc $ renderInvalidJsonError msg
       JSON.BadSchema ks errSpecifics ->
         case errSpecifics of
           JSON.CustomError err -> displayException err
           _ ->
             renderErrorDoc $
             foundError3
               ("Detected JSON parser failure")
               (Pretty.vsep ["In the following entry:"
                     , mempty
                     , Pretty.indent 2 $ renderPathPieces ks
                     , mempty
                     , "The JSON API returned the following error:"
                     , mempty
                     , Pretty.indent 2 $ Pretty.reflow $ Text.pack $ show errSpecifics
                     ]
               )
               []
    )

resolveConfig :: Monad m => Spec.ConfigSpec -> [Resolver m] -> m Config
resolveConfig spec resolvers =
  mconcat <$>
  mapM (\(priorityIndex, resolver) ->
          runResolver resolver priorityIndex spec)
    (zip [(1 :: Int)..] $ reverse resolvers)
