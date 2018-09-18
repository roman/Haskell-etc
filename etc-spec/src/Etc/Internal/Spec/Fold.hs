{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- TODO: Move this to the Resolver module
module Etc.Internal.Spec.Fold (ResolverResult(..), resolveSpecToConfig) where

import           RIO
import qualified RIO.Map as Map
import           RIO.Seq (Seq, (|>))
import qualified RIO.Seq as Seq
import qualified RIO.Set as Set

import qualified Data.Aeson as JSON

import           Etc.Internal.Config
    (IConfigSource(..), Config (..), ConfigValue (..), SomeConfigSource, emptySubConfig)
import qualified Etc.Internal.Spec.Parser as Spec
import qualified Etc.Internal.Spec.Types  as Spec
import qualified Etc.Internal.Resolver.Types as Resolver
import Etc.Internal.Renderer (HumanErrorMessage)


type TypeMismatchBuilder e =
  [Text] -> Spec.ConfigValueType -> JSON.Value -> e

data ResolverResult e
  = ResolverResult { onValueTypeMismatch :: !(TypeMismatchBuilder e)
                   , resolverValue       :: !SomeConfigSource
                   }

resolveSpecToConfig_ ::
     (Show e, Typeable e, HumanErrorMessage e, MonadThrow m)
  => (JSON.Value -> m (Maybe (ResolverResult e)))
  -> Seq Text
  -> Map Text Spec.CustomType
  -> Spec.ConfigValue
  -> m (Maybe ConfigValue)
resolveSpecToConfig_ mapEntrySpec keyPath customTypes spec =
  case spec of
    Spec.SubConfig subConfig -> do
      result <-
        Map.traverseMaybeWithKey
          (\key specConfigValue ->
             resolveSpecToConfig_
               mapEntrySpec
               (keyPath |> key)
               customTypes
               specConfigValue)
          subConfig
      return $
        if Map.null result
          then Nothing
          else Just (SubConfig result)

    Spec.ConfigValue configValueData -> do
      result <-
        mapEntrySpec
          (Spec.configValueJSON configValueData)
      case result of
        Nothing -> return Nothing
        Just (ResolverResult onTypeMismatch val) -> do
          let typeCheck =
                Spec.assertFieldTypeMatchesE
                  (onTypeMismatch (toList keyPath))
                  customTypes
                  (Spec.configValueType configValueData)
                  (sourceValue val)
          case typeCheck of
            Left err -> throwM (Resolver.ResolverError err)
            _ -> return (Just $
                         ConfigValue
                           (Spec.configValueSensitive configValueData)
                           (Set.singleton val))

resolveSpecToConfig
  :: (Show e, Typeable e, HumanErrorMessage e, MonadThrow m)
  => (JSON.Value -> m (Maybe (ResolverResult e)))
  -> Map Text Spec.CustomType
  -> Spec.ConfigSpec
  -> m Config
resolveSpecToConfig mapEntrySpec customTypes spec = do
  result <-
    resolveSpecToConfig_
      mapEntrySpec
      Seq.empty
      customTypes
      (Spec.SubConfig $ Spec.configSpecEntries spec)
  return $ maybe (Config emptySubConfig) Config result
