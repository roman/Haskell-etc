{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Etc.Internal.Resolver.Fold (ResolverResult(..), resolveSpecEntries) where

import           RIO
import qualified RIO.Map as Map
import           RIO.Seq ((|>))
import qualified RIO.Seq as Seq
import qualified RIO.Set as Set

import Control.Monad.Trans.Maybe (MaybeT(..))

import qualified Data.Aeson.BetterErrors as JSON
import qualified Data.Aeson as JSON

import           Etc.Internal.Config
    (IConfigSource(..), Config (..), ConfigValue (..), SomeConfigSource, emptySubConfig)
import qualified Etc.Internal.Spec.Parser as Spec
import qualified Etc.Internal.Spec.Types  as Spec
import qualified Etc.Internal.Resolver.Types as Resolver
import Etc.Internal.Renderer (HumanErrorMessage)

--------------------------------------------------------------------------------

type TypeMismatchError e =
  Spec.SpecFilePath -> Spec.SpecEntryPath -> Spec.ConfigValueType -> JSON.Value -> e

data ResolverResult e
  = ResolverResult { onValueTypeMismatch :: !(TypeMismatchError e)
                   , resolverResult      :: !SomeConfigSource
                   }

resolveSpecEntries ::
     (Show e, Typeable e, MonadThrow m, HumanErrorMessage e)
  => JSON.Parse e spec
  -> (spec -> m (Maybe (ResolverResult e)))
  -> Map Text Spec.CustomType
  -> Spec.ConfigSpec
  -> m Config
resolveSpecEntries specParser evalSpec customTypes spec0 = do
  let specFilePath = Spec.configSpecFilePath spec0
      traverseSubConfig keyPath subConfig = do
        result <-
          Map.traverseMaybeWithKey
            (\key specConfigValue
             -- recursive call
              -> resolverTraverse (keyPath |> key) specConfigValue)
            subConfig
        return $
          if Map.null result
            then Nothing
            else Just (SubConfig result)

      -- Tries to parse the configuration metadata for the field, if it is
      -- not able to parse anything, we just skip the entry
      parseFieldSpec configValueData =
        MaybeT $
        case JSON.parseValue specParser (Spec.configValueJSON configValueData) of
          Left _ -> return Nothing
          Right result -> return (Just result)

      -- We were able to parse a spec that can be resolved, calling the evalSpec function
      -- if the evaluator function returns Nothing, we skip the entry
      evalFieldSpec fieldSpec = MaybeT $ evalSpec fieldSpec

      -- After we have a value, we need to assert that it matches the given type in the spec
      -- in here is worth to throw an exception in case of a type/value mismatch error
      assertFieldType keyPath configValueData (ResolverResult onTypeMismatchCallback fieldValue) =
        let typeCheck =
              Spec.assertFieldTypeMatchesE
                (onTypeMismatchCallback specFilePath (toList keyPath))
                customTypes
                (Spec.configValueType configValueData)
                (sourceValue fieldValue)
         in case typeCheck of
              Left err -> throwM (Resolver.ResolverError err)
              Right _ -> return ()

      -- Create a ConfigValue from the ResolverResult
      toConfigValue configValueData (ResolverResult _ fieldValue) =
        return
          ((ConfigValue
              (Spec.configValueSensitive configValueData)
              (Set.singleton fieldValue)))

      -- This function ties the loop on all the different smaller functions
      traverseConfigValue keyPath configValueData =
        runMaybeT $ do
          fieldSpec <- parseFieldSpec configValueData
          resolverResult <- evalFieldSpec fieldSpec
          lift $ assertFieldType keyPath configValueData resolverResult
          toConfigValue configValueData resolverResult

      -- We are going to traverse through all the entries in the config spec
      -- and we are going to evaluate the resolver for each entry that contains
      -- the metadata we are looking for
      resolverTraverse keyPath spec =
        case spec of
          Spec.SubConfig subConfig ->
            traverseSubConfig keyPath subConfig
          Spec.ConfigValue configValueData ->
            traverseConfigValue keyPath configValueData
   in do
    result <-
      resolverTraverse
             Seq.empty
             (Spec.SubConfig $ Spec.configSpecEntries spec0)
    return $ maybe (Config emptySubConfig) Config result
