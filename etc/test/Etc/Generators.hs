{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Etc.Generators where

import           RIO
import qualified RIO.HashMap as HashMap
import qualified RIO.Map     as Map
import qualified RIO.Text    as Text

import Control.Monad (replicateM)

import qualified Data.Aeson as JSON

import Test.QuickCheck

import Etc.Internal.Spec.Serializer ()
import Etc.Internal.Spec.Types

instance Arbitrary SingleConfigValueType where
  arbitrary =
    -- custom <- (CVTCustom . Text.pack) <$> arbitrary
    -- elements [CVTString, CVTBool, CVTObject, custom]
    elements [CVTString, CVTBool, CVTObject]

instance Arbitrary ConfigValueType where
  arbitrary = do
    single <- CVTSingle <$> arbitrary
    array <- CVTArray <$> arbitrary
    elements [single, array]

instance Arbitrary ConfigValueData where
  arbitrary = do
    typeVal <- arbitrary
    sensitiveVal <- arbitrary
    return $ ConfigValueData Nothing typeVal sensitiveVal (JSON.object [])

instance Arbitrary ConfigValue where
  arbitrary = do
    subConfigKeyCount <- choose (1, 7)
    SubConfig . Map.fromList <$>
      replicateM subConfigKeyCount ((Text.pack *** ConfigValue) <$> arbitrary)

instance Arbitrary ConfigSpec where
  arbitrary = do
    subConfigKeyCount <- choose (1, 50)
    entriesMap <-
      Map.fromList <$>
      replicateM subConfigKeyCount (first Text.pack <$> arbitrary)
    return $ ConfigSpec "<<string>>" HashMap.empty entriesMap
