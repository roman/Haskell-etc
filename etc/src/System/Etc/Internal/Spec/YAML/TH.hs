{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module System.Etc.Internal.Spec.YAML.TH where

import RIO

import Data.Proxy (Proxy)

import Language.Haskell.TH        (ExpQ, runIO)
import Language.Haskell.TH.Syntax (Lift)

import qualified Data.Aeson as JSON

import System.Etc.Internal.Spec.Types (ConfigSpec)
import System.Etc.Internal.Spec.YAML  (readConfigSpec)

readConfigSpecTH_
  :: (Lift k, JSON.FromJSON k) => Proxy k -> (Text -> IO (ConfigSpec k)) -> Text -> ExpQ
readConfigSpecTH_ _ readSpec filepath = do
  configSpec <- runIO $ readSpec filepath
  [| configSpec |]

readConfigSpecTH :: (Lift k, JSON.FromJSON k) => Proxy k -> Text -> ExpQ
readConfigSpecTH = flip readConfigSpecTH_ readConfigSpec
