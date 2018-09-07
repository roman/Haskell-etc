{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import RIO

import qualified Etc.Spec as Etc

import Types (customTypes)

--------------------------------------------------------------------------------
-- We specify the support commands for our program

configSpec :: Etc.ConfigSpec
configSpec =
  $(Etc.readConfigSpecTH Etc.yamlSpec customTypes "examples/etc-spec-errors-example/resources/spec1.yaml")

main :: IO ()
main = runSimpleApp $ do
  logInfo (displayShow configSpec)
