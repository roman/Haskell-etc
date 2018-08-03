{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main where


import RIO

import Data.FileEmbed (embedFile)

import qualified Etc.Spec         as Etc

--------------------------------------------------------------------------------
-- We specify the support commands for our program

configSpecData :: ByteString
configSpecData = $(embedFile "resources/spec1.json")

main :: IO ()
main = runSimpleApp $ do
  configSpec <- Etc.parseConfigSpec configSpecData
  logInfo (displayShow configSpec)
