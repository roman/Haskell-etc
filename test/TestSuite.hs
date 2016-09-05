{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import UB.Prelude
import Test.Tasty (defaultMain)
import qualified UB.ConfigTest

main :: IO ()
main = defaultMain UB.ConfigTest.tests
