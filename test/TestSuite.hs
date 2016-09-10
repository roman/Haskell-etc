{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import UB.Prelude
import Test.Tasty (defaultMain)
import qualified System.EtcTest

main :: IO ()
main = defaultMain System.EtcTest.tests
