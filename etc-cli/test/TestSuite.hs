{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import RIO
import Test.Hspec

main :: IO ()
main = hspec $ do
  xdescribe "pending tests" $ do
    xit "foobar" $ pendingWith "pending test"
