{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import RIO
import Spec (spec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec spec
