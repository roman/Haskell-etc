{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import RIO
import Test.Hspec

import Etc.SpecSpec (spec)

main :: IO ()
main = hspec spec
