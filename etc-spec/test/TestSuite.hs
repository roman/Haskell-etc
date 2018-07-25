{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module TestSuite where

import RIO
import Test.Hspec

main :: IO ()
main = hspec spec
