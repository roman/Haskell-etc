{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
module Main where

import RIO
import qualified Spec

main :: IO ()
main = Spec.main
