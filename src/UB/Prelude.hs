{-# LANGUAGE NoImplicitPrelude #-}
module UB.Prelude
      ( module UB.Prelude
      , module Protolude
      ) where

import Protolude hiding (($), (.), const, always)
import qualified Protolude as P

(<|) :: (a -> b) -> a -> b
(<|) a b = a P.$ b

(<<) :: (b -> c) -> (a -> b) -> a -> c
(<<) a b = a P.. b

(|>) :: a -> (a -> b) -> b
(|>) = flip (<|)

(>>) :: (a -> b) -> (b -> c) -> a -> c
(>>) = flip (<<)

always :: a -> b -> a
always = P.const

infixr 0 <|
infixl 9 <<
