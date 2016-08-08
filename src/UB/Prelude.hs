{-# LANGUAGE NoImplicitPrelude #-}
module UB.Prelude
      ( module UB.Prelude
      , module Protolude
      ) where

import Protolude hiding (($), (.), const, always)
import qualified Protolude as P

(<|) a b = a P.$ b
(<<) a b = a P.. b

(|>) = flip (<|)
(>>) = flip (<<)

always = P.const

infixr 0 <|
infixl 9 <<
