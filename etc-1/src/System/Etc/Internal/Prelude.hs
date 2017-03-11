{-# LANGUAGE NoImplicitPrelude #-}
module System.Etc.Internal.Prelude
      ( module System.Etc.Internal.Prelude
      , module Protolude
      ) where

import qualified Prelude
import qualified Data.Text as Text

import Protolude hiding ((>>), ($), (.), const, always)
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

crash :: Text -> a
crash = Text.unpack >> Prelude.error


infixr 0 <|
infixl 9 <<
