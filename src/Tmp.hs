{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Tmp where

import Data.Maybe (fromJust)
import UB.Prelude hiding (toList)
import GHC.TypeLits
import Data.HashMap.Strict
import qualified Data.Text as Text

type family Foo (k :: Symbol) :: *

data FooExistential
  = forall s. KnownSymbol s => F (Proxy s, Foo s)

type instance Foo "hello" = Int
type instance Foo "world" = Text

someValues :: HashMap Text FooExistential
someValues =
  fromList
    [ ("hello", F (Proxy :: Proxy "hello", 123))
    , ("world", F (Proxy :: Proxy "world", "hehe"))
    ]

example :: Proxy k -> Maybe (Foo k)
example proxy =
  let
    key =
      toList someValues
      |> head
      |> fmap fst
      |> fromJust

    someSymbol =
      someSymbolVal (Text.unpack key)
  in do
    F (otherProxy, value) <- lookup key someValues
    sameSymbol proxy otherProxy
    return value
    -- case lookup key someValues of
    --   Nothing ->
    --     Nothing
    --   Just (F (otherProxy, value)) ->
    --     case sameSymbol proxy otherProxy of
    --       Nothing ->
    --         Nothing
    --       Just
    --     value
