{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module UB.Test.Util where

import UB.Prelude
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.Text as Text
import qualified Data.EDN as EDN

import Test.Tasty.HUnit (assertFailure, assertEqual)

assertFromEDN :: (Show a, Eq a, EDN.FromEDN a) => Text -> LB8.ByteString -> a -> IO ()
assertFromEDN errMsg input expected =
  case EDN.decode input of
    Nothing ->
      assertFailure <| Text.unpack errMsg

    Just actual ->
      assertEqual (Text.unpack errMsg) expected actual
