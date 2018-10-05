{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_HADDOCK hide #-}
-- | @since 1.0.0.0
module Etc.Internal.CustomType where

import RIO

import Data.Proxy      (Proxy (..))
import Data.Scientific (toBoundedInteger, toBoundedRealFloat)

import qualified Data.Aeson              as JSON hiding (withScientific, withText)
import qualified Data.Aeson.BetterErrors as JSON

import Etc.Internal.Spec.Types

aesonCustomType1 ::
     forall a. (JSON.FromJSON a)
  => Proxy a
  -> CustomType
aesonCustomType1 _ =
  CustomType {
    customTypeParser =
      const () <$> (JSON.fromAesonParser :: JSON.Parse () a)
  }

-- | Creates a 'CustomType' from a 'JSON.FromJSON' instance. You'll need
-- to enable the @TypeApplications@ GHC extension for this to work.
--
-- ==== Example
--
-- @
-- newtype IpAddress = IpAddress Text
--
-- instance FromJSON IpAddress where
--   parseJSON = withString "IpAddress" $ \text ->
--     maybe (fail "invalid ip address") return (parseIpAddress text)
--
-- ipAddressTy = aesonCustomType @IpAddress
-- @
--
-- @since 1.0.0.0
aesonCustomType :: forall a. (JSON.FromJSON a) => CustomType
aesonCustomType =
  aesonCustomType1 @a Proxy

-- | Creates a 'CustomType' from a predicate that checks strings.
--
-- ==== Example
--
-- @
-- ipAddressTy = textCustomType (isJust . parseIpAddress)
-- @
--
-- @since 1.0.0.0
textCustomType :: (Text -> Bool) -> CustomType
textCustomType predFn =
  let
    parser = JSON.withText $ \input -> do
      if predFn input
        then Right ()
        else Left ()
  in
    CustomType {
        customTypeParser = parser
      }

-- | Creates a 'CustomType' from a predicate that checks bounded integers.
--
-- ==== Example
--
-- @
-- parsePortNumber :: Int -> Bool
-- parsePortNumber n = (n > 0) && (n < 10000)
--
-- portNumberTy = boundedIntCustomType parsePortNumber
-- @
--
-- @since 1.0.0.0
boundedIntCustomType :: (Integral i, Bounded i) => (i -> Bool) -> CustomType
boundedIntCustomType predFn =
  let
    parser = JSON.withScientific $ \input ->
      case toBoundedInteger input of
        Just number
          | predFn number -> Right ()
        _ -> Left ()
  in
    CustomType {
        customTypeParser = parser
      }

-- | Creates a 'CustomType' from a predicate that checks bounded floats.
--
-- @since 1.0.0.0
boundedFloatCustomType :: (RealFloat a) => (a -> Bool) -> CustomType
boundedFloatCustomType predFn =
  let
    parser = JSON.withScientific $ \input -> do
      case toBoundedRealFloat input of
        Right number
          | predFn number -> Right ()
        _ -> Left ()
  in
    CustomType {
        customTypeParser = parser
      }

runJsonParser :: CustomType -> JSON.Parse () ()
runJsonParser (CustomType {customTypeParser}) = customTypeParser

parseCustomType :: Monad m => Bool -> err -> JSON.Value -> CustomType -> JSON.ParseT err m ()
parseCustomType isArray err jsonValue customType = do
  let parser =
        if isArray
          then void $ JSON.eachInArray (runJsonParser customType)
          else runJsonParser customType
  let typeCheck =
        JSON.parseValue parser jsonValue
  case typeCheck of
    Left _ ->
      JSON.throwCustomError err
    Right _ ->
      return ()
