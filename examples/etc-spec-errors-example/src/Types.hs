{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Types where

import           RIO
import qualified RIO.Text as Text

import qualified Data.Aeson as JSON

import qualified Etc.Spec as Etc

newtype TcpUri = TcpUri Text

instance JSON.FromJSON TcpUri where
  parseJSON = JSON.withText "TcpUri" $ \uri ->
    if "tcp://" `Text.isPrefixOf` uri
      then return (TcpUri uri)
      else fail $ "Expecting TCP URI, got unformatted string instead " <> Text.unpack uri

tcpUriType :: Etc.CustomType
tcpUriType = Etc.textCustomType $ \uri ->
  "tcp://" `Text.isPrefixOf` uri

customTypes :: [(Text, Etc.CustomType)]
customTypes =
  [ ("tcp-aeson", Etc.aesonCustomType @TcpUri)
  , ("tcp", tcpUriType)
  ]
