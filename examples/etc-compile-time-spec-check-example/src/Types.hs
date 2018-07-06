{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import           RIO
import qualified RIO.Text as Text

import Language.Haskell.TH.Syntax (Lift)

import qualified Data.Aeson       as JSON
import qualified Data.Aeson.Types as JSON (typeMismatch)

data Cmd
  = PrintConfig
  | RunMain
  deriving (Show, Eq, Generic, Lift)

instance Hashable Cmd

instance JSON.FromJSON Cmd where
  parseJSON json =
    case json of
      JSON.String cmdName
        | cmdName == "config" ->
          return PrintConfig

        | cmdName == "run" ->
          return RunMain

        | otherwise ->
          JSON.typeMismatch ("Cmd (" <> Text.unpack cmdName <> ")") json
      _ ->
        JSON.typeMismatch "Cmd" json

instance JSON.ToJSON Cmd where
  toJSON cmd =
    case cmd of
      PrintConfig ->
        JSON.String "config"
      RunMain ->
        JSON.String "run"
