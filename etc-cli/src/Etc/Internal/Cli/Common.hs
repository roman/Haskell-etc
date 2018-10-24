{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Etc.Internal.Cli.Common where

import           RIO
import qualified RIO.Text as Text

import qualified Data.Aeson              as JSON
import qualified Data.Aeson.BetterErrors as JSON
import qualified Options.Applicative     as Opt

import           Etc.Internal.Cli.Error ()
import           Etc.Internal.Cli.Plain.Parser
import           Etc.Internal.Cli.Types
import           Etc.Internal.Renderer ()
import qualified Etc.Internal.Resolver.Types   as Resolver
import           Etc.Internal.Spec.Error ()
import qualified Etc.Internal.Spec.Types       as Spec

fetchCliInfoSpec :: MonadThrow m => Spec.ConfigSpec -> m CliInfoSpec
fetchCliInfoSpec Spec.ConfigSpec {Spec.configSpecFilePath, Spec.configSpecJSON} = do
  let result =
        JSON.parseValue
          (JSON.keyMay "etc/cli" parseCliInfoSpec)
          (JSON.Object configSpecJSON)
  case result of
    Left err ->
      throwM (Resolver.ResolverError (err :: JSON.ParseError CliResolverError))
    Right Nothing ->
      throwM (Resolver.ResolverError (InfoModMissing configSpecFilePath))
    Right (Just cliInfoSpec) -> pure cliInfoSpec


fetchPlainCliInfoSpec :: MonadThrow m => Spec.ConfigSpec -> m CliInfoSpecData
fetchPlainCliInfoSpec spec@Spec.ConfigSpec {Spec.configSpecFilePath} = do
  result <- fetchCliInfoSpec spec
  case result of
    PlainCliInfoSpec topLevelInfo ->
      return topLevelInfo
    CommandCliInfoSpec {} ->
      throwM (Resolver.ResolverError $ PlainInfoModExpected configSpecFilePath)

fetchCommandCliInfoSpec :: MonadThrow m => Spec.ConfigSpec -> m (CliInfoSpecData, Map Text CliInfoSpecData)
fetchCommandCliInfoSpec spec@Spec.ConfigSpec {Spec.configSpecFilePath} = do
  result <- fetchCliInfoSpec spec
  case result of
    PlainCliInfoSpec _ ->
      throwM (Resolver.ResolverError $ CommandInfoModExpected configSpecFilePath)
    CommandCliInfoSpec topLevelInfo infoPerCmdName ->
      return (topLevelInfo, infoPerCmdName)

toOptInfoMod :: CliInfoSpecData -> Opt.InfoMod a
toOptInfoMod cliInfoSpecData =
  Opt.fullDesc <>
  Opt.progDesc (Text.unpack $ cisProgDesc cliInfoSpecData) <>
  maybe Opt.idm (Opt.header . Text.unpack) (cisHeader cliInfoSpecData) <>
  maybe Opt.idm (Opt.footer . Text.unpack) (cisFooter cliInfoSpecData)
