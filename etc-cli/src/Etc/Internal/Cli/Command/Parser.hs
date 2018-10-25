{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Etc.Internal.Cli.Command.Parser where

import           RIO
import qualified RIO.Map as Map
import           RIO.Seq (Seq)
import qualified RIO.Set as Set

import qualified Data.Aeson              as JSON
import qualified Data.Aeson.BetterErrors as JSON
import qualified Etc.Internal.Spec.Types as Spec

import Etc.Internal.Cli.Plain.Parser   (parseCliInfoSpec, parseCliEntrySpec)
import Etc.Internal.Cli.Plain.Resolver (fromCliParseError)
import Etc.Internal.Cli.Types

parseCliEntryCommandsSpec :: JSON.Parse err [CmdName]
parseCliEntryCommandsSpec =
  JSON.key "commands" (JSON.eachInArray (CmdName <$> JSON.asText))

parseCliCommands :: JSON.Parse err (Map Text CliInfoSpec)
parseCliCommands =
  Map.fromList <$> JSON.eachInObject parseCliInfoSpec

fetchConfigValueCliSpec ::
     (MonadReader BuilderEnv m, MonadThrow m)
  => Seq EntryKey
  -> Spec.ConfigValueData
  -> m (Maybe (Set CmdName, CliEntrySpec))
fetchConfigValueCliSpec keyPath specData = do
  specFilePath <- (Spec.configSpecFilePath . envConfigSpec) <$> ask
  allCmdNames <- envCommandNames <$> ask
  case configValueJSON of
    JSON.Object _ -> do
      let result =
            JSON.parseValue
              (JSON.keyMay "cli" parseCliCommandEntrySpec)
              configValueJSON
      case result of
        Left err ->
          throwM (fromCliParseError specFilePath keyPath err)
        Right Nothing -> return Nothing
        Right (Just (cmdNames, cliSpec)) -> do
          let cmdNamesSet = Set.fromList cmdNames
              unknownCmdSet = Set.difference cmdNamesSet allCmdNames
          unless (Set.null unknownCmdSet) $
            throwM $
            Spec.SpecError $
            UnknownCommandOnEntry
              specFilePath
              (toList $ keyPath)
              allCmdNames
              unknownCmdSet
          return $ Just (Set.fromList cmdNames, cliSpec)
    _ -> return Nothing
  where
    parseCliCommandEntrySpec =
      (,) <$> parseCliEntryCommandsSpec <*> parseCliEntrySpec configValueType
    Spec.ConfigValueData {Spec.configValueType, Spec.configValueJSON} = specData
