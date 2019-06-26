{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Etc.Internal.Resolver.Cli.Common where

import qualified Prelude as P

import           RIO
import qualified RIO.ByteString.Lazy as BL (toStrict)
import qualified RIO.Set             as Set
import qualified RIO.Text            as Text
import qualified RIO.Vector          as Vector

import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Internal as JSON (IResult (..), iparse)
import qualified Data.Text.IO        as Text
import qualified Options.Applicative as Opt

#if !MIN_VERSION_rio(0,1,9)
import System.Exit
#endif

import qualified System.Etc.Internal.Spec.Parser as Spec
import qualified System.Etc.Internal.Spec.Types  as Spec
import           System.Etc.Internal.Types

--------------------------------------------------------------------------------

{-| A wrapper for sub-routines that return an error message; this was
    created for error report purposes.
-}
newtype GetErrorMessage
  = GetErrorMessage {
    -- | Unwrap IO action with error message
    getErrorMessage :: IO Text
  }

instance Show GetErrorMessage where
  show _ = "<<error message>>"

{-| Error when evaluating the ConfigSpec or when executing the OptParser

-}
data CliConfigError
  -- | The type of the Command Key is invalid
  = InvalidCliCommandKey Text
  -- | Trying to use command for an entry without setting commands section
  | CommandsKeyNotDefined
  -- | Trying to use a command that is not defined in commands section
  | UnknownCommandKey Text
  -- | The command setting is missing on a Command Cli
  | CommandKeyMissing
  -- | There is a command setting on a plain Cli
  | CommandKeyOnPlainCli
  -- | The internal OptParser API failed
  | CliEvalExited ExitCode GetErrorMessage
  deriving (Show)

instance Exception CliConfigError

--------------------------------------------------------------------------------

#if MIN_VERSION_optparse_applicative(0,14,0)
specToCliOptFieldMod :: Opt.HasName f => Spec.CliOptMetadata -> Opt.Mod f a
#endif
specToCliOptFieldMod meta =
  maybe Opt.idm (Opt.long . Text.unpack) (Spec.optLong meta)
    `mappend` maybe Opt.idm Opt.short                shortOption
    `mappend` maybe Opt.idm (Opt.help . Text.unpack) (Spec.optHelp meta)
 where
  shortOption = do
    shortStr <- Spec.optShort meta
    fst <$> Text.uncons shortStr

#if MIN_VERSION_optparse_applicative(0,14,0)
specToCliSwitchFieldMod :: Opt.HasName f => Spec.CliSwitchMetadata -> Opt.Mod f a
#endif
specToCliSwitchFieldMod meta =
  Opt.long (Text.unpack $ Spec.switchLong meta)
    `mappend` maybe Opt.idm (Opt.help . Text.unpack) (Spec.switchHelp meta)

specToCliArgFieldMod :: Spec.CliArgMetadata -> Opt.Mod f a
specToCliArgFieldMod meta = maybe Opt.idm (Opt.help . Text.unpack) (Spec.argHelp meta)

commandToKey :: (MonadThrow m, JSON.ToJSON cmd) => cmd -> m [Text]
commandToKey cmd = case JSON.toJSON cmd of
  JSON.String commandStr -> return [commandStr]
  JSON.Array  jsonList   -> jsonList & Vector.toList & mapM commandToKey & (concat <$>)
  _ ->
    cmd
      & JSON.encode
      & BL.toStrict
      & Text.decodeUtf8'
      & either tshow id
      & InvalidCliCommandKey
      & throwM

jsonOptReader :: Spec.ConfigValueType -> Bool -> String -> Either String (Value JSON.Value)
jsonOptReader cvType isSensitive content =
  let contentText = Text.pack content
      jsonValue   = fromMaybe (JSON.String contentText)
                              (JSON.decodeStrict' $ Text.encodeUtf8 contentText)
  in  case Spec.coerceConfigValueType contentText jsonValue cvType of
        Nothing -> Left "input is not valid"
        Just jsonValue1
          | Spec.matchesConfigValueType jsonValue1 cvType -> Right
          $ markAsSensitive isSensitive jsonValue1
          | otherwise -> Left "input is not valid"

settingsToJsonCli
  :: Spec.ConfigValueType
  -> Bool
  -> Spec.CliEntryMetadata
  -> Opt.Parser (Maybe (Value JSON.Value))
settingsToJsonCli cvType isSensitive specSettings = case specSettings of
  Spec.Opt meta ->
    let requiredCombinator = if Spec.optRequired meta then (Just <$>) else Opt.optional
    in  requiredCombinator $ Opt.option
          (Opt.eitherReader $ jsonOptReader cvType isSensitive)
          (specToCliOptFieldMod meta)

  Spec.Arg meta ->
    let requiredCombinator = if Spec.argRequired meta then (Just <$>) else Opt.optional
    in  requiredCombinator $ Opt.argument
          (Opt.eitherReader $ jsonOptReader cvType isSensitive)
          (specToCliArgFieldMod meta)

  Spec.Switch meta ->
    let requiredCombinator = fmap (Just . Plain . JSON.Bool)
    in  requiredCombinator (Opt.switch (specToCliSwitchFieldMod meta)) <|> pure Nothing



parseCommandJsonValue :: (MonadThrow m, JSON.FromJSON a) => JSON.Value -> m a
parseCommandJsonValue commandValue = case JSON.iparse JSON.parseJSON commandValue of
  JSON.IError _path err -> throwM (InvalidCliCommandKey $ Text.pack err)

  JSON.ISuccess result  -> return result

jsonToConfigValue :: Maybe (Value JSON.Value) -> ConfigValue
jsonToConfigValue specEntryDefVal =
  ConfigValue $ Set.fromList $ maybe [] ((: []) . cliSource 3) specEntryDefVal

handleCliResult :: Either SomeException a -> IO a
handleCliResult result = case result of
  Right config -> return config

  Left  err    -> case fromException err of
    Just (CliEvalExited ExitSuccess (GetErrorMessage getMsg)) -> do
      getMsg >>= Text.putStrLn
      exitSuccess

    Just (CliEvalExited exitCode (GetErrorMessage getMsg)) -> do
      getMsg >>= Text.hPutStrLn stderr
      exitWith exitCode

    _ -> throwIO err

programResultToResolverResult :: MonadThrow m => Text -> Opt.ParserResult a -> m a
programResultToResolverResult progName programResult = case programResult of
  Opt.Success result -> return result

  Opt.Failure failure ->
    let (outputMsg, exitCode) = Opt.renderFailure failure $ Text.unpack progName
    in  throwM $ CliEvalExited exitCode (GetErrorMessage $ return (Text.pack outputMsg))

  Opt.CompletionInvoked compl ->
    let getMsg = Text.pack <$> Opt.execCompletion compl (Text.unpack progName)
    in  throwM $ CliEvalExited ExitSuccess (GetErrorMessage getMsg)
