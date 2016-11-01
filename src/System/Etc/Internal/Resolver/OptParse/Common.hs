{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module System.Etc.Internal.Resolver.OptParse.Common where

import System.Etc.Internal.Prelude hiding ((&))
import qualified Prelude as P

import Control.Monad.Catch (MonadThrow, throwM)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Internal as JSON (iparse, IResult(..))
import qualified Data.ByteString.Lazy.Char8 as BL (unpack)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified Options.Applicative as Opt

import System.Etc.Internal.Types
import qualified System.Etc.Internal.Spec as Spec

--------------------------------------------------------------------------------

newtype GetMessage
  = GetMessage (IO Text)

instance Show GetMessage where
  show _ = "<<message>>"

data OptParseConfigError
  = InvalidOptParseCommandKey Text
  -- ^ The type of the Command Key is invalid
  | CommandsKeyNotDefined
  -- ^ Trying to use command for an entry without setting commands section
  | UnknownCommandKey Text
  -- ^ Trying to use a command that is not defined in commands section
  | CommandKeyMissing
  -- ^ The command setting is missing on a Command OptParser
  | CommandKeyOnPlainParser
  -- ^ There is a command setting on a plain OptParser
  | OptParseEvalExited ExitCode GetMessage
  deriving (Show)

instance Exception OptParseConfigError

--------------------------------------------------------------------------------

specToOptParserSwitchFieldMod specSettings =
  maybe Opt.idm
        (Text.unpack >> Opt.long)
        (Spec.optParseLong specSettings)
  `mappend` maybe Opt.idm
                  (Text.head >> Opt.short)
                  (Spec.optParseShort specSettings)
  `mappend` maybe Opt.idm
                  (Text.unpack >> Opt.help)
                  (Spec.optParseHelp specSettings)

specToOptParserVarFieldMod specSettings =
  specToOptParserSwitchFieldMod specSettings
  `mappend` maybe Opt.idm
                  (Text.unpack >> Opt.metavar)
                  (Spec.optParseMetavar specSettings)


commandToKey :: (MonadThrow m, JSON.ToJSON cmd) => cmd -> m [Text]
commandToKey cmd =
  case JSON.toJSON cmd of
    JSON.String commandStr ->
      return [commandStr]
    JSON.Array jsonList ->
      jsonList
        |> Vector.toList
        |> mapM commandToKey
        |> (concat <$>)
    _ ->
      cmd
        |> JSON.encode
        |> BL.unpack
        |> Text.pack
        |> InvalidOptParseCommandKey
        |> throwM

settingsToJsonOptParser
  :: Spec.OptParseEntrySpecSettings
    -> Opt.Parser (Maybe JSON.Value)
settingsToJsonOptParser specSettings =
  let
    requiredCombinator =
        if Spec.optParseRequired specSettings then
          (Just <$>)
        else
          Opt.optional
  in
    requiredCombinator <|
    case specSettings of
      Spec.Option {} ->
        case Spec.optParseOptionValueType specSettings of
          Spec.OptParseOptionString ->
            (Text.pack >> JSON.String)
            <$> Opt.strOption (specToOptParserVarFieldMod specSettings)

          Spec.OptParseOptionNumber ->
            (fromInteger >> JSON.Number)
            <$> Opt.option Opt.auto (specToOptParserVarFieldMod specSettings)

          Spec.OptParseOptionSwitch ->
            JSON.Bool
            <$> Opt.switch (specToOptParserSwitchFieldMod specSettings)

      Spec.Argument {} ->
        case Spec.optParseArgValueType specSettings of
          Spec.OptParseArgString ->
            (Text.pack >> JSON.String)
            <$> Opt.strArgument ( specSettings
                                  |> Spec.optParseMetavar
                                  |> maybe Opt.idm (Text.unpack >> Opt.metavar))
          Spec.OptParseArgNumber ->
            (fromInteger >> JSON.Number)
            <$> Opt.argument Opt.auto
                             ( specSettings
                                  |> Spec.optParseMetavar
                                  |> maybe Opt.idm (Text.unpack >> Opt.metavar))

parseCommandJsonValue
  :: (MonadThrow m, JSON.FromJSON a)
    => JSON.Value
    -> m a
parseCommandJsonValue commandValue =
  case JSON.iparse JSON.parseJSON commandValue of
    JSON.IError _path err ->
      throwM (InvalidOptParseCommandKey <| Text.pack err)

    JSON.ISuccess result ->
      return result

jsonToConfigValue
  :: Maybe JSON.Value
    -> Maybe JSON.Value
    -> ConfigValue
jsonToConfigValue specEntryDefVal mJsonValue =
  ConfigValue
    <| Set.fromList
    <| case (mJsonValue, specEntryDefVal) of
      (Just val, Just defValue) ->
        [OptParse val, Default defValue]

      (Just val, _) ->
        [OptParse val]

      (_, Just defValue) ->
        [Default defValue]

      _ ->
        crash "invalid spec creation"

handleOptParseResult
  :: Either SomeException a -> IO a
handleOptParseResult result =
  case result of
    Right config ->
      return config

    Left err ->
      case fromException err of
        Just (OptParseEvalExited ExitSuccess (GetMessage getMsg)) -> do
          getMsg >>= putStrLn
          exitSuccess

        Just (OptParseEvalExited exitCode (GetMessage getMsg)) -> do
          getMsg >>= Text.hPutStrLn stderr
          exitWith exitCode

        _ ->
          throwIO err

programResultToResolverResult
  :: MonadThrow m
    => Text
    -> Opt.ParserResult a
    -> m a
programResultToResolverResult progName programResult =
  case programResult of
    Opt.Success result ->
      return result

    Opt.Failure failure ->
      let
        (outputMsg, exitCode) =
          Opt.renderFailure failure <| Text.unpack progName
      in
        throwM
        <| OptParseEvalExited exitCode (GetMessage <| return (Text.pack outputMsg))

    Opt.CompletionInvoked compl ->
      let
        getMsg =
          Text.pack <$> (Opt.execCompletion compl <| Text.unpack progName)
      in
        throwM
        <| OptParseEvalExited ExitSuccess (GetMessage getMsg)
