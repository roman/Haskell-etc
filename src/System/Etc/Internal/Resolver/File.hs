{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module System.Etc.Internal.Resolver.File where

import Control.Lens hiding ((<|), (|>))
import Control.Monad.Catch (MonadThrow(..))
import Data.List (foldl')
import Data.Maybe (catMaybes)
import System.Directory (doesFileExist)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Internal as JSON (iparse, IResult(..))
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Text as Text

import System.Etc.Internal.Prelude
import System.Etc.Internal.Types
import qualified System.Etc.Internal.Spec as Spec

--------------------------------------------------------------------------------

parseConfigValue
  :: Monad m
  => Int
  -> Text
  -> JSON.Value
  -> m ConfigValue
parseConfigValue fileIndex filepath' json =
  case json of
    JSON.Object object ->
      SubConfig
        <$> foldM
              (\acc (key, subconfigValue) -> do
                  value1 <- parseConfigValue fileIndex filepath' subconfigValue
                  return <| HashMap.insert key value1 acc)
              HashMap.empty
              (HashMap.toList object)

    _ ->
      return <|
        ConfigValue (Set.singleton <| File fileIndex filepath' json)


parseConfig
  :: MonadThrow m
  => Int
  -> Text
  -> LB8.ByteString
  -> m Config
parseConfig fileIndex filepath' contents =
  case JSON.eitherDecode contents of
    Left err ->
      throwM <| InvalidConfiguration (Text.pack err)

    Right json ->
      case JSON.iparse (parseConfigValue fileIndex filepath') json of
        JSON.IError _ err ->
          throwM <| InvalidConfiguration (Text.pack err)

        JSON.ISuccess result ->
          return (Config result)


readConfigFromFiles :: [Text] -> IO Config
readConfigFromFiles files =
  files
  |> imapM (\fileIndex filepath' ->
               let
                 filepathS = Text.unpack filepath'
               in do
                 fileExists <- doesFileExist filepathS
                 if fileExists then do
                   contents <- LB8.readFile filepathS
                   Just <$> parseConfig fileIndex filepath' contents
                 else
                   return Nothing)
  |> (catMaybes <$>)
  |> ((foldl' (<>) mempty) <$>)

resolveFiles :: Spec.ConfigSpec cmd -> IO Config
resolveFiles =
  Spec.specConfigFilepaths
  >> readConfigFromFiles
