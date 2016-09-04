{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module UB.Config.Internal.Plain where

import Control.Lens hiding ((<|), (|>))
import Control.Monad.Catch (MonadThrow(..))
import Data.Ord (comparing)
import Data.List (foldl1')
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Internal as JSON (iparse, IResult(..))
import qualified Data.Aeson.Parser as Parser
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Text as Text

import UB.Prelude
import UB.Config.Internal.Types

--------------------------------------------------------------------------------

parseConfigValue
  :: Monad m
  => Int
  -> Text
  -> JSON.Value
  -> m ConfigValue
parseConfigValue index filepath json =
  case json of
    JSON.Object object ->
      SubConfig
        <$> foldM
              (\acc (key, value) -> do
                  value1 <- parseConfigValue index filepath value
                  return <| HashMap.insert key value1 acc)
              HashMap.empty
              (HashMap.toList object)

    _ ->
      return <|
        ConfigValue (Set.singleton <| File index filepath json)


parseConfig
  :: MonadThrow m
  => Int
  -> Text
  -> LB8.ByteString
  -> m Config
parseConfig index filepath contents =
  case JSON.eitherDecode contents of
    Left err ->
      throwM <| InvalidConfiguration (Text.pack err)

    Right json ->
      case JSON.iparse (parseConfigValue index filepath) json of
        JSON.IError _ err ->
          throwM <| InvalidConfiguration (Text.pack err)

        JSON.ISuccess result ->
          return (Config result)


readConfigFromFiles :: [Text] -> IO Config
readConfigFromFiles files =
  files
  |> imapM (\index filepath -> do
               contents <- LB8.readFile <| Text.unpack filepath
               parseConfig index filepath contents)
  |> ((foldl1' (<>)) <$>)
