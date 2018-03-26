{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Etc.Internal.Extra.Printer (
    renderConfig
  , printPrettyConfig
  , hPrintPrettyConfig
  ) where

import           RIO         hiding ((<>))
import qualified RIO.HashMap as HashMap
import           RIO.List    (intersperse, maximum)
import qualified RIO.Set     as Set
import qualified RIO.Text    as Text

import qualified Data.Aeson as JSON

import Text.PrettyPrint.ANSI.Leijen

import System.Etc.Internal.Types

renderJsonValue :: Value JSON.Value -> (Doc, Int)
renderJsonValue value' = case value' of
  Plain JSON.Null         -> (text "null", 4)

  Plain (JSON.String str) -> (text $ Text.unpack str, Text.length str)

  Plain (JSON.Number scientific) ->
    let number = show scientific in (text number, length number)
  Plain     (JSON.Bool bool') -> if bool' then (text "true", 5) else (text "false", 5)
  Sensitive _                 -> (text "<<sensitive>>", 13)
  _ ->
    value'
      & tshow
      & ("Invalid configuration value creation " `mappend`)
      & InvalidConfiguration
      & show
      & error


renderConfig :: Config -> Doc
renderConfig (Config configValue0) =
  let
    brackets' = enclose (lbracket <> space) (space <> rbracket)

    renderSource :: ConfigSource -> ((Doc, Int), Doc)
    renderSource source' = case source' of
      Default value' -> (renderJsonValue value', brackets' (fill 10 (text "Default")))

      File _index filepath' value' ->
        ( renderJsonValue value'
        , brackets' (fill 10 (text "File:" <+> text (Text.unpack filepath')))
        )

      Env varname value' ->
        ( renderJsonValue value'
        , brackets' (fill 10 (text "Env:" <+> text (Text.unpack varname)))
        )

      Cli value' -> (renderJsonValue value', brackets' (fill 10 (text "Cli")))

      None       -> ((mempty, 0), mempty)

    renderSources :: [ConfigSource] -> Doc
    renderSources sources0 =
      let
        sources@(((selValueDoc, _), selSourceDoc) : others) = map renderSource sources0

        fillingWidth  = sources & map (snd . fst) & maximum & max 10

        selectedValue = [green $ fill fillingWidth selValueDoc <+> selSourceDoc]

        otherValues   = map
          (\((valueDoc, _), sourceDoc) -> fill fillingWidth valueDoc <+> sourceDoc)
          others
      in
        selectedValue & flip mappend otherValues & vcat & indent 2

    configEntryRenderer :: [Text] -> [Doc] -> Text -> ConfigValue -> [Doc]
    configEntryRenderer keys resultDoc configKey configValue =
      resultDoc `mappend` loop (configKey : keys) configValue

    loop keys configValue = case configValue of
      SubConfig subConfigm ->
        HashMap.foldlWithKey' (configEntryRenderer keys) mempty subConfigm

      ConfigValue sources0 ->
        let configKey = keys & reverse & Text.intercalate "."

            sources   = Set.toDescList sources0
        in  if null sources
              then []
              else [blue (text (Text.unpack configKey)) <$$> renderSources sources]
  in
    loop [] configValue0 & intersperse (linebreak <> linebreak) & hcat & (<> linebreak)

printPrettyConfig :: Config -> IO ()
printPrettyConfig = putDoc . renderConfig

hPrintPrettyConfig :: Handle -> Config -> IO ()
hPrintPrettyConfig handle' = hPutDoc handle' . renderConfig
