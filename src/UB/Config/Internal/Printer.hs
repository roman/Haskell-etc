{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module UB.Config.Internal.Printer where

import Text.PrettyPrint.ANSI.Leijen
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Text as Text

import UB.Prelude hiding ((<>), (<$>))
import UB.Config.Internal.Types

renderJsonValue :: JSON.Value -> (Doc, Int)
renderJsonValue value' =
  case value' of
    JSON.String str ->
      (text <| Text.unpack str, Text.length str)

    JSON.Number scientific ->
      let
        number =
          show scientific
      in
        (text number, length number)
    JSON.Bool bool' ->
      if bool' then
        (text "true", 5)
      else
        (text "false", 5)
    _ ->
      error <| "invalid config value creation" `mappend` (show value')

renderConfig :: Config -> Doc
renderConfig (Config configValue0) =
  let
    brackets' = enclose (lbracket <> space) (space <> rbracket)

    renderSource :: ConfigSource -> ((Doc, Int), Doc)
    renderSource source' =
      case source' of
        Default value' ->
          ( renderJsonValue value'
          , brackets' (fill 10 (text "Default"))
          )

        File _index filepath' value' ->
          ( renderJsonValue value'
          , brackets' (fill 10 (text "File:" <+> text (Text.unpack filepath')))
          )

        EnvVar varname value' ->
          ( renderJsonValue value'
          , brackets' (fill 10 (text "Env:" <+> text (Text.unpack varname)))
          )

        OptParse value' ->
          ( renderJsonValue value'
          , brackets' (fill 10 (text "OptParse"))
          )

    renderSources :: [ConfigSource] -> Doc
    renderSources sources0 =
      let
        sources@(((selValueDoc, _), selSourceDoc):others) =
          map renderSource sources0

        fillingWidth =
          sources
          |> map (fst >> snd)
          |> maximum
          |> max 10

        selectedValue =
          [ green <| fill fillingWidth selValueDoc <+> selSourceDoc ]

        otherValues =
          map (\((valueDoc, _), sourceDoc) ->
                  fill fillingWidth valueDoc <+> sourceDoc)
              others
      in
        selectedValue
        |> flip mappend otherValues
        |> vcat
        |> indent 2

    configEntryRenderer :: [Text] -> [Doc] -> Text -> ConfigValue -> [Doc]
    configEntryRenderer keys resultDoc configKey configValue =
      resultDoc `mappend` loop (configKey : keys) configValue

    loop keys configValue =
      case configValue of
        SubConfig subConfigm ->
          HashMap.foldlWithKey'
            (configEntryRenderer keys)
            mempty
            subConfigm

        ConfigValue sources0 ->
          let
            configKey =
              keys
              |> reverse
              |> Text.intercalate "."

            sources =
              Set.toDescList sources0
          in
            [ blue (text (Text.unpack configKey))
             <$$> renderSources sources ]
  in
    loop [] configValue0
    |> intersperse (linebreak <> linebreak)
    |> hcat
    |> (<> linebreak)

printPrettyConfig :: Config -> IO ()
printPrettyConfig =
  renderConfig
  >> putDoc

hPrintPrettyConfig :: Handle -> Config -> IO ()
hPrintPrettyConfig handle' =
  renderConfig
  >> hPutDoc handle'
