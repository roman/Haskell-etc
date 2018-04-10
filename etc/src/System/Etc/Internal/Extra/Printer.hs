{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Etc.Internal.Extra.Printer (
    renderConfig
  , renderConfigColor
  , printPrettyConfig
  , hPrintPrettyConfig
  ) where

import           RIO              hiding ((<>))
import qualified RIO.HashMap      as HashMap
import           RIO.List         (intersperse)
import           RIO.List.Partial (maximum)
import qualified RIO.Set          as Set
import qualified RIO.Text         as Text

import qualified Data.Aeson as JSON

import Text.PrettyPrint.ANSI.Leijen

import System.Etc.Internal.Types

renderJsonValue :: Text -> Value JSON.Value -> (Doc, Int)
renderJsonValue key value' = case value' of
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
      & InvalidConfiguration (Just key)
      & show
      & error

data ColorFn
  = ColorFn {
    greenColor :: !(Doc -> Doc)
  , blueColor  :: !(Doc -> Doc)
  }

renderConfig' :: ColorFn -> Config -> Doc
renderConfig' ColorFn { greenColor, blueColor } (Config configValue0) =
  let
    brackets' = enclose (lbracket <> space) (space <> rbracket)

    renderSource :: Text -> ConfigSource -> ((Doc, Int), Doc)
    renderSource key source' = case source' of
      Default value' -> (renderJsonValue key value', brackets' (fill 10 (text "Default")))

      File _index fileSource value' -> case fileSource of
        FilePathSource filepath' ->
          ( renderJsonValue key value'
          , brackets' (fill 10 (text "File:" <+> text (Text.unpack filepath')))
          )
        EnvVarFileSource envVar filepath' ->
          ( renderJsonValue key value'
          , brackets'
            (fill
              10
              (text "File:" <+> text (Text.unpack envVar) <> "=" <> text
                (Text.unpack filepath')
              )
            )
          )

      Env varname value' ->
        ( renderJsonValue key value'
        , brackets' (fill 10 (text "Env:" <+> text (Text.unpack varname)))
        )

      Cli value' -> (renderJsonValue key value', brackets' (fill 10 (text "Cli")))

      None       -> ((mempty, 0), mempty)

    renderSources :: Text -> [ConfigSource] -> Doc
    renderSources keys sources0 =
      let
        -- NOTE: I've already checked for the list to not be empty,
        -- so is safe to do this destructuring here
        sources@(((selValueDoc, _), selSourceDoc) : others) =
          map (renderSource keys) sources0

        -- NOTE: I've already checked for the list to not be empty,
        -- so is safe to use partial function maximum here
        fillingWidth  = sources & map (snd . fst) & maximum & max 10

        selectedValue = [greenColor $ fill fillingWidth selValueDoc <+> selSourceDoc]

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
        let
          configKey = keys & reverse & Text.intercalate "."

          sources   = Set.toDescList sources0
        in
          if null sources
          then
            []
          else
            [blueColor (text (Text.unpack configKey)) <$$> renderSources configKey sources]
  in
    loop [] configValue0 & intersperse (linebreak <> linebreak) & hcat & (<> linebreak)


renderConfigColor :: Config -> Doc
renderConfigColor = renderConfig' ColorFn {greenColor = green, blueColor = blue}

renderConfig :: Config -> Doc
renderConfig = renderConfig' ColorFn {greenColor = id, blueColor = id}

printPrettyConfig :: Config -> IO ()
printPrettyConfig = putDoc . renderConfig

hPrintPrettyConfig :: Handle -> Config -> IO ()
hPrintPrettyConfig handle' = hPutDoc handle' . renderConfig
