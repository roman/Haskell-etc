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
import qualified RIO.Set          as Set
import qualified RIO.Text         as Text
import qualified RIO.Vector       as Vector

import qualified Data.Aeson as JSON

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import System.Etc.Internal.Types

data ColorFn
  = ColorFn {
    greenColor :: !(Doc -> Doc)
  , blueColor  :: !(Doc -> Doc)
  }

renderConfigValueJSON :: JSON.Value -> Either Text Doc
renderConfigValueJSON value = case value of
  JSON.Null -> Right $ text "null"
  JSON.String str -> Right $ text $ Text.unpack str
  JSON.Number scientific -> Right $ text $ show scientific
  JSON.Bool b -> Right $ if b then text "true" else text "false"
  _ ->
    Left $ "Trying to render Unsupported JSON value " `mappend` (tshow value)

renderConfigValue :: (JSON.Value -> Either Text Doc) -> Value JSON.Value -> Either Text [Doc]
renderConfigValue f value =
  case value of
    Plain (JSON.Array jsonArray) ->
      fmap Vector.toList <$> forM jsonArray $ \jsonValue -> do
        valueDoc <- f jsonValue
        return $ text "-" <+> valueDoc
    Plain jsonValue -> fmap return (f jsonValue)
    Sensitive {} -> Right $ return $ text "<<sensitive>>"

renderConfigSource :: (JSON.Value -> Either Text Doc) -> ConfigSource -> Either Text ([Doc], Doc)
renderConfigSource f configSource =
  case configSource of
    Default value -> do
      let sourceDoc = text "Default"
      valueDoc <- renderConfigValue f value
      return (valueDoc, sourceDoc)

    File _index fileSource value ->
      let
        sourceDoc =
          case fileSource of
            FilePathSource filepath ->
              text "File:" <+> text (Text.unpack filepath)
            EnvVarFileSource envVar filepath ->
              text "File:"
              <+> text (Text.unpack envVar) <> "=" <> text (Text.unpack filepath)
      in do
        valueDoc <- renderConfigValue f value
        return (valueDoc, sourceDoc)

    Env varname value -> do
      let sourceDoc = text "Env:" <+> text (Text.unpack varname)
      valueDoc <- renderConfigValue f value
      return (valueDoc, sourceDoc)

    Cli value -> do
      let sourceDoc = text "Cli"
      valueDoc <- renderConfigValue f value
      return (valueDoc, sourceDoc)

    None ->
      return (mempty, mempty)

renderConfig_ :: MonadThrow m => ColorFn -> Config -> m Doc
renderConfig_ ColorFn { blueColor } (Config configMap) =
  let
    renderSources :: MonadThrow m => Text -> [ConfigSource] -> m Doc
    renderSources keyPath sources =
      let
        eSourceDocs =
          mapM (renderConfigSource renderConfigValueJSON) sources

        brackets' = enclose (lbracket <> space) (space <> rbracket)

        layoutSourceValueDoc valueDocs sourceDoc =
          case valueDocs of
            [] ->
              throwM $ InvalidConfiguration (Just keyPath) "Trying to render config entry with no values"

            [singleValueDoc] ->
              -- [Default]
              --   Value 1
              --
              return $ sourceDoc <$$> indent 2 singleValueDoc

            multipleValues ->
              -- [Default]
              --   - Value 1
              --   - Value 2
              --   - Value 3
              --
              return $ sourceDoc <$$> (indent 2 $ align (vsep multipleValues))
      in
        case eSourceDocs of
          Left err ->
            throwM $ InvalidConfiguration (Just keyPath) err

          Right [] ->
              throwM $ InvalidConfiguration (Just keyPath) "Trying to render config entry with no values"

          -- [ (*) CLI ]
          --   - Value 1
          -- [ Default ]
          --   - Value
          Right ((selectedValueDoc, selectedSourceDoc) : otherSourceDocs) -> do
            selectedDoc <-
              layoutSourceValueDoc selectedValueDoc
              $ brackets' (parens (text "*") <+> selectedSourceDoc)

            othersDoc <- forM otherSourceDocs $ \(value, source) ->
              layoutSourceValueDoc value $ brackets' source

            return $ indent 2 $ vsep $ selectedDoc : othersDoc

    renderConfigEntry :: MonadThrow m => [Text] -> [Doc] -> Text -> ConfigValue -> m [Doc]
    renderConfigEntry keyPath accDoc configKey configValue = do
      currentDoc <- loop (configKey : keyPath) configValue
      return $ accDoc `mappend` currentDoc

    loop :: MonadThrow m => [Text] -> ConfigValue -> m [Doc]
    loop keys configValue = case configValue of
      SubConfig subConfigm ->
        foldM (\acc (k,v) -> renderConfigEntry keys acc k v)
              mempty
              (HashMap.toList subConfigm)

      ConfigValue sources0 ->
        let
          keyPathText = Text.intercalate "." $ reverse keys
          sources     = Set.toDescList sources0
        in
          if null sources
          then
            return []
          else do
            configSources <- renderSources keyPathText sources
            return [ blueColor (text $ Text.unpack keyPathText) <$$> configSources ]
  in do
    result <- loop [] configMap
    return $ (hcat $ intersperse (linebreak <> linebreak) $ result) <> linebreak


renderConfigColor :: MonadThrow m => Config -> m Doc
renderConfigColor = renderConfig_ ColorFn {greenColor = green, blueColor = blue}

renderConfig :: MonadThrow m => Config -> m Doc
renderConfig = renderConfig_ ColorFn {greenColor = id, blueColor = id}

printPrettyConfig :: Config -> IO ()
printPrettyConfig = putDoc <=< renderConfigColor

hPrintPrettyConfig :: Handle -> Config -> IO ()
hPrintPrettyConfig someHandle = hPutDoc someHandle <=< renderConfigColor
