{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module System.Etc.Internal.Extra.Printer (
    renderConfig
  , renderConfigColor
  , printPrettyConfig
  , hPrintPrettyConfig
  ) where

import           RIO               hiding ((<>))
import qualified RIO.HashMap       as HashMap
import           RIO.List          (intersperse)
import qualified RIO.Set           as Set
import qualified RIO.Text          as Text
import qualified RIO.Vector        as Vector
import qualified RIO.Vector.Unsafe as Vector (unsafeHead)

import qualified Data.Aeson as JSON

import Text.PrettyPrint.ANSI.Leijen

import System.Etc.Internal.Types

data ColorFn
  = ColorFn {
    greenColor :: !(Doc -> Doc)
  , blueColor  :: !(Doc -> Doc)
  }

renderConfigValueJSON :: JSON.Value -> Doc
renderConfigValueJSON value = case value of
  JSON.Null                         -> text "null"
  JSON.String str                   -> text $ Text.unpack str
  JSON.Number scientific            -> text $ show scientific
  JSON.Bool   b                     -> if b then text "true" else text "false"
  JSON.Array  (Vector.null -> True) -> text "[]"
  JSON.Array  arr                   -> do
    -- unsafeHead is not unsafe here because of previous check; also we are
    -- assuming all values in the array are of the same type
    let h = Vector.unsafeHead arr
    case h of
      -- When rendering Objects within Arrays; output:
      --
      -- - hello: world
      --   hola: mundo
      -- - foo: bar
      --   baz: wat
      --
      JSON.Object{} -> align
        (vsep $ Vector.toList $ Vector.map
          (\v -> hang 2 ("-" <+> renderConfigValueJSON v))
          arr
        )

      -- When rendering primitive values:
      --
      -- - hello
      -- - world
      --
      _ -> align
        (vsep $ Vector.toList $ Vector.map (\v -> "-" <+> renderConfigValueJSON v) arr)

  JSON.Object obj -> align $ vsep $ map
    (\(k, v) -> case v of
        -- When rendering Objects within Objects; output:
        --
        -- attr1:
        --  attr2: hello
        --
      JSON.Object{} ->
        nest 2 (text (Text.unpack k) <> ":" <> hardline <> renderConfigValueJSON v)

      -- When rendering Arrays within Objects; output:
      --
      -- attr1:
      -- - hello
      --
      JSON.Array{} -> text (Text.unpack k) <> ":" <> hardline <> renderConfigValueJSON v

      -- When rendering primitive values
      --
      -- hello: world
      --
      --
      _            -> text (Text.unpack k) <> ":" <+> renderConfigValueJSON v
    )
    (HashMap.toList obj)

renderConfigSource :: (JSON.Value -> Doc) -> SomeConfigSource -> ([Doc], Doc)
renderConfigSource = sourcePretty

renderConfig_ :: MonadThrow m => ColorFn -> Config -> m Doc
renderConfig_ ColorFn { blueColor } (Config configMap) =
  let
    renderSources :: MonadThrow m => [SomeConfigSource] -> m Doc
    renderSources sources =
      let sourceDocs = map (renderConfigSource renderConfigValueJSON) sources

          brackets'  = enclose (lbracket <> space) (space <> rbracket)

          layoutSourceValueDoc :: MonadThrow m => [Doc] -> Doc -> m Doc
          layoutSourceValueDoc valueDocs sourceDoc = case valueDocs of
            [] ->
              -- [Default]
              --   [] (empty array)
              return $ sourceDoc <$$> indent 2 "[] (empty array)"

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
              return $ sourceDoc <$$> indent 2 (align (vsep multipleValues))
      in  case sourceDocs of
            -- No Value Found
            [] -> return $ indent 2 $ text "No Value Found"

            -- [ (*) CLI ]
            --   - Value 1
            -- [ Default ]
            --   - Value
            ((selectedValueDoc, selectedSourceDoc) : otherSourceDocs) -> do
              selectedDoc <- layoutSourceValueDoc selectedValueDoc
                $ brackets' (parens (text "*") <+> selectedSourceDoc)

              othersDoc <- forM otherSourceDocs
                $ \(value, source) -> layoutSourceValueDoc value $ brackets' source

              return $ indent 2 $ vsep $ selectedDoc : othersDoc

    renderConfigEntry :: MonadThrow m => [Text] -> [Doc] -> Text -> ConfigValue -> m [Doc]
    renderConfigEntry keyPath accDoc configKey configValue = do
      currentDoc <- loop (configKey : keyPath) configValue
      return $ accDoc `mappend` currentDoc

    loop :: MonadThrow m => [Text] -> ConfigValue -> m [Doc]
    loop keys configValue = case configValue of
      SubConfig subConfigm -> foldM (\acc (k, v) -> renderConfigEntry keys acc k v)
                                    mempty
                                    (HashMap.toList subConfigm)

      ConfigValue sources0 ->
        let keyPathText = Text.intercalate "." $ reverse keys
            sources     = Set.toDescList sources0
        in  if null sources
              then return []
              else do
                configSources <- renderSources sources
                return [blueColor (text $ Text.unpack keyPathText) <$$> configSources]
  in
    do
      result <- loop [] configMap
      return $ hcat (intersperse (linebreak <> linebreak) result) <> linebreak


renderConfigColor :: MonadThrow m => Config -> m Doc
renderConfigColor = renderConfig_ ColorFn {greenColor = green, blueColor = blue}

renderConfig :: MonadThrow m => Config -> m Doc
renderConfig = renderConfig_ ColorFn {greenColor = id, blueColor = id}

printPrettyConfig :: Config -> IO ()
printPrettyConfig = putDoc <=< renderConfigColor

hPrintPrettyConfig :: Handle -> Config -> IO ()
hPrintPrettyConfig someHandle = hPutDoc someHandle <=< renderConfigColor
