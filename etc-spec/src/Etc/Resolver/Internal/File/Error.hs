{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Etc.Resolver.Internal.File.Error where

import           RIO
import qualified RIO.Text as Text

import qualified Data.Aeson as JSON

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util (reflow)

import System.FilePath (takeExtension)

import Etc.Renderer
import Etc.Resolver.Internal.File.Types
import Etc.Spec.Internal.Types

--------------------------------------------------------------------------------
-- File Resolver

renderFileOrigin :: FileValueOrigin -> Doc Ann
renderFileOrigin origin =
  case origin of
    ConfigFileOrigin path -> annotate Filepath (dquotes (pretty path))
    EnvFileOrigin env path ->
      annotate Filepath (dquotes (pretty path)) <+>
      "(from ENV" <+> annotate Envvar (dquotes (pretty env)) <> ")"

renderFileOrigin1 :: FileValueOrigin -> Doc Ann
renderFileOrigin1 origin =
  case origin of
    ConfigFileOrigin path -> annotate Filepath (dquotes (pretty path))
    EnvFileOrigin _env path ->
      annotate Filepath (dquotes (pretty path))

getFileFromOrigin :: FileValueOrigin -> Doc ann
getFileFromOrigin origin =
  case origin of
    ConfigFileOrigin path   -> dquotes (pretty path)
    EnvFileOrigin _env path -> dquotes (pretty path)

--------------------------------------------------------------------------------
-- ConfigSpecFilesEntryMissing

configSpecFilesEntryMissingBody :: [Doc ann] -> Doc ann
configSpecFilesEntryMissingBody siblingKeys =
  vsep
    [ "Other keys found in the configuration spec file"
    , mempty
    , indent 2 $ vsep $ map (\k -> k <> ": ...") siblingKeys
    ]

renderConfigSpecFilesEntryMissingBody :: [Text] -> Doc Ann
renderConfigSpecFilesEntryMissingBody siblingKeys =
  foundError3
    (reflow "The \"etc/files\" is entry missing in the configuration spec file")
    (configSpecFilesEntryMissingBody (map pretty siblingKeys))
    [ reflow
        "Make sure to include the \"etc/files\" entry in your configuration spec file, you can find more information at <PENDING_URL>"
    ]

--------------------------------------------------------------------------------
-- ConfigSpecFilesPathsEntryIsEmpty

renderConfigSpecFilesPathsEntryIsEmpty :: Doc Ann
renderConfigSpecFilesPathsEntryIsEmpty =
  foundError2
    (reflow
       "The \"etc/files.path\" entry in your configuration spec file is empty")
    [ reflow
        "Make sure to have a valid \"etc/files\" entry in your configuration spec file, you can find more information at <PENDING_URL>"
    ]

--------------------------------------------------------------------------------
-- UnsupportedFileExtensionGiven

unsupportedFileExtensionGivenBody :: Doc Ann -> Doc Ann
unsupportedFileExtensionGivenBody filepath =
  vsep
    [ "In the configuration spec file"
    , mempty
    , indent 2 $
      renderKeyPathBody ["etc/files", "path"] $
      newlineBody $ vsep ["- ...", annotate Error $ pointed ("-" <+> filepath), "- ..."]
    ]


renderUnsupportedFileExtensionGiven :: Text -> [Text] -> Doc Ann
renderUnsupportedFileExtensionGiven filepath supportedExtensions =
  foundError3
    (reflow "Detected a configuration file with an unsupported extension")
    (unsupportedFileExtensionGivenBody $ pretty filepath)
    (map (\supportedExtension ->
            reflow "Change the file extension from" <+>
           dquotes (pretty $ takeExtension (Text.unpack filepath)) <+>
           reflow "to supported extension" <+> dquotes ("." <> (pretty supportedExtension)))
         supportedExtensions)


--------------------------------------------------------------------------------
-- ConfigFileValueTypeMismatch


configFileValueTypeMismatchFoundBody ::
  FileValueOrigin -> [Doc Ann] -> ConfigValueType -> JSON.Value -> Doc Ann
configFileValueTypeMismatchFoundBody origin keyPath cvType jsonVal =
  vsep
    [ "In the configuration file" <+> renderFileOrigin origin
    , mempty
    , indent 2 $
      renderKeyPathBody keyPath $
      newlineBody $ annotate Current (renderJsonValue jsonVal)
    , mempty
    , "The" <+>
      annotate Current "current value" <+>
      "does not match the" <+>
      "expected type" <+> annotate Expected (renderConfigValueType cvType)
    ]

renderConfigFileValueTypeMismatchFound ::
  FileValueOrigin -> [Text] -> ConfigValueType -> JSON.Value -> Doc Ann
renderConfigFileValueTypeMismatchFound origin keyPath cvType jsonVal =
  foundError3
    (reflow
       "There is a mistmach between a configuration file value and the type specified in the configuration spec file")
    (configFileValueTypeMismatchFoundBody origin (map pretty keyPath) cvType jsonVal)
    [ reflow "Change the value to match the given type" <+>
      dquotes (renderConfigValueType cvType)
    , case renderJsonType jsonVal of
        Just jsonTyDoc ->
          reflow "In the configuration spec file, change the entry \"type\" to" <+>
          dquotes jsonTyDoc <+> reflow "to match the file value"
        Nothing -> mempty
    ]

--------------------------------------------------------------------------------
-- ConfigFileNotPresent

renderConfigFileNotPresent :: Text -> Doc Ann
renderConfigFileNotPresent filepath =
  let
    filepathDoc = dquotes (pretty filepath)
  in
    foundError3
      (reflow "Didn't find a configuration file specified in the configuration spec file")
      (vsep [
             reflow "In the configuration spec file"
            , mempty
            , indent 2 $
              renderKeyPathBody ["etc/files", "path"] $
              newlineBody $ vsep ["- ...", annotate Error $ pointed ("-" <+> filepathDoc), "- ..."]
            ])
      [ reflow
          "Add a new configuration file at location" <+> annotate Filepath filepathDoc
      ]

--------------------------------------------------------------------------------
-- UnknownConfigKeyFound

unknownConfigKeyFoundBody :: FileValueOrigin -> [Doc Ann] -> Doc Ann -> Doc Ann
unknownConfigKeyFoundBody fileSource keyPath unknownKey =
  vsep
    [ "On an entry in the configuration file" <+> renderFileOrigin fileSource
    , mempty
    , indent 2 $
      renderKeyPathBody keyPath $
      newlineBody $ (annotate Error $ pointed (unknownKey <> ": ..."))
    ]

renderUnknownConfigKeyFound :: FileValueOrigin -> [Text] -> Text -> Doc Ann
renderUnknownConfigKeyFound origin keyPath unknownKey =
  let
    keyPathDoc = map pretty keyPath
    unknownKeyDoc = pretty unknownKey
  in
    foundError3
      (reflow
         "Detected a configuration file with an entry not present in the configuration spec file")
      (unknownConfigKeyFoundBody origin keyPathDoc unknownKeyDoc)
      [ reflow "Remove unknown entry" <+> dquotes unknownKeyDoc <+> reflow "from the configuration file" <+> renderFileOrigin1 origin
      , vsep [ reflow "Add the following entry to your configuration spec file:"
             , mempty
             , indent 4 $
               renderSpecKeyPath (keyPathDoc ++ [unknownKeyDoc]) "..."
             ]
      ]


--------------------------------------------------------------------------------
-- ConfigInvalidSyntaxFound

renderConfigInvalidSyntaxFound :: FileValueOrigin -> Doc Ann
renderConfigInvalidSyntaxFound origin =
  let filepathDoc = getFileFromOrigin origin
   in foundError3
        (reflow "Found a configuration file with unsupported syntax")
        (vsep
           [ reflow "In the configuration spec file"
           , mempty
           , indent 2 $
             renderKeyPathBody ["etc/files", "path"] $
             newlineBody $
             vsep
               [ "- ..."
               , annotate Error $ pointed ("-" <+> filepathDoc)
               , "- ..."
               ]
           ])
        [ reflow "Make sure the contents of the configuration file" <+>
           filepathDoc <+> reflow "are valid"
        ]


--------------------------------------------------------------------------------
-- SubConfigEntryExpected

subConfigEntryExpectedBody ::
  FileValueOrigin -> [Doc Ann] -> JSON.Value -> Doc Ann
subConfigEntryExpectedBody origin keyPath jsonVal =
  vsep
    [ "In the configuration file" <+> renderFileOrigin origin
    , mempty
    , indent 2 $
      renderKeyPathBody keyPath $
      newlineBody $ annotate Current (renderJsonValue jsonVal)
    , mempty
    , "The" <+>
      annotate Current "current entry" <+>
      "does not match the definition in the configuration spec"
    ]

renderSubConfigEntryExpected ::
  FileValueOrigin -> [Text] -> JSON.Value -> Doc Ann
renderSubConfigEntryExpected origin keyPath jsonVal =
  foundError3
    (reflow
       "There is a mistmach between a configuration file entry and the spec specified in the configuration spec file")
    (subConfigEntryExpectedBody origin (map pretty keyPath) jsonVal)
    [ reflow "Change the entry found in the configuration file" <+>
      renderFileOrigin1 origin <+>
      reflow "to match the definition found in the spec"
    ]

--------------------------------------------------------------------------------

instance Exception FileResolverError where
  displayException err =
    renderErrorDoc $ case err of
      ConfigSpecFilesEntryMissing siblingKeys ->
        renderConfigSpecFilesEntryMissingBody siblingKeys

      ConfigSpecFilesPathsEntryIsEmpty ->
        renderConfigSpecFilesPathsEntryIsEmpty

      UnsupportedFileExtensionGiven path ext ->
        renderUnsupportedFileExtensionGiven path ext

      ConfigFileValueTypeMismatch origin keyPath cvType jsonVal ->
        renderConfigFileValueTypeMismatchFound origin keyPath cvType jsonVal

      ConfigFileNotPresent path ->
        renderConfigFileNotPresent path

      UnknownConfigKeyFound origin keyPath keyName _siblingKeys ->
        renderUnknownConfigKeyFound origin keyPath keyName

      ConfigInvalidSyntaxFound origin ->
        renderConfigInvalidSyntaxFound origin

      SubConfigEntryExpected origin keyPath jsonVal ->
        renderSubConfigEntryExpected origin keyPath jsonVal
