{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Etc.Internal.Extra.EnvMisspell (
    EnvMisspell (..)
  , getEnvMisspellings
  , getEnvMisspellingsPure
  , renderEnvMisspellings
  , hPrintEnvMisspellings
  , reportEnvMisspellingWarnings
  ) where

import           RIO         hiding ((<$>), (<>))
import qualified RIO.HashMap as HashMap
import qualified RIO.Text    as Text
import qualified RIO.Vector  as Vector

import System.Environment (getEnvironment)

import qualified Text.EditDistance as Distance

import System.Etc.Internal.Spec.Types
import Text.PrettyPrint.ANSI.Leijen

data EnvMisspell
  = EnvMisspell {
    currentText    :: Text
  , suggestionText :: Text
  }
  deriving (Show, Eq, Generic)

lookupSpecEnvKeys :: ConfigSpec a -> Vector Text
lookupSpecEnvKeys spec =
  let foldEnvSettings val acc = case val of
        ConfigValue ConfigValueData { configSources } ->
          maybe acc (`Vector.cons` acc) (envVar configSources)
        SubConfig hsh -> HashMap.foldr foldEnvSettings acc hsh
  in  foldEnvSettings (SubConfig $ specConfigValues spec) Vector.empty

{-|

-}
getEnvMisspellingsPure :: ConfigSpec a -> Vector Text -> Vector EnvMisspell
getEnvMisspellingsPure spec env = do
  specEnvName    <- lookupSpecEnvKeys spec
  currentEnvName <- env

  let distance = Distance.levenshteinDistance Distance.defaultEditCosts
                                              (Text.unpack specEnvName)
                                              (Text.unpack currentEnvName)

  guard (distance >= 1 && distance < 4)
  return $ EnvMisspell currentEnvName specEnvName

{-|

-}
getEnvMisspellings :: ConfigSpec a -> IO (Vector EnvMisspell)
getEnvMisspellings spec =
  getEnvironment & fmap (Vector.fromList . map (Text.pack . fst)) & fmap
    (getEnvMisspellingsPure spec)

{-|

-}
renderEnvMisspellings :: Vector EnvMisspell -> Doc
renderEnvMisspellings misspells
  | Vector.null misspells
  = mempty
  | otherwise
  = misspells
    & Vector.map
        (\misspell ->
          text "WARNING: Environment variable `"
            <> text (Text.unpack $ currentText misspell)
            <> text "' found, perhaps you meant `"
            <> text (Text.unpack $ suggestionText misspell)
            <> text "'"
        )
    & Vector.foldl' (<$>) mempty
    & (<$> mempty)
    & (<$> mempty)

{-|

-}
hPrintEnvMisspellings :: Handle -> Vector EnvMisspell -> IO ()
hPrintEnvMisspellings h = hPutDoc h . renderEnvMisspellings

{-|

-}
reportEnvMisspellingWarnings :: ConfigSpec a -> IO ()
reportEnvMisspellingWarnings spec =
  getEnvMisspellings spec >>= hPrintEnvMisspellings stderr
