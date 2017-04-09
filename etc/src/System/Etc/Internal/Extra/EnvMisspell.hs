{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Etc.Internal.Extra.EnvMisspell (
    EnvMisspell (..)
  , getEnvMisspells
  , getEnvMisspellsPure
  , renderEnvMisspells
  , hPrintEnvMisspells
  , printEnvMisspellingWarnings
  ) where

import Protolude hiding ((<$>), (<>))

import Data.Vector        (Vector)
import System.Environment (getEnvironment)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text           as Text
import qualified Data.Vector         as Vector
import qualified Text.EditDistance   as Distance

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
  let
    foldEnvSettings val acc =
      case val of
        ConfigValue _ sources ->
          maybe acc (`Vector.cons` acc) (envVar sources)
        SubConfig hsh ->
          HashMap.foldr foldEnvSettings acc hsh
  in
    foldEnvSettings (SubConfig $ specConfigValues spec) Vector.empty

{-|

-}
getEnvMisspellsPure :: ConfigSpec a -> Vector Text -> Vector EnvMisspell
getEnvMisspellsPure spec env = do
   specEnvName  <- lookupSpecEnvKeys spec
   currentEnvName <- env

   let
     distance =
       Distance.levenshteinDistance
           Distance.defaultEditCosts
           (Text.unpack specEnvName)
           (Text.unpack currentEnvName)

   guard (distance >= 1 && distance < 4)
   return $ EnvMisspell currentEnvName specEnvName

{-|

-}
getEnvMisspells :: ConfigSpec a -> IO (Vector EnvMisspell)
getEnvMisspells spec =
  getEnvironment
  & fmap (Vector.fromList . map (Text.pack . fst))
  & fmap (getEnvMisspellsPure spec)

{-|

-}
renderEnvMisspells :: Vector EnvMisspell -> Doc
renderEnvMisspells misspells =
  misspells
  & Vector.map
        (\misspell ->
            text "WARNING: Environment variable `"
            <> text (Text.unpack $ currentText misspell)
            <> text "' found, perhaps you meant `"
            <> text (Text.unpack $ suggestionText misspell)
            <> text "'")
  & Vector.foldl' (<$>) mempty
  & (<$> mempty)


{-|

-}
hPrintEnvMisspells :: Handle -> Vector EnvMisspell -> IO ()
hPrintEnvMisspells h =
  hPutDoc h . renderEnvMisspells


{-|

-}
printEnvMisspellingWarnings :: ConfigSpec a -> IO ()
printEnvMisspellingWarnings spec =
  getEnvMisspells spec >>=
  hPrintEnvMisspells stderr
