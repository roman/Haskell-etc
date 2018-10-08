{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_HADDOCK hide #-}
module Etc.Internal.Config.TH where

import RIO
import qualified RIO.Map as Map
import qualified RIO.Text as Text

import Language.Haskell.TH        (ExpQ, runIO)

import qualified  Data.Text.Prettyprint.Doc as Doc

import Etc.Internal.Renderer
import Etc.Internal.Spec.Types as Spec
import Etc.Internal.Config as Config

data CheckedConfigValueError
  = UnexpectedSubConfigEntry
      !Spec.SpecFilePath
      !Spec.SpecEntryPath
      ![Text] -- Valid keys after the given SpecEntryPath
  | UnexpectedConfigValueEntry
      !Spec.SpecFilePath
      ![Text] -- Expected valid key path
      ![Text] -- Extra invalid keys
  | FoundUnknownKey
       !Spec.SpecFilePath
       ![Text] -- Correct Path
       ![Text] -- Expected valid keys
       !Text   -- Unknown key used instead
  deriving (Show)

instance HumanErrorMessage CheckedConfigValueError where
  humanErrorMessage = Doc.pretty . show

assertKeyPath :: [Text] -> ConfigSpec -> IO ()
assertKeyPath keyPath (ConfigSpec {configSpecFilePath, configSpecEntries}) =
  loop keyPath (Spec.SubConfig configSpecEntries)
  where
    loop [] (Spec.SubConfig hsh) =
      throwM
        (Spec.SpecError $
         UnexpectedSubConfigEntry configSpecFilePath keyPath (Map.keys hsh))
    loop [] _ = return ()
    loop ks (Spec.ConfigValue {}) =
      let expectedOnly = take (length keyPath - length ks) keyPath
          gotExtra = ks
       in throwM
            (Spec.SpecError $
             UnexpectedConfigValueEntry configSpecFilePath expectedOnly gotExtra)
    loop kss@(k:ks) (Spec.SubConfig hsh) =
      case Map.lookup k hsh of
        Just inner -> loop ks inner
        Nothing ->
          let correctPath = take (length keyPath - length kss) keyPath
              knownKeys = Map.keys hsh
              invalidKey = k
           in throwM
                (Spec.SpecError $
                 FoundUnknownKey
                   configSpecFilePath
                   correctPath
                   knownKeys
                   invalidKey)

-- | An enhanced version of 'Config.getConfigValue' which checks the given keys
-- are present in the configuration spec entries.
--
-- This invocation will fail at /compilation time/ if the given key path is not
-- present in the configuration spec.
--
-- ==== To keep in mind
--
-- This function requires the @TemplateHaskell@ GHC extension to work.
--
-- ==== Example
--
-- @
-- main = do
--   ...
--   configValue - $(checkedConfigValue configSpec [\"db\", \"password\"]) config
--   ...
-- @
--
-- @since 1.0.0.0
checkedConfigValue :: Spec.ConfigSpec -> [Text] -> ExpQ
checkedConfigValue spec keyPath = do
  runIO $ assertKeyPath keyPath spec
  let keyPathStr = map Text.unpack keyPath
  [|Config.getConfigValue (map Text.pack keyPathStr)|]
