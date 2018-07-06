{-# LANGUAGE DeriveLift          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main where

import qualified Prelude
import           RIO

import Data.Proxy (Proxy)

import qualified System.Etc as Etc

-- NOTE: Given TemplateHaskell restrictions, this need to be created
-- in a different module
import Types (Cmd (..))

main :: IO ()
main = do
  let configSpec = $(Etc.readConfigSpecTH (undefined :: Proxy Cmd) "./resources/spec.yaml")

  Etc.reportEnvMisspellingWarnings configSpec

  (configFiles, _fileWarnings) <- Etc.resolveFiles configSpec
  configEnv                    <- Etc.resolveEnv configSpec
  (cmd, configOptParser)       <- Etc.resolveCommandCli configSpec

  let configDefault = Etc.resolveDefault configSpec

      config =
        configFiles `mappend` configDefault `mappend` configEnv `mappend` configOptParser

  case cmd of
    PrintConfig -> liftIO $ Etc.printPrettyConfig config

    RunMain     -> do
      Prelude.putStrLn "Executing main program"
      liftIO $ Etc.printPrettyConfig config
