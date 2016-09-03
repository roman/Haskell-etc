{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
module UB.Internal.Config.Resolver where

import UB.Prelude hiding ((&))
import UB.Lens.EDN

import Data.Vector (Vector)
import Control.Lens ((&), (.~), (%~))
import System.Environment (getEnv, lookupEnv)
import Data.Maybe (fromMaybe)

import qualified Control.Lens as L
import qualified UB.Internal.Config.Unresolved as Unresolved
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.EDN as EDN
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.Map as Map

type ConfigKey = Text

data ConfigSource
  = File     { filepath :: Text
             , index    :: Int
             , value    :: EDN.Value }
  | EnvVar   { value :: EDN.Value, envVar :: Text }
  | OptParse { option :: Text }
  deriving (Show)

data ConfigValue
  = ConfigValue { configValue  :: EDN.Value
                , configSource :: Vector [ConfigSource] }
  | SubConfig { subConfig :: Map EDN.Value ConfigValue }
  deriving (Show)

newtype Config
  = Config { fromConfig :: ConfigValue }
  deriving (Show)


$(L.makePrisms ''ConfigValue)
$(L.makePrisms ''Config)

-- Works like the _Just prism, but instead of not doing anything on Nothing, it
-- creates a ConfigValue record
_JustConfigValue
  :: EDN.Value
    -> Vector [ConfigSource]
    -> L.Prism (Maybe ConfigValue) (Maybe ConfigValue) ConfigValue ConfigValue
_JustConfigValue ednVal source =
  L.prism Just <| maybe (Right <| ConfigValue ednVal source) Right


_JustSubConfig :: L.Prism (Maybe ConfigValue) (Maybe ConfigValue) ConfigValue ConfigValue
_JustSubConfig =
  L.prism Just <| maybe (Right <| SubConfig Map.empty) Right

--------------------------------------------------------------------------------

readEnvVar :: Text -> IO Text
readEnvVar name =
  Text.pack <$> getEnv (Text.unpack name)

maybeReadEnvVar :: Text -> IO (Maybe Text)
maybeReadEnvVar name =
  (Text.pack <$>) <$> lookupEnv (Text.unpack name)

-- buildConfigResolver
--   :: (Int, Text)
--     -> L.Lens' Unresolved.Config Unresolved.Config
--     -> Vector (Config -> IO Config)
buildConfigResolver (index, filepath) lensFn config =
  case config of
    Unresolved.Config (Unresolved.SubConfig configm) ->
      let

        step key unresolvedConfigVal acc =
          case unresolvedConfigVal of
            Unresolved.ConfigValue defaultVal sources ->
              case (defaultVal, sources) of

                -- there is no default value on the file configuration
                -- but there is an EnvVar
                ( Nothing
                  , Unresolved.ConfigSources
                      (Unresolved.Pending (Unresolved.EnvVar varname))
                      _
                      _
                  ) ->
                  let
                    result =
                      Vector.singleton <|
                      (\resolvedConfig -> do
                          envValue <- EDN.String <$> readEnvVar varname

                          let
                            sources =
                              [ EnvVar envValue varname ]

                            fieldLens =
                              lensFn
                              << _SubConfig
                              << L.at key
                              << (_JustConfigValue envValue Vector.empty)
                              << _ConfigValue

                          return <|
                            resolvedConfig
                            & (fieldLens << L._1) .~ envValue
                            & (fieldLens << L._2) %~ (Vector.cons sources))
                  in
                    Vector.concat [ acc
                                  , result ]


                -- there is a default value on the file configuration
                -- but there is an EnvVar and it has precedence
                ( Just fileValue
                  , Unresolved.ConfigSources
                      (Unresolved.Pending (Unresolved.EnvVar varname))
                      _
                      _
                  ) ->
                  let
                    result =
                      Vector.singleton <|
                      (\resolvedConfig -> do
                          menvValue <- (EDN.String <$>) <$> maybeReadEnvVar varname

                          let
                            ednValue =
                              fromMaybe fileValue menvValue

                            sources =
                              maybe
                              [ File filepath index fileValue ]
                              (\envValue ->
                                  [ EnvVar envValue varname
                                  , File filepath index fileValue ])
                              menvValue

                            fieldLens =
                              lensFn
                              << _SubConfig
                              << L.at key
                              << (_JustConfigValue ednValue Vector.empty)
                              << _ConfigValue

                          return <|
                            resolvedConfig
                            & (fieldLens << L._1)
                            .~ ednValue
                            & (fieldLens << L._2)
                            %~ (Vector.cons sources))
                  in
                    Vector.concat [ acc
                                  , result ]


                -- there is only a default value on the file configuration
                ( Just fileValue
                  , Unresolved.ConfigSources Unresolved.Skip
                                             _
                                             _
                  ) ->
                  let
                    result =
                      Vector.singleton <|
                        (\resolvedConfig ->
                           let
                             sources =
                               [ File filepath index fileValue ]

                             fieldLens =
                               lensFn
                               << _SubConfig
                               << L.at key
                               << (_JustConfigValue fileValue Vector.empty)
                               << _ConfigValue

                           in
                             return <|
                               resolvedConfig
                                 & (fieldLens << L._1) .~ fileValue
                                 & (fieldLens << L._2) %~ (Vector.cons sources))
                  in
                    Vector.concat [ acc
                                  , result ]

                -- there is no default value, and no EnvVar entry, this
                -- means is possibly another source like OptParser
                ( Nothing
                  , configSources@(
                      Unresolved.ConfigSources Unresolved.Skip
                                               (Unresolved.Pending _)
                                               -- OptParser option should be
                                               -- there
                                               _)
                  ) ->
                  acc

                _ ->
                  acc

            Unresolved.SubConfig {} ->
              Vector.concat [ acc
                            , buildConfigResolver
                                   (index, filepath)
                                   (lensFn << _SubConfig << L.at key << _JustSubConfig)
                                   (Unresolved.Config unresolvedConfigVal)
                            ]
      in
        if Map.null configm then
          Vector.empty
        else
          Map.foldWithKey step Vector.empty configm

    Unresolved.Config _ ->
      -- error "malformed Unresolved.Config"
      Vector.empty

buildResolvers :: [Text] -> IO (Vector (Config -> IO Config))
buildResolvers files =
  let
    step input@(index, filepath) = do
      mUnresolvedConfig <- EDN.decode <$> B8.readFile (Text.unpack filepath)
      case mUnresolvedConfig of
        Nothing ->
          return Vector.empty

        Just unresolvedConfig -> do
          return <| buildConfigResolver input _Config unresolvedConfig
  in
    files
    |> zip [0..]
    |> mapM step
    |> (Vector.concat <$>)


buildConfiguration :: [Text] -> IO Config
buildConfiguration files =
  files
  |> buildResolvers
  >>= foldM (|>) (Config (SubConfig Map.empty))
