{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module System.Etc.Internal.Resolver.OptParse where

import Control.Lens hiding ((<|), (|>))
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Options.Applicative as Opt

import UB.Prelude hiding ((&))
import System.Etc.Internal.Types
import qualified System.Etc.Internal.Spec as Spec

optParseSpecToOptSwitchFieldMod optParseOptionSpec =
  maybe Opt.idm (Text.unpack >> Opt.long) (Spec.optParseLong optParseOptionSpec)
  `mappend` maybe Opt.idm (Text.head >> Opt.short) (Spec.optParseShort optParseOptionSpec)
  `mappend` maybe Opt.idm (Text.unpack >> Opt.help) (Spec.optParseHelp optParseOptionSpec)

optParseSpecToOptVarFieldMod optParseOptionSpec =
  optParseSpecToOptSwitchFieldMod optParseOptionSpec
  `mappend` maybe Opt.idm (Text.unpack >> Opt.metavar) (Spec.optParseMetavar optParseOptionSpec)

optParseSpecToJSONParser
  :: Spec.OptParseOptionSpec
  -> Opt.Parser (Maybe JSON.Value)
optParseSpecToJSONParser optParseOptionSpec =
  let
    optParseField =
      case Spec.optParseType optParseOptionSpec of
        Spec.OptParseString ->
          (Text.pack >> JSON.String)
          <$> Opt.strOption (optParseSpecToOptVarFieldMod optParseOptionSpec)

        Spec.OptParseNumber ->
          (fromInteger >> JSON.Number)
          <$> Opt.option Opt.auto (optParseSpecToOptVarFieldMod optParseOptionSpec)

        Spec.OptParseSwitch ->
          JSON.Bool
          <$> Opt.switch (optParseSpecToOptSwitchFieldMod optParseOptionSpec)
  in
    if Spec.optParseRequired optParseOptionSpec then
      Just <$> optParseField
    else
      Opt.optional optParseField

configSpecToConfigValueOptParser
  :: Text
  -> Spec.ConfigValue
  -> Opt.Parser ConfigValue
  -> Opt.Parser ConfigValue
configSpecToConfigValueOptParser key specConfigValue parentConfigValueParser =
  case specConfigValue of
    Spec.ConfigValue mDefaultValue sources ->
      case Spec.optParse sources of
        Nothing ->
          parentConfigValueParser

        Just (Spec.OptParse optParseOptionSpec) ->
          let
            jsonToConfigValue mValue =
              ConfigValue
              <| Set.singleton
              <| case (mValue, mDefaultValue) of
                   (Just val, _) ->
                     OptParse val

                   (_, Just defValue) ->
                     Default defValue

                   (Nothing, Nothing) ->
                     error <| "invalid spec creation" <> show sources

            configValueOptParser =
              fmap jsonToConfigValue
                   (optParseSpecToJSONParser optParseOptionSpec)
          in
            (\configValue parentConfigValue ->
               parentConfigValue
                 &  (_SubConfig << at key << _JustConfigValue Set.empty)
                 .~ configValue)
              <$> configValueOptParser
              <*> parentConfigValueParser

        Just _ ->
          error <| "invalid spec creation" <> show sources

    Spec.SubConfig subConfigSpec ->
      let
        subConfigParser =
          HashMap.foldrWithKey
            configSpecToConfigValueOptParser
            (pure <| SubConfig HashMap.empty)
            subConfigSpec
      in
        (\subConfig' parentConfigValue ->
           parentConfigValue
           & (_SubConfig << at key << _JustSubConfig)
           .~ subConfig')
         <$> subConfigParser
         <*> parentConfigValueParser


configSpecToOptParser :: Spec.ConfigSpec -> Opt.Parser Config
configSpecToOptParser (Spec.ConfigSpec _ _ specConfigValue) =
  case specConfigValue of
    Spec.ConfigValue {} ->
      error "invalid spec creation"

    Spec.SubConfig configSpec ->
      Config
      <$> HashMap.foldrWithKey
            configSpecToConfigValueOptParser
            (pure <| SubConfig HashMap.empty)
            configSpec

resolveOptParser :: Spec.ConfigSpec -> IO Config
resolveOptParser configSpec =
  let
    configParser =
      configSpecToOptParser configSpec

    programModFlags =
      case Spec.optParseProgramSpec configSpec of
           Just programSpec ->
             Opt.fullDesc
              `mappend` (programSpec
                        |> Spec.optParseProgramDesc
                        |> Text.unpack
                        |> Opt.progDesc)
              `mappend` (programSpec
                        |> Spec.optParseProgramHeader
                        |> Text.unpack
                        |> Opt.header)
           Nothing ->
             mempty

    programParser =
      Opt.info (Opt.helper <*> configParser)
               programModFlags
  in
    Opt.execParser programParser
