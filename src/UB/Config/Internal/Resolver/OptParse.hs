{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module UB.Config.Internal.Resolver.OptParse where

import Control.Lens hiding ((<|))
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Options.Applicative as Opt

import UB.Prelude hiding ((&))
import UB.Config.Internal.Types
import qualified UB.Config.Internal.Spec as Spec

optParseSpecToOptSwitchFieldMod optParseSpec =
  maybe Opt.idm (Text.unpack >> Opt.long) (Spec.optParseLong optParseSpec)
  `mappend` maybe Opt.idm (Text.head >> Opt.short) (Spec.optParseShort optParseSpec)
  `mappend` maybe Opt.idm (Text.unpack >> Opt.help) (Spec.optParseHelp optParseSpec)

optParseSpecToOptVarFieldMod optParseSpec =
  optParseSpecToOptSwitchFieldMod optParseSpec
  `mappend` maybe Opt.idm (Text.unpack >> Opt.metavar) (Spec.optParseMetavar optParseSpec)

optParseSpecToConfigValueParser
  :: Spec.OptParseSpec
  -> Opt.Parser (Maybe JSON.Value)
optParseSpecToConfigValueParser optParseSpec =
  let
    optParseField =
      case Spec.optParseType optParseSpec of
        Spec.OptParseString ->
          (Text.pack >> JSON.String)
          <$> Opt.strOption (optParseSpecToOptVarFieldMod optParseSpec)

        Spec.OptParseNumber ->
          (fromInteger >> JSON.Number)
          <$> Opt.option Opt.auto (optParseSpecToOptVarFieldMod optParseSpec)

        Spec.OptParseSwitch ->
          JSON.Bool
          <$> Opt.switch (optParseSpecToOptSwitchFieldMod optParseSpec)
  in
    if Spec.optParseRequired optParseSpec then
      Just <$> optParseField
    else
      Opt.optional optParseField

configSpecToConfigValueOptParser_
  :: Text
  -> Spec.ConfigValue
  -> Opt.Parser ConfigValue
  -> Opt.Parser ConfigValue
configSpecToConfigValueOptParser_ key specConfigValue parentConfigValueParser =
  case specConfigValue of
    Spec.ConfigValue mDefaultValue sources ->
      case Spec.optParse sources of
        Nothing ->
          parentConfigValueParser

        Just (Spec.OptParse optParseSpec) ->
          let
            sourceOptParser =
              fmap
                (\mValue ->
                    ConfigValue
                    <| Set.singleton
                    <| case (mValue, mDefaultValue) of
                         (Just val, _) ->
                           OptParse val

                         (_, Just defValue) ->
                           Default defValue

                         (Nothing, Nothing) ->
                           error <| "invalid spec creation" <> show sources)
                (optParseSpecToConfigValueParser optParseSpec)
          in
            (\configValue parentConfigValue ->
               parentConfigValue
                 &  (_SubConfig << at key << _JustConfigValue Set.empty)
                 .~ configValue)
              <$> sourceOptParser
              <*> parentConfigValueParser

        Just _ ->
          error <| "invalid spec creation" <> show sources

    Spec.SubConfig subConfigSpec ->
      let
        subConfigParser =
          HashMap.foldrWithKey
            configSpecToConfigValueOptParser_
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
configSpecToOptParser (Spec.ConfigSpec specConfigValue) =
  case specConfigValue of
    Spec.ConfigValue {} ->
      error "invalid spec creation"

    Spec.SubConfig configSpec ->
      Config
      <$> HashMap.foldrWithKey
            configSpecToConfigValueOptParser_
            (pure <| SubConfig HashMap.empty)
            configSpec
