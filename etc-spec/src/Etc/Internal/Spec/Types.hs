{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveLift          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Etc.Internal.Spec.Types where

import           RIO
import qualified RIO.HashMap as HashMap
import qualified RIO.Map     as Map
import qualified RIO.Text    as Text

import qualified Data.Aeson.BetterErrors    as JSON
import           Language.Haskell.TH.Syntax (Lift (..))

import qualified Data.Aeson as JSON

newtype SpecError err = SpecError err

data SpecParserError
  = CannotInferTypeFromDefault !Text ![Text] !JSON.Value
  | InferredNestedArrayOnDefault !Text ![Text] !JSON.Value
  | UnknownConfigValueType !Text ![Text] !Text
  | DefaultValueTypeMismatchFound !Text ![Text] !ConfigValueType !JSON.Value
  | RedundantKeysOnValueSpec !Text ![Text] ![Text]
  | InvalidSpecEntries !Text !ConfigValue
  deriving (Show)

instance Exception SpecParserError

-- | Represents a type that can be used in your configuration values.
--
-- Once you define a CustomType, the library will check that configuration
-- files can be transformed to the specified custom type.
--
newtype CustomType
  = CustomType {customTypeParser :: JSON.Parse () () }

-- |
data SingleConfigValueType
  = CVTString
  | CVTNumber
  | CVTBool
  | CVTObject
  | CVTCustom Text
  deriving (Generic, Show, Read, Eq)

instance Lift SingleConfigValueType where
  lift cvType =
    case cvType of
      CVTString -> [| CVTString |]
      CVTNumber -> [| CVTNumber |]
      CVTBool ->   [| CVTBool   |]
      CVTObject -> [| CVTObject |]
      CVTCustom txt ->
        let
          str = Text.unpack txt
        in
          [| CVTCustom (Text.pack str) |]

-- |
data ConfigValueType
  = CVTSingle !SingleConfigValueType
  | CVTArray  !SingleConfigValueType
  deriving (Generic, Show, Read, Eq, Lift)



stripArrayWrapper :: ConfigValueType -> SingleConfigValueType
stripArrayWrapper (CVTArray inner)  = inner
stripArrayWrapper (CVTSingle inner) = inner

isCVTArray :: ConfigValueType -> Bool
isCVTArray (CVTArray {}) = True
isCVTArray _             = False

-- |
data ConfigValueData =
  ConfigValueData {
    configValueDefault   :: !(Maybe JSON.Value)
  , configValueType      :: !ConfigValueType
  , configValueSensitive :: !Bool
  , configValueJSON      :: !JSON.Value
  }
  deriving (Generic, Show, Eq, Lift)

-- |
data ConfigValue
  = ConfigValue !ConfigValueData
  | SubConfig   !(Map Text ConfigValue)
  deriving (Generic, Show, Eq)

instance Lift ConfigValue where
  lift val =
    case val of
      ConfigValue configVal -> [|ConfigValue configVal|]
      SubConfig subConfig ->
        let subConfigList = map (first Text.unpack) $ Map.toList subConfig
         in [|SubConfig $ Map.fromList $ map (first Text.pack) subConfigList|]

type SpecFilePath = Text
type SpecEntryPath = [Text]

-- |
data ConfigSpec
  = ConfigSpec { configSpecFilePath :: !SpecFilePath
               , configSpecJSON     :: !JSON.Object
               , configSpecEntries  :: !(Map Text ConfigValue)
               }
  deriving (Generic, Show, Eq)

instance Lift ConfigSpec where
  lift (ConfigSpec specSourceName specJson specEntries) =
    let sourceName = Text.unpack specSourceName
        specJsonList = map (first Text.unpack) $ HashMap.toList specJson
        entriesList = map (first Text.unpack) $ Map.toList specEntries
     in [|ConfigSpec
            { configSpecFilePath = Text.pack sourceName
            , configSpecJSON =
                HashMap.fromList $ map (first Text.pack) specJsonList
            , configSpecEntries =
                Map.fromList $ map (first Text.pack) entriesList
            }|]

getConfigSpecJSON :: ConfigSpec -> JSON.Object
getConfigSpecJSON = configSpecJSON

getConfigSpecEntries :: ConfigSpec -> ConfigValue
getConfigSpecEntries = SubConfig . configSpecEntries

blankConfigValueJSON :: ConfigSpec -> ConfigSpec
blankConfigValueJSON spec@ConfigSpec { configSpecEntries } = spec
  { configSpecEntries = Map.map blankValue configSpecEntries
  }
 where
  blankValue val = case val of
    ConfigValue valueData -> ConfigValue (valueData { configValueJSON = JSON.object [] })
    SubConfig   subConfig -> SubConfig $ Map.map blankValue subConfig
