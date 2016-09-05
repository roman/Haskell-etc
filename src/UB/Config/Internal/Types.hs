{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
module UB.Config.Internal.Types where

import Control.Lens hiding ((<|))
import Data.HashMap.Strict (HashMap)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set

import UB.Prelude

--------------------
-- Error Types

data ConfigurationError
  = InvalidConfiguration Text
  | InvalidConfigKeyPath [Text]
  deriving (Show)

instance Exception ConfigurationError

--------------------
-- Configuration Types

data ConfigSource
  = File     { configIndex :: Int
             , filepath    :: Text
             , value       :: JSON.Value }
  | EnvVar   { envVar :: Text
             , value :: JSON.Value }
  | OptParse { option :: Text
             , value :: JSON.Value }
  | Default  { value :: JSON.Value }
  deriving (Show, Eq)

instance Ord ConfigSource where
  compare a b =
    case (a, b) of
      (Default {}, _) ->
        LT

      (OptParse {}, _) ->
        GT

      (_, OptParse {}) ->
        LT

      (EnvVar {}, _) ->
        GT

      (_, EnvVar {}) ->
        LT

      (File {}, File {}) ->
        comparing configIndex a b

      (File {}, _) ->
        GT

data ConfigValue
  = ConfigValue { configSource :: Set ConfigSource }
  | SubConfig { subConfig :: HashMap Text ConfigValue }
  deriving (Show)

deepMerge :: ConfigValue -> ConfigValue -> ConfigValue
deepMerge left right =
  case (left, right) of
    (SubConfig leftm, SubConfig rightm) ->
      SubConfig <|
        HashMap.foldrWithKey
            (\key rightv result ->
                case HashMap.lookup key result of
                  Just leftv ->
                    HashMap.insert key (deepMerge leftv rightv) result
                  _ ->
                    HashMap.insert key rightv result)
            leftm
            rightm
    (ConfigValue leftSources, ConfigValue rightSources) ->
      ConfigValue <| Set.union leftSources rightSources
    _ ->
      right

instance Semigroup ConfigValue where
  (<>) = deepMerge

newtype Config
  = Config { fromConfig :: ConfigValue }
  deriving (Show, Semigroup)

$(makePrisms ''ConfigValue)
$(makePrisms ''Config)

-- Works like the _Just prism, but instead of not doing anything on Nothing, it
-- creates a ConfigValue record
_JustConfigValue
  :: Set ConfigSource
    -> Prism (Maybe ConfigValue) (Maybe ConfigValue) ConfigValue ConfigValue
_JustConfigValue source =
  prism Just <| maybe (Right <| ConfigValue source) Right


_JustSubConfig :: Prism (Maybe ConfigValue) (Maybe ConfigValue) ConfigValue ConfigValue
_JustSubConfig =
  prism Just <| maybe (Right <| SubConfig HashMap.empty) Right
