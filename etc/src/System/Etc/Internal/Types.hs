{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RankNTypes                 #-}

module System.Etc.Internal.Types
  ( module System.Etc.Internal.Types
  , module System.Etc.Internal.Spec.Types
  ) where

import Protolude

import qualified Data.Aeson          as JSON
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Ord            (comparing)
import           Data.Set            (Set)
import qualified Data.Set            as Set

import System.Etc.Internal.Spec.Types (ConfigurationError (..))

--------------------
-- Configuration Types

data ConfigSource
  = File {
      configIndex :: Int
    , filepath    :: Text
    , value       :: JSON.Value
    }
  | Env {
      envVar :: Text
    , value  :: JSON.Value
    }
  | Cli {
      value :: JSON.Value
    }
  | Default {
      value :: JSON.Value
    }
  | None
  deriving (Show, Eq)

instance Ord ConfigSource where
  compare a b =
    if a == b then
      EQ
    else
      case (a, b) of
        (None, _) ->
          LT

        (_, None) ->
          GT

        (Default {}, _) ->
          LT

        (Cli {}, _) ->
          GT

        (_, Cli {}) ->
          LT

        (Env {}, _) ->
          GT

        (_, Env {}) ->
          LT

        (File {}, File {}) ->
          comparing configIndex a b

        (File {}, _) ->
          GT

data ConfigValue
  = ConfigValue {
      configSource :: Set ConfigSource
    }
  | SubConfig {
      configMap :: HashMap Text ConfigValue
    }
  deriving (Eq, Show)

deepMerge :: ConfigValue -> ConfigValue -> ConfigValue
deepMerge left right =
  case (left, right) of
    (SubConfig leftm, SubConfig rightm) ->
      SubConfig $
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
      ConfigValue $ Set.union leftSources rightSources
    _ ->
      right

instance Semigroup ConfigValue where
  (<>) = deepMerge

newtype Config
  = Config { fromConfig :: ConfigValue }
  deriving (Eq, Show, Semigroup)

instance Monoid Config where
  mempty = Config $ SubConfig HashMap.empty
  mappend (Config a) (Config b) = Config (a <> b)
