{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RankNTypes                 #-}

module System.Etc.Internal.Types
  ( module System.Etc.Internal.Types
  , module System.Etc.Internal.Spec.Types
  ) where

import           RIO
import qualified RIO.HashMap as HashMap
import qualified RIO.Set     as Set

import qualified Data.Semigroup as Semigroup

import qualified Data.Aeson       as JSON
import qualified Data.Aeson.Types as JSON (Parser)

import qualified System.Etc.Internal.Spec.Types as Spec
import System.Etc.Internal.Spec.Types (ConfigurationError (..))

--------------------
-- Configuration Types

data JsonValue
  = Plain !JSON.Value
  | Sensitive !JSON.Value
  deriving (Generic, Eq)

instance Show JsonValue where
  show (Plain jsonVal) = show jsonVal
  show (Sensitive _) = "<<redacted>>"

data ConfigSource
  = File {
      configIndex :: !Int
    , filepath    :: !Text
    , value       :: !JsonValue
    }
  | Env {
      envVar :: !Text
    , value  :: !JsonValue
    }
  | Cli {
      value :: !JsonValue
    }
  | Default {
      value :: !JsonValue
    }
  | None
  deriving (Generic, Show, Eq)

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
      configSource :: !(Set ConfigSource)
    }
  | SubConfig {
      configMap :: !(HashMap Text ConfigValue)
    }
  deriving (Generic, Eq, Show)

deepMerge :: ConfigValue -> ConfigValue -> ConfigValue
deepMerge left right = case (left, right) of
  (SubConfig leftm, SubConfig rightm) -> SubConfig $ HashMap.foldrWithKey
    (\key rightv result -> case HashMap.lookup key result of
      Just leftv -> HashMap.insert key (deepMerge leftv rightv) result
      _          -> HashMap.insert key rightv result
    )
    leftm
    rightm
  (ConfigValue leftSources, ConfigValue rightSources) ->
    ConfigValue $ Set.union leftSources rightSources
  _ -> right

instance Semigroup.Semigroup ConfigValue where
  (<>) = deepMerge

instance Monoid ConfigValue where
  mempty  = emptySubConfig
  mappend = (Semigroup.<>)

newtype Config
  = Config { fromConfig :: ConfigValue }
  deriving (Generic, Eq, Show, Semigroup, Monoid)

isEmptySubConfig :: ConfigValue -> Bool
isEmptySubConfig val = case val of
  SubConfig hsh -> HashMap.null hsh
  ConfigValue{} -> False

emptySubConfig :: ConfigValue
emptySubConfig = SubConfig HashMap.empty

writeInSubConfig :: Text -> ConfigValue -> ConfigValue -> ConfigValue
writeInSubConfig key val subConfig = case subConfig of
  SubConfig hsh -> SubConfig $ HashMap.insert key val hsh
  _             -> subConfig

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe pfn mvalue = case mvalue of
  Just a | pfn a     -> Nothing
         | otherwise -> mvalue
  Nothing -> Nothing


class IConfig config where
  -- | Fetches a configuration value from a given key, if key
  -- is not found, you may pick the failure mode via the 'MonadThrow'
  -- interface.
  --
  -- example:
  --
  -- >>> getConfigValue ["db", "user"] config :: Maybe Text
  -- Just "root"
  -- >>> getConfigValue ["db", "password"] config :: Maybe Text
  -- Nothing
  getConfigValue
    :: (MonadThrow m, JSON.FromJSON result)
    => [Text]   -- ^ Key to fetch from config map
    -> config   -- ^ Config record
    -> m result
  -- | Fetches a configuration value from a given key, normally this key will
  -- point to a sub-config JSON object, which is then passed to the given JSON
  -- parser function. If key is not found, you may pick the failure mode via the
  -- 'MonadThrow' interface.
  --
  -- example:
  --
  -- >>> import qualified Data.Aeson as JSON
  -- >>> import qualified Data.Aeson.Types as JSON (Parser)
  --
  -- >>> connectInfoParser :: JSON.Value -> JSON.Parser DbConnectInfo
  --
  -- >>> getConfigValueWith connectInfoParser ["db"] config
  -- Just (DbConnectInfo {...})
  --
  getConfigValueWith
    :: (MonadThrow m)
    => (JSON.Value -> JSON.Parser result) -- ^ JSON Parser function
    -> [Text]                             -- ^ Key to fetch from config map
    -> config                             -- ^ Config record
    -> m result
  getAllConfigSources
    :: (MonadThrow m)
    => [Text]
    -> config
    -> m (Set ConfigSource)
  getSelectedConfigSource
    :: (MonadThrow m)
    => [Text]
    -> config
    -> m ConfigSource

toJsonValue :: Maybe (Spec.ConfigValue cmd) -> JSON.Value -> JsonValue
toJsonValue mspec jsonVal =
  case mspec of
    Nothing ->
      Plain jsonVal
    Just (Spec.SubConfig {}) ->
      Plain jsonVal
    Just spec@(Spec.ConfigValue {})
      | Spec.isSensitiveValue spec -> Sensitive jsonVal
      | otherwise -> Plain jsonVal
