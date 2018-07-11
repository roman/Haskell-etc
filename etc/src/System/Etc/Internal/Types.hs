{-# LANGUAGE DeriveGeneric              #-}
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

import           Data.Bool      (bool)
import qualified Data.Semigroup as Semigroup

import qualified Data.Aeson       as JSON
import qualified Data.Aeson.Types as JSON (Parser)

import System.Etc.Internal.Spec.Types (ConfigValueType, ConfigurationError (..))

--------------------
-- Configuration Types

data Value a
  = Plain { fromValue :: !a }
  | Sensitive { fromValue :: !a }
  deriving (Generic, Eq, Ord)

instance Show a => Show (Value a) where
  show (Plain a)     = show a
  show (Sensitive _) = "<<sensitive>>"

instance Functor Value where
  fmap f val =
    case val of
      Plain a     -> Plain (f a)
      Sensitive a -> Sensitive (f a)

instance Applicative Value where
  pure = Plain
  (<*>) vf va =
    case (vf, va) of
      (Plain f, Plain a)         -> Plain (f a)
      (Sensitive f, Sensitive a) -> Sensitive (f a)
      (Sensitive f, Plain a)     -> Sensitive (f a)
      (Plain f, Sensitive a)     -> Sensitive (f a)

instance IsString a => IsString (Value a) where
  fromString = Plain . fromString

markAsSensitive :: Bool -> (a -> Value a)
markAsSensitive = bool Plain Sensitive

data FileSource
  = FilePathSource { fileSourcePath :: !Text }
  | EnvVarFileSource { fileSourceEnvVar :: !Text,  fileSourcePath :: !Text }
  deriving (Show, Eq)

data ConfigSource
  = File {
      configIndex :: !Int
    , filepath    :: !FileSource
    , value       :: !(Value JSON.Value)
    }
  | Env {
      envVar :: !Text
    , value  :: !(Value JSON.Value)
    }
  | Cli {
      value :: !(Value JSON.Value)
    }
  | Default {
      value :: !(Value JSON.Value)
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

        (_, _)
          | fromValue (value a) == JSON.Null -> LT
          | fromValue (value b) == JSON.Null -> GT

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
  deriving (Eq, Show)

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
  deriving (Eq, Show, Semigroup, Monoid)

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
