{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
module Etc.Config where

import           RIO
import qualified RIO.HashMap as HashMap
import qualified RIO.Map  as Map
import qualified RIO.Set  as Set
import qualified RIO.Text as Text

import qualified Data.Semigroup as Semigroup

import Data.Typeable (cast, typeOf)

import Control.Exception (throw)

import Data.Text.Prettyprint.Doc (Doc)

import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Internal as JSON (IResult (..), formatError, iparse)
import qualified Data.Aeson.Types    as JSON (Parser)

--------------------------------------------------------------------------------

-- | Thrown when calling the 'getConfig' or 'getConfigWith' functions on a key
-- that does not exist in the configuration spec
newtype InvalidConfigKeyPath = InvalidConfigKeyPath [Text]
  deriving (Generic, Show, Read, Eq)

instance Exception InvalidConfigKeyPath

-- | Thrown when there is a type mismatch in a JSON parser given via
-- 'getConfigWith'
data ConfigValueParserFailed = ConfigValueParserFailed ![Text] !Text
  deriving (Generic, Show, Read, Eq)

instance Exception ConfigValueParserFailed

data InvalidConfigSourceComparison
  = InvalidConfigSourceComparison !SomeConfigSource !SomeConfigSource
  deriving (Show)

instance Exception InvalidConfigSourceComparison

data Value a
  = Plain { fromValue :: !a }
  | Sensitive { fromValue :: !a }
  deriving (Generic, Eq, Ord)

instance NFData a => NFData (Value a)

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

class (Show source, Typeable source) =>
      IConfigSource source
  where
  sourceValue :: source -> Value JSON.Value
  sourcePrettyDoc :: source -> Doc ann
  compareSources :: source -> source -> Ordering
  compareSources _ _ = EQ

data SomeConfigSource =
  forall source. (IConfigSource source) =>
                 SomeConfigSource !Int
                                  !source

instance Show SomeConfigSource where
  show (SomeConfigSource i a) = "SomeConfigSource " <> show i <> " (" <> show a <> ")"

instance IConfigSource SomeConfigSource where
  sourcePrettyDoc (SomeConfigSource _ inner) = sourcePrettyDoc inner
  sourceValue (SomeConfigSource _ inner) = sourceValue inner
  compareSources x@(SomeConfigSource ia a) y@(SomeConfigSource ib b)
    | ia == ib =
      if fromValue (sourceValue a) == JSON.Null && fromValue (sourceValue b) == JSON.Null then
        EQ
      else if typeOf a == typeOf b then
        let b' = fromMaybe (throw (InvalidConfigSourceComparison x y)) (cast b)
        in compareSources a b'
      else
        throw (InvalidConfigSourceComparison x y)
    | fromValue (sourceValue a) == JSON.Null = LT
    | fromValue (sourceValue b) == JSON.Null = GT
    | otherwise =
      compare ia ib

instance Eq SomeConfigSource where
  (==) a b = compareSources a b == EQ

instance Ord SomeConfigSource where
  compare = compareSources

data ConfigValue
  = ConfigValue !(Set SomeConfigSource)
  | SubConfig   !(Map Text ConfigValue)

  deriving (Eq, Show)

deepMerge :: ConfigValue -> ConfigValue -> ConfigValue
deepMerge left right = case (left, right) of
  (SubConfig leftm, SubConfig rightm) -> SubConfig $ Map.foldrWithKey
    (\key rightv result -> case Map.lookup key result of
      Just leftv -> Map.insert key (deepMerge leftv rightv) result
      _          -> Map.insert key rightv result
    )
    leftm
    rightm
  (ConfigValue leftSources, ConfigValue rightSources) ->
    ConfigValue $ Set.union leftSources rightSources
  _ -> right

instance Semigroup ConfigValue where
  (<>) = deepMerge

instance Monoid ConfigValue where
  mempty  = emptySubConfig
  mappend = (Semigroup.<>)

isEmptySubConfig :: ConfigValue -> Bool
isEmptySubConfig val = case val of
  SubConfig hsh -> Map.null hsh
  ConfigValue{} -> False

emptySubConfig :: ConfigValue
emptySubConfig = SubConfig Map.empty

writeInSubConfig :: Text -> ConfigValue -> ConfigValue -> ConfigValue
writeInSubConfig key val subConfig = case subConfig of
  SubConfig hsh -> SubConfig $ Map.insert key val hsh
  _             -> subConfig

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe pfn mvalue = case mvalue of
  Just a | pfn a     -> Nothing
         | otherwise -> mvalue
  Nothing -> Nothing

newtype Config
  = Config { fromConfig :: ConfigValue }
  deriving (Eq, Show, Semigroup, Monoid)

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
    -> m (Set SomeConfigSource)

  getSelectedConfigSource
    :: (MonadThrow m, IConfigSource source)
    => [Text]
    -> config
    -> m source

configValueToJsonObject :: ConfigValue -> JSON.Value
configValueToJsonObject configValue = case configValue of
  ConfigValue sources -> case Set.maxView sources of
    Nothing          -> JSON.Null

    Just (source, _) -> fromValue $ sourceValue source

  SubConfig configm ->
    configm
      & Map.foldrWithKey
          (\key innerConfigValue acc ->
            HashMap.insert key (configValueToJsonObject innerConfigValue) acc
          )
          HashMap.empty
      & JSON.Object

_getConfigValueWith
  :: (MonadThrow m) => (JSON.Value -> JSON.Parser result) -> [Text] -> Config -> m result
_getConfigValueWith parser keys0 (Config configValue0) =
  let
    loop keys configValue = case (keys, configValue) of
      ([], ConfigValue sources) -> case Set.maxView sources of
        Nothing          -> throwM $ InvalidConfigKeyPath keys0

        Just (source, _) -> case JSON.iparse parser (fromValue $ sourceValue source) of

          JSON.IError path err ->
            JSON.formatError path err & Text.pack & ConfigValueParserFailed keys0 & throwM

          JSON.ISuccess result -> return result

      ([], innerConfigValue) ->
        case JSON.iparse parser (configValueToJsonObject innerConfigValue) of
          JSON.IError path err ->
            JSON.formatError path err & Text.pack & ConfigValueParserFailed keys0 & throwM

          JSON.ISuccess result -> return result

      (k : keys1, SubConfig configm) -> case Map.lookup k configm of
        Nothing           -> throwM $ InvalidConfigKeyPath keys0
        Just configValue1 -> loop keys1 configValue1

      _ -> throwM $ InvalidConfigKeyPath keys0
  in  loop keys0 configValue0

_getSelectedConfigSource
  :: (MonadThrow m, IConfigSource result) => [Text] -> Config -> m result
_getSelectedConfigSource keys0 (Config configValue0) =
  let loop keys configValue = case (keys, configValue) of
        ([], ConfigValue sources) -> case Set.maxView sources of
          Nothing -> throwM $ InvalidConfigKeyPath keys0

          Just (SomeConfigSource _ source, _) ->
            -- TODO: Change exception from InvalidConfigKeyPath
            maybe (throwM $ InvalidConfigKeyPath keys0) return (cast source)

        (k : keys1, SubConfig configm) -> case Map.lookup k configm of
          Nothing           -> throwM $ InvalidConfigKeyPath keys0
          Just configValue1 -> loop keys1 configValue1

        _ -> throwM $ InvalidConfigKeyPath keys0
  in  loop keys0 configValue0


_getAllConfigSources :: (MonadThrow m) => [Text] -> Config -> m (Set SomeConfigSource)
_getAllConfigSources keys0 (Config configValue0) =
  let loop keys configValue = case (keys, configValue) of
        ([]       , ConfigValue sources) -> return sources

        (k : keys1, SubConfig configm  ) -> case Map.lookup k configm of
          Nothing           -> throwM $ InvalidConfigKeyPath keys0
          Just configValue1 -> loop keys1 configValue1

        _ -> throwM $ InvalidConfigKeyPath keys0
  in  loop keys0 configValue0

_getConfigValue :: (MonadThrow m, JSON.FromJSON result) => [Text] -> Config -> m result
_getConfigValue = _getConfigValueWith JSON.parseJSON

instance IConfig Config where
  getConfigValue = _getConfigValue
  getConfigValueWith = _getConfigValueWith
  getAllConfigSources = _getAllConfigSources
  getSelectedConfigSource = _getSelectedConfigSource
