{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}

module System.Etc.Internal.Types
  ( module System.Etc.Internal.Types
  , module System.Etc.Internal.Spec.Types
  ) where

import           RIO         hiding ((<>))
import qualified RIO.HashMap as HashMap
import qualified RIO.Set     as Set
import qualified RIO.Text    as Text

import           Text.PrettyPrint.ANSI.Leijen (Doc, (<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as Doc



import Control.Exception (throw)

import           Data.Bool      (bool)
import           Data.Monoid    ((<>))
import qualified Data.Semigroup as Semigroup
import           Data.Typeable  (cast, typeOf)

import qualified Data.Aeson       as JSON
import qualified Data.Aeson.Types as JSON (Parser)

import System.Etc.Internal.Spec.Types (ConfigValueType)

--------------------
-- Configuration Types

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

data FileValueOrigin
  = ConfigFileOrigin { fileSourcePath :: !Text }
  | EnvFileOrigin    { fileSourceEnvVar :: !Text,  fileSourcePath :: !Text }
  deriving (Generic, Show, Eq)

instance NFData FileValueOrigin

class IConfigSource source where
  sourceValue     :: source -> Value JSON.Value
  sourcePrettyDoc :: source -> Doc
  compareSources  :: source -> source -> Ordering
  compareSources _ _ = EQ

data SomeConfigSource =
  forall source. ( Show source
                 , NFData source
                 , Typeable source
                 , IConfigSource source
                 ) =>
                 SomeConfigSource !Int
                                  !source

instance Show SomeConfigSource where
  show (SomeConfigSource i a) = "SomeConfigSource " <> show i <> " (" <> show a <> ")"

-- | Thrown when comparing config sources of different types on a same
-- precedence level, this should never happen because config source values of
-- the same type are created and compared on a single execution; if this does
-- happen, it maybe either be an urgent bug or you used the private API
-- incorrectly.
data InvalidConfigSourceComparison
  = InvalidConfigSourceComparison !SomeConfigSource !SomeConfigSource
  deriving (Show)

instance Exception InvalidConfigSourceComparison

instance IConfigSource SomeConfigSource where
  sourcePrettyDoc (SomeConfigSource _ inner) = sourcePrettyDoc inner
  sourceValue (SomeConfigSource _ inner) = sourceValue inner
  compareSources x@(SomeConfigSource ia a) y@(SomeConfigSource ib b)
    | ia == ib =
      if fromValue (sourceValue a) == JSON.Null && fromValue (sourceValue b) == JSON.Null then
        EQ
      else if typeOf a == typeOf b then
        let b' = fromMaybe (throw (InvalidConfigSourceComparison x y)) (cast a)
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

data FileSource = FileSource
  { fsConfigIndex :: !Int
  , fsValueOrigin :: !FileValueOrigin
  , fsValue       :: !(Value JSON.Value) }
  deriving (Generic, Typeable, Show, Eq)

instance NFData FileSource
instance IConfigSource FileSource where
  compareSources = comparing fsConfigIndex
  sourceValue = fsValue
  sourcePrettyDoc (FileSource _index origin _value) =
    case origin of
      ConfigFileOrigin filepath -> Doc.text "File:" <+> Doc.text (Text.unpack filepath)
      EnvFileOrigin envVar filepath ->
        Doc.text "File:" <+> Doc.text (Text.unpack envVar) <> "=" <> Doc.text (Text.unpack filepath)

fileSource :: Int -> Int -> FileValueOrigin -> Value JSON.Value -> SomeConfigSource
fileSource precedenceOrder index origin val =
  SomeConfigSource precedenceOrder $ FileSource index origin val

data EnvSource = EnvSource
  {
    esVarName :: !Text
  , esValue   :: !(Value JSON.Value)
  }
  deriving (Generic, Typeable, Show, Eq)

instance NFData EnvSource
instance IConfigSource EnvSource where
  sourceValue = esValue
  sourcePrettyDoc (EnvSource varname _value) =
    Doc.text "Env:" <+> Doc.text (Text.unpack varname)

envSource :: Int -> Text -> Value JSON.Value -> SomeConfigSource
envSource precedenceOrder varName val =
  SomeConfigSource precedenceOrder $ EnvSource varName val

newtype DefaultSource =
  DefaultSource (Value JSON.Value)
  deriving (Generic, Typeable, Show, Eq, NFData)

instance IConfigSource DefaultSource where
  sourceValue (DefaultSource value) = value
  sourcePrettyDoc (DefaultSource _value) = Doc.text "Default"

defaultSource :: Value JSON.Value -> SomeConfigSource
defaultSource = SomeConfigSource 0 . DefaultSource

--------------------------------------------------------------------------------
-- TODO: Split out

newtype CliSource
  = CliSource (Value JSON.Value)
  deriving (Generic, Typeable, Show, Eq, NFData)

instance IConfigSource CliSource where
  sourceValue (CliSource value) = value
  sourcePrettyDoc (CliSource _value) = Doc.text "Cli"

cliSource :: Int -> Value JSON.Value -> SomeConfigSource
cliSource precedenceOrder val = SomeConfigSource precedenceOrder $ CliSource val

--------------------------------------------------------------------------------

data ConfigValue
  = ConfigValue {
      configSource :: !(Set SomeConfigSource)
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
    -> m (Set SomeConfigSource)
  getSelectedConfigSource
    :: (MonadThrow m, Typeable source, IConfigSource source)
    => [Text]
    -> config
    -> m source
