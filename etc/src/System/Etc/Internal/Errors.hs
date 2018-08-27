{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
module System.Etc.Internal.Errors where

import RIO

import qualified Data.Aeson as JSON

import System.Etc.Internal.Spec.Types

-- | Thrown when calling the 'getConfig' or 'getConfigWith' functions on a key
-- that does not exist in the configuration spec
newtype InvalidConfigKeyPath = InvalidConfigKeyPath {
    inputKeys :: [Text] -- ^ Input Keys
  }
  deriving (Generic, Show, Read, Eq)

instance Exception InvalidConfigKeyPath

-- | Thrown when there is a type mismatch in a JSON parser given via
-- 'getConfigWith'
data ConfigValueParserFailed = ConfigValueParserFailed {
      inputKeys          :: ![Text] -- ^ Input Keys
    , parserErrorMessage :: !Text   -- ^ Parser Error Message
    }
  deriving (Generic, Show, Read, Eq)

instance Exception ConfigValueParserFailed

-- | Thrown when the 'resolveFile' function finds a key on a configuration
-- file that is not specified in the given configuration spec
data UnknownConfigKeyFound = UnknownConfigKeyFound {
      parentKeys  :: ![Text] -- ^ Parent Keys
    , keyName     :: !Text   -- ^ Key Name
    , siblingKeys :: ![Text] -- ^ Sibling Keys (other keys in the same level)
    }
  deriving (Generic, Show, Read, Eq)

instance Exception UnknownConfigKeyFound

-- | Thrown when there is a type mismatch on a configuration entry,
-- specifically, when there is a raw value instead of a sub-config in a
-- configuration file
data SubConfigEntryExpected =
  SubConfigEntryExpected {
      keyName     :: !Text -- ^ Key Name
    , configValue :: !JSON.Value -- ^ Config Value
    }
  deriving (Generic, Show, Read, Eq)

instance Exception SubConfigEntryExpected

-- | This error is thrown when a type mismatch is found in a raw value when
-- calling 'resolveFile'
data DefaultValueTypeMismatchFound = DefaultValueTypeMismatchFound {
      keyName              :: !Text -- ^ Key Name
    , configValueEntry     :: !JSON.Value -- ^ Config Value
    , configValueEntryType :: !ConfigValueType -- ^ Config Value Type
    }
  deriving (Generic, Show, Read, Eq)

instance Exception DefaultValueTypeMismatchFound

-- | Thrown when a specified configuration file is not found in the system
newtype ConfigurationFileNotFound =
  ConfigurationFileNotFound {
      configFilepath :: Text -- ^ Config FilePath
    }
  deriving (Generic, Show, Read, Eq)

instance Exception ConfigurationFileNotFound

-- | Thrown when an input configuration file contains an unsupported file
-- extension
newtype UnsupportedFileExtensionGiven =
  UnsupportedFileExtensionGiven {
      configFilepath :: Text -- ^ Config FilePath
  }
  deriving (Generic, Show, Read, Eq)

instance Exception UnsupportedFileExtensionGiven

-- | Thrown when an input configuration file contains invalid syntax
data ConfigInvalidSyntaxFound
  = ConfigInvalidSyntaxFound {
      configFilepath     :: !Text -- ^ Config FilePath
    , parserErrorMessage :: !Text -- ^ Parser Error Message
    }
  deriving (Generic, Show, Read, Eq)

instance Exception ConfigInvalidSyntaxFound

-- | Thrown when an configuration spec file contains invalid syntax
data SpecInvalidSyntaxFound = SpecInvalidSyntaxFound {
     specFilepath      :: !(Maybe Text) -- ^ Spec FilePath
   , parseErrorMessage :: !Text -- ^ Parser Error Message
   }
  deriving (Generic, Show, Read, Eq)

instance Exception SpecInvalidSyntaxFound
