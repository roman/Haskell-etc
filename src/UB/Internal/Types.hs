{-# LANGUAGE NoImplicitPrelude #-}
module UB.Internal.Types where

import UB.Prelude

data ConfigurationError
  = InvalidConfiguration Text
  | InvalidConfigKeyPath [Text]
  deriving (Show)

instance Exception ConfigurationError
