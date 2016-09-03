{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module UB.Lens.EDN where

import UB.Prelude
import Control.Lens
import qualified Data.EDN as EDN

$(makePrisms ''EDN.Value)
