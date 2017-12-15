{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.FlaggedIntAttribute
  ( FlaggedIntAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Int32

data FlaggedIntAttribute = FlaggedIntAttribute
  { flaggedIntAttributeFlag :: Bool
  , flaggedIntAttributeInt :: Int32
  } deriving (Eq, Ord, Show)

$(deriveJson ''FlaggedIntAttribute)
