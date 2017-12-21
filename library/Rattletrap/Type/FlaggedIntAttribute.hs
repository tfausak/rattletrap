{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.FlaggedIntAttribute
  ( FlaggedIntAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Int32le

data FlaggedIntAttribute = FlaggedIntAttribute
  { flaggedIntAttributeFlag :: Bool
  , flaggedIntAttributeInt :: Int32le
  } deriving (Eq, Ord, Show)

$(deriveJson ''FlaggedIntAttribute)
