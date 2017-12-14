module Rattletrap.Type.FloatAttribute
  ( FloatAttribute(..)
  ) where

import Rattletrap.Type.Float32

newtype FloatAttribute = FloatAttribute
  { floatAttributeValue :: Float32
  } deriving (Eq, Ord, Show)
