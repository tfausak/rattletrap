module Rattletrap.Type.IntAttribute
  ( IntAttribute(..)
  ) where

import Rattletrap.Type.Int32

newtype IntAttribute = IntAttribute
  { intAttributeValue :: Int32
  } deriving (Eq, Ord, Show)
