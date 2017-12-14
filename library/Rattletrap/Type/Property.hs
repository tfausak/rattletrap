module Rattletrap.Type.Property
  ( Property(..)
  ) where

import Rattletrap.Type.Text
import Rattletrap.Type.Word64
import Rattletrap.Type.PropertyValue

data Property = Property
  { propertyKind :: Text
  , propertySize :: Word64
  -- ^ Not used.
  , propertyValue :: PropertyValue Property
  } deriving (Eq, Ord, Show)
