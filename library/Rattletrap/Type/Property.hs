{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Property
  ( Property(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Text
import Rattletrap.Type.Word64le
import Rattletrap.Type.PropertyValue

data Property = Property
  { propertyKind :: Text
  , propertySize :: Word64le
  -- ^ Not used.
  , propertyValue :: PropertyValue Property
  } deriving (Eq, Ord, Show)

$(deriveJson ''Property)
