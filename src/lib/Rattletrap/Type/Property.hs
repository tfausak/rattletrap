{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Property
  ( Property(..)
  )
where

import Rattletrap.Type.Common
import Rattletrap.Type.PropertyValue
import Rattletrap.Type.Str
import Rattletrap.Type.Word64le

data Property = Property
  { propertyKind :: Str
  , propertySize :: Word64le
  -- ^ Not used.
  , propertyValue :: PropertyValue Property
  } deriving (Eq, Ord, Show)

$(deriveJson ''Property)
