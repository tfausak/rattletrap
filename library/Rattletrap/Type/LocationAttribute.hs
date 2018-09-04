{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.LocationAttribute
  ( LocationAttribute(..)
  )
where

import Rattletrap.Type.Common
import Rattletrap.Type.Vector

newtype LocationAttribute = LocationAttribute
  { locationAttributeValue :: Vector
  } deriving (Eq, Ord, Show)

$(deriveJson ''LocationAttribute)
