{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.Property
  ( Property(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Text
import Rattletrap.Type.Word64
import Rattletrap.Type.PropertyValue

data Property = Property
  { propertyKind :: Text
  , propertySize :: Word64
  -- ^ Not used.
  , propertyValue :: PropertyValue Property
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON Property where
  parseJSON = defaultParseJson "Property"

instance ToJSON Property where
  toEncoding = defaultToEncoding "Property"
  toJSON = defaultToJson "Property"
