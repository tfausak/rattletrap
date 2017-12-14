{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.LocationAttribute
  ( LocationAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Vector

newtype LocationAttribute = LocationAttribute
  { locationAttributeValue :: Vector
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON LocationAttribute where
  parseJSON = defaultParseJson "LocationAttribute"

instance ToJSON LocationAttribute where
  toEncoding = defaultToEncoding "LocationAttribute"
  toJSON = defaultToJson "LocationAttribute"
