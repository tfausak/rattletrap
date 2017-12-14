{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.PropertyValue
  ( PropertyValue(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.List
import Rattletrap.Type.Dictionary
import Rattletrap.Type.Word8
import Rattletrap.Type.Text
import Rattletrap.Type.Float32
import Rattletrap.Type.Int32
import Rattletrap.Type.Word64

data PropertyValue a
  = ArrayProperty (List (Dictionary a))
  -- ^ Yes, a list of dictionaries. No, it doesn't make sense. These usually
  -- only have one element.
  | BoolProperty Word8
  | ByteProperty Text
                 (Maybe Text)
  -- ^ This is a strange name for essentially a key-value pair.
  | FloatProperty Float32
  | IntProperty Int32
  | NameProperty Text
  -- ^ It's unclear how exactly this is different than a 'StrProperty'.
  | QWordProperty Word64
  | StrProperty Text
  deriving (Eq, Generic, Ord, Show)

instance FromJSON a => FromJSON (PropertyValue a) where
  parseJSON = defaultParseJson "PropertyValue"

instance ToJSON a => ToJSON (PropertyValue a) where
  toEncoding = defaultToEncoding "PropertyValue"
  toJSON = defaultToJson "PropertyValue"
