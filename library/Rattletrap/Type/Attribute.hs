{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.Attribute
  ( Attribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.AttributeValue
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.Text

data Attribute = Attribute
  { attributeId :: CompressedWord
  , attributeName :: Text
  -- ^ Read-only! Changing an attribute's name requires editing the class
  -- attribute map.
  , attributeValue :: AttributeValue
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON Attribute where
  parseJSON = defaultParseJson "Attribute"

instance ToJSON Attribute where
  toEncoding = defaultToEncoding "Attribute"
  toJSON = defaultToJson "Attribute"
