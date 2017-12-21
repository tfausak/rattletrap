{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute
  ( Attribute(..)
  ) where

import Rattletrap.Type.AttributeValue
import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.Str

data Attribute = Attribute
  { attributeId :: CompressedWord
  , attributeName :: Str
  -- ^ Read-only! Changing an attribute's name requires editing the class
  -- attribute map.
  , attributeValue :: AttributeValue
  } deriving (Eq, Ord, Show)

$(deriveJson ''Attribute)
