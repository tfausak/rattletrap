{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute where

import Rattletrap.Type.AttributeValue
import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.Str

import qualified Data.Binary.Bits.Put as BinaryBits

data Attribute = Attribute
  { attributeId :: CompressedWord
  , attributeName :: Str
  -- ^ Read-only! Changing an attribute's name requires editing the class
  -- attribute map.
  , attributeValue :: AttributeValue
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''Attribute)

putAttributes :: [Attribute] -> BinaryBits.BitPut ()
putAttributes attributes = case attributes of
  [] -> BinaryBits.putBool False
  [attribute] -> do
    putAttribute attribute
    BinaryBits.putBool False
  first : rest -> do
    putAttribute first
    putAttributes rest

putAttribute :: Attribute -> BinaryBits.BitPut ()
putAttribute attribute = do
  BinaryBits.putBool True
  putCompressedWord (attributeId attribute)
  putAttributeValue (attributeValue attribute)
