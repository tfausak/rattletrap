module Rattletrap.Encode.Attribute
  ( putAttributes
  ) where

import Rattletrap.Encode.AttributeValue
import Rattletrap.Encode.CompressedWord
import Rattletrap.Type.Attribute

import qualified Data.Binary.Bits.Put as BinaryBits

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
