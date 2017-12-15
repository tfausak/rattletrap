module Rattletrap.Encode.Attribute
  ( putAttributes
  , putAttribute
  ) where

import Rattletrap.Encode.AttributeValue
import Rattletrap.Encode.CompressedWord
import Rattletrap.Type.Attribute

import qualified Data.Binary.Bits.Put as BinaryBit

putAttributes :: [Attribute] -> BinaryBit.BitPut ()
putAttributes attributes = do
  mapM_ putAttribute attributes
  BinaryBit.putBool False

putAttribute :: Attribute -> BinaryBit.BitPut ()
putAttribute attribute = do
  BinaryBit.putBool True
  putCompressedWord (attributeId attribute)
  putAttributeValue (attributeValue attribute)
