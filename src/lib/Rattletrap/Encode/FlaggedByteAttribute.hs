module Rattletrap.Encode.FlaggedByteAttribute
  ( putFlaggedByteAttribute
  ) where

import Rattletrap.Type.Word8le
import Rattletrap.Type.FlaggedByteAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putFlaggedByteAttribute :: FlaggedByteAttribute -> BinaryBits.BitPut ()
putFlaggedByteAttribute flaggedByteAttribute = do
  BinaryBits.putBool (flaggedByteAttributeFlag flaggedByteAttribute)
  putWord8Bits (flaggedByteAttributeByte flaggedByteAttribute)
