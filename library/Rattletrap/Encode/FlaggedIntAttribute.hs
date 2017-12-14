module Rattletrap.Encode.FlaggedIntAttribute
  ( putFlaggedIntAttribute
  ) where

import Rattletrap.Type.FlaggedIntAttribute
import Rattletrap.Encode.Int32

import qualified Data.Binary.Bits.Put as BinaryBit

putFlaggedIntAttribute :: FlaggedIntAttribute -> BinaryBit.BitPut ()
putFlaggedIntAttribute flaggedIntAttribute = do
  BinaryBit.putBool (flaggedIntAttributeFlag flaggedIntAttribute)
  putInt32Bits (flaggedIntAttributeInt flaggedIntAttribute)
