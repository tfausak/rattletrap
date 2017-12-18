module Rattletrap.Encode.FlaggedIntAttribute
  ( putFlaggedIntAttribute
  ) where

import Rattletrap.Encode.Int32le
import Rattletrap.Type.FlaggedIntAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putFlaggedIntAttribute :: FlaggedIntAttribute -> BinaryBits.BitPut ()
putFlaggedIntAttribute flaggedIntAttribute = do
  BinaryBits.putBool (flaggedIntAttributeFlag flaggedIntAttribute)
  putInt32Bits (flaggedIntAttributeInt flaggedIntAttribute)
