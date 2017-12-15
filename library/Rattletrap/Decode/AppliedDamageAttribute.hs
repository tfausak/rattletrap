module Rattletrap.Decode.AppliedDamageAttribute
  ( getAppliedDamageAttribute
  ) where

import Rattletrap.Decode.Int32le
import Rattletrap.Decode.Vector
import Rattletrap.Decode.Word8le
import Rattletrap.Type.AppliedDamageAttribute

import qualified Data.Binary.Bits.Get as BinaryBit

getAppliedDamageAttribute :: BinaryBit.BitGet AppliedDamageAttribute
getAppliedDamageAttribute = do
  unknown1 <- getWord8Bits
  location <- getVector
  unknown3 <- getInt32Bits
  unknown4 <- getInt32Bits
  pure (AppliedDamageAttribute unknown1 location unknown3 unknown4)
