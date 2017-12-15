module Rattletrap.Decode.DamageStateAttribute
  ( getDamageStateAttribute
  ) where

import Rattletrap.Type.DamageStateAttribute
import Rattletrap.Decode.Word8le
import Rattletrap.Decode.Int32le
import Rattletrap.Decode.Vector

import qualified Data.Binary.Bits.Get as BinaryBit

getDamageStateAttribute :: BinaryBit.BitGet DamageStateAttribute
getDamageStateAttribute = do
  unknown1 <- getWord8Bits
  unknown2 <- BinaryBit.getBool
  unknown3 <- getInt32Bits
  unknown4 <- getVector
  unknown5 <- BinaryBit.getBool
  unknown6 <- BinaryBit.getBool
  pure
    ( DamageStateAttribute
      unknown1
      unknown2
      unknown3
      unknown4
      unknown5
      unknown6
    )
