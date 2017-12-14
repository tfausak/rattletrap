module Rattletrap.Decode.DemolishAttribute
  ( getDemolishAttribute
  ) where

import Rattletrap.Type.DemolishAttribute
import Rattletrap.Decode.Word32
import Rattletrap.Decode.Vector

import qualified Data.Binary.Bits.Get as BinaryBit

getDemolishAttribute :: BinaryBit.BitGet DemolishAttribute
getDemolishAttribute = do
  attackerFlag <- BinaryBit.getBool
  attackerActorId <- getWord32Bits
  victimFlag <- BinaryBit.getBool
  victimActorId <- getWord32Bits
  attackerVelocity <- getVector
  victimVelocity <- getVector
  pure
    ( DemolishAttribute
      attackerFlag
      attackerActorId
      victimFlag
      victimActorId
      attackerVelocity
      victimVelocity
    )
