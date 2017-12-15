module Rattletrap.Decode.DemolishAttribute
  ( getDemolishAttribute
  ) where

import Rattletrap.Decode.Vector
import Rattletrap.Decode.Word32le
import Rattletrap.Type.DemolishAttribute

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
