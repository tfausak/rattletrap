module Rattletrap.Encode.DemolishAttribute
  ( putDemolishAttribute
  ) where

import Rattletrap.Type.DemolishAttribute
import Rattletrap.Encode.Word32
import Rattletrap.Encode.Vector

import qualified Data.Binary.Bits.Put as BinaryBit

putDemolishAttribute :: DemolishAttribute -> BinaryBit.BitPut ()
putDemolishAttribute demolishAttribute = do
  BinaryBit.putBool (demolishAttributeAttackerFlag demolishAttribute)
  putWord32Bits (demolishAttributeAttackerActorId demolishAttribute)
  BinaryBit.putBool (demolishAttributeVictimFlag demolishAttribute)
  putWord32Bits (demolishAttributeVictimActorId demolishAttribute)
  putVector (demolishAttributeAttackerVelocity demolishAttribute)
  putVector (demolishAttributeVictimVelocity demolishAttribute)
