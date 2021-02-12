module Rattletrap.Encode.DemolishAttribute
  ( putDemolishAttribute
  ) where

import Rattletrap.Type.Vector
import Rattletrap.Type.Word32le
import Rattletrap.Type.DemolishAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putDemolishAttribute :: DemolishAttribute -> BinaryBits.BitPut ()
putDemolishAttribute demolishAttribute = do
  BinaryBits.putBool (demolishAttributeAttackerFlag demolishAttribute)
  putWord32Bits (demolishAttributeAttackerActorId demolishAttribute)
  BinaryBits.putBool (demolishAttributeVictimFlag demolishAttribute)
  putWord32Bits (demolishAttributeVictimActorId demolishAttribute)
  putVector (demolishAttributeAttackerVelocity demolishAttribute)
  putVector (demolishAttributeVictimVelocity demolishAttribute)
