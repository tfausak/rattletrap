{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.DemolishAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.Vector
import Rattletrap.Type.Word32le

import qualified Data.Binary.Bits.Put as BinaryBits

data DemolishAttribute = DemolishAttribute
  { demolishAttributeAttackerFlag :: Bool
  , demolishAttributeAttackerActorId :: Word32le
  , demolishAttributeVictimFlag :: Bool
  , demolishAttributeVictimActorId :: Word32le
  , demolishAttributeAttackerVelocity :: Vector
  , demolishAttributeVictimVelocity :: Vector
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''DemolishAttribute)

putDemolishAttribute :: DemolishAttribute -> BinaryBits.BitPut ()
putDemolishAttribute demolishAttribute = do
  BinaryBits.putBool (demolishAttributeAttackerFlag demolishAttribute)
  putWord32Bits (demolishAttributeAttackerActorId demolishAttribute)
  BinaryBits.putBool (demolishAttributeVictimFlag demolishAttribute)
  putWord32Bits (demolishAttributeVictimActorId demolishAttribute)
  putVector (demolishAttributeAttackerVelocity demolishAttribute)
  putVector (demolishAttributeVictimVelocity demolishAttribute)
