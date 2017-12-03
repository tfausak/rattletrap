module Rattletrap.Attribute.Demolish where

import Rattletrap.Primitive

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data DemolishAttribute = DemolishAttribute
  { demolishAttributeAttackerFlag :: Bool
  , demolishAttributeAttackerActorId :: Word32
  , demolishAttributeVictimFlag :: Bool
  , demolishAttributeVictimActorId :: Word32
  , demolishAttributeAttackerVelocity :: Vector
  , demolishAttributeVictimVelocity :: Vector
  } deriving (Eq, Ord, Show)

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

putDemolishAttribute :: DemolishAttribute -> BinaryBit.BitPut ()
putDemolishAttribute demolishAttribute = do
  BinaryBit.putBool (demolishAttributeAttackerFlag demolishAttribute)
  putWord32Bits (demolishAttributeAttackerActorId demolishAttribute)
  BinaryBit.putBool (demolishAttributeVictimFlag demolishAttribute)
  putWord32Bits (demolishAttributeVictimActorId demolishAttribute)
  putVector (demolishAttributeAttackerVelocity demolishAttribute)
  putVector (demolishAttributeVictimVelocity demolishAttribute)
