module Rattletrap.AttributeValue.Demolish where

import Rattletrap.Vector
import Rattletrap.Word32

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data DemolishAttributeValue = DemolishAttributeValue
  { demolishAttributeValueAttackerFlag :: Bool
  , demolishAttributeValueAttackerActorId :: Word32
  , demolishAttributeValueVictimFlag :: Bool
  , demolishAttributeValueVictimActorId :: Word32
  , demolishAttributeValueAttackerVelocity :: Vector
  , demolishAttributeValueVictimVelocity :: Vector
  } deriving (Eq, Ord, Show)

getDemolishAttributeValue :: BinaryBit.BitGet DemolishAttributeValue
getDemolishAttributeValue = do
  attackerFlag <- BinaryBit.getBool
  attackerActorId <- getWord32Bits
  victimFlag <- BinaryBit.getBool
  victimActorId <- getWord32Bits
  attackerVelocity <- getVector
  victimVelocity <- getVector
  pure
    (DemolishAttributeValue
       attackerFlag
       attackerActorId
       victimFlag
       victimActorId
       attackerVelocity
       victimVelocity)

putDemolishAttributeValue :: DemolishAttributeValue -> BinaryBit.BitPut ()
putDemolishAttributeValue demolishAttributeValue = do
  BinaryBit.putBool (demolishAttributeValueAttackerFlag demolishAttributeValue)
  putWord32Bits (demolishAttributeValueAttackerActorId demolishAttributeValue)
  BinaryBit.putBool (demolishAttributeValueVictimFlag demolishAttributeValue)
  putWord32Bits (demolishAttributeValueVictimActorId demolishAttributeValue)
  putVector (demolishAttributeValueAttackerVelocity demolishAttributeValue)
  putVector (demolishAttributeValueVictimVelocity demolishAttributeValue)
