{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Demolish where

import Rattletrap.Type.Common
import Rattletrap.Type.Vector
import Rattletrap.Type.Word32le
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data DemolishAttribute = DemolishAttribute
  { demolishAttributeAttackerFlag :: Bool
  , demolishAttributeAttackerActorId :: Word32le
  , demolishAttributeVictimFlag :: Bool
  , demolishAttributeVictimActorId :: Word32le
  , demolishAttributeAttackerVelocity :: Vector
  , demolishAttributeVictimVelocity :: Vector
  }
  deriving (Eq, Show)

$(deriveJson ''DemolishAttribute)

putDemolishAttribute :: DemolishAttribute -> BinaryBits.BitPut ()
putDemolishAttribute demolishAttribute = do
  BinaryBits.putBool (demolishAttributeAttackerFlag demolishAttribute)
  putWord32Bits (demolishAttributeAttackerActorId demolishAttribute)
  BinaryBits.putBool (demolishAttributeVictimFlag demolishAttribute)
  putWord32Bits (demolishAttributeVictimActorId demolishAttribute)
  putVector (demolishAttributeAttackerVelocity demolishAttribute)
  putVector (demolishAttributeVictimVelocity demolishAttribute)

decodeDemolishAttributeBits :: (Int, Int, Int) -> BitGet DemolishAttribute
decodeDemolishAttributeBits version =
  DemolishAttribute
    <$> getBool
    <*> decodeWord32leBits
    <*> getBool
    <*> decodeWord32leBits
    <*> decodeVectorBits version
    <*> decodeVectorBits version
