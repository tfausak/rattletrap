{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Demolish where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data DemolishAttribute = DemolishAttribute
  { demolishAttributeAttackerFlag :: Bool
  , demolishAttributeAttackerActorId :: Word32le.Word32le
  , demolishAttributeVictimFlag :: Bool
  , demolishAttributeVictimActorId :: Word32le.Word32le
  , demolishAttributeAttackerVelocity :: Vector.Vector
  , demolishAttributeVictimVelocity :: Vector.Vector
  }
  deriving (Eq, Show)

$(deriveJson ''DemolishAttribute)

putDemolishAttribute :: DemolishAttribute -> BitPut ()
putDemolishAttribute demolishAttribute = do
  BinaryBits.putBool (demolishAttributeAttackerFlag demolishAttribute)
  Word32le.bitPut (demolishAttributeAttackerActorId demolishAttribute)
  BinaryBits.putBool (demolishAttributeVictimFlag demolishAttribute)
  Word32le.bitPut (demolishAttributeVictimActorId demolishAttribute)
  Vector.bitPut (demolishAttributeAttackerVelocity demolishAttribute)
  Vector.bitPut (demolishAttributeVictimVelocity demolishAttribute)

decodeDemolishAttributeBits :: (Int, Int, Int) -> BitGet DemolishAttribute
decodeDemolishAttributeBits version =
  DemolishAttribute
    <$> getBool
    <*> Word32le.bitGet
    <*> getBool
    <*> Word32le.bitGet
    <*> Vector.bitGet version
    <*> Vector.bitGet version
