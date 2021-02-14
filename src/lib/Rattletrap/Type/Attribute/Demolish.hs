{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Demolish where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data DemolishAttribute = DemolishAttribute
  { attackerFlag :: Bool
  , attackerActorId :: Word32le.Word32le
  , victimFlag :: Bool
  , victimActorId :: Word32le.Word32le
  , attackerVelocity :: Vector.Vector
  , victimVelocity :: Vector.Vector
  }
  deriving (Eq, Show)

$(deriveJson ''DemolishAttribute)

bitPut :: DemolishAttribute -> BitPut ()
bitPut demolishAttribute = do
  BinaryBits.putBool (attackerFlag demolishAttribute)
  Word32le.bitPut (attackerActorId demolishAttribute)
  BinaryBits.putBool (victimFlag demolishAttribute)
  Word32le.bitPut (victimActorId demolishAttribute)
  Vector.bitPut (attackerVelocity demolishAttribute)
  Vector.bitPut (victimVelocity demolishAttribute)

bitGet :: (Int, Int, Int) -> BitGet DemolishAttribute
bitGet version =
  DemolishAttribute
    <$> getBool
    <*> Word32le.bitGet
    <*> getBool
    <*> Word32le.bitGet
    <*> Vector.bitGet version
    <*> Vector.bitGet version
