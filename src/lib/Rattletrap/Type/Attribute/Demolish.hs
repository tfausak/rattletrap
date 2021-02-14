{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Demolish where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.Type.U32 as U32
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data DemolishAttribute = DemolishAttribute
  { attackerFlag :: Bool
  , attackerActorId :: U32.U32
  , victimFlag :: Bool
  , victimActorId :: U32.U32
  , attackerVelocity :: Vector.Vector
  , victimVelocity :: Vector.Vector
  }
  deriving (Eq, Show)

$(deriveJson ''DemolishAttribute)

bitPut :: DemolishAttribute -> BitPut ()
bitPut demolishAttribute = do
  BinaryBits.putBool (attackerFlag demolishAttribute)
  U32.bitPut (attackerActorId demolishAttribute)
  BinaryBits.putBool (victimFlag demolishAttribute)
  U32.bitPut (victimActorId demolishAttribute)
  Vector.bitPut (attackerVelocity demolishAttribute)
  Vector.bitPut (victimVelocity demolishAttribute)

bitGet :: (Int, Int, Int) -> BitGet DemolishAttribute
bitGet version =
  DemolishAttribute
    <$> getBool
    <*> U32.bitGet
    <*> getBool
    <*> U32.bitGet
    <*> Vector.bitGet version
    <*> Vector.bitGet version
