module Rattletrap.Encode.WeldedInfoAttribute
  ( putWeldedInfoAttribute
  ) where

import Rattletrap.Type.WeldedInfoAttribute
import Rattletrap.Encode.Int32
import Rattletrap.Encode.Vector
import Rattletrap.Encode.Float32
import Rattletrap.Encode.Int8Vector

import qualified Data.Binary.Bits.Put as BinaryBit

putWeldedInfoAttribute :: WeldedInfoAttribute -> BinaryBit.BitPut ()
putWeldedInfoAttribute weldedInfoAttribute = do
  BinaryBit.putBool (weldedInfoAttributeActive weldedInfoAttribute)
  putInt32Bits (weldedInfoAttributeActorId weldedInfoAttribute)
  putVector (weldedInfoAttributeOffset weldedInfoAttribute)
  putFloat32Bits (weldedInfoAttributeMass weldedInfoAttribute)
  putInt8Vector (weldedInfoAttributeRotation weldedInfoAttribute)
