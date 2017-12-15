module Rattletrap.Encode.WeldedInfoAttribute
  ( putWeldedInfoAttribute
  ) where

import Rattletrap.Encode.Float32le
import Rattletrap.Encode.Int32le
import Rattletrap.Encode.Int8Vector
import Rattletrap.Encode.Vector
import Rattletrap.Type.WeldedInfoAttribute

import qualified Data.Binary.Bits.Put as BinaryBit

putWeldedInfoAttribute :: WeldedInfoAttribute -> BinaryBit.BitPut ()
putWeldedInfoAttribute weldedInfoAttribute = do
  BinaryBit.putBool (weldedInfoAttributeActive weldedInfoAttribute)
  putInt32Bits (weldedInfoAttributeActorId weldedInfoAttribute)
  putVector (weldedInfoAttributeOffset weldedInfoAttribute)
  putFloat32Bits (weldedInfoAttributeMass weldedInfoAttribute)
  putInt8Vector (weldedInfoAttributeRotation weldedInfoAttribute)
