{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.WeldedInfoAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.Float32le
import Rattletrap.Type.Int32le
import Rattletrap.Type.Int8Vector
import Rattletrap.Type.Vector
import Rattletrap.Encode.Float32le

import qualified Data.Binary.Bits.Put as BinaryBits

data WeldedInfoAttribute = WeldedInfoAttribute
  { weldedInfoAttributeActive :: Bool
  , weldedInfoAttributeActorId :: Int32le
  , weldedInfoAttributeOffset :: Vector
  , weldedInfoAttributeMass :: Float32le
  , weldedInfoAttributeRotation :: Int8Vector
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''WeldedInfoAttribute)

putWeldedInfoAttribute :: WeldedInfoAttribute -> BinaryBits.BitPut ()
putWeldedInfoAttribute weldedInfoAttribute = do
  BinaryBits.putBool (weldedInfoAttributeActive weldedInfoAttribute)
  putInt32Bits (weldedInfoAttributeActorId weldedInfoAttribute)
  putVector (weldedInfoAttributeOffset weldedInfoAttribute)
  putFloat32Bits (weldedInfoAttributeMass weldedInfoAttribute)
  putInt8Vector (weldedInfoAttributeRotation weldedInfoAttribute)
