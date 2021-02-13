{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.WeldedInfo where

import Rattletrap.Type.Common
import Rattletrap.Type.Float32le
import Rattletrap.Type.Int32le
import Rattletrap.Type.Int8Vector
import Rattletrap.Type.Vector
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data WeldedInfoAttribute = WeldedInfoAttribute
  { weldedInfoAttributeActive :: Bool
  , weldedInfoAttributeActorId :: Int32le
  , weldedInfoAttributeOffset :: Vector
  , weldedInfoAttributeMass :: Float32le
  , weldedInfoAttributeRotation :: Int8Vector
  }
  deriving (Eq, Show)

$(deriveJson ''WeldedInfoAttribute)

putWeldedInfoAttribute :: WeldedInfoAttribute -> BitPut ()
putWeldedInfoAttribute weldedInfoAttribute = do
  BinaryBits.putBool (weldedInfoAttributeActive weldedInfoAttribute)
  putInt32Bits (weldedInfoAttributeActorId weldedInfoAttribute)
  putVector (weldedInfoAttributeOffset weldedInfoAttribute)
  putFloat32Bits (weldedInfoAttributeMass weldedInfoAttribute)
  putInt8Vector (weldedInfoAttributeRotation weldedInfoAttribute)

decodeWeldedInfoAttributeBits
  :: (Int, Int, Int) -> BitGet WeldedInfoAttribute
decodeWeldedInfoAttributeBits version =
  WeldedInfoAttribute
    <$> getBool
    <*> decodeInt32leBits
    <*> decodeVectorBits version
    <*> decodeFloat32leBits
    <*> decodeInt8VectorBits
