{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.WeldedInfo where

import Rattletrap.Type.Common
import Rattletrap.Type.Float32le
import Rattletrap.Type.Int32le
import Rattletrap.Type.Int8Vector
import qualified Rattletrap.Type.Vector as Vector
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data WeldedInfoAttribute = WeldedInfoAttribute
  { weldedInfoAttributeActive :: Bool
  , weldedInfoAttributeActorId :: Int32le
  , weldedInfoAttributeOffset :: Vector.Vector
  , weldedInfoAttributeMass :: Float32le
  , weldedInfoAttributeRotation :: Int8Vector
  }
  deriving (Eq, Show)

$(deriveJson ''WeldedInfoAttribute)

putWeldedInfoAttribute :: WeldedInfoAttribute -> BitPut ()
putWeldedInfoAttribute weldedInfoAttribute = do
  BinaryBits.putBool (weldedInfoAttributeActive weldedInfoAttribute)
  putInt32Bits (weldedInfoAttributeActorId weldedInfoAttribute)
  Vector.bitPut (weldedInfoAttributeOffset weldedInfoAttribute)
  putFloat32Bits (weldedInfoAttributeMass weldedInfoAttribute)
  putInt8Vector (weldedInfoAttributeRotation weldedInfoAttribute)

decodeWeldedInfoAttributeBits
  :: (Int, Int, Int) -> BitGet WeldedInfoAttribute
decodeWeldedInfoAttributeBits version =
  WeldedInfoAttribute
    <$> getBool
    <*> decodeInt32leBits
    <*> Vector.bitGet version
    <*> decodeFloat32leBits
    <*> decodeInt8VectorBits
