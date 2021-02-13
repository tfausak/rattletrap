{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.WeldedInfo where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Float32le as Float32le
import qualified Rattletrap.Type.Int32le as Int32le
import qualified Rattletrap.Type.Int8Vector as Int8Vector
import qualified Rattletrap.Type.Vector as Vector
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data WeldedInfoAttribute = WeldedInfoAttribute
  { weldedInfoAttributeActive :: Bool
  , weldedInfoAttributeActorId :: Int32le.Int32le
  , weldedInfoAttributeOffset :: Vector.Vector
  , weldedInfoAttributeMass :: Float32le.Float32le
  , weldedInfoAttributeRotation :: Int8Vector.Int8Vector
  }
  deriving (Eq, Show)

$(deriveJson ''WeldedInfoAttribute)

putWeldedInfoAttribute :: WeldedInfoAttribute -> BitPut ()
putWeldedInfoAttribute weldedInfoAttribute = do
  BinaryBits.putBool (weldedInfoAttributeActive weldedInfoAttribute)
  Int32le.bitPut (weldedInfoAttributeActorId weldedInfoAttribute)
  Vector.bitPut (weldedInfoAttributeOffset weldedInfoAttribute)
  Float32le.bitPut (weldedInfoAttributeMass weldedInfoAttribute)
  Int8Vector.bitPut (weldedInfoAttributeRotation weldedInfoAttribute)

decodeWeldedInfoAttributeBits
  :: (Int, Int, Int) -> BitGet WeldedInfoAttribute
decodeWeldedInfoAttributeBits version =
  WeldedInfoAttribute
    <$> getBool
    <*> Int32le.bitGet
    <*> Vector.bitGet version
    <*> Float32le.bitGet
    <*> Int8Vector.bitGet
