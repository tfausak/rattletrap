module Rattletrap.Attribute.WeldedInfo where

import Rattletrap.Primitive

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data WeldedInfoAttribute = WeldedInfoAttribute
  { weldedInfoAttributeActive :: Bool
  , weldedInfoAttributeActorId :: Int32
  , weldedInfoAttributeOffset :: Vector
  , weldedInfoAttributeMass :: Float32
  , weldedInfoAttributeRotation :: Int8Vector
  } deriving (Eq, Ord, Show)

getWeldedInfoAttribute :: BinaryBit.BitGet WeldedInfoAttribute
getWeldedInfoAttribute = do
  active <- BinaryBit.getBool
  actorId <- getInt32Bits
  offset <- getVector
  mass <- getFloat32Bits
  rotation <- getInt8Vector
  pure (WeldedInfoAttribute active actorId offset mass rotation)

putWeldedInfoAttribute :: WeldedInfoAttribute -> BinaryBit.BitPut ()
putWeldedInfoAttribute weldedInfoAttribute = do
  BinaryBit.putBool (weldedInfoAttributeActive weldedInfoAttribute)
  putInt32Bits (weldedInfoAttributeActorId weldedInfoAttribute)
  putVector (weldedInfoAttributeOffset weldedInfoAttribute)
  putFloat32Bits (weldedInfoAttributeMass weldedInfoAttribute)
  putInt8Vector (weldedInfoAttributeRotation weldedInfoAttribute)
