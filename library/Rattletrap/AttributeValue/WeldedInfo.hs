module Rattletrap.AttributeValue.WeldedInfo where

import Rattletrap.Primitive.Float32
import Rattletrap.Primitive.Int32
import Rattletrap.Primitive.Int8Vector
import Rattletrap.Primitive.Vector

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data WeldedInfoAttributeValue = WeldedInfoAttributeValue
  { weldedInfoAttributeValueActive :: Bool
  , weldedInfoAttributeValueActorId :: Int32
  , weldedInfoAttributeValueOffset :: Vector
  , weldedInfoAttributeValueMass :: Float32
  , weldedInfoAttributeValueRotation :: Int8Vector
  } deriving (Eq, Ord, Show)

getWeldedInfoAttributeValue :: BinaryBit.BitGet WeldedInfoAttributeValue
getWeldedInfoAttributeValue = do
  active <- BinaryBit.getBool
  actorId <- getInt32Bits
  offset <- getVector
  mass <- getFloat32Bits
  rotation <- getInt8Vector
  pure (WeldedInfoAttributeValue active actorId offset mass rotation)

putWeldedInfoAttributeValue :: WeldedInfoAttributeValue -> BinaryBit.BitPut ()
putWeldedInfoAttributeValue weldedInfoAttributeValue = do
  BinaryBit.putBool (weldedInfoAttributeValueActive weldedInfoAttributeValue)
  putInt32Bits (weldedInfoAttributeValueActorId weldedInfoAttributeValue)
  putVector (weldedInfoAttributeValueOffset weldedInfoAttributeValue)
  putFloat32Bits (weldedInfoAttributeValueMass weldedInfoAttributeValue)
  putInt8Vector (weldedInfoAttributeValueRotation weldedInfoAttributeValue)
