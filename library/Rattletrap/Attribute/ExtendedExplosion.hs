module Rattletrap.Attribute.ExtendedExplosion where

import Rattletrap.Type.Int32
import Rattletrap.Decode.Int32
import Rattletrap.Encode.Int32
import Rattletrap.Type.Vector
import Rattletrap.Decode.Vector
import Rattletrap.Encode.Vector

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data ExtendedExplosionAttribute = ExtendedExplosionAttribute
  { extendedExplosionAttributeActorId :: Int32
  , extendedExplosionAttributeLocation :: Vector
  , extendedExplosionAttributeUnknown1 :: Bool
  , extendedExplosionAttributeUnknown2 :: Int32
  } deriving (Eq, Ord, Show)

getExtendedExplosionAttribute :: BinaryBit.BitGet ExtendedExplosionAttribute
getExtendedExplosionAttribute = do
  False <- BinaryBit.getBool
  actorId <- getInt32Bits
  location <- getVector
  unknown1 <- BinaryBit.getBool
  unknown2 <- getInt32Bits
  pure (ExtendedExplosionAttribute actorId location unknown1 unknown2)

putExtendedExplosionAttribute
  :: ExtendedExplosionAttribute -> BinaryBit.BitPut ()
putExtendedExplosionAttribute extendedExplosionAttribute = do
  BinaryBit.putBool False
  putInt32Bits (extendedExplosionAttributeActorId extendedExplosionAttribute)
  putVector (extendedExplosionAttributeLocation extendedExplosionAttribute)
  BinaryBit.putBool
    (extendedExplosionAttributeUnknown1 extendedExplosionAttribute)
  putInt32Bits (extendedExplosionAttributeUnknown2 extendedExplosionAttribute)
