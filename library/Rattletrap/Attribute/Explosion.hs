module Rattletrap.Attribute.Explosion where

import Rattletrap.Type.Int32
import Rattletrap.Decode.Int32
import Rattletrap.Encode.Int32
import Rattletrap.Type.Vector
import Rattletrap.Decode.Vector
import Rattletrap.Encode.Vector

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data ExplosionAttribute = ExplosionAttribute
  { explosionAttributeActorId :: Int32
  , explosionAttributeLocation :: Vector
  } deriving (Eq, Ord, Show)

getExplosionAttribute :: BinaryBit.BitGet ExplosionAttribute
getExplosionAttribute = do
  False <- BinaryBit.getBool
  actorId <- getInt32Bits
  location <- getVector
  pure (ExplosionAttribute actorId location)

putExplosionAttribute :: ExplosionAttribute -> BinaryBit.BitPut ()
putExplosionAttribute explosionAttribute = do
  BinaryBit.putBool False
  putInt32Bits (explosionAttributeActorId explosionAttribute)
  putVector (explosionAttributeLocation explosionAttribute)
