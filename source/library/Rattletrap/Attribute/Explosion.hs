module Rattletrap.Attribute.Explosion where

import Rattletrap.Primitive

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data ExplosionAttribute = ExplosionAttribute
  { explosionAttributeActorId :: Int32
  , explosionAttributeLocation :: Vector
  } deriving (Eq, Show)

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
