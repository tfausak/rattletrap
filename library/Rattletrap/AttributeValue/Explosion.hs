module Rattletrap.AttributeValue.Explosion where

import Rattletrap.Primitive.Int32
import Rattletrap.Primitive.Vector

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data ExplosionAttributeValue = ExplosionAttributeValue
  { explosionAttributeValueActorId :: Int32
  , explosionAttributeValueLocation :: Vector
  } deriving (Eq, Ord, Show)

getExplosionAttributeValue :: BinaryBit.BitGet ExplosionAttributeValue
getExplosionAttributeValue = do
  False <- BinaryBit.getBool
  actorId <- getInt32Bits
  location <- getVector
  pure (ExplosionAttributeValue actorId location)

putExplosionAttributeValue :: ExplosionAttributeValue -> BinaryBit.BitPut ()
putExplosionAttributeValue explosionAttributeValue = do
  BinaryBit.putBool False
  putInt32Bits (explosionAttributeValueActorId explosionAttributeValue)
  putVector (explosionAttributeValueLocation explosionAttributeValue)
