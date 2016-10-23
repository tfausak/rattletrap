module Rattletrap.AttributeValue.Explosion where

import Rattletrap.Int32
import Rattletrap.Vector

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data ExplosionAttributeValue = ExplosionAttributeValue
  { explosionAttributeValueMaybeActorId :: Maybe Int32
  , explosionAttributeValueLocation :: Vector
  } deriving (Eq, Ord, Show)

getExplosionAttributeValue :: BinaryBit.BitGet ExplosionAttributeValue
getExplosionAttributeValue = do
  actorless <- BinaryBit.getBool
  maybeActorId <-
    if actorless
      then pure Nothing
      else do
        actorId <- getInt32Bits
        pure (Just actorId)
  location <- getVector
  pure (ExplosionAttributeValue maybeActorId location)

putExplosionAttributeValue :: ExplosionAttributeValue -> BinaryBit.BitPut ()
putExplosionAttributeValue explosionAttributeValue = do
  case explosionAttributeValueMaybeActorId explosionAttributeValue of
    Nothing -> BinaryBit.putBool True
    Just actorId -> do
      BinaryBit.putBool False
      putInt32Bits actorId
  putVector (explosionAttributeValueLocation explosionAttributeValue)
