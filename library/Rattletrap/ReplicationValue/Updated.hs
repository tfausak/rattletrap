module Rattletrap.ReplicationValue.Updated where

import Rattletrap.ActorMap
import Rattletrap.Attribute
import Rattletrap.ClassAttributeMap
import Rattletrap.Primitive

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

newtype UpdatedReplicationValue = UpdatedReplicationValue
  { updatedReplicationValueAttributes :: [Attribute]
  } deriving (Eq, Ord, Show)

getUpdatedReplicationValue
  :: (Int, Int)
  -> ClassAttributeMap
  -> ActorMap
  -> CompressedWord
  -> BinaryBit.BitGet UpdatedReplicationValue
getUpdatedReplicationValue version classAttributeMap actorMap actorId = do
  attributes <- getAttributes version classAttributeMap actorMap actorId
  pure (UpdatedReplicationValue attributes)

putUpdatedReplicationValue :: UpdatedReplicationValue -> BinaryBit.BitPut ()
putUpdatedReplicationValue updatedReplicationValue =
  putAttributes (updatedReplicationValueAttributes updatedReplicationValue)
