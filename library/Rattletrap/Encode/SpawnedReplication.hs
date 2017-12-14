module Rattletrap.Encode.SpawnedReplication
  ( putSpawnedReplication
  ) where

import Rattletrap.Initialization
import Rattletrap.Encode.Word32
import Rattletrap.Type.SpawnedReplication

import qualified Data.Binary.Bits.Put as BinaryBit

putSpawnedReplication :: SpawnedReplication -> BinaryBit.BitPut ()
putSpawnedReplication spawnedReplication = do
  BinaryBit.putBool (spawnedReplicationFlag spawnedReplication)
  case spawnedReplicationNameIndex spawnedReplication of
    Nothing -> pure ()
    Just nameIndex -> putWord32Bits nameIndex
  putWord32Bits (spawnedReplicationObjectId spawnedReplication)
  putInitialization (spawnedReplicationInitialization spawnedReplication)
