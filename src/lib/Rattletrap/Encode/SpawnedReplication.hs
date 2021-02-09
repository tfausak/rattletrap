module Rattletrap.Encode.SpawnedReplication
  ( putSpawnedReplication
  ) where

import Rattletrap.Encode.Initialization
import Rattletrap.Encode.Word32le
import Rattletrap.Type.SpawnedReplication

import qualified Data.Binary.Bits.Put as BinaryBits

putSpawnedReplication :: SpawnedReplication -> BinaryBits.BitPut ()
putSpawnedReplication spawnedReplication = do
  BinaryBits.putBool (spawnedReplicationFlag spawnedReplication)
  case spawnedReplicationNameIndex spawnedReplication of
    Nothing -> pure ()
    Just nameIndex -> putWord32Bits nameIndex
  putWord32Bits (spawnedReplicationObjectId spawnedReplication)
  putInitialization (spawnedReplicationInitialization spawnedReplication)
