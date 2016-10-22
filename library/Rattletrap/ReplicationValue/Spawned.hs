module Rattletrap.ReplicationValue.Spawned where

import Rattletrap.ActorMap
import Rattletrap.ClassAttributeMap
import Rattletrap.CompressedWord
import Rattletrap.Initialization
import Rattletrap.Word32

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data SpawnedReplicationValue = SpawnedReplicationValue
  { spawnedReplicationValueFlag :: Bool
  , spawnedReplicationValueObjectId :: Word32
  , spawnedReplicationValueInitialization :: Initialization
  } deriving (Eq, Ord, Show)

getSpawnedReplicationValue
  :: ClassAttributeMap
  -> ActorMap
  -> CompressedWord
  -> BinaryBit.BitGet (SpawnedReplicationValue, ActorMap)
getSpawnedReplicationValue classAttributeMap actorMap actorId = do
  flag <- BinaryBit.getBool
  objectId <- getWord32Bits
  let newActorMap = updateActorMap actorId objectId actorMap
  case getObjectName classAttributeMap objectId of
    Nothing -> fail ("could not get object name for id " ++ show objectId)
    Just objectName ->
      case getClassName objectName of
        Nothing ->
          fail ("could not get class name for object " ++ show objectName)
        Just className -> do
          let hasLocation = classHasLocation className
          let hasRotation = classHasRotation className
          initialization <- getInitialization hasLocation hasRotation
          pure
            (SpawnedReplicationValue flag objectId initialization, newActorMap)

putSpawnedReplicationValue :: SpawnedReplicationValue -> BinaryBit.BitPut ()
putSpawnedReplicationValue spawnedReplicationValue = do
  BinaryBit.putBool (spawnedReplicationValueFlag spawnedReplicationValue)
  putWord32Bits (spawnedReplicationValueObjectId spawnedReplicationValue)
  putInitialization
    (spawnedReplicationValueInitialization spawnedReplicationValue)
