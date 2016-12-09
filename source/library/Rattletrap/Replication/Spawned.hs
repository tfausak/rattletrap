module Rattletrap.Replication.Spawned where

import Rattletrap.ActorMap
import Rattletrap.ClassAttributeMap
import Rattletrap.Initialization
import Rattletrap.Primitive

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data SpawnedReplication = SpawnedReplication
  { spawnedReplicationFlag :: Bool
  -- ^ Unclear what this is.
  , spawnedReplicationNameIndex :: Maybe Word32
  , spawnedReplicationObjectId :: Word32
  , spawnedReplication_objectName :: Text
  -- ^ Read-only! Changing a replication's object requires editing the class
  -- attribute map.
  , spawnedReplication_className :: Text
  -- ^ Read-only! Changing a replication's class requires editing the class
  -- attribute map.
  , spawnedReplicationInitialization :: Initialization
  } deriving (Eq, Ord, Show)

getSpawnedReplication
  :: (Int, Int)
  -> ClassAttributeMap
  -> ActorMap
  -> CompressedWord
  -> BinaryBit.BitGet (SpawnedReplication, ActorMap)
getSpawnedReplication version classAttributeMap actorMap actorId = do
  flag <- BinaryBit.getBool
  nameIndex <-
    if version < (868, 14)
      then pure Nothing
      else do
        nameIndex <- getWord32Bits
        pure (Just nameIndex)
  objectId <- getWord32Bits
  let newActorMap = updateActorMap actorId objectId actorMap
  objectName <- lookupObjectName classAttributeMap objectId
  className <- lookupClassName objectName
  let hasLocation = classHasLocation className
  let hasRotation = classHasRotation className
  initialization <- getInitialization hasLocation hasRotation
  pure
    ( SpawnedReplication
        flag
        nameIndex
        objectId
        objectName
        className
        initialization
    , newActorMap)

putSpawnedReplication :: SpawnedReplication -> BinaryBit.BitPut ()
putSpawnedReplication spawnedReplication = do
  BinaryBit.putBool (spawnedReplicationFlag spawnedReplication)
  case spawnedReplicationNameIndex spawnedReplication of
    Nothing -> pure ()
    Just nameIndex -> putWord32Bits nameIndex
  putWord32Bits (spawnedReplicationObjectId spawnedReplication)
  putInitialization (spawnedReplicationInitialization spawnedReplication)

lookupObjectName
  :: Monad m
  => ClassAttributeMap -> Word32 -> m Text
lookupObjectName classAttributeMap objectId =
  case getObjectName (classAttributeMapObjectMap classAttributeMap) objectId of
    Nothing -> fail ("could not get object name for id " ++ show objectId)
    Just objectName -> pure objectName

lookupClassName
  :: Monad m
  => Text -> m Text
lookupClassName objectName =
  case getClassName objectName of
    Nothing -> fail ("could not get class name for object " ++ show objectName)
    Just className -> pure className
