module Rattletrap.Replication.Spawned where

import Rattletrap.ActorMap
import Rattletrap.ClassAttributeMap
import Rattletrap.Initialization
import Rattletrap.Primitive

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Map as Map

data SpawnedReplication = SpawnedReplication
  { spawnedReplicationFlag :: Bool
  -- ^ Unclear what this is.
  , spawnedReplicationNameIndex :: Maybe Word32
  , spawnedReplicationName :: Maybe Text
  -- ^ Read-only! Changing a replication's name requires editing the
  -- 'spawnedReplicationNameIndex' and maybe the class attribute map.
  , spawnedReplicationObjectId :: Word32
  , spawnedReplicationObjectName :: Text
  -- ^ Read-only! Changing a replication's object requires editing the class
  -- attribute map.
  , spawnedReplicationClassName :: Text
  -- ^ Read-only! Changing a replication's class requires editing the class
  -- attribute map.
  , spawnedReplicationInitialization :: Initialization
  } deriving (Eq, Ord, Show)

getSpawnedReplication
  :: (Int, Int, Int)
  -> ClassAttributeMap
  -> ActorMap
  -> CompressedWord
  -> BinaryBit.BitGet (SpawnedReplication, ActorMap)
getSpawnedReplication version classAttributeMap actorMap actorId = do
  flag <- BinaryBit.getBool
  nameIndex <- if version < (868, 14, 0)
    then pure Nothing
    else do
      nameIndex <- getWord32Bits
      pure (Just nameIndex)
  name <- lookupName classAttributeMap nameIndex
  objectId <- getWord32Bits
  let newActorMap = Map.insert actorId objectId actorMap
  objectName <- lookupObjectName classAttributeMap objectId
  className <- lookupClassName objectName
  let hasLocation = classHasLocation className
  let hasRotation = classHasRotation className
  initialization <- getInitialization hasLocation hasRotation
  pure
    ( SpawnedReplication
      flag
      nameIndex
      name
      objectId
      objectName
      className
      initialization
    , newActorMap
    )

putSpawnedReplication :: SpawnedReplication -> BinaryBit.BitPut ()
putSpawnedReplication spawnedReplication = do
  BinaryBit.putBool (spawnedReplicationFlag spawnedReplication)
  case spawnedReplicationNameIndex spawnedReplication of
    Nothing -> pure ()
    Just nameIndex -> putWord32Bits nameIndex
  putWord32Bits (spawnedReplicationObjectId spawnedReplication)
  putInitialization (spawnedReplicationInitialization spawnedReplication)

lookupName :: Monad m => ClassAttributeMap -> Maybe Word32 -> m (Maybe Text)
lookupName classAttributeMap maybeNameIndex = case maybeNameIndex of
  Nothing -> pure Nothing
  Just nameIndex ->
    case getName (classAttributeMapNameMap classAttributeMap) nameIndex of
      Nothing -> fail ("could not get name for index " ++ show nameIndex)
      Just name -> pure (Just name)

lookupObjectName :: Monad m => ClassAttributeMap -> Word32 -> m Text
lookupObjectName classAttributeMap objectId =
  case getObjectName (classAttributeMapObjectMap classAttributeMap) objectId of
    Nothing -> fail ("could not get object name for id " ++ show objectId)
    Just objectName -> pure objectName

lookupClassName :: Monad m => Text -> m Text
lookupClassName objectName = case getClassName objectName of
  Nothing -> fail ("could not get class name for object " ++ show objectName)
  Just className -> pure className
