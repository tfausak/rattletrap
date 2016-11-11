module Rattletrap.ReplicationValue.Spawned where

import Rattletrap.ActorMap
import Rattletrap.ClassAttributeMap
import Rattletrap.Primitive.CompressedWord
import Rattletrap.Initialization
import Rattletrap.Primitive.Text
import Rattletrap.Primitive.Word32

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data SpawnedReplicationValue = SpawnedReplicationValue
  { spawnedReplicationValueFlag :: Bool
  , spawnedReplicationValueObjectId :: Word32
  , spawnedReplicationValue_objectName :: Text
  , spawnedReplicationValue_className :: Text
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
  objectName <- lookupObjectName classAttributeMap objectId
  className <- lookupClassName objectName
  let hasLocation = classHasLocation className
  let hasRotation = classHasRotation className
  initialization <- getInitialization hasLocation hasRotation
  pure
    ( SpawnedReplicationValue flag objectId objectName className initialization
    , newActorMap)

putSpawnedReplicationValue :: SpawnedReplicationValue -> BinaryBit.BitPut ()
putSpawnedReplicationValue spawnedReplicationValue = do
  BinaryBit.putBool (spawnedReplicationValueFlag spawnedReplicationValue)
  putWord32Bits (spawnedReplicationValueObjectId spawnedReplicationValue)
  putInitialization
    (spawnedReplicationValueInitialization spawnedReplicationValue)

lookupObjectName
  :: Monad m
  => ClassAttributeMap -> Word32 -> m Text
lookupObjectName classAttributeMap objectId =
  case getObjectName classAttributeMap objectId of
    Nothing -> fail ("could not get object name for id " ++ show objectId)
    Just objectName -> pure objectName

lookupClassName
  :: Monad m
  => Text -> m Text
lookupClassName objectName =
  case getClassName objectName of
    Nothing -> fail ("could not get class name for object " ++ show objectName)
    Just className -> pure className
