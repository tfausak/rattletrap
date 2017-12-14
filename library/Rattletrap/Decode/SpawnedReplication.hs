module Rattletrap.Decode.SpawnedReplication
  ( getSpawnedReplication
  ) where

import Rattletrap.Type.ActorMap
import Rattletrap.Type.ClassAttributeMap
import Rattletrap.Decode.Initialization
import Rattletrap.Type.Word32
import Rattletrap.Decode.Word32
import Rattletrap.Type.Text
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.SpawnedReplication

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Map as Map

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
