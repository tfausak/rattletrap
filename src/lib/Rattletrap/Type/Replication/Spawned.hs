{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Replication.Spawned where

import Rattletrap.Type.Common
import Rattletrap.Type.Initialization
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Decode.Common
import Rattletrap.Type.ClassAttributeMap
import Rattletrap.Type.CompressedWord
import Rattletrap.Encode.Common

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map
import qualified Data.Binary.Bits.Put as BinaryBits

data SpawnedReplication = SpawnedReplication
  { spawnedReplicationFlag :: Bool
  -- ^ Unclear what this is.
  , spawnedReplicationNameIndex :: Maybe Word32le.Word32le
  , spawnedReplicationName :: Maybe Str.Str
  -- ^ Read-only! Changing a replication's name requires editing the
  -- 'spawnedReplicationNameIndex' and maybe the class attribute map.
  , spawnedReplicationObjectId :: Word32le.Word32le
  , spawnedReplicationObjectName :: Str.Str
  -- ^ Read-only! Changing a replication's object requires editing the class
  -- attribute map.
  , spawnedReplicationClassName :: Str.Str
  -- ^ Read-only! Changing a replication's class requires editing the class
  -- attribute map.
  , spawnedReplicationInitialization :: Initialization
  }
  deriving (Eq, Show)

$(deriveJson ''SpawnedReplication)

putSpawnedReplication :: SpawnedReplication -> BitPut ()
putSpawnedReplication spawnedReplication = do
  BinaryBits.putBool (spawnedReplicationFlag spawnedReplication)
  case spawnedReplicationNameIndex spawnedReplication of
    Nothing -> pure ()
    Just nameIndex -> Word32le.bitPut nameIndex
  Word32le.bitPut (spawnedReplicationObjectId spawnedReplication)
  putInitialization (spawnedReplicationInitialization spawnedReplication)

decodeSpawnedReplicationBits
  :: (Int, Int, Int)
  -> ClassAttributeMap
  -> CompressedWord
  -> State.StateT
       (Map.Map CompressedWord Word32le.Word32le)
       BitGet
       SpawnedReplication
decodeSpawnedReplicationBits version classAttributeMap actorId = do
  flag <- Trans.lift getBool
  nameIndex <- decodeWhen
    (version >= (868, 14, 0))
    (Trans.lift Word32le.bitGet)
  name <- either fail pure (lookupName classAttributeMap nameIndex)
  objectId <- Trans.lift Word32le.bitGet
  State.modify (Map.insert actorId objectId)
  objectName <- either fail pure (lookupObjectName classAttributeMap objectId)
  className <- either fail pure (lookupClassName objectName)
  let hasLocation = classHasLocation className
  let hasRotation = classHasRotation className
  initialization <- Trans.lift
    (decodeInitializationBits version hasLocation hasRotation)
  pure
    (SpawnedReplication
      flag
      nameIndex
      name
      objectId
      objectName
      className
      initialization
    )

lookupName :: ClassAttributeMap -> Maybe Word32le.Word32le -> Either String (Maybe Str.Str)
lookupName classAttributeMap maybeNameIndex = case maybeNameIndex of
  Nothing -> Right Nothing
  Just nameIndex ->
    case getName (classAttributeMapNameMap classAttributeMap) nameIndex of
      Nothing ->
        Left ("[RT11] could not get name for index " <> show nameIndex)
      Just name -> Right (Just name)

lookupObjectName :: ClassAttributeMap -> Word32le.Word32le -> Either String Str.Str
lookupObjectName classAttributeMap objectId =
  case getObjectName (classAttributeMapObjectMap classAttributeMap) objectId of
    Nothing ->
      Left ("[RT12] could not get object name for id " <> show objectId)
    Just objectName -> Right objectName

lookupClassName :: Str.Str -> Either String Str.Str
lookupClassName objectName = case getClassName objectName of
  Nothing ->
    Left ("[RT13] could not get class name for object " <> show objectName)
  Just className -> Right className
