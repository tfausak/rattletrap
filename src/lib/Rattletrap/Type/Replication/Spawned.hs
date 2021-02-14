{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Replication.Spawned where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Initialization as Initialization
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Decode.Common
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import Rattletrap.Encode.Common

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map
import qualified Data.Binary.Bits.Put as BinaryBits

data Spawned = Spawned
  { flag :: Bool
  -- ^ Unclear what this is.
  , nameIndex :: Maybe Word32le.Word32le
  , name :: Maybe Str.Str
  -- ^ Read-only! Changing a replication's name requires editing the
  -- 'nameIndex' and maybe the class attribute map.
  , objectId :: Word32le.Word32le
  , objectName :: Str.Str
  -- ^ Read-only! Changing a replication's object requires editing the class
  -- attribute map.
  , className :: Str.Str
  -- ^ Read-only! Changing a replication's class requires editing the class
  -- attribute map.
  , initialization :: Initialization.Initialization
  }
  deriving (Eq, Show)

$(deriveJson ''Spawned)

bitPut :: Spawned -> BitPut ()
bitPut spawnedReplication = do
  BinaryBits.putBool (flag spawnedReplication)
  case nameIndex spawnedReplication of
    Nothing -> pure ()
    Just nameIndex_ -> Word32le.bitPut nameIndex_
  Word32le.bitPut (objectId spawnedReplication)
  Initialization.bitPut (initialization spawnedReplication)

bitGet
  :: (Int, Int, Int)
  -> ClassAttributeMap.ClassAttributeMap
  -> CompressedWord.CompressedWord
  -> State.StateT
       (Map.Map CompressedWord.CompressedWord Word32le.Word32le)
       BitGet
       Spawned
bitGet version classAttributeMap actorId = do
  flag_ <- Trans.lift getBool
  nameIndex_ <- decodeWhen
    (version >= (868, 14, 0))
    (Trans.lift Word32le.bitGet)
  name_ <- either fail pure (lookupName classAttributeMap nameIndex_)
  objectId_ <- Trans.lift Word32le.bitGet
  State.modify (Map.insert actorId objectId_)
  objectName_ <- either fail pure (lookupObjectName classAttributeMap objectId_)
  className_ <- either fail pure (lookupClassName objectName_)
  let hasLocation = ClassAttributeMap.classHasLocation className_
  let hasRotation = ClassAttributeMap.classHasRotation className_
  initialization_ <- Trans.lift
    (Initialization.bitGet version hasLocation hasRotation)
  pure
    (Spawned
      flag_
      nameIndex_
      name_
      objectId_
      objectName_
      className_
      initialization_
    )

lookupName :: ClassAttributeMap.ClassAttributeMap -> Maybe Word32le.Word32le -> Either String (Maybe Str.Str)
lookupName classAttributeMap maybeNameIndex = case maybeNameIndex of
  Nothing -> Right Nothing
  Just nameIndex_ ->
    case ClassAttributeMap.getName (ClassAttributeMap.nameMap classAttributeMap) nameIndex_ of
      Nothing ->
        Left ("[RT11] could not get name for index " <> show nameIndex_)
      Just name_ -> Right (Just name_)

lookupObjectName :: ClassAttributeMap.ClassAttributeMap -> Word32le.Word32le -> Either String Str.Str
lookupObjectName classAttributeMap objectId_ =
  case ClassAttributeMap.getObjectName (ClassAttributeMap.objectMap classAttributeMap) objectId_ of
    Nothing ->
      Left ("[RT12] could not get object name for id " <> show objectId_)
    Just objectName_ -> Right objectName_

lookupClassName :: Str.Str -> Either String Str.Str
lookupClassName objectName_ = case ClassAttributeMap.getClassName objectName_ of
  Nothing ->
    Left ("[RT13] could not get class name for object " <> show objectName_)
  Just className_ -> Right className_
