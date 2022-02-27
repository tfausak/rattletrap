module Rattletrap.Type.Replication.Spawned where

import qualified Data.Map as Map
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Exception.MissingClassName as MissingClassName
import qualified Rattletrap.Exception.MissingObjectName as MissingObjectName
import qualified Rattletrap.Exception.UnknownName as UnknownName
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.Initialization as Initialization
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Monad as Monad
import qualified Rattletrap.Vendor.Argo as Argo

data Spawned = Spawned
  { flag :: Bool
  -- ^ Unclear what this is.
  , nameIndex :: Maybe U32.U32
  , name :: Maybe Str.Str
  -- ^ Read-only! Changing a replication's name requires editing the
  -- 'nameIndex' and maybe the class attribute map.
  , objectId :: U32.U32
  , objectName :: Str.Str
  -- ^ Read-only! Changing a replication's object requires editing the class
  -- attribute map.
  , className :: Str.Str
  -- ^ Read-only! Changing a replication's class requires editing the class
  -- attribute map.
  , initialization :: Initialization.Initialization
  }
  deriving (Eq, Show)

instance Argo.HasCodec Spawned where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Forbid
      $ Spawned
      <$> Argo.required flag "flag"
      <*> Argo.optional nameIndex "name_index"
      <*> Argo.optional name "name"
      <*> Argo.required objectId "object_id"
      <*> Argo.required objectName "object_name"
      <*> Argo.required className "class_name"
      <*> Argo.required initialization "initialization"

bitPut :: Spawned -> BitPut.BitPut
bitPut spawnedReplication =
  BitPut.bool (flag spawnedReplication)
    <> foldMap U32.bitPut (nameIndex spawnedReplication)
    <> U32.bitPut (objectId spawnedReplication)
    <> Initialization.bitPut (initialization spawnedReplication)

bitGet
  :: Maybe Str.Str
  -> Version.Version
  -> ClassAttributeMap.ClassAttributeMap
  -> CompressedWord.CompressedWord
  -> Map.Map CompressedWord.CompressedWord U32.U32
  -> BitGet.BitGet
       (Map.Map CompressedWord.CompressedWord U32.U32, Spawned)
bitGet matchType version classAttributeMap actorId actorMap =
  BitGet.label "Spawned" $ do
    flag <- BitGet.label "flag" BitGet.bool
    nameIndex <- BitGet.label "nameIndex"
      $ Monad.whenMaybe (hasNameIndex matchType version) U32.bitGet
    name <- lookupName classAttributeMap nameIndex
    objectId <- BitGet.label "objectId" U32.bitGet
    objectName <- lookupObjectName classAttributeMap objectId
    className <- lookupClassName objectName
    let hasLocation = ClassAttributeMap.classHasLocation className
    let hasRotation = ClassAttributeMap.classHasRotation className
    initialization <- BitGet.label "initialization"
      $ Initialization.bitGet version hasLocation hasRotation
    pure
      ( Map.insert actorId objectId actorMap
      , Spawned
        { flag
        , nameIndex
        , name
        , objectId
        , objectName
        , className
        , initialization
        }
      )

hasNameIndex :: Maybe Str.Str -> Version.Version -> Bool
hasNameIndex matchType version =
  Version.atLeast 868 14 0 version && matchType /= Just "Lan"

lookupName
  :: ClassAttributeMap.ClassAttributeMap
  -> Maybe U32.U32
  -> BitGet.BitGet (Maybe Str.Str)
lookupName classAttributeMap maybeNameIndex = case maybeNameIndex of
  Nothing -> pure Nothing
  Just nameIndex_ ->
    case
        ClassAttributeMap.getName
          (ClassAttributeMap.nameMap classAttributeMap)
          nameIndex_
      of
        Nothing ->
          BitGet.throw . UnknownName.UnknownName $ U32.toWord32 nameIndex_
        Just name_ -> pure (Just name_)

lookupObjectName
  :: ClassAttributeMap.ClassAttributeMap -> U32.U32 -> BitGet.BitGet Str.Str
lookupObjectName classAttributeMap objectId_ =
  case
      ClassAttributeMap.getObjectName
        (ClassAttributeMap.objectMap classAttributeMap)
        objectId_
    of
      Nothing ->
        BitGet.throw . MissingObjectName.MissingObjectName $ U32.toWord32
          objectId_
      Just objectName_ -> pure objectName_

lookupClassName :: Str.Str -> BitGet.BitGet Str.Str
lookupClassName objectName_ =
  case ClassAttributeMap.getClassName objectName_ of
    Nothing -> BitGet.throw . MissingClassName.MissingClassName $ Str.toString
      objectName_
    Just className_ -> pure className_
