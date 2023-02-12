module Rattletrap.Type.Replication.Spawned where

import qualified Data.Map as Map
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Exception.MissingClassName as MissingClassName
import qualified Rattletrap.Exception.MissingObjectName as MissingObjectName
import qualified Rattletrap.Exception.UnknownName as UnknownName
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.Initialization as Initialization
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json
import qualified Rattletrap.Utility.Monad as Monad

data Spawned = Spawned
  { -- | Unclear what this is.
    flag :: Bool,
    nameIndex :: Maybe U32.U32,
    -- | Read-only! Changing a replication's name requires editing the
    -- 'nameIndex' and maybe the class attribute map.
    name :: Maybe Str.Str,
    objectId :: U32.U32,
    -- | Read-only! Changing a replication's object requires editing the class
    -- attribute map.
    objectName :: Str.Str,
    -- | Read-only! Changing a replication's class requires editing the class
    -- attribute map.
    className :: Str.Str,
    initialization :: Initialization.Initialization
  }
  deriving (Eq, Show)

instance Json.FromJSON Spawned where
  parseJSON = Json.withObject "Spawned" $ \object -> do
    flag <- Json.required object "flag"
    nameIndex <- Json.optional object "name_index"
    name <- Json.optional object "name"
    objectId <- Json.required object "object_id"
    objectName <- Json.required object "object_name"
    className <- Json.required object "class_name"
    initialization <- Json.required object "initialization"
    pure
      Spawned
        { flag,
          nameIndex,
          name,
          objectId,
          objectName,
          className,
          initialization
        }

instance Json.ToJSON Spawned where
  toJSON x =
    Json.object
      [ Json.pair "flag" $ flag x,
        Json.pair "name_index" $ nameIndex x,
        Json.pair "name" $ name x,
        Json.pair "object_id" $ objectId x,
        Json.pair "object_name" $ objectName x,
        Json.pair "class_name" $ className x,
        Json.pair "initialization" $ initialization x
      ]

schema :: Schema.Schema
schema =
  Schema.named "replication-spawned" $
    Schema.object
      [ (Json.pair "flag" $ Schema.ref Schema.boolean, True),
        (Json.pair "name_index" . Schema.json $ Schema.maybe U32.schema, False),
        (Json.pair "name" . Schema.json $ Schema.maybe Str.schema, False),
        (Json.pair "object_id" $ Schema.ref U32.schema, True),
        (Json.pair "object_name" $ Schema.ref Str.schema, True),
        (Json.pair "class_name" $ Schema.ref Str.schema, True),
        (Json.pair "initialization" $ Schema.ref Initialization.schema, True)
      ]

bitPut :: Spawned -> BitPut.BitPut
bitPut spawnedReplication =
  BitPut.bool (flag spawnedReplication)
    <> foldMap U32.bitPut (nameIndex spawnedReplication)
    <> U32.bitPut (objectId spawnedReplication)
    <> Initialization.bitPut (initialization spawnedReplication)

bitGet ::
  Maybe Str.Str ->
  Version.Version ->
  ClassAttributeMap.ClassAttributeMap ->
  CompressedWord.CompressedWord ->
  Map.Map CompressedWord.CompressedWord U32.U32 ->
  BitGet.BitGet
    (Map.Map CompressedWord.CompressedWord U32.U32, Spawned)
bitGet matchType version classAttributeMap actorId actorMap =
  BitGet.label "Spawned" $ do
    flag <- BitGet.label "flag" BitGet.bool
    nameIndex <-
      BitGet.label "nameIndex" $
        Monad.whenMaybe (hasNameIndex matchType version) U32.bitGet
    name <- lookupName classAttributeMap nameIndex
    objectId <- BitGet.label "objectId" U32.bitGet
    objectName <- lookupObjectName classAttributeMap objectId
    className <- lookupClassName objectName
    let hasLocation = ClassAttributeMap.classHasLocation className
    let hasRotation = ClassAttributeMap.classHasRotation className
    initialization <-
      BitGet.label "initialization" $
        Initialization.bitGet version hasLocation hasRotation
    pure
      ( Map.insert actorId objectId actorMap,
        Spawned
          { flag,
            nameIndex,
            name,
            objectId,
            objectName,
            className,
            initialization
          }
      )

hasNameIndex :: Maybe Str.Str -> Version.Version -> Bool
hasNameIndex matchType version =
  Version.atLeast 868 20 0 version
    || Version.atLeast 868 14 0 version
      && (matchType /= Just (Str.fromString "Lan"))

lookupName ::
  ClassAttributeMap.ClassAttributeMap ->
  Maybe U32.U32 ->
  BitGet.BitGet (Maybe Str.Str)
lookupName classAttributeMap maybeNameIndex = case maybeNameIndex of
  Nothing -> pure Nothing
  Just nameIndex_ ->
    case ClassAttributeMap.getName
      (ClassAttributeMap.nameMap classAttributeMap)
      nameIndex_ of
      Nothing ->
        BitGet.throw . UnknownName.UnknownName $ U32.toWord32 nameIndex_
      Just name_ -> pure (Just name_)

lookupObjectName ::
  ClassAttributeMap.ClassAttributeMap -> U32.U32 -> BitGet.BitGet Str.Str
lookupObjectName classAttributeMap objectId_ =
  case ClassAttributeMap.getObjectName
    (ClassAttributeMap.objectMap classAttributeMap)
    objectId_ of
    Nothing ->
      BitGet.throw . MissingObjectName.MissingObjectName $
        U32.toWord32
          objectId_
    Just objectName_ -> pure objectName_

lookupClassName :: Str.Str -> BitGet.BitGet Str.Str
lookupClassName objectName_ =
  case ClassAttributeMap.getClassName objectName_ of
    Nothing ->
      BitGet.throw . MissingClassName.MissingClassName $
        Str.toString
          objectName_
    Just className_ -> pure className_
