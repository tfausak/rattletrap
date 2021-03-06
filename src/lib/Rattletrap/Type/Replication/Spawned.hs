module Rattletrap.Type.Replication.Spawned where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.Initialization as Initialization
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json
import Rattletrap.Utility.Monad

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map

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

instance Json.FromJSON Spawned where
  parseJSON = Json.withObject "Spawned" $ \object -> do
    flag <- Json.required object "flag"
    nameIndex <- Json.optional object "name_index"
    name <- Json.optional object "name"
    objectId <- Json.required object "object_id"
    objectName <- Json.required object "object_name"
    className <- Json.required object "class_name"
    initialization <- Json.required object "initialization"
    pure Spawned
      { flag
      , nameIndex
      , name
      , objectId
      , objectName
      , className
      , initialization
      }

instance Json.ToJSON Spawned where
  toJSON x = Json.object
    [ Json.pair "flag" $ flag x
    , Json.pair "name_index" $ nameIndex x
    , Json.pair "name" $ name x
    , Json.pair "object_id" $ objectId x
    , Json.pair "object_name" $ objectName x
    , Json.pair "class_name" $ className x
    , Json.pair "initialization" $ initialization x
    ]

schema :: Schema.Schema
schema = Schema.named "replication-spawned" $ Schema.object
  [ (Json.pair "flag" $ Schema.ref Schema.boolean, True)
  , (Json.pair "name_index" . Schema.json $ Schema.maybe U32.schema, False)
  , (Json.pair "name" . Schema.json $ Schema.maybe Str.schema, False)
  , (Json.pair "object_id" $ Schema.ref U32.schema, True)
  , (Json.pair "object_name" $ Schema.ref Str.schema, True)
  , (Json.pair "class_name" $ Schema.ref Str.schema, True)
  , (Json.pair "initialization" $ Schema.ref Initialization.schema, True)
  ]

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
  -> State.StateT
       (Map.Map CompressedWord.CompressedWord U32.U32)
       BitGet.BitGet
       Spawned
bitGet matchType version classAttributeMap actorId = do
  flag_ <- Trans.lift BitGet.bool
  nameIndex_ <- whenMaybe (hasNameIndex matchType version) $ Trans.lift U32.bitGet
  name_ <- either fail pure (lookupName classAttributeMap nameIndex_)
  objectId_ <- Trans.lift U32.bitGet
  State.modify (Map.insert actorId objectId_)
  objectName_ <- either
    fail
    pure
    (lookupObjectName classAttributeMap objectId_)
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

hasNameIndex :: Maybe Str.Str -> Version.Version -> Bool
hasNameIndex matchType version = Version.atLeast 868 14 0 version
  && matchType /= Just (Str.fromString "Lan")

lookupName
  :: ClassAttributeMap.ClassAttributeMap
  -> Maybe U32.U32
  -> Either String (Maybe Str.Str)
lookupName classAttributeMap maybeNameIndex = case maybeNameIndex of
  Nothing -> Right Nothing
  Just nameIndex_ ->
    case
        ClassAttributeMap.getName
          (ClassAttributeMap.nameMap classAttributeMap)
          nameIndex_
      of
        Nothing ->
          Left ("[RT11] could not get name for index " <> show nameIndex_)
        Just name_ -> Right (Just name_)

lookupObjectName
  :: ClassAttributeMap.ClassAttributeMap -> U32.U32 -> Either String Str.Str
lookupObjectName classAttributeMap objectId_ =
  case
      ClassAttributeMap.getObjectName
        (ClassAttributeMap.objectMap classAttributeMap)
        objectId_
    of
      Nothing ->
        Left ("[RT12] could not get object name for id " <> show objectId_)
      Just objectName_ -> Right objectName_

lookupClassName :: Str.Str -> Either String Str.Str
lookupClassName objectName_ =
  case ClassAttributeMap.getClassName objectName_ of
    Nothing ->
      Left ("[RT13] could not get class name for object " <> show objectName_)
    Just className_ -> Right className_
