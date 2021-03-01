module Rattletrap.Type.ClassAttributeMap
  ( ClassAttributeMap(..)
  , classHasLocation
  , classHasRotation
  , getAttributeIdLimit
  , getAttributeMap
  , getAttributeName
  , getClassName
  , getName
  , getObjectName
  , make
  ) where

import qualified Rattletrap.Data as Data
import qualified Rattletrap.Type.AttributeMapping as AttributeMapping
import qualified Rattletrap.Type.Cache as Cache
import qualified Rattletrap.Type.ClassMapping as ClassMapping
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32

import qualified Data.IntMap as IntMap
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Tuple as Tuple

-- | This data structure holds all the information about classes, objects, and
-- attributes in the replay. The class hierarchy is not fixed; it is encoded
-- in the 'Rattletrap.Content.Content'. Similarly, the attributes that belong
-- to each class are not fixed either. Converting the raw data into a usable
-- structure is tedious; see 'make'.
data ClassAttributeMap = ClassAttributeMap
  { objectMap :: Map.Map U32.U32 Str.Str
  -- ^ A map from object IDs to their names.
  , objectClassMap :: Map.Map U32.U32 U32.U32
  -- ^ A map from object IDs to their class IDs.
  , value :: Map.Map U32.U32 (Map.Map U32.U32 U32.U32)
  -- ^ A map from class IDs to a map from attribute stream IDs to attribute
  -- IDs.
  , nameMap :: IntMap.IntMap Str.Str
  }
  deriving (Eq, Show)

type Bimap l r = (Map.Map l r, Map.Map r l)

bimap :: (Ord l, Ord r) => [(l, r)] -> Bimap l r
bimap xs = (Map.fromList xs, Map.fromList (fmap Tuple.swap xs))

lookupL :: Ord l => l -> Bimap l r -> Maybe r
lookupL k = Map.lookup k . fst

lookupR :: Ord r => r -> Bimap l r -> Maybe l
lookupR k = Map.lookup k . snd

-- | Makes a 'ClassAttributeMap' given the necessary fields from the
-- 'Rattletrap.Content.Content'.
make
  :: List.List Str.Str
  -- ^ From 'Rattletrap.Content.objects'.
  -> List.List ClassMapping.ClassMapping
  -- ^ From 'Rattletrap.Content.classMappings'.
  -> List.List Cache.Cache
  -- ^ From 'Rattletrap.Content.caches'.
  -> List.List Str.Str
  -- ^ From 'Rattletrap.Content.names'.
  -> ClassAttributeMap
make objects classMappings caches names =
  let
    objectMap_ = makeObjectMap objects
    classMap = makeClassMap classMappings
    objectClassMap_ = makeObjectClassMap objectMap_ classMap
    classCache = makeClassCache classMap caches
    attributeMap = makeAttributeMap caches
    classIds = fmap (\(_, classId, _, _) -> classId) classCache
    parentMap = makeParentMap classCache
    value_ = Map.fromList
      (fmap
        (\classId ->
          let
            ownAttributes =
              Maybe.fromMaybe Map.empty (Map.lookup classId attributeMap)
            parentsAttributes = case Map.lookup classId parentMap of
              Nothing -> []
              Just parentClassIds -> fmap
                (\parentClassId -> Maybe.fromMaybe
                  Map.empty
                  (Map.lookup parentClassId attributeMap)
                )
                parentClassIds
            attributes = ownAttributes : parentsAttributes
          in (classId, Map.fromList (concatMap Map.toList attributes))
        )
        classIds
      )
    nameMap_ = makeNameMap names
  in ClassAttributeMap objectMap_ objectClassMap_ value_ nameMap_

makeNameMap :: List.List Str.Str -> IntMap.IntMap Str.Str
makeNameMap names =
  IntMap.fromDistinctAscList (zip [0 ..] (List.toList names))

getName :: IntMap.IntMap Str.Str -> U32.U32 -> Maybe Str.Str
getName nameMap_ nameIndex =
  IntMap.lookup (fromIntegral (U32.toWord32 nameIndex)) nameMap_

makeObjectClassMap
  :: Map.Map U32.U32 Str.Str
  -> Bimap U32.U32 Str.Str
  -> Map.Map U32.U32 U32.U32
makeObjectClassMap objectMap_ classMap = do
  let objectIds = Map.keys objectMap_
  let classIds = fmap (getClassId objectMap_ classMap) objectIds
  let rawPairs = zip objectIds classIds
  let
    pairs = Maybe.mapMaybe
      (\(objectId, maybeClassId) -> case maybeClassId of
        Nothing -> Nothing
        Just classId -> Just (objectId, classId)
      )
      rawPairs
  Map.fromList pairs

getClassId
  :: Map.Map U32.U32 Str.Str
  -> Bimap U32.U32 Str.Str
  -> U32.U32
  -> Maybe U32.U32
getClassId objectMap_ classMap objectId = do
  objectName <- getObjectName objectMap_ objectId
  className <- getClassName objectName
  lookupR className classMap

makeClassCache
  :: Bimap U32.U32 Str.Str
  -> List.List Cache.Cache
  -> [(Maybe Str.Str, U32.U32, U32.U32, U32.U32)]
makeClassCache classMap caches = fmap
  (\cache ->
    let classId = Cache.classId cache
    in
      ( lookupL classId classMap
      , classId
      , Cache.cacheId cache
      , Cache.parentCacheId cache
      )
  )
  (List.toList caches)

makeClassMap :: List.List ClassMapping.ClassMapping -> Bimap U32.U32 Str.Str
makeClassMap classMappings = bimap
  (fmap
    (\classMapping ->
      (ClassMapping.streamId classMapping, ClassMapping.name classMapping)
    )
    (List.toList classMappings)
  )

makeAttributeMap
  :: List.List Cache.Cache -> Map.Map U32.U32 (Map.Map U32.U32 U32.U32)
makeAttributeMap caches = Map.fromList
  (fmap
    (\cache ->
      ( Cache.classId cache
      , Map.fromList
        (fmap
          (\attributeMapping ->
            ( AttributeMapping.streamId attributeMapping
            , AttributeMapping.objectId attributeMapping
            )
          )
          (List.toList (Cache.attributeMappings cache))
        )
      )
    )
    (List.toList caches)
  )

makeShallowParentMap
  :: [(Maybe Str.Str, U32.U32, U32.U32, U32.U32)] -> Map.Map U32.U32 U32.U32
makeShallowParentMap classCache = Map.fromList
  (Maybe.mapMaybe
    (\xs -> case xs of
      [] -> Nothing
      (maybeClassName, classId, _, parentCacheId) : rest -> do
        parentClassId <- getParentClass maybeClassName parentCacheId rest
        pure (classId, parentClassId)
    )
    (List.tails (reverse classCache))
  )

makeParentMap
  :: [(Maybe Str.Str, U32.U32, U32.U32, U32.U32)] -> Map.Map U32.U32 [U32.U32]
makeParentMap classCache =
  let shallowParentMap = makeShallowParentMap classCache
  in
    Map.mapWithKey
      (\classId _ -> getParentClasses shallowParentMap classId)
      shallowParentMap

getParentClasses :: Map.Map U32.U32 U32.U32 -> U32.U32 -> [U32.U32]
getParentClasses shallowParentMap classId =
  case Map.lookup classId shallowParentMap of
    Nothing -> []
    Just parentClassId ->
      parentClassId : getParentClasses shallowParentMap parentClassId

getParentClass
  :: Maybe Str.Str
  -> U32.U32
  -> [(Maybe Str.Str, U32.U32, U32.U32, U32.U32)]
  -> Maybe U32.U32
getParentClass maybeClassName parentCacheId xs = case maybeClassName of
  Nothing -> getParentClassById parentCacheId xs
  Just className -> getParentClassByName className parentCacheId xs

getParentClassById
  :: U32.U32 -> [(Maybe Str.Str, U32.U32, U32.U32, U32.U32)] -> Maybe U32.U32
getParentClassById parentCacheId xs =
  case dropWhile (\(_, _, cacheId, _) -> cacheId /= parentCacheId) xs of
    [] -> if parentCacheId == U32.fromWord32 0
      then Nothing
      else getParentClassById
        (U32.fromWord32 (U32.toWord32 parentCacheId - 1))
        xs
    (_, parentClassId, _, _) : _ -> Just parentClassId

getParentClassByName
  :: Str.Str
  -> U32.U32
  -> [(Maybe Str.Str, U32.U32, U32.U32, U32.U32)]
  -> Maybe U32.U32
getParentClassByName className parentCacheId xs =
  case Map.lookup (Str.toText className) Data.parentClasses of
    Nothing -> getParentClassById parentCacheId xs
    Just parentClassName -> Maybe.maybe
      (getParentClassById parentCacheId xs)
      Just
      (Maybe.listToMaybe
        (fmap
          (\(_, parentClassId, _, _) -> parentClassId)
          (filter
            (\(_, _, cacheId, _) -> cacheId <= parentCacheId)
            (filter
              (\(maybeClassName, _, _, _) ->
                fmap Str.toText maybeClassName == Just parentClassName
              )
              xs
            )
          )
        )
      )

makeObjectMap :: List.List Str.Str -> Map.Map U32.U32 Str.Str
makeObjectMap objects =
  Map.fromAscList (zip (fmap U32.fromWord32 [0 ..]) (List.toList objects))

getObjectName :: Map.Map U32.U32 Str.Str -> U32.U32 -> Maybe Str.Str
getObjectName objectMap_ objectId = Map.lookup objectId objectMap_

getClassName :: Str.Str -> Maybe Str.Str
getClassName rawObjectName =
  Str.fromText
    <$> Map.lookup
          (Str.toText $ normalizeObjectName rawObjectName)
          Data.objectClasses

normalizeObjectName :: Str.Str -> Str.Str
normalizeObjectName objectName =
  let
    name = Str.toText objectName
    crowdActor = Text.pack "TheWorld:PersistentLevel.CrowdActor_TA"
    crowdManager = Text.pack "TheWorld:PersistentLevel.CrowdManager_TA"
    boostPickup = Text.pack "TheWorld:PersistentLevel.VehiclePickup_Boost_TA"
    mapScoreboard = Text.pack "TheWorld:PersistentLevel.InMapScoreboard_TA"
    breakout = Text.pack "TheWorld:PersistentLevel.BreakOutActor_Platform_TA"
  in if Text.isInfixOf crowdActor name
    then Str.fromText crowdActor
    else if Text.isInfixOf crowdManager name
      then Str.fromText crowdManager
      else if Text.isInfixOf boostPickup name
        then Str.fromText boostPickup
        else if Text.isInfixOf mapScoreboard name
          then Str.fromText mapScoreboard
          else if Text.isInfixOf breakout name
            then Str.fromText breakout
            else objectName

classHasLocation :: Str.Str -> Bool
classHasLocation className =
  Set.member (Str.toText className) Data.classesWithLocation

classHasRotation :: Str.Str -> Bool
classHasRotation className =
  Set.member (Str.toText className) Data.classesWithRotation

getAttributeIdLimit :: Map.Map U32.U32 U32.U32 -> Maybe Word
getAttributeIdLimit attributeMap = do
  ((streamId, _), _) <- Map.maxViewWithKey attributeMap
  pure (fromIntegral (U32.toWord32 streamId))

getAttributeName
  :: ClassAttributeMap
  -> Map.Map U32.U32 U32.U32
  -> CompressedWord.CompressedWord
  -> Maybe Str.Str
getAttributeName classAttributeMap attributeMap streamId = do
  let key = U32.fromWord32 (fromIntegral (CompressedWord.value streamId))
  attributeId <- Map.lookup key attributeMap
  let objectMap_ = objectMap classAttributeMap
  Map.lookup attributeId objectMap_

getAttributeMap
  :: ClassAttributeMap
  -> Map.Map CompressedWord.CompressedWord U32.U32
  -> CompressedWord.CompressedWord
  -> Maybe (Map.Map U32.U32 U32.U32)
getAttributeMap classAttributeMap actorMap actorId = do
  objectId <- Map.lookup actorId actorMap
  let objectClassMap_ = objectClassMap classAttributeMap
  classId <- Map.lookup objectId objectClassMap_
  let value_ = value classAttributeMap
  Map.lookup classId value_
