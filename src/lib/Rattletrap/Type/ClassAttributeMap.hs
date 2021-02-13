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
  , makeClassAttributeMap
  ) where

import qualified Rattletrap.Data as Data
import Rattletrap.Type.AttributeMapping
import Rattletrap.Type.Cache
import Rattletrap.Type.ClassMapping
import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.List
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.Word32le as Word32le

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
-- structure is tedious; see 'makeClassAttributeMap'.
data ClassAttributeMap = ClassAttributeMap
  { classAttributeMapObjectMap :: Map Word32le.Word32le Str.Str
  -- ^ A map from object IDs to their names.
  , classAttributeMapObjectClassMap :: Map Word32le.Word32le Word32le.Word32le
  -- ^ A map from object IDs to their class IDs.
  , classAttributeMapValue :: Map Word32le.Word32le (Map Word32le.Word32le Word32le.Word32le)
  -- ^ A map from class IDs to a map from attribute stream IDs to attribute
  -- IDs.
  , classAttributeMapNameMap :: IntMap.IntMap Str.Str
  }
  deriving (Eq, Show)

type Bimap l r = (Map l r, Map r l)

bimap :: (Ord l, Ord r) => [(l, r)] -> Bimap l r
bimap xs = (Map.fromList xs, Map.fromList (fmap Tuple.swap xs))

lookupL :: Ord l => l -> Bimap l r -> Maybe r
lookupL k = Map.lookup k . fst

lookupR :: Ord r => r -> Bimap l r -> Maybe l
lookupR k = Map.lookup k . snd

-- | Makes a 'ClassAttributeMap' given the necessary fields from the
-- 'Rattletrap.Content.Content'.
makeClassAttributeMap
  :: List Str.Str
  -- ^ From 'Rattletrap.Content.contentObjects'.
  -> List ClassMapping
  -- ^ From 'Rattletrap.Content.contentClassMappings'.
  -> List Cache
  -- ^ From 'Rattletrap.Content.contentCaches'.
  -> List Str.Str
  -- ^ From 'Rattletrap.Content.contentNames'.
  -> ClassAttributeMap
makeClassAttributeMap objects classMappings caches names =
  let
    objectMap = makeObjectMap objects
    classMap = makeClassMap classMappings
    objectClassMap = makeObjectClassMap objectMap classMap
    classCache = makeClassCache classMap caches
    attributeMap = makeAttributeMap caches
    classIds = fmap (\(_, classId, _, _) -> classId) classCache
    parentMap = makeParentMap classCache
    value = Map.fromList
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
    nameMap = makeNameMap names
  in ClassAttributeMap objectMap objectClassMap value nameMap

makeNameMap :: List Str.Str -> IntMap.IntMap Str.Str
makeNameMap names = IntMap.fromDistinctAscList (zip [0 ..] (listValue names))

getName :: IntMap.IntMap Str.Str -> Word32le.Word32le -> Maybe Str.Str
getName nameMap nameIndex =
  IntMap.lookup (fromIntegral (Word32le.toWord32 nameIndex)) nameMap

makeObjectClassMap
  :: Map Word32le.Word32le Str.Str -> Bimap Word32le.Word32le Str.Str -> Map Word32le.Word32le Word32le.Word32le
makeObjectClassMap objectMap classMap = do
  let objectIds = Map.keys objectMap
  let classIds = fmap (getClassId objectMap classMap) objectIds
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
  :: Map Word32le.Word32le Str.Str -> Bimap Word32le.Word32le Str.Str -> Word32le.Word32le -> Maybe Word32le.Word32le
getClassId objectMap classMap objectId = do
  objectName <- getObjectName objectMap objectId
  className <- getClassName objectName
  lookupR className classMap

makeClassCache
  :: Bimap Word32le.Word32le Str.Str
  -> List Cache
  -> [(Maybe Str.Str, Word32le.Word32le, Word32le.Word32le, Word32le.Word32le)]
makeClassCache classMap caches = fmap
  (\cache ->
    let classId = cacheClassId cache
    in
      ( lookupL classId classMap
      , classId
      , cacheCacheId cache
      , cacheParentCacheId cache
      )
  )
  (listValue caches)

makeClassMap :: List ClassMapping -> Bimap Word32le.Word32le Str.Str
makeClassMap classMappings = bimap
  (fmap
    (\classMapping ->
      (classMappingStreamId classMapping, classMappingName classMapping)
    )
    (listValue classMappings)
  )

makeAttributeMap :: List Cache -> Map Word32le.Word32le (Map Word32le.Word32le Word32le.Word32le)
makeAttributeMap caches = Map.fromList
  (fmap
    (\cache ->
      ( cacheClassId cache
      , Map.fromList
        (fmap
          (\attributeMapping ->
            ( attributeMappingStreamId attributeMapping
            , attributeMappingObjectId attributeMapping
            )
          )
          (listValue (cacheAttributeMappings cache))
        )
      )
    )
    (listValue caches)
  )

makeShallowParentMap
  :: [(Maybe Str.Str, Word32le.Word32le, Word32le.Word32le, Word32le.Word32le)] -> Map Word32le.Word32le Word32le.Word32le
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
  :: [(Maybe Str.Str, Word32le.Word32le, Word32le.Word32le, Word32le.Word32le)] -> Map Word32le.Word32le [Word32le.Word32le]
makeParentMap classCache =
  let shallowParentMap = makeShallowParentMap classCache
  in
    Map.mapWithKey
      (\classId _ -> getParentClasses shallowParentMap classId)
      shallowParentMap

getParentClasses :: Map Word32le.Word32le Word32le.Word32le -> Word32le.Word32le -> [Word32le.Word32le]
getParentClasses shallowParentMap classId =
  case Map.lookup classId shallowParentMap of
    Nothing -> []
    Just parentClassId ->
      parentClassId : getParentClasses shallowParentMap parentClassId

getParentClass
  :: Maybe Str.Str
  -> Word32le.Word32le
  -> [(Maybe Str.Str, Word32le.Word32le, Word32le.Word32le, Word32le.Word32le)]
  -> Maybe Word32le.Word32le
getParentClass maybeClassName parentCacheId xs = case maybeClassName of
  Nothing -> getParentClassById parentCacheId xs
  Just className -> getParentClassByName className parentCacheId xs

getParentClassById
  :: Word32le.Word32le -> [(Maybe Str.Str, Word32le.Word32le, Word32le.Word32le, Word32le.Word32le)] -> Maybe Word32le.Word32le
getParentClassById parentCacheId xs =
  case dropWhile (\(_, _, cacheId, _) -> cacheId /= parentCacheId) xs of
    [] -> if parentCacheId == Word32le.fromWord32 0
      then Nothing
      else getParentClassById (Word32le.fromWord32 (Word32le.toWord32 parentCacheId - 1)) xs
    (_, parentClassId, _, _) : _ -> Just parentClassId

getParentClassByName
  :: Str.Str
  -> Word32le.Word32le
  -> [(Maybe Str.Str, Word32le.Word32le, Word32le.Word32le, Word32le.Word32le)]
  -> Maybe Word32le.Word32le
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

makeObjectMap :: List Str.Str -> Map Word32le.Word32le Str.Str
makeObjectMap objects =
  Map.fromAscList (zip (fmap Word32le.fromWord32 [0 ..]) (listValue objects))

getObjectName :: Map Word32le.Word32le Str.Str -> Word32le.Word32le -> Maybe Str.Str
getObjectName objectMap objectId = Map.lookup objectId objectMap

getClassName :: Str.Str -> Maybe Str.Str
getClassName rawObjectName =
  Str.fromText <$> Map.lookup (Str.toText $ normalizeObjectName rawObjectName) Data.objectClasses

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
classHasLocation className = Set.member (Str.toText className) Data.classesWithLocation

classHasRotation :: Str.Str -> Bool
classHasRotation className = Set.member (Str.toText className) Data.classesWithRotation

getAttributeIdLimit :: Map Word32le.Word32le Word32le.Word32le -> Maybe Word
getAttributeIdLimit attributeMap = do
  ((streamId, _), _) <- Map.maxViewWithKey attributeMap
  pure (fromIntegral (Word32le.toWord32 streamId))

getAttributeName
  :: ClassAttributeMap -> Map Word32le.Word32le Word32le.Word32le -> CompressedWord -> Maybe Str.Str
getAttributeName classAttributeMap attributeMap streamId = do
  let key = Word32le.fromWord32 (fromIntegral (compressedWordValue streamId))
  attributeId <- Map.lookup key attributeMap
  let objectMap = classAttributeMapObjectMap classAttributeMap
  Map.lookup attributeId objectMap

getAttributeMap
  :: ClassAttributeMap
  -> Map CompressedWord Word32le.Word32le
  -> CompressedWord
  -> Maybe (Map Word32le.Word32le Word32le.Word32le)
getAttributeMap classAttributeMap actorMap actorId = do
  objectId <- Map.lookup actorId actorMap
  let objectClassMap = classAttributeMapObjectClassMap classAttributeMap
  classId <- Map.lookup objectId objectClassMap
  let value = classAttributeMapValue classAttributeMap
  Map.lookup classId value
