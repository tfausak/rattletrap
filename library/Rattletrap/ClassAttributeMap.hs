module Rattletrap.ClassAttributeMap where

import Rattletrap.ActorMap
import Rattletrap.AttributeMapping
import Rattletrap.Cache
import Rattletrap.ClassMapping
import Rattletrap.Data
import Rattletrap.Primitive

import qualified Data.Bimap as Bimap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text

-- | This data structure holds all the information about classes, objects, and
-- attributes in the replay. The class hierarchy is not fixed; it is encoded
-- in the 'Rattletrap.Content.Content'. Similarly, the attributes that belong
-- to each class are not fixed either. Converting the raw data into a usable
-- structure is tedious; see 'makeClassAttributeMap'.
data ClassAttributeMap = ClassAttributeMap
  { classAttributeMapObjectMap :: Map.Map Word32 Text
  -- ^ A map from object IDs to their names.
  , classAttributeMapObjectClassMap :: Map.Map Word32 Word32
  -- ^ A map from object IDs to their class IDs.
  , classAttributeMapValue :: Map.Map Word32 (Map.Map Word32 Word32)
  -- ^ A map from class IDs to a map from attribute stream IDs to attribute
  -- IDs.
  , classAttributeMapNameMap :: IntMap.IntMap Text
  } deriving (Eq, Ord, Show)

-- | Makes a 'ClassAttributeMap' given the necessary fields from the
-- 'Rattletrap.Content.Content'.
makeClassAttributeMap ::
     List Text
  -- ^ From 'Rattletrap.Content.contentObjects'.
  -> List ClassMapping
  -- ^ From 'Rattletrap.Content.contentClassMappings'.
  -> List Cache
  -- ^ From 'Rattletrap.Content.contentCaches'.
  -> List Text
  -- ^ From 'Rattletrap.Content.contentNames'.
  -> ClassAttributeMap
makeClassAttributeMap objects classMappings caches names =
  let objectMap = makeObjectMap objects
      classMap = makeClassMap classMappings
      objectClassMap = makeObjectClassMap objectMap classMap
      classCache = makeClassCache classMap caches
      attributeMap = makeAttributeMap caches
      classIds = map (\(_, classId, _, _) -> classId) classCache
      parentMap = makeParentMap classCache
      value =
        Map.fromList
          (map
             (\classId ->
                let ownAttributes =
                      Maybe.fromMaybe
                        Map.empty
                        (Map.lookup classId attributeMap)
                    parentsAttributes =
                      case Map.lookup classId parentMap of
                        Nothing -> []
                        Just parentClassIds ->
                          map
                            (\parentClassId ->
                               Maybe.fromMaybe
                                 Map.empty
                                 (Map.lookup parentClassId attributeMap))
                            parentClassIds
                    attributes = ownAttributes : parentsAttributes
                in (classId, Map.fromList (concatMap Map.toList attributes)))
             classIds)
      nameMap = makeNameMap names
  in ClassAttributeMap objectMap objectClassMap value nameMap

makeNameMap :: List Text -> IntMap.IntMap Text
makeNameMap names = IntMap.fromDistinctAscList (zip [0 ..] (listValue names))

getName :: IntMap.IntMap Text -> Word32 -> Maybe Text
getName nameMap nameIndex =
  IntMap.lookup (fromIntegral (word32Value nameIndex)) nameMap

makeObjectClassMap ::
     Map.Map Word32 Text -> Bimap.Bimap Word32 Text -> Map.Map Word32 Word32
makeObjectClassMap objectMap classMap = do
  let objectIds = Map.keys objectMap
  let classIds = map (getClassId objectMap classMap) objectIds
  let rawPairs = zip objectIds classIds
  let pairs =
        Maybe.mapMaybe
          (\(objectId, maybeClassId) ->
             case maybeClassId of
               Nothing -> Nothing
               Just classId -> Just (objectId, classId))
          rawPairs
  Map.fromList pairs

getClassId ::
     Map.Map Word32 Text -> Bimap.Bimap Word32 Text -> Word32 -> Maybe Word32
getClassId objectMap classMap objectId = do
  objectName <- getObjectName objectMap objectId
  className <- getClassName objectName
  Bimap.lookupR className classMap

makeClassCache ::
     Bimap.Bimap Word32 Text
  -> List Cache
  -> [(Maybe Text, Word32, Word32, Word32)]
makeClassCache classMap caches =
  map
    (\cache ->
       let classId = cacheClassId cache
       in ( Bimap.lookup classId classMap
          , classId
          , cacheCacheId cache
          , cacheParentCacheId cache))
    (listValue caches)

makeClassMap :: List ClassMapping -> Bimap.Bimap Word32 Text
makeClassMap classMappings =
  Bimap.fromList
    (map
       (\classMapping ->
          (classMappingStreamId classMapping, classMappingName classMapping))
       (listValue classMappings))

makeAttributeMap :: List Cache -> Map.Map Word32 (Map.Map Word32 Word32)
makeAttributeMap caches =
  Map.fromList
    (map
       (\cache ->
          ( cacheClassId cache
          , Map.fromList
              (map
                 (\attributeMapping ->
                    ( attributeMappingStreamId attributeMapping
                    , attributeMappingObjectId attributeMapping))
                 (listValue (cacheAttributeMappings cache)))))
       (listValue caches))

makeShallowParentMap ::
     [(Maybe Text, Word32, Word32, Word32)] -> Map.Map Word32 Word32
makeShallowParentMap classCache =
  Map.fromList
    (Maybe.mapMaybe
       (\xs ->
          case xs of
            [] -> Nothing
            (maybeClassName, classId, _, parentCacheId):rest -> do
              parentClassId <- getParentClass maybeClassName parentCacheId rest
              pure (classId, parentClassId))
       (List.tails (reverse classCache)))

makeParentMap ::
     [(Maybe Text, Word32, Word32, Word32)] -> Map.Map Word32 [Word32]
makeParentMap classCache =
  let shallowParentMap = makeShallowParentMap classCache
  in Map.mapWithKey
       (\classId _ -> getParentClasses shallowParentMap classId)
       shallowParentMap

getParentClasses :: Map.Map Word32 Word32 -> Word32 -> [Word32]
getParentClasses shallowParentMap classId =
  case Map.lookup classId shallowParentMap of
    Nothing -> []
    Just parentClassId ->
      parentClassId : getParentClasses shallowParentMap parentClassId

getParentClass ::
     Maybe Text
  -> Word32
  -> [(Maybe Text, Word32, Word32, Word32)]
  -> Maybe Word32
getParentClass maybeClassName parentCacheId xs =
  case maybeClassName of
    Nothing -> getParentClassById parentCacheId xs
    Just className -> getParentClassByName className parentCacheId xs

getParentClassById ::
     Word32 -> [(Maybe Text, Word32, Word32, Word32)] -> Maybe Word32
getParentClassById parentCacheId xs =
  case dropWhile (\(_, _, cacheId, _) -> cacheId /= parentCacheId) xs of
    [] ->
      if parentCacheId == Word32 0
        then Nothing
        else getParentClassById (Word32 (word32Value parentCacheId - 1)) xs
    (_, parentClassId, _, _):_ -> Just parentClassId

getParentClassByName ::
     Text -> Word32 -> [(Maybe Text, Word32, Word32, Word32)] -> Maybe Word32
getParentClassByName className parentCacheId xs =
  case Map.lookup className parentClasses of
    Nothing -> getParentClassById parentCacheId xs
    Just parentClassName ->
      Maybe.maybe
        (getParentClassById parentCacheId xs)
        Just
        (Maybe.listToMaybe
           (map
              (\(_, parentClassId, _, _) -> parentClassId)
              (filter
                 (\(_, _, cacheId, _) -> cacheId <= parentCacheId)
                 (filter
                    (\(maybeClassName, _, _, _) ->
                       maybeClassName == Just parentClassName)
                    xs))))

parentClasses :: Map.Map Text Text
parentClasses =
  Map.map
    stringToText
    (Map.mapKeys stringToText (Map.fromList rawParentClasses))

makeObjectMap :: List Text -> Map.Map Word32 Text
makeObjectMap objects =
  Map.fromAscList (zip (map Word32 [0 ..]) (listValue objects))

getObjectName :: Map.Map Word32 Text -> Word32 -> Maybe Text
getObjectName objectMap objectId = Map.lookup objectId objectMap

getClassName :: Text -> Maybe Text
getClassName rawObjectName =
  Map.lookup (normalizeObjectName rawObjectName) objectClasses

normalizeObjectName :: Text -> Text
normalizeObjectName objectName =
  let name = textValue objectName
      crowdActor = Text.pack "TheWorld:PersistentLevel.CrowdActor_TA"
      crowdManager = Text.pack "TheWorld:PersistentLevel.CrowdManager_TA"
      boostPickup = Text.pack "TheWorld:PersistentLevel.VehiclePickup_Boost_TA"
      mapScoreboard = Text.pack "TheWorld:PersistentLevel.InMapScoreboard_TA"
      breakout = Text.pack "TheWorld:PersistentLevel.BreakOutActor_Platform_TA"
  in if Text.isInfixOf crowdActor name
       then Text crowdActor
       else if Text.isInfixOf crowdManager name
              then Text crowdManager
              else if Text.isInfixOf boostPickup name
                     then Text boostPickup
                     else if Text.isInfixOf mapScoreboard name
                            then Text mapScoreboard
                            else if Text.isInfixOf breakout name
                                   then Text breakout
                                   else objectName

objectClasses :: Map.Map Text Text
objectClasses =
  Map.map
    stringToText
    (Map.mapKeys stringToText (Map.fromList rawObjectClasses))

classHasLocation :: Text -> Bool
classHasLocation className = Set.member className classesWithLocation

classesWithLocation :: Set.Set Text
classesWithLocation = Set.fromList (map stringToText rawClassesWithLocation)

classHasRotation :: Text -> Bool
classHasRotation className = Set.member className classesWithRotation

classesWithRotation :: Set.Set Text
classesWithRotation = Set.fromList (map stringToText rawClassesWithRotation)

getAttributeIdLimit :: Map.Map Word32 Word32 -> Maybe Word
getAttributeIdLimit attributeMap = do
  ((streamId, _), _) <- Map.maxViewWithKey attributeMap
  let limit = fromIntegral (word32Value streamId)
  pure limit

getAttributeName ::
     ClassAttributeMap -> Map.Map Word32 Word32 -> CompressedWord -> Maybe Text
getAttributeName classAttributeMap attributeMap streamId = do
  let key = Word32 (fromIntegral (compressedWordValue streamId))
  attributeId <- Map.lookup key attributeMap
  let objectMap = classAttributeMapObjectMap classAttributeMap
  Map.lookup attributeId objectMap

getAttributeMap ::
     ClassAttributeMap
  -> ActorMap
  -> CompressedWord
  -> Maybe (Map.Map Word32 Word32)
getAttributeMap classAttributeMap actorMap actorId = do
  objectId <- Map.lookup actorId actorMap
  let objectClassMap = classAttributeMapObjectClassMap classAttributeMap
  classId <- Map.lookup objectId objectClassMap
  let value = classAttributeMapValue classAttributeMap
  Map.lookup classId value
