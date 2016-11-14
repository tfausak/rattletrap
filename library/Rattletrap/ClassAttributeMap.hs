module Rattletrap.ClassAttributeMap where

import Rattletrap.ActorMap
import Rattletrap.AttributeMapping
import Rattletrap.Cache
import Rattletrap.ClassMapping
import Rattletrap.Data
import Rattletrap.Primitive

import qualified Data.Bimap as Bimap
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text

data ClassAttributeMap = ClassAttributeMap
  { classAttributeMapObjectMap :: Map.Map Word32 Text
  , classAttributeMapObjectClassMap :: Map.Map Word32 Word32
  , classAttributeMapValue :: Map.Map Word32 (Map.Map Word32 Word32)
  } deriving (Eq, Show)

makeClassAttributeMap :: List Text
                      -> List ClassMapping
                      -> List Cache
                      -> ClassAttributeMap
makeClassAttributeMap objects classMappings caches =
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
  in ClassAttributeMap objectMap objectClassMap value

makeObjectClassMap :: Map.Map Word32 Text
                   -> Bimap.Bimap Word32 Text
                   -> Map.Map Word32 Word32
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

getClassId :: Map.Map Word32 Text
           -> Bimap.Bimap Word32 Text
           -> Word32
           -> Maybe Word32
getClassId objectMap classMap objectId = do
  objectName <- getObjectName objectMap objectId
  className <- getClassName objectName
  Bimap.lookupR className classMap

makeClassCache :: Bimap.Bimap Word32 Text
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

makeShallowParentMap :: [(Maybe Text, Word32, Word32, Word32)]
                     -> Map.Map Word32 Word32
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

makeParentMap :: [(Maybe Text, Word32, Word32, Word32)]
              -> Map.Map Word32 [Word32]
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

getParentClass :: Maybe Text
               -> Word32
               -> [(Maybe Text, Word32, Word32, Word32)]
               -> Maybe Word32
getParentClass maybeClassName parentCacheId xs =
  case maybeClassName of
    Nothing -> getParentClassById parentCacheId xs
    Just className -> getParentClassByName className parentCacheId xs

getParentClassById :: Word32
                   -> [(Maybe Text, Word32, Word32, Word32)]
                   -> Maybe Word32
getParentClassById parentCacheId xs =
  case dropWhile (\(_, _, cacheId, _) -> cacheId /= parentCacheId) xs of
    [] ->
      if parentCacheId == Word32 0
        then Nothing
        else getParentClassById (Word32 (word32Value parentCacheId - 1)) xs
    (_, parentClassId, _, _):_ -> Just parentClassId

getParentClassByName :: Text
                     -> Word32
                     -> [(Maybe Text, Word32, Word32, Word32)]
                     -> Maybe Word32
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
                 (\(_, _, cacheId, _) -> cacheId == parentCacheId)
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
      toText text =
        Text
          (Int32 (fromIntegral (Text.length text + 1)))
          (Text.snoc text '\x00')
  in if Text.isInfixOf crowdActor name
       then toText crowdActor
       else if Text.isInfixOf crowdManager name
              then toText crowdManager
              else if Text.isInfixOf boostPickup name
                     then toText boostPickup
                     else if Text.isInfixOf mapScoreboard name
                            then toText mapScoreboard
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

getAttributeName :: ClassAttributeMap
                 -> Map.Map Word32 Word32
                 -> CompressedWord
                 -> Maybe Text
getAttributeName classAttributeMap attributeMap streamId = do
  let key = Word32 (fromIntegral (compressedWordValue streamId))
  attributeId <- Map.lookup key attributeMap
  let objectMap = classAttributeMapObjectMap classAttributeMap
  Map.lookup attributeId objectMap

getAttributeMap
  :: ClassAttributeMap
  -> ActorMap
  -> CompressedWord
  -> Maybe (Map.Map Word32 Word32)
getAttributeMap classAttributeMap actorMap actorId = do
  objectId <- Map.lookup actorId actorMap
  let objectClassMap = classAttributeMapObjectClassMap classAttributeMap
  classId <- Map.lookup objectId objectClassMap
  let value = classAttributeMapValue classAttributeMap
  Map.lookup classId value
