module Rattletrap.Map.ClassAttribute where

import Rattletrap.AttributeMapping
import Rattletrap.Cache
import Rattletrap.ClassMapping
import Rattletrap.Data
import Rattletrap.Map.Actor
import Rattletrap.Map.Attribute
import Rattletrap.Map.Class
import Rattletrap.Map.Name
import Rattletrap.Map.Object
import Rattletrap.Map.ObjectClass
import Rattletrap.Map.Stream
import Rattletrap.Primitive

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.Word as Word

-- | This data structure holds all the information about classes, objects, and
-- attributes in the replay. The class hierarchy is not fixed; it is encoded
-- in the 'Rattletrap.Content.Content'. Similarly, the attributes that belong
-- to each class are not fixed either. Converting the raw data into a usable
-- structure is tedious; see 'makeClassAttributeMap'.
data ClassAttributeMap = ClassAttributeMap
  { classAttributeMapObjectMap :: ObjectMap
  -- ^ A map from object IDs to their names.
  , classAttributeMapObjectClassMap :: ObjectClassMap
  -- ^ A map from object IDs to their class IDs.
  , classAttributeMapStreamMap :: StreamMap
  -- ^ A map from class IDs to a map from attribute stream IDs to attribute
  -- IDs.
  , classAttributeMapNameMap :: NameMap
  } deriving (Eq, Show)

-- | Makes a 'ClassAttributeMap' given the necessary fields from the
-- 'Rattletrap.Content.Content'.
makeClassAttributeMap
  :: List Text
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
      streamMap = makeStreamMap attributeMap parentMap classIds
      nameMap = makeNameMap names
  in ClassAttributeMap objectMap objectClassMap streamMap nameMap

makeClassCache :: ClassMap
               -> List Cache
               -> [(Maybe Text, Word32, Word32, Word32)]
makeClassCache classMap caches =
  map
    (\cache ->
       let classId = cacheClassId cache
       in ( classMapLookup classId classMap
          , classId
          , cacheCacheId cache
          , cacheParentCacheId cache))
    (Vector.toList (listValue caches))

makeAttributeMap :: List Cache
                 -> HashMap.HashMap Word.Word32 (HashMap.HashMap Word.Word32 Word32)
makeAttributeMap caches =
  let getInnerKey x = word32Value (attributeMappingStreamId x)
      getInnerValue = attributeMappingObjectId
      getOuterKey x = word32Value (cacheClassId x)
      getOuterValue x =
        Vector.foldr
          (\y -> HashMap.insert (getInnerKey y) (getInnerValue y))
          HashMap.empty
          (listValue (cacheAttributeMappings x))
  in Vector.foldr
       (\x -> HashMap.insert (getOuterKey x) (getOuterValue x))
       HashMap.empty
       (listValue caches)

makeShallowParentMap :: [(Maybe Text, Word32, Word32, Word32)]
                     -> HashMap.HashMap Word.Word32 Word32
makeShallowParentMap classCache =
  HashMap.fromList
    (Maybe.mapMaybe
       (\xs ->
          case xs of
            [] -> Nothing
            (maybeClassName, classId, _, parentCacheId):rest -> do
              parentClassId <- getParentClass maybeClassName parentCacheId rest
              pure (word32Value classId, parentClassId))
       (List.tails (reverse classCache)))

makeParentMap :: [(Maybe Text, Word32, Word32, Word32)]
              -> HashMap.HashMap Word.Word32 [Word32]
makeParentMap classCache =
  let shallowParentMap = makeShallowParentMap classCache
  in HashMap.mapWithKey
       (\classId _ -> getParentClasses shallowParentMap classId)
       shallowParentMap

getParentClasses :: HashMap.HashMap Word.Word32 Word32
                 -> Word.Word32
                 -> [Word32]
getParentClasses shallowParentMap classId =
  case HashMap.lookup classId shallowParentMap of
    Nothing -> []
    Just parentClassId ->
      parentClassId :
      getParentClasses shallowParentMap (word32Value parentClassId)

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
  case HashMap.lookup (textValue className) parentClasses of
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

parentClasses :: HashMap.HashMap Text.Text Text
parentClasses =
  HashMap.fromList
    (map (\(k, v) -> (Text.pack k, stringToText v)) rawParentClasses)

classHasLocation :: Text -> Bool
classHasLocation className =
  HashSet.member (textValue className) classesWithLocation

classesWithLocation :: HashSet.HashSet Text.Text
classesWithLocation = HashSet.fromList (map Text.pack rawClassesWithLocation)

classHasRotation :: Text -> Bool
classHasRotation className =
  HashSet.member (textValue className) classesWithRotation

classesWithRotation :: HashSet.HashSet Text.Text
classesWithRotation = HashSet.fromList (map Text.pack rawClassesWithRotation)

getAttributeName :: ClassAttributeMap
                 -> AttributeMap
                 -> CompressedWord
                 -> Maybe Text
getAttributeName classAttributeMap attributeMap streamId = do
  attributeId <- attributeMapLookup streamId attributeMap
  let objectMap = classAttributeMapObjectMap classAttributeMap
  objectMapLookup attributeId objectMap

getAttributeMap :: ClassAttributeMap
                -> ActorMap
                -> CompressedWord
                -> Maybe AttributeMap
getAttributeMap classAttributeMap actorMap actorId = do
  objectId <- actorMapLookup actorId actorMap
  let objectClassMap = classAttributeMapObjectClassMap classAttributeMap
  classId <- objectClassMapLookup objectId objectClassMap
  let streamMap = classAttributeMapStreamMap classAttributeMap
  streamMapLookup classId streamMap
