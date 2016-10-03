module Rattletrap.ClassAttributeMap where

import Rattletrap.Cache
import Rattletrap.ClassMapping
import Rattletrap.CompressedWord
import Rattletrap.Data
import Rattletrap.List
import Rattletrap.Text
import Rattletrap.Utility
import Rattletrap.Word32

import qualified Data.Bimap as Bimap
import qualified Data.Map as Map
import qualified Data.Set as Set

data ClassAttributeMap = ClassAttributeMap
  { classAttributeMapObjectMap :: Bimap.Bimap Word32 Text
  } deriving (Eq, Show)

makeClassAttributeMap :: List Text
                      -> List ClassMapping
                      -> List Cache
                      -> ClassAttributeMap
makeClassAttributeMap objects _classMappings _caches =
  ClassAttributeMap {classAttributeMapObjectMap = makeObjectMap objects}

makeObjectMap :: List Text -> Bimap.Bimap Word32 Text
makeObjectMap objects =
  Bimap.fromList (zip (map Word32 [0 ..]) (listValue objects))

getObjectName :: ClassAttributeMap -> Word32 -> Maybe Text
getObjectName classAttributeMap objectId =
  Bimap.lookup objectId (classAttributeMapObjectMap classAttributeMap)

getClassName :: Text -> Maybe Text
getClassName rawObjectName =
  Map.lookup (normalizeObjectName rawObjectName) objectClasses

normalizeObjectName :: Text -> Text
normalizeObjectName objectName =
  stringToText
    (replace
       "_[0-9]+$"
       ""
       (replace "^[A-Z_a-z]+[.]TheWorld:" "TheWorld:" (textToString objectName)))

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

getAttributeIdLimit :: ClassAttributeMap -> CompressedWord -> Word
getAttributeIdLimit _classAttributeMap _actorId = error "getAttributeIdLimit"

getAttributeName :: ClassAttributeMap
                 -> CompressedWord
                 -> CompressedWord
                 -> Text
getAttributeName _classAttributeMap _actorId _attributeId =
  error "getAttributeName"
