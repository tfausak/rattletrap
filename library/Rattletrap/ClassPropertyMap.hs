module Rattletrap.ClassPropertyMap where

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

data ClassPropertyMap = ClassPropertyMap
  { classPropertyMapObjectMap :: Bimap.Bimap Word32 Text
  } deriving (Eq, Show)

makeClassPropertyMap :: List Text
                     -> List ClassMapping
                     -> List Cache
                     -> ClassPropertyMap
makeClassPropertyMap objects _classMappings _caches =
  ClassPropertyMap {classPropertyMapObjectMap = makeObjectMap objects}

makeObjectMap :: List Text -> Bimap.Bimap Word32 Text
makeObjectMap objects =
  Bimap.fromList (zip (map Word32 [0 ..]) (listValue objects))

getObjectName :: ClassPropertyMap -> Word32 -> Maybe Text
getObjectName classPropertyMap objectId =
  Bimap.lookup objectId (classPropertyMapObjectMap classPropertyMap)

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

getAttributeIdLimit :: ClassPropertyMap -> CompressedWord -> Word
getAttributeIdLimit _classPropertyMap _actorId = error "getAttributeIdLimit"

getAttributeName :: ClassPropertyMap -> CompressedWord -> CompressedWord -> Text
getAttributeName _classPropertyMap _actorId _attributeId =
  error "getAttributeName"
