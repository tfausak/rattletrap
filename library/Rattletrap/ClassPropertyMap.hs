module Rattletrap.ClassPropertyMap where

import Rattletrap.Cache
import Rattletrap.ClassMapping
import Rattletrap.CompressedWord
import Rattletrap.List
import Rattletrap.Text
import Rattletrap.Word32

data ClassPropertyMap = ClassPropertyMap
  {
  } deriving (Eq, Ord, Show)

makeClassPropertyMap :: List Text
                     -> List ClassMapping
                     -> List Cache
                     -> ClassPropertyMap
makeClassPropertyMap _objects _classMappings _caches = ClassPropertyMap {}

getClassName :: ClassPropertyMap -> Word32 -> Text
getClassName _classPropertyMap _objectId = error "getClassName"

classHasLocation :: Text -> Bool
classHasLocation _className = error "classHasLocation"

classHasRotation :: Text -> Bool
classHasRotation _className = error "classHasRotation"

attributeIdLimit :: ClassPropertyMap -> CompressedWord -> Word
attributeIdLimit _classPropertyMap _actorId = error "attributeIdLimit"

getAttributeName :: ClassPropertyMap -> CompressedWord -> CompressedWord -> Text
getAttributeName _classPropertyMap _actorId _attributeId =
  error "getAttributeName"
