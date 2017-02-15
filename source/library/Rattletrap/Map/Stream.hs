module Rattletrap.Map.Stream
  ( StreamMap
  , makeStreamMap
  , streamMapLookup
  ) where

import Rattletrap.Map.Attribute
import Rattletrap.Primitive

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Word as Word

newtype StreamMap =
  StreamMap (HashMap.HashMap Word.Word32 AttributeMap)
  deriving (Eq, Show)

makeStreamMap
  :: HashMap.HashMap Word.Word32 (HashMap.HashMap Word.Word32 Word32)
  -> HashMap.HashMap Word.Word32 [Word32]
  -> [Word32]
  -> StreamMap
makeStreamMap attributeMap parentMap classIds =
  let getAttributes classId =
        HashMap.lookupDefault HashMap.empty classId attributeMap
      getParentClassIds classId =
        HashMap.lookupDefault [] (word32Value classId) parentMap
      toElement classId =
        let ownAttributes = getAttributes (word32Value classId)
            parentsAttributes =
              map
                (\parentClassId -> getAttributes (word32Value parentClassId))
                (getParentClassIds classId)
            attributes = ownAttributes : parentsAttributes
        in (word32Value classId, AttributeMap (HashMap.unions attributes))
  in StreamMap (HashMap.fromList (map toElement classIds))

streamMapLookup :: Word32 -> StreamMap -> Maybe AttributeMap
streamMapLookup classId (StreamMap streamMap) =
  HashMap.lookup (word32Value classId) streamMap
