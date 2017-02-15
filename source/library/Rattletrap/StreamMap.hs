module Rattletrap.StreamMap where

import Rattletrap.Primitive

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.Word as Word

newtype StreamMap = StreamMap
  { streamMapValue :: HashMap.HashMap Word.Word32 (Map.Map Word32 Word32)
  } deriving (Eq, Show)

makeStreamMap
  :: Map.Map Word32 (Map.Map Word32 Word32)
  -> Map.Map Word32 [Word32]
  -> [Word32]
  -> StreamMap
makeStreamMap attributeMap parentMap classIds =
  let getAttributes classId =
        Map.findWithDefault Map.empty classId attributeMap
      getParentClassIds classId = Map.findWithDefault [] classId parentMap
      toElement classId =
        let ownAttributes = getAttributes classId
            parentsAttributes = map getAttributes (getParentClassIds classId)
            attributes = ownAttributes : parentsAttributes
        in (word32Value classId, Map.unions attributes)
  in StreamMap (HashMap.fromList (map toElement classIds))

streamMapLookup :: Word32 -> StreamMap -> Maybe (Map.Map Word32 Word32)
streamMapLookup classId (StreamMap streamMap) =
  HashMap.lookup (word32Value classId) streamMap
